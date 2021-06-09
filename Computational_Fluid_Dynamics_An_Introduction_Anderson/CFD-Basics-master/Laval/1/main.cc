#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <cmath>
#include <algorithm>
#include <cstddef>

using namespace std;

const size_t WIDTH = 16;
const size_t DIGITS = 6;
const size_t OUTPUT_GAP = 100;
const size_t MAX_STEP = 10000;

const double G0 = 1.4;
const double G1 = 1 / G0;
const double G2 = G0 - 1;
const double R = 8.314e3; // J/(Kmol*K)
const double W_air = 28.9; // Kg/Kmol
const double Rw = R / W_air;

const double P0 = 10 * 101325.0; // Pa
const double Pe = 0.93 * P0; // Pa
const double T0 = 300.0; // K
const double rho0 = P0 / (R*T0);
const double a0 = sqrt(G0 * Rw * T0);
const double A_t = 1.0;

const double CFL = 0.5;
size_t iter_cnt = 0;
double t = 0.0;

ofstream history;

const int N = 31;
const double dx = 0.1;
vector<double> A(N, 0.0), x(N, 0.0);
vector<double> a(N), rho(N), T(N), V(N), P(N), Ma(N), mdot(N);
vector<double> rho_bar(N), T_bar(N), V_bar(N);
vector<double> drhodt(N, 0.0), dVdt(N, 0.0), dTdt(N, 0.0);
vector<double> drhodt_bar(N, 0.0), dVdt_bar(N, 0.0), dTdt_bar(N, 0.0);
vector<double> drhodt_av(N, 0.0), dVdt_av(N, 0.0), dTdt_av(N, 0.0);

void write_result()
{
    history << iter_cnt << '\t' << t << endl;
    for(int i = 0; i < N; ++i)
    {
        history << setw(WIDTH) << setprecision(DIGITS) << rho[i];
        history << setw(WIDTH) << setprecision(DIGITS) << V[i];
        history << setw(WIDTH) << setprecision(DIGITS) << T[i];
        history << setw(WIDTH) << setprecision(DIGITS) << P[i];
        history << setw(WIDTH) << setprecision(DIGITS) << Ma[i];
        history << setw(WIDTH) << setprecision(DIGITS) << mdot[i] << endl;
    }
}

void init()
{
    // Grid
    for(int i = 1; i < N; ++i)
        x[i] = x[i-1] + dx;

    // Cross-section area
    for(int i = 0; i < N; ++i)
    {
        if(x[i] <= 1.5)
            A[i] = A_t + 2.2*pow(x[i]-1.5, 2);
        else
            A[i] = A_t + 0.2223*pow(x[i]-1.5, 2);
    }
    // I.C.
    for(int i = 0; i < N; ++i)
    {
        rho[i] = 1.0 - 0.023 * x[i];
        T[i] = 1.0 - 0.009333 * x[i];
        V[i] = 0.05 + 0.11*x[i];
        P[i] = rho[i] * T[i];
        Ma[i] = V[i] / sqrt(T[i]);
        mdot[i] = rho[i] * V[i] * A[i];
    }

    // Output
    history.open("flow.txt");
    history << N << endl;
    for(int i = 0; i < N; ++i)
        history << x[i] << '\t';
    history << endl;
    for(int i = 0; i < N; ++i)
        history << A[i] << '\t';
    history << endl;
    write_result(); // Initial field
}

void loop()
{
    // Local Sound Speed
    for(int i = 0; i < N; ++i)
        a[i] = sqrt(T[i]);

    // Determine time-step
    double dt = 1.0;
    for(int i = 0; i < N; ++i)
    {
        double loc_dt = CFL * dx / (a[i] + abs(V[i]));
        if(loc_dt < dt)
            dt = loc_dt;
    }
    cout << "\tdt=" << dt << endl;

    // Forward derivative
    for(int i = 1; i < N-1; ++i)
    {
        const double drho = rho[i+1]-rho[i];
        const double dV = V[i+1]-V[i];
        const double dT = T[i+1] - T[i];
        const double dlnA = log(A[i+1])-log(A[i]);

        drhodt[i] = -rho[i]*dV/dx - rho[i]*V[i]*dlnA/dx - V[i]*drho/dx;
        dVdt[i] = -V[i]*dV/dx - G1*(dT/dx + T[i]/rho[i]*drho/dx);
        dTdt[i] = -V[i]*dT/dx - G2*T[i]*(dV/dx+V[i]*dlnA/dx);
    }

    // Prediction
    // Do NOT miss the bar value at left boundary, which will be used for backward derivatives!!!
    for(int i = 0; i < N; ++i)
    {
        rho_bar[i] = rho[i] + drhodt[i] * dt;
        V_bar[i] = V[i] + dVdt[i] * dt;
        T_bar[i] = T[i] + dTdt[i] * dt;
    }

    // Backward derivative
    for(int i = 1; i < N-1; ++i)
    {
        const double drho = rho_bar[i]-rho_bar[i-1];
        const double dV = V_bar[i]-V_bar[i-1];
        const double dT = T_bar[i] - T_bar[i-1];
        const double dlnA = log(A[i])-log(A[i-1]);

        drhodt_bar[i] = -rho_bar[i]*dV/dx - rho_bar[i]*V_bar[i]*dlnA/dx - V_bar[i]*drho/dx;
        dVdt_bar[i] = -V_bar[i]*dV/dx - G1*(dT/dx + T_bar[i]/rho_bar[i]*drho/dx);
        dTdt_bar[i] = -V_bar[i]*dT/dx - G2*T_bar[i]*(dV/dx+V_bar[i]*dlnA/dx);
    }

    // Correction
    for(int i = 1; i < N-1; ++i)
    {
        drhodt_av[i] = 0.5 * (drhodt[i] + drhodt_bar[i]);
        dVdt_av[i] = 0.5 * (dVdt[i] + dVdt_bar[i]);
        dTdt_av[i] = 0.5 * (dTdt[i] + dTdt_bar[i]);
    }
    for(int i = 1; i < N-1; ++i)
    {
        rho[i] += drhodt_av[i] * dt;
        V[i] += dVdt_av[i] * dt;
        T[i] += dTdt_av[i] * dt;
    }

    // B.C. at inlet
    // Subsonic inlet, with eigenvalue: u-a < 0, u > 0, u+a > 0
    // Let V determined from interior, rho and T fixed
    rho[0] = 1.0;
    V[0] = 2 * V[1] - V[2]; 
    T[0] = 1.0;
    P[0] = 1.0;

    // B.C. at outlet
    // Subsonic outlet, with eigenvalue: u-a < 0, u > 0, u+a > 0
    // Let P fixed, rho, V, T determined from interior
    rho[N-1] = 2 * rho[N-2] - rho[N-3];
    V[N-1] = 2 * V[N-2] - V[N-3];
    P[N-1] = Pe / P0;
    T[N-1] = P[N-1] / rho[N-1];

    // Mach Number
    for(int i = 0; i < N; ++i)
        Ma[i] = V[i] / sqrt(T[i]);

    // Pressure
    for(int i = 1; i < N-1; ++i)
        P[i] = rho[i] * T[i];

    // Mass flux
    for(int i = 0; i < N; ++i)
        mdot[i] = rho[i] * V[i] * A[i];

	t += dt;
}

double fder1(double fl, double fr)
{
    return 0.5 * (fr - fl) / dx;
}

bool check_convergence()
{
    double rms = 0.0;
    for(int i = 1; i < N-1; ++i)
        rms += pow(fder1(mdot[i-1], mdot[i+1]), 2);
    rms = sqrt(rms / (N-1));

    cout << "\tRMS=" << rms << endl;

    return rms < 1e-3 || iter_cnt > MAX_STEP;
}

void output_tecplot()
{
    if(iter_cnt % OUTPUT_GAP != 0)
        return;
    
    stringstream ss;
    ss << "iter" << iter_cnt << "_t=" << t << ".dat";
    ofstream fout(ss.str());
	fout << "Variables = ";
    fout << "x/L";
    fout << setw(WIDTH) << "A/A_t";
    fout << setw(WIDTH) << "rho/rho0";
    fout << setw(WIDTH) << "V/a0";
    fout << setw(WIDTH) << "T/T0";
    fout << setw(WIDTH) << "p/p0";
    fout << setw(WIDTH) << "Ma";
    fout << setw(WIDTH) << "mdot" << endl;
	fout << "Zone I=" << N << ", F = point" << endl;
    for(int i = 0; i < N; ++i)
    {
        fout << setw(WIDTH) << setprecision(DIGITS) << x[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << A[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << rho[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << V[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << T[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << P[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << Ma[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << mdot[i] << endl;
    }

    fout.close();
}

void solve()
{
    bool ok = false;
    while(!ok)
    {
        //output_tecplot();
        cout << "Iter" << ++iter_cnt << ":\n";
        loop();
        write_result();
        ok = check_convergence();
    }
}

void finalize()
{
    history.close();
}

int main(int argc, char *argv[])
{
    init();
    solve();
    finalize();
    return 0;
}

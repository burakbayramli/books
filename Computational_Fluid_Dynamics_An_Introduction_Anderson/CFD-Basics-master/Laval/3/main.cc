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
const size_t OUTPUT_GAP = 50;
const size_t MAX_STEP = 2000;

const double G0 = 1.4;
const double G1 = 1 / G0;
const double G2 = G0 - 1;
const double G3 = G2 * G1;
const double R = 8.314e3; // J/(Kmol*K)
const double W_air = 28.9; // Kg/Kmol
const double Rw = R / W_air;

const double P0 = 10 * 101325.0; // Pa
const double Pe = 0.6784 * P0;
const double T0 = 300.0; // K
const double rho0 = P0 / (R*T0);
const double a0 = sqrt(G0 * Rw * T0);
const double A_star = 1.0;

const double Cx = 0.2;
const double CFL = 0.5;
size_t iter_cnt = 0;
double t = 0.0;

ofstream result;

const int N = 61;
const double L = 3.0;
const double dx = L / (N - 1);
vector<double> A(N, 0.0), x(N, 0.0), lnA(N, 0.0), dlnAdx(N, 0.0);
vector<double> a(N), rho(N), T(N), V(N), P(N), Ma(N); // Normalized quantities
vector<double> U1(N), U2(N), U3(N), F1(N), F2(N), F3(N), J2(N);
vector<double> dU1dt(N, 0.0), dU2dt(N, 0.0), dU3dt(N, 0.0);
vector<double> U1_bar(N, 0.0), U2_bar(N, 0.0), U3_bar(N, 0.0);
vector<double> dU1dt_bar(N, 0.0), dU2dt_bar(N, 0.0), dU3dt_bar(N, 0.0);
vector<double> dU1dt_av(N, 0.0), dU2dt_av(N, 0.0), dU3dt_av(N, 0.0);
vector<double> S1(N, 0.0), S2(N, 0.0), S3(N, 0.0);
vector<double> S1_bar(N, 0.0), S2_bar(N, 0.0), S3_bar(N, 0.0), P_bar(N, 0.0);

void write_result()
{
    result << iter_cnt << '\t' << t << endl;
    for(int i = 0; i < N; ++i)
    {
        result << setw(WIDTH) << setprecision(DIGITS) << rho[i];
        result << setw(WIDTH) << setprecision(DIGITS) << V[i];
        result << setw(WIDTH) << setprecision(DIGITS) << T[i];
        result << setw(WIDTH) << setprecision(DIGITS) << P[i];
        result << setw(WIDTH) << setprecision(DIGITS) << Ma[i];
        result << setw(WIDTH) << setprecision(DIGITS) << U1[i];
        result << setw(WIDTH) << setprecision(DIGITS) << U2[i];
        result << setw(WIDTH) << setprecision(DIGITS) << U3[i];
        result << endl;
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
        A[i] = A_star + 2.2*pow(x[i]-1.5, 2);
        lnA[i] = log(A[i]);
        dlnAdx[i] = 4.4 * (x[i]-1.5) / A[i];
    }
    
    // I.C.
    for(int i = 0; i < N; ++i)
    {
        if(x[i] <= 0.5)
        {
            rho[i] = 1.0;
            T[i] = 1.0;
        }
        else if(x[i] <= 1.5)
        {
            rho[i] = 1.0 - 0.366 * (x[i] - 0.5);
            T[i] = 1.0 - 0.167 * (x[i] - 0.5);
        }
        else if(x[i] <= 2.1)
        {
            rho[i] = 0.634 - 0.702 * (x[i] - 1.5);
            T[i] = 0.833 - 0.4908 * (x[i] - 1.5);
        }
		else
		{
			rho[i] = 0.5892 + 0.10228 * (x[i] - 2.1);
			T[i] = 0.93968 + 0.0622 * (x[i] - 2.1);
		}

        U1[i] = rho[i] * A[i];
        U2[i] = 0.59;
        V[i] = U2[i] / U1[i];
        P[i] = rho[i] * T[i];
        Ma[i] = V[i] / sqrt(T[i]);
        U3[i] = U1[i] * (T[i] / G2 + G0 / 2 * pow(V[i], 2));
    }
    
    // Output
    result.open("flow.txt");
    result << N << endl;
    for(int i = 0; i < N; ++i)
        result << x[i] << '\t';
    result << endl;
    for(int i = 0; i < N; ++i)
        result << A[i] << '\t';
    result << endl;
    write_result();
}

void finalize()
{
    result.close();
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
    
    // Flux
    for(int i = 0; i < N; ++i)
    {
        double tmp = pow(U2[i], 2) / U1[i];
        F1[i] = U2[i];
        F2[i] = tmp + G3 * (U3[i] - 0.5 * G0 * tmp);
        F3[i] = G0 * U2[i] * U3[i] / U1[i] - G0 * G2 / 2 * pow(U2[i], 3) / pow(U1[i], 2);
        J2[i] = G3 * (U3[i] - 0.5 * G0 * tmp) * dlnAdx[i];
    }

    // Forward derivative
    for(int i = 1; i < N-1; ++i)
    {
        dU1dt[i] = -(F1[i+1] - F1[i])/dx;
        dU2dt[i] = -(F2[i+1] - F2[i])/dx + J2[i];
        dU3dt[i] = -(F3[i+1] - F3[i])/dx;
    }

	// Artificial Viscosity
	for (int i = 1; i < N - 1; ++i)
	{
		double tmp = Cx * abs(P[i + 1] - 2 * P[i] + P[i - 1]) / (P[i + 1] + 2 * P[i] + P[i - 1]);

		S1[i] = tmp * (U1[i + 1] - 2 * U1[i] + U1[i - 1]);
		S2[i] = tmp * (U2[i + 1] - 2 * U2[i] + U2[i - 1]);
		S3[i] = tmp * (U3[i + 1] - 2 * U3[i] + U3[i - 1]);
	}

    // Prediction
	U1_bar = U1;
	U2_bar = U2;
	U3_bar = U3;
    for(int i = 1; i < N-1; ++i)
    {
        U1_bar[i] += (dU1dt[i] * dt + S1[i]);
        U2_bar[i] += (dU2dt[i] * dt + S2[i]);
        U3_bar[i] += (dU3dt[i] * dt + S3[i]);

		double loc_rho = U1_bar[i] / A[i];
		double loc_V = U2_bar[i] / U1_bar[i];
		double loc_T = G2 * (U3_bar[i] / U1_bar[i] - 0.5 * G0 * pow(loc_V, 2));
		P_bar[i] = loc_rho * loc_T;
	}
    
    // Flux
    for(int i = 0; i < N; ++i)
    {
        double tmp = pow(U2_bar[i], 2) / U1_bar[i];
        F1[i] = U2_bar[i];
        F2[i] = tmp + G3 * (U3_bar[i] - 0.5 * G0 * tmp);
        F3[i] = G0 * U2_bar[i] * U3_bar[i] / U1_bar[i] - G0 * G2 / 2 * pow(U2_bar[i], 3) / pow(U1_bar[i], 2);
        J2[i] = G3 * (U3_bar[i] - 0.5 * G0 * tmp) * dlnAdx[i];
    }

    // Backward derivative
    for(int i = 1; i < N-1; ++i)
    {
        dU1dt_bar[i] = -(F1[i] - F1[i-1])/dx;
        dU2dt_bar[i] = -(F2[i] - F2[i-1])/dx + J2[i];
        dU3dt_bar[i] = -(F3[i] - F3[i-1])/dx;
    }

	// Artificial Viscosity
	for (int i = 1; i < N - 1; ++i)
	{
		double tmp = Cx * abs(P_bar[i + 1] - 2 * P_bar[i] + P_bar[i - 1]) / (P_bar[i + 1] + 2 * P_bar[i] + P_bar[i - 1]);

		S1_bar[i] = tmp * (U1_bar[i + 1] - 2 * U1_bar[i] + U1_bar[i - 1]);
		S2_bar[i] = tmp * (U2_bar[i + 1] - 2 * U2_bar[i] + U2_bar[i - 1]);
		S3_bar[i] = tmp * (U3_bar[i + 1] - 2 * U3_bar[i] + U3_bar[i - 1]);
	}

    // Correction
    for(int i = 1; i < N-1; ++i)
    {
        dU1dt_av[i] = 0.5 * (dU1dt[i] + dU1dt_bar[i]);
        dU2dt_av[i] = 0.5 * (dU2dt[i] + dU2dt_bar[i]);
        dU3dt_av[i] = 0.5 * (dU3dt[i] + dU3dt_bar[i]);
    }
    for(int i = 1; i < N-1; ++i)
    {
        U1[i] += (dU1dt_av[i] * dt + S1_bar[i]);
        U2[i] += (dU2dt_av[i] * dt + S2_bar[i]);
        U3[i] += (dU3dt_av[i] * dt + S3_bar[i]);
    }

    // B.C. at inlet
    // Subsonic inlet, one determined from interior
    // Eigenvalue: u-a < 0, u > 0, u+a > 0
    rho[0] = 1.0;
    U1[0] = rho[0] * A[0];
    U2[0] = 2 * U2[1] - U2[2];
    V[0] = U2[0] / U1[0];
    U3[0] = U1[0] * (T[0] / G2 + 0.5 * G0 * pow(V[0], 2));

    // B.C. at outlet
    // Subsonic outlet, one determined from interior
    // Eigenvalue: u-a < 0, u > 0, u+a > 0
	U1[N - 1] = 2 * U1[N - 2] - U1[N - 3];
	U2[N - 1] = 2 * U2[N - 2] - U2[N - 3];
	V[N - 1] = U2[N - 1] / U1[N - 1];
	U3[N - 1] = Pe / P0 * A[N - 1] / G2 + 0.5 * G0 * U2[N - 1] * V[N - 1];
    
    // Mapback to primitive variables
    for(int i = 0; i < N; ++i)
    {
        rho[i] = U1[i] / A[i];
        V[i] = U2[i] / U1[i];
        T[i] = G2 * (U3[i] / U1[i] - 0.5 * G0 * pow(V[i], 2));
        
        Ma[i] = V[i] / sqrt(T[i]);
        P[i] = rho[i] * T[i];
    }
	
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
    {
        double mdot_L = rho[i-1] * V[i-1] * A[i-1];
        double mdot_R = rho[i+1] * V[i+1] * A[i+1];
        rms += pow(fder1(mdot_L, mdot_R), 2);
    }
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
    fout << setw(WIDTH) << "A/A_star";
    fout << setw(WIDTH) << "rho/rho0";
    fout << setw(WIDTH) << "V/a0";
    fout << setw(WIDTH) << "T/T0";
    fout << setw(WIDTH) << "p/p0";
    fout << setw(WIDTH) << "Ma" << endl;
	fout << "Zone I=" << N << ", F = point" << endl;
    for(int i = 0; i < N; ++i)
    {
        fout << setw(WIDTH) << setprecision(DIGITS) << x[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << A[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << rho[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << V[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << T[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << P[i];
        fout << setw(WIDTH) << setprecision(DIGITS) << Ma[i] << endl;
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

int main(int argc, char *argv[])
{
    init();
    solve();
    finalize();
    return 0;
}

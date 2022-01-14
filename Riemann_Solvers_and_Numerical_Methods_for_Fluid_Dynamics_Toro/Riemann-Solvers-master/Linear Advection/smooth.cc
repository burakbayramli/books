// 
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cmath>
#include <exception>
#include <algorithm>
#include <functional>
#include <string>

using namespace std;

const double a = 1.0;
const double ALPHA = 1.0;
const double BETA = 8.0;

const double CFL = 0.8;

double u0(double x)
{
    if (abs(x) <= 1.0)
        return ALPHA * exp(-BETA * pow(x, 2));
    else
        return 0.f;
}

const int NumOfPnt = 101;
const double xL = -1.0, xR = 1.0;
const double L = xR - xL;
const double dx = (xR - xL)/(NumOfPnt - 1);
vector<double> x(NumOfPnt, 0.f);
vector<double> u_prev(NumOfPnt+2, 0.f);
vector<double> u_cur(NumOfPnt+2, 0.f);

const int NumOfStep = 12500;
const double dt = CFL * dx / a;
const double c = a * dt / dx;

double flux(double u)
{
    return a * u;
}

void write_u(ofstream &f, const vector<double> &x)
{
    const int n = x.size()-1;

    f << x[1];
    for (int i = 2; i < n; i++)
        f << '\t' << x[i];
    f << endl;
}

void write_x(ofstream &f, const vector<double> &x)
{
    f << x[0];
    for(int k = 1; k < NumOfPnt; k++)
        f << '\t' << x[k];
    f << endl;
}

void Exact()
{
    ofstream fout("Exact.txt");
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    write_x(fout, x);

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Periodical BC
    u_prev[0] = u_prev[NumOfPnt];
    u_prev[NumOfPnt+1] = u_prev[1];

    write_u(fout, u_prev);

    //Iterate over time
    double t = 0;
    for(int k = 1; k < NumOfStep; k++)
    {
        t += dt;
        for(int i = 1; i <= NumOfPnt; i++)
        {
            double x_origin = x[i-1] - a * t;
            while (x_origin < xL)
                x_origin += L;
            while (x_origin > xR)
                x_origin -= L;
            u_cur[i] = u0(x_origin);
        }
        
        //Periodical BC
        u_cur[0] = u_cur[NumOfPnt];
        u_cur[NumOfPnt+1] = u_cur[1];

        write_u(fout, u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

void CIR()
{
    ofstream fout("CIR.txt");
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    write_x(fout, x);

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Periodical BC
    u_prev[0] = u_prev[NumOfPnt];
    u_prev[NumOfPnt+1] = u_prev[1];

    write_u(fout, u_prev);

    const double a_plus = (a + abs(a))/2, a_minus = (a - abs(a))/2;
    const double c_plus = dt * a_plus / dx, c_minus = dt * a_minus / dx;

    const double b_l1 = c_plus;
    const double b_0 = 1-abs(CFL);
    const double b_r1 = -c_minus;

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        for(int i = 1; i <= NumOfPnt; i++)
            u_cur[i] = b_l1 * u_prev[i-1] + b_0 * u_prev[i] + b_r1 * u_prev[i+1];
        
        //Periodical BC
        u_cur[0] = u_cur[NumOfPnt];
        u_cur[NumOfPnt+1] = u_cur[1];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

void Lax_Friedrichs()
{
    ofstream fout("Lax-Friedrichs.txt");
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    write_x(fout, x);

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Periodical BC
    u_prev[0] = u_prev[NumOfPnt];
    u_prev[NumOfPnt+1] = u_prev[1];

    write_u(fout, u_prev);

    const double b_l1 = (1+CFL)/2;
    const double b_0 = 0;
    const double b_r1 = (1-CFL)/2;

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        for(int i = 1; i <= NumOfPnt; i++)
            u_cur[i] = b_l1 * u_prev[i-1] + b_r1 * u_prev[i+1];
        
        //Periodical BC
        u_cur[0] = u_cur[NumOfPnt];
        u_cur[NumOfPnt+1] = u_cur[1];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

void Lax_Wendroff()
{
    ofstream fout("Lax-Wendroff.txt");
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    write_x(fout, x);

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Periodical BC
    u_prev[0] = u_prev[NumOfPnt];
    u_prev[NumOfPnt+1] = u_prev[1];

    write_u(fout, u_prev);

    const double b_l1 = CFL*(1+CFL)/2;
    const double b_0 = 1-pow(CFL, 2);
    const double b_r1 = -CFL*(1-CFL)/2;

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        for(int i = 1; i <= NumOfPnt; i++)
            u_cur[i] = b_l1 * u_prev[i-1] + b_0 * u_prev[i] + b_r1 * u_prev[i+1];
        
        //Periodical BC
        u_cur[0] = u_cur[NumOfPnt];
        u_cur[NumOfPnt+1] = u_cur[1];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

void Warming_Beam()
{
    ofstream fout("Warming-Beam.txt");
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    for(int k = 0; k < NumOfPnt; k++)
        fout << x[k] << '\t';
    fout << endl;

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Periodical BC
    u_prev[0] = u_prev[NumOfPnt];
    u_prev[NumOfPnt+1] = u_prev[1];

    write_u(fout, u_prev);

    const double b_l2 = CFL * (CFL - 1) / 2;
    const double b_l1 = CFL * (2 - CFL);
    const double b_0 = (CFL - 1) * (CFL - 2) /2;

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        u_cur[1] = b_l2 * u_prev[NumOfPnt-1] + b_l1 * u_prev[0] + b_0 * u_prev[1];
        for(int i = 2; i <= NumOfPnt; i++)
            u_cur[i] = b_l2 * u_prev[i-2] + b_l1 * u_prev[i-1] + b_0 * u_prev[i];
        
        //Periodical BC
        u_cur[0] = u_cur[NumOfPnt];
        u_cur[NumOfPnt+1] = u_cur[1];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

void Godunov()
{
    ofstream fout("Godunov.txt");
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    write_x(fout, x);

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Periodical BC
    u_prev[0] = u_prev[NumOfPnt];
    u_prev[NumOfPnt+1] = u_prev[1];

    write_u(fout, u_prev);

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        for(int i = 1; i <= NumOfPnt; i++)
        {
            double godunov_flux = 0;
            if (a > 0)
                godunov_flux = flux(u_prev[i-1]) - flux(u_prev[i]);
            else
                godunov_flux = flux(u_prev[i]) - flux(u_prev[i+1]);

            u_cur[i] = u_prev[i] + dt/dx * godunov_flux;
        }
        
        //Periodical BC
        u_cur[0] = u_cur[NumOfPnt];
        u_cur[NumOfPnt+1] = u_cur[1];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

void WAF(double alpha)
{
    stringstream ss;
    ss << "WAF_" << alpha << ".txt";
    ofstream fout(ss.str());
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    write_x(fout, x);

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Periodical BC
    u_prev[0] = u_prev[NumOfPnt];
    u_prev[NumOfPnt+1] = u_prev[1];

    write_u(fout, u_prev);

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        for(int i = 1; i <= NumOfPnt; i++)
        {
            const double fl = flux(u_prev[i-1]);
            const double fm = flux(u_prev[i]);
            const double fr = flux(u_prev[i+1]);

            const double fwaf_l = 0.5 * (1+2*alpha*c) * fl + 0.5 * (1-2*alpha*c) *fm;
            const double fwaf_r = 0.5 * (1+2*alpha*c) * fm + 0.5 * (1-2*alpha*c) *fr;

            u_cur[i] = u_prev[i] + dt/dx * (fwaf_l - fwaf_r);
        }
        
        //Periodical BC
        u_cur[0] = u_cur[NumOfPnt];
        u_cur[NumOfPnt+1] = u_cur[1];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "alpha=" << alpha << " Done!" << endl;
}

double ULTRAA(double r)
{
    if(r < 0)
        return 1.0;
    else if(r < fabs(c)/(1-fabs(c)))
        return 1 - 2 * r * (1-fabs(c)) / fabs(c);
    else
        return -1.0;
}

double SUPERA(double r)
{
    if(r < 0)
        return 1.0;
    else if(r < 0.5)
        return 1 - 2 * (1-fabs(c)) * r;
    else if(r < 1)
        return fabs(c);
    else if(r < 2)
        return 1 - (1-fabs(c)) * r;
    else
        return 2 * fabs(c) - 1;
}

double VANLEER(double r)
{
    if(r < 0)
        return 1.0;
    else
        return 1 - 2 * (1-fabs(c)) * r / (1 + r);
}

double VANALBADA(double r)
{
    if(r < 0)
        return 1.0;
    else
        return 1 - (1-fabs(c)) * r * (1 + r) / (1 + pow(r, 2.0));
}

double MINA(double r)
{
    if(r < 0)
        return 1.0;
    else if(r < 1)
        return 1 - (1-fabs(c)) * r;
    else
        return fabs(c);
}

double ratio_of_upwind_change(const vector<double> &u, int i)
{
    const auto N = u.size() - 2;

    double loc_du = u[i+1] - u[i];
    double upw_du = 0.0;
    if(a > 0)
        upw_du = u[i] - u[i-1];
    else
    {
        // For periodical B.C.
        if(i==N)
            upw_du = u[2] - u[i+1];
        else 
            upw_du = u[i+2] - u[i+1];
    }

    return upw_du / loc_du;
}

void TVD_WAF(function<double(double)> phi, const string &NameOfLimiter)
{
    stringstream ss;
    ss << "TVD_WAF(" << NameOfLimiter << ").txt";
    ofstream fout(ss.str());
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    write_x(fout, x);

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Periodical BC
    u_prev[0] = u_prev[NumOfPnt];
    u_prev[NumOfPnt+1] = u_prev[1];

    write_u(fout, u_prev);

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        for(int i = 1; i <= NumOfPnt; i++)
        {
            const double fl = flux(u_prev[i-1]);
            const double fm = flux(u_prev[i]);
            const double fr = flux(u_prev[i+1]);

            const double rl = ratio_of_upwind_change(u_prev, i-1);
            const double rr = ratio_of_upwind_change(u_prev, i);

            const double pl = phi(rl);
            const double pr = phi(rr);

            const double fwaf_l = 0.5 * (1+pl) * fl + 0.5 * (1-pl) *fm;
            const double fwaf_r = 0.5 * (1+pr) * fm + 0.5 * (1-pr) *fr;

            u_cur[i] = u_prev[i] + dt/dx * (fwaf_l - fwaf_r);
        }
        
        //Periodical BC
        u_cur[0] = u_cur[NumOfPnt];
        u_cur[NumOfPnt+1] = u_cur[1];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

int main(int argc, char *argv[])
{
    cout << "===================================================" << endl;
    cout << "Numerical solution of the Linear Advection Problem:" <<endl; 
    cout << "\t u_t + a * u_x = 0"<<endl; 
    cout << "with smooth initial velocity profile."<<endl;
    cout << "===================================================" << endl;
    cout << "a = " << a << endl;
    cout << "CFL = " << CFL << endl;

    //Grid coordinate & initial velocity profile
    cout << "Init grid..." << endl;
    x[0] = xL;
    for(int i = 1; i < NumOfPnt; i++)
        x[i] = x[i-1] + dx;

    cout << "=======================Exact=======================" << endl;
    Exact();
    cout << "========================CIR========================" << endl;
    CIR();
    cout << "===================Lax-Friedrichs==================" << endl;
    Lax_Friedrichs();
    cout << "====================Lax-Wendroff===================" << endl;
    Lax_Wendroff();
    cout << "====================Warming-Beam===================" << endl;
    Warming_Beam();
    cout << "=======================Godunov=====================" << endl;
    Godunov();
    cout << "=================WAF(Lax-Wendroff)=================" << endl;
    WAF(0.5);
    cout << "==========WAF(Godunov 1st-order upwind)============" << endl;
    WAF(0.5/fabs(c));
    cout << "=========WAF(Godunov 1st-order centered)===========" << endl;
    WAF(1.0);
    cout << "====================WAF(FORCE)=====================" << endl;
    WAF((1+pow(c, 2))/(4*pow(c, 2)));
    cout << "===============WAF(Lax-Friedrichs)=================" << endl;
    WAF(0.5/pow(c, 2));
    cout << "===========TVD version of WAF(ULTRAA)==============" << endl;
    TVD_WAF(ULTRAA, "ULTRAA");
    cout << "===========TVD version of WAF(SUPERA)==============" << endl;
    TVD_WAF(SUPERA, "SUPERA");
    cout << "==========TVD version of WAF(VANLEER)==============" << endl;
    TVD_WAF(VANLEER, "VANLEER");
    cout << "=========TVD version of WAF(VANALBADA)=============" << endl;
    TVD_WAF(VANALBADA, "VANALBADA");
    cout << "============TVD version of WAF(MINA)===============" << endl;
    TVD_WAF(MINA, "MINA");
    cout << "=========================End=======================" << endl;
    return 0;
}

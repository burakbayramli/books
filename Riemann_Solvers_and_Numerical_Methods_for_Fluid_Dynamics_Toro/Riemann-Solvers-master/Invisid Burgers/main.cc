#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <exception>
#include <algorithm>

using namespace std;

const double CFL = 0.8;

const double x1 = 0.5, x2 = 1.0;
const double u1 = -0.5, u2 = 1.0, u3 = 0.0;
const double S = (u2 + u3)/2;
double u0(double x)
{
    if (x <= x1)
        return u1;
    else if(x <= x2)
        return u2;
    else
        return u3;
}

double exact_riemann(double x, double t)
{
    const double xb1 = x1 + u1 * t;
    const double xb2 = x1 + u2 * t;
    const double xb3 = x2 + S * t;

    if (x <= xb1)
        return u1;
    else
    {
        if(xb2 <= xb3)
        {
            if(x <= xb2)
                return u1 + (x - xb1)/(xb2 - xb1) * (u2 - u1);
            else if(x <= xb3)
                return u2;
            else
                return u3;
        }
        else
        {
            if(x <= xb3)
                return u1 + (x - xb1)/(xb3 - xb1) * (u2 - u1);
            else
                return u3;
        }
    }
}

const int NumOfPnt = 76;
const double xL = 0, xR = 1.5;
const double L = xR - xL;
const double dx = L/(NumOfPnt - 1);

vector<double> x(NumOfPnt, 0.f);

vector<double> u_prev(NumOfPnt+2, 0.f);
vector<double> u_cur(NumOfPnt+2, 0.f);

const int NumOfStep = 12500;
const double dt = CFL * dx / u2;

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
    
    //Transparent BC
    u_prev[0] = u_prev[1];
    u_prev[NumOfPnt+1] = u_prev[NumOfPnt];

    write_u(fout, u_prev);

    //Iterate over time
    double t = 0;
    for(int k = 1; k < NumOfStep; k++)
    {
        t += dt;
        for(int i = 1; i <= NumOfPnt; i++)
            u_cur[i] = exact_riemann(x[i-1], t);
        
        //Transparent BC
        u_cur[0] = u_cur[1];
        u_cur[NumOfPnt+1] = u_cur[NumOfPnt];

        write_u(fout, u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}


void CIR()
{
    //Upwind
    ofstream fout("CIR.txt");
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    write_x(fout, x);

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Transparent BC
    u_prev[0] = u_prev[1];
    u_prev[NumOfPnt+1] = u_prev[NumOfPnt];

    write_u(fout, u_prev);

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        for(int i = 1; i <= NumOfPnt; i++)
        {
            if(u_prev[i] > 0)
                u_cur[i] = u_prev[i] - dt * u_prev[i] * (u_prev[i] - u_prev[i-1]) / dx;
            else
                u_cur[i] = u_prev[i] - dt * u_prev[i] * (u_prev[i+1] - u_prev[i]) / dx;
        }
        
        //Transparent BC
        u_cur[0] = u_cur[1];
        u_cur[NumOfPnt+1] = u_cur[NumOfPnt];

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
    
    //Transparent BC
    u_prev[0] = u_prev[1];
    u_prev[NumOfPnt+1] = u_prev[NumOfPnt];

    write_u(fout, u_prev);

    const double b_l1 = (1+CFL)/2;
    const double b_0 = 0;
    const double b_r1 = (1-CFL)/2;

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        for(int i = 1; i <= NumOfPnt; i++)
            u_cur[i] = b_l1 * u_prev[i-1] + b_r1 * u_prev[i+1];
        
        //Transparent BC
        u_cur[0] = u_cur[1];
        u_cur[NumOfPnt+1] = u_cur[NumOfPnt];

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
    
    //Transparent BC
    u_prev[0] = u_prev[1];
    u_prev[NumOfPnt+1] = u_prev[NumOfPnt];

    write_u(fout, u_prev);

    const double b_l1 = CFL*(1+CFL)/2;
    const double b_0 = 1-pow(CFL, 2);
    const double b_r1 = -CFL*(1-CFL)/2;

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        for(int i = 1; i <= NumOfPnt; i++)
            u_cur[i] = b_l1 * u_prev[i-1] + b_0 * u_prev[i] + b_r1 * u_prev[i+1];
        
        //Transparent BC
        u_cur[0] = u_cur[1];
        u_cur[NumOfPnt+1] = u_cur[NumOfPnt];

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
    
    //Transparent BC
    u_prev[0] = u_prev[1];
    u_prev[NumOfPnt+1] = u_prev[NumOfPnt];

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
        
        //Transparent BC
        u_cur[0] = u_cur[1];
        u_cur[NumOfPnt+1] = u_cur[NumOfPnt];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

inline double flux(double u)
{
    return 0.5 * pow(u, 2);
}

double intermediate_u(double ul, double ur)
{
    double u = 0;
    if(ul > ur)
    {
        //Shock wave
        double S = (ul + ur)/2;
        if (S >= 0)
            u = ul;
        else
            u = ur;
    }
    else
    {
        //Rarefaction wave
        if(ul >= 0)
            u = ul;
        else if(ur <= 0)
            u = ur;
        else
            u = 0;
    }

    return u;
}

double intercell_speed(double ul, double ur)
{
    if(ul >= ur)
        return abs(ul + ur)/2;
    else
        return max(abs(ul), abs(ur));
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
    
    //Transparent BC
    u_prev[0] = u_prev[1];
    u_prev[NumOfPnt+1] = u_prev[NumOfPnt];

    write_u(fout, u_prev);

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        vector<double> u_mid(NumOfPnt+1, 0);
        double S_max = 0;

        for(int i = 0; i <= NumOfPnt; i++)
        {
            u_mid[i] = intermediate_u(u_prev[i], u_prev[i+1]);
            S_max = max(S_max, intercell_speed(u_prev[i], u_prev[i+1]));
        }

        for(int i = 1; i <= NumOfPnt; i++)
        {
            double godunov_flux = flux(u_mid[i-1]) - flux(u_mid[i]);
            u_cur[i] = u_prev[i] + CFL/S_max * godunov_flux;
        }
        
        //Transparent BC
        u_cur[0] = u_cur[1];
        u_cur[NumOfPnt+1] = u_cur[NumOfPnt];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

double osher_intercell_flux(double u0, double u1)
{
    if(u0>0 && u1>0)
        return flux(u0);
    else if(u0<0 && u1<0)
        return flux(u1);
    else if(u0>=0 && 0 >= u1)
        return flux(u0) + flux(u1);
    else
        return 0.0;
}

void Osher()
{
    ofstream fout("Osher.txt");
    if(!fout)
        throw "Failed to create file!\n";

    fout << NumOfStep << "\t" << NumOfPnt << endl;
    write_x(fout, x);

    //IC
    for(int i = 1; i <= NumOfPnt; i++)
        u_prev[i] = u0(x[i-1]);
    
    //Transparent BC
    u_prev[0] = u_prev[1];
    u_prev[NumOfPnt+1] = u_prev[NumOfPnt];

    write_u(fout, u_prev);

    //Iterate over time
    for(int k = 1; k < NumOfStep; k++)
    {
        vector<double> u_mid(NumOfPnt+1, 0);
        double S_max = 0;

        for(int i = 0; i <= NumOfPnt; i++)
        {
            u_mid[i] = intermediate_u(u_prev[i], u_prev[i+1]);
            S_max = max(S_max, intercell_speed(u_prev[i], u_prev[i+1]));
        }

        for(int i = 1; i <= NumOfPnt; i++)
        {
            double osher_flux_l = osher_intercell_flux(u_prev[i-1], u_prev[i]);
            double osher_flux_r = osher_intercell_flux(u_prev[i], u_prev[i+1]);
            u_cur[i] = u_prev[i] + CFL/S_max * (osher_flux_l - osher_flux_r);
        }
        
        //Transparent BC
        u_cur[0] = u_cur[1];
        u_cur[NumOfPnt+1] = u_cur[NumOfPnt];

        write_u(fout, u_cur);
        u_prev.swap(u_cur);
    }
    fout.close();
    cout << "Done!" << endl;
}

int main(int argc, char *argv[])
{
    cout << "===================================================" << endl;
    cout << "Numerical solution of the Inviscid Burgers Equation:" <<endl; 
    cout << "\t u_t + u * u_x = 0"<<endl; 
    cout << "with discontinous initial velocity profile."<<endl;
    cout << "===================================================" << endl;
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
    cout << "========================Osher======================" << endl;
    Osher();
    cout << "=========================End=======================" << endl;

    return 0;
}

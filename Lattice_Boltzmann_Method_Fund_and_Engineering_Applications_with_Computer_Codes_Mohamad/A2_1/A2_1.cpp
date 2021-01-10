#include <float.h>
#include <iostream>
#include <math.h>
#include<fstream>
using namespace std;

int main()
{
	const int m = 100;
	float dens, f1[m + 1], f2[m + 1], rho[m + 1], x[m + 1], feq1, feq2, u=0.1, ck, csq, alpha, omega, twall;
	int i, dt = 1, dx = 1, mstep=400, kk;
	x[0] = 0.0;
	for ( i = 1; i <= m; i++)
	{
		x[i] = x[i - 1] + dx;
	}
	ck = dx / dt;
	csq = ck*ck;
	alpha = 0.25;
	omega = 1.0 / (alpha / (dt*csq) + 0.5);
	twall = 1.0;
	//初始化
	for ( i = 0; i <= m; i++)
	{
		rho[i] = 0.0;
		f1[i] = 0.5*rho[i];
		f2[i] = 0.5*rho[i];
	}
	//主循环
	for ( kk = 1; kk <= mstep; kk++)
	{
		//碰撞
		for ( i = 0; i <= m; i++)
		{
			rho[i] = f1[i] + f2[i];
			feq1 = 0.5*rho[i] * (1.0 + u / ck);
			feq2 = 0.5*rho[i] * (1.0 - u / ck);
			f1[i] = (1 - omega)*f1[i] + omega*feq1;
			f2[i] = (1 - omega)*f2[i] + omega*feq2;
		}
		//流动
		for ( i = 0; i < m; i++)
		{
			f2[i] = f2[i + 1];
		}
		for ( i = m; i > 0; i--)
		{
			f1[i] = f1[i - 1];
		}
		//边界条件
		f1[0] = twall - f2[0];
		f1[m] = f1[m - 1];
		f2[m] = f2[m - 1];
	}
	rho[i] = f1[i] + f2[i];
	ofstream fout;
	fout.open("data.dat", ios::app);
	fout << "TITLE =  \"A1.1\" \nvariables = \"x\", \"rho\"\nZone I = 1,J = 100 F=POINT" << endl;
	for (i = 0; i < m; i++)
	{
		fout << x[i] << "\t" << rho[i] << endl;
	}
	fout.close();
	return 0;
}


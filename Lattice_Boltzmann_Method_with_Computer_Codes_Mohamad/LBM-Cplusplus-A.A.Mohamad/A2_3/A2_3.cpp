#include <float.h>
#include <iostream>
#include <math.h>
#include<fstream>
using namespace std;


int main()
{
	const int n = 100, m = 100, mstep=200;
	float f1[n + 1][m + 1], f2[n + 1][m + 1], f3[n + 1][m + 1], f4[n + 1][m + 1], rho[n + 1][m + 1], x[n + 1], y[m + 1];
	float feq1, feq2, feq3, feq4, u = 0.1, v = 0.2, ck, csq, alpha, omega;
	int i, j, dx=1, dy=1, dt=1, kk;
	x[0] = 0.0, y[0] = 0.0;
	for ( i = 1; i <= n; i++)
	{
		x[i] = x[i - 1] + dx;
	}
	for ( j = 1; j <= m; j++)
	{
		y[j] = y[j - 1] + dy;
	}
	ck = dx / dt;
	csq = ck*ck;
	alpha = 1.0;
	omega = 1.0 / (2 * alpha / (dt*csq) + 0.5);
	//初始化
	for ( j = 0; j <= m; j++)
	{
		for ( i = 0; i <= n; i++)
		{
			rho[i][j] = 0.0;
		}
	}
	for ( j = 0; j <= m; j++)
	{
		for ( i = 0; i <= n; i++)
		{
			f1[i][j] = 0.25*rho[i][j];
			f2[i][j] = 0.25*rho[i][j];
			f3[i][j] = 0.25*rho[i][j];
			f4[i][j] = 0.25*rho[i][j];
		}
	}
	//主循环
	for ( kk = 1; kk <= mstep; kk++)
	{
		//碰撞
		for ( j = 0; j <= m; j++)
		{
			for ( i = 0; i <= n; i++)
			{
				feq1 = 0.25*rho[i][j] * (1.0 + 2.0*u / ck);
				feq2 = 0.25*rho[i][j] * (1.0 - 2.0*u / ck);
				feq3 = 0.25*rho[i][j] * (1.0 + 2.0*v / ck);
				feq4 = 0.25*rho[i][j] * (1.0 - 2.0*v / ck);
				f1[i][j] = omega*feq1 + (1 - omega)*f1[i][j];
				f2[i][j] = omega*feq2 + (1 - omega)*f2[i][j];
				f3[i][j] = omega*feq3 + (1 - omega)*f3[i][j];
				f4[i][j] = omega*feq4 + (1 - omega)*f4[i][j];
			}
		}
		//流动
		for ( j = 0; j <= m ; j++)
		{
			for ( i = 1; i <= n; i++)
			{
				f1[n-i+1][j] = f1[n-i][j];
				f2[i-1][j] = f2[i][j];
			}
		}
		for ( i = 0; i <= n; i++)
		{
			for ( j = 1; j <= m; j++)
			{
				f3[i][m - j + 1] = f3[i][m - j];
				f4[i][j - 1] = f4[i][j];
			}
		}
		//边界条件
		for ( j = 1; j <= m; j++)
		{
			f1[0][j] = 0.5 - f2[0][j];
			f3[0][j] = 0.5 - f4[0][j];
			f1[n][j] = f1[n - 1][j];
			f2[n][j] = f2[n - 1][j];
			f3[n][j] = f3[n - 1][j];
			f4[n][j] = f4[n - 1][j];
		}
		for ( i = 1; i <= n; i++)
		{
			f1[i][m] = 0.0;
			f2[i][m] = 0.0;
			f3[i][m] = 0.0;
			f4[i][m] = 0.0;
			f1[i][0] = f1[i][1];
			f2[i][0] = f2[i][1];
			f3[i][0] = f3[i][1];
			f4[i][0] = f4[i][1];
		}
		for ( j = 0; j <= m; j++)
		{
			for ( i = 0; i <= n; i++)
			{
				rho[i][j] = f1[i][j] + f2[i][j] + f3[i][j] + f4[i][j];
			}
		}
	}
	ofstream fout;
	fout.open("data.dat", ios::app);
	fout << "TITLE = \"contour\"\nvariables = \"x\", \"y\", \"rho\"\nZone I = 101, J = 101 F = POINT" << endl;
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			fout << x[i] << "\t" << y[j] << "\t" << rho[i][j] << endl;
		}
	}
	fout.close();
	return 0;
}


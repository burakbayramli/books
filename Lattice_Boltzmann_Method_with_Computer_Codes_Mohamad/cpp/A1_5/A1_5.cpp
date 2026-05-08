#include <float.h>
#include <iostream>
#include <math.h>
#include<fstream>
using namespace std;


int main()
{
	const int n = 100, m = 100;
	float f[9][n + 1][m + 1], feq, rho[n + 1][m + 1], x[n + 1], y[m + 1], w[9], alpha, csq, omega, sum;
	int dx = 1, dy = 1, dt = 1, tw = 1, i, j, k, kk, mstep=400;
	x[0] = 0.0, y[0] = 0.0;
	for ( i = 1; i <= n; i++)
	{
		x[i] = x[i - 1] + dx;
	}
	for ( j = 1; j <= m; j++)
	{
		y[j] = y[j - 1] + dy;
	}
	alpha = 0.25;
	csq = dx*dx / dt*dt;
	omega = 1.0 / (3 * alpha / (csq*dt) + 0.5);
	//权重系数
	w[0] = 4.0 / 9;
	for ( i = 1; i <= 4; i++)
	{
		w[i] = 1.0 / 9;
	}
	for ( i = 5; i <= 8; i++)
	{
		w[i] = 1.0 / 36;
	}
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
			for ( k = 0; k <= 8; k++)
			{
				f[k][i][j] = w[k] * rho[i][j];
				if (i == 0)
					f[k][i][j] = w[k] * tw;
			}
		}
	}
	//主循环
	for ( kk = 1; kk <= mstep; kk++)
	{
		for ( j = 0; j <= m; j++)
		{
			for ( i = 0; i <= n; i++)
			{
				sum = 0.0;
				for ( k = 0; k <= 8; k++)
				{
					sum = sum + f[k][i][j];
				}
				rho[i][j] = sum;
			}
		}
		//碰撞
		for ( j = 0; j <= m; j++)
		{
			for ( i = 0; i <= n; i++)
			{
				for ( k = 0; k <= 8; k++)
				{
					feq = w[k] * rho[i][j];
					f[k][i][j] = omega*feq + (1 - omega)*f[k][i][j];
				}
			}
		}
		//流动
		for ( j = m; j > 0 ; j--)
		{
			for ( i = 0; i < n; i++)
			{
				f[2][i][j] = f[2][i][j - 1];
				f[6][i][j] = f[6][i + 1][j - 1];
			}
		}
		for ( j = m; j > 0; j--)
		{
			for ( i = n; i > 0; i--)
			{
				f[1][i][j] = f[1][i - 1][j];
				f[5][i][j] = f[5][i - 1][j - 1];
			}
		}
		for ( j = 0; j < m; j++)
		{
			for ( i = n; i > 0; i--)
			{
				f[4][i][j] = f[4][i][j + 1];
				f[8][i][j] = f[8][i - 1][j + 1];
			}
		}
		for ( j = 0; j < m; j++)
		{
			for ( i = 0; i < n; i++)
			{
				f[3][i][j] = f[3][i + 1][j];
				f[7][i][j] = f[7][i + 1][j + 1];
			}
		}
		//边界条件
		for ( j = 0; j <= m; j++)
		{
			f[1][0][j] = w[1] * tw + w[3] * tw - f[3][0][j];
			f[5][0][j] = w[5] * tw + w[7] * tw - f[7][0][j];
			f[8][0][j] = w[8] * tw + w[6] * tw - f[6][0][j];
			f[3][n][j] = -f[1][n][j];
			f[6][n][j] = -f[8][n][j];
			f[7][n][j] = -f[5][n][j];
		}
		for ( i = 0; i <= n; i++)
		{
			f[4][i][m] = -f[2][i][m];
			f[7][i][m] = -f[5][i][m];
			f[8][i][m] = -f[6][i][m];
			for ( k = 1; k <= 8; k++)
			{
				f[k][i][0] = f[k][i][1];
			}
		}
	}
	for ( j = 0; j <= m; j++)
	{
		for ( i = 0; i <= n; i++)
		{
			sum = 0.0;
			for ( k = 0; k <= 8; k++)
			{
				sum = sum + f[k][i][j];
			}
			rho[i][j] = sum;
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


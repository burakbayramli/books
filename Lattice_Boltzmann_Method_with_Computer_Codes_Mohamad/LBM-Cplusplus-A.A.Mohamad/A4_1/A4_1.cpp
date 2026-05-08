#include <float.h>
#include <iostream>
#include <math.h>
#include<fstream>
using namespace std;


int main()
{
	const int n = 100, m = 100, mstep = 40000;
	float x[n+1], y[m+1], f[9][n + 1][m + 1], feq[9][n + 1][m + 1], rho[n + 1][m + 1], w[9], u[n + 1][m + 1], v[n + 1][m + 1], g[9][n + 1][m + 1], geq[9][n + 1][m + 1], th[n + 1][m + 1], velocity[n+1][m+1];
	float uo, vo, sumvelo, rhoo, rhon, dx, dy, dt, tw, visco, pr, alpha, Re, omega, omegat, t1, t2, ssum, usum, vsum, sum;
	int i, j, k, kk;
	static float cx[9] = { 0.0,1.0,0.0,-1.0,0.0,1.0,-1.0,-1.0,1.0 }, cy[9] = { 0.0,0.0,1.0,0.0,-1.0,1.0,1.0,-1.0,-1.0 };
	dx = 1.0, dy = 1.0;
	x[0] = 0.0;
	for ( i = 1; i <= 100; i++)
	{
		x[i] = x[i - 1] + dx;
	}
	y[0] = 0.0;
	for ( j = 1; j <= 100; j++)
	{
		y[j] = y[j - 1] + dy;
	}
	w[0] = 4.0 / 9;
	for ( i = 1; i <= 4; i++)
	{
		w[i] = 1.0 / 9;
	}
	for ( i = 5; i <= 8; i++)
	{
		w[i] = 1.0 / 36;
	}
	uo = 0.2, vo=0.0, sumvelo = 0.0, rhoo = 5.0, dx = 1.0, dy = dx, dt = 1.0, tw = 1.0, visco=0.02, pr=0.71;
	for ( j = 0; j <= m; j++)
	{
		for ( i = 0; i <= n; i++)
		{
			th[i][j] = 0.0;
			for ( k = 0; k <= 8; k++)
			{
				g[k][i][j] = 0.0;
			}
		}
	}
	alpha = visco / pr, Re = uo*m / pr;
	omega = 1.0 / (3 * visco + 0.5);
	omegat = 1.0 / (3 * alpha + 0.5);
	for ( j = 0; j <= m; j++)
	{
		for ( i = 0; i <= n; i++)
		{
			rho[i][j] = rhoo;
			u[i][j] = 0.0;
			v[i][j] = 0.0;
			for ( k = 0; k <= 8; k++)
			{
				f[k][i][j] = w[k] * rho[i][j];
			}
		}
	}
	for ( i = 1; i <= n-1; i++)
	{
		u[i][m] = uo;
		v[i][m] = vo;
	}
	//主循环
	for ( kk = 1; kk <= mstep; kk++)
	{
		//碰撞
		for ( i = 0; i <= n; i++)
		{
			for ( j = 0; j <= m; j++)
			{
				t1 = u[i][j] * u[i][j] + v[i][j] * v[i][j];
				for ( k = 0; k <= 8; k++)
				{
					t2 = u[i][j] * cx[k] + v[i][j] * cy[k];
					feq[k][i][j] = rho[i][j] * w[k] * (1.0 + 3.0*t2 + 4.50*t2*t2 - 1.50*t1);
					f[k][i][j] = omega*feq[k][i][j] + (1 - omega)*f[k][i][j];
				}
			}
		}
		//流动
		for ( j = 0; j <= m; j++)
		{
			for ( i = n; i > 0; i--)
			{
				f[1][i][j] = f[1][i - 1][j];
			}
			for ( i = 0; i <= n-1; i++)
			{
				f[3][i][j] = f[3][i + 1][j];
			}
		}
		for ( j = m; j > 0; j--)
		{
			for ( i = 0; i <= n; i++)
			{
				f[2][i][j] = f[2][i][j - 1];
			}
			for ( i = n; i > 0; i--)
			{
				f[5][i][j] = f[5][i - 1][j - 1];
			}
			for ( i = 0; i <= n-1; i++)
			{
				f[6][i][j] = f[6][i + 1][j - 1];
			}
		}
		for ( j = 0; j <= m-1; j++)
		{
			for ( i = 0; i <= n; i++)
			{
				f[4][i][j] = f[4][i][j + 1];
			}
			for ( i = 0; i <= n-1; i++)
			{
				f[7][i][j] = f[7][i + 1][j + 1];
			}
			for ( i = n; i > 0; i--)
			{
				f[8][i][j] = f[8][i - 1][j + 1];
			}
		}
		//边界条件
		for ( j = 0; j <= m; j++)
		{
			f[1][0][j] = f[3][0][j];
			f[5][0][j] = f[7][0][j];
			f[8][0][j] = f[6][0][j];
			f[3][n][j] = f[1][n][j];
			f[7][n][j] = f[5][n][j];
			f[6][n][j] = f[8][n][j];
		}
		for ( i = 0; i <= n; i++)
		{
			f[2][i][0] = f[4][i][0];
			f[5][i][0] = f[7][i][0];
			f[6][i][0] = f[8][i][0];
		}
		for ( i = 1; i <= n-1; i++)
		{
			rhon = f[0][i][m] + f[1][i][m] + f[3][i][m] + 2 * (f[2][i][m] + f[6][i][m] + f[5][i][m]);
			f[4][i][m] = f[2][i][m];
			f[8][i][m] = f[6][i][m]+rhon*uo/6.0;
			f[7][i][m] = f[5][i][m]-rhon*uo/6.0;
		}
		//计算u，v
		for ( j = 0; j <= m; j++)
		{
			for ( i = 0; i <= n; i++)
			{
				ssum = 0.0;
				for ( k = 0; k <= 8; k++)
				{
					ssum = ssum + f[k][i][j];
				}
				rho[i][j] = ssum;
			}
		}
		for ( i = 1; i <= n; i++)
		{
			for ( j = 1; j <= m-1; j++)
			{
				usum = 0.0;
				vsum = 0.0;
				for ( k = 0; k <= 8; k++)
				{
					usum = usum + f[k][i][j] * cx[k];
					vsum = vsum + f[k][i][j] * cy[k];
				}
				u[i][j] = usum / rho[i][j];
				v[i][j] = vsum / rho[i][j];
			}
		}
		for ( j = 0; j <= m; j++)
		{
			for ( i = 0; i <= n; i++)
			{
				sum = 0.0;
				for ( k = 0; k <= 8; k++)
				{
					sum = sum + g[k][i][j];
				}
				th[i][j] = sum;
			}
		}
		//标量的碰撞
		for ( i = 0; i <= n; i++)
		{
			for ( j = 0; j <= m; j++)
			{
				for ( k = 0; k <= 8; k++)
				{
					geq[k][i][j] = th[i][j] * w[k] * (1.0 + 3.0*(u[i][j] * cx[k] + v[i][j] * cy[k]));
					g[k][i][j] = omegat*geq[k][i][j] + (1.0 - omegat)*g[k][i][j];
				}
			}
		}
		//标量的流动
		for (j = 0; j <= m; j++)
		{
			for (i = n; i > 0; i--)
			{
				g[1][i][j] = g[1][i - 1][j];
			}
			for (i = 0; i <= n - 1; i++)
			{
				g[3][i][j] = g[3][i + 1][j];
			}
		}
		for (j = m; j > 0; j--)
		{
			for (i = 0; i <= n; i++)
			{
				g[2][i][j] = g[2][i][j - 1];
			}
			for (i = n; i > 0; i--)
			{
				g[5][i][j] = g[5][i - 1][j - 1];
			}
			for (i = 0; i <= n - 1; i++)
			{
				g[6][i][j] = g[6][i + 1][j - 1];
			}
		}
		for (j = 0; j <= m - 1; j++)
		{
			for (i = 0; i <= n; i++)
			{
				g[4][i][j] = g[4][i][j + 1];
			}
			for (i = 0; i <= n - 1; i++)
			{
				g[7][i][j] = g[7][i + 1][j + 1];
			}
			for (i = n; i > 0; i--)
			{
				g[8][i][j] = g[8][i - 1][j + 1];
			}
		}
		//标量的边界条件
		for ( j = 0; j <= m; j++)
		{
			g[1][0][j] = -g[3][0][j];
			g[5][0][j] = -g[7][0][j];
			g[8][0][j] = -g[6][0][j];
			g[6][n][j] = -g[8][n][j];
			g[3][n][j] = -g[1][n][j];
			g[7][n][j] = -g[5][n][j];
			g[2][n][j] = -g[4][n][j];
			g[0][n][j] = 0.0;
		}
		for ( i = 0; i <= n; i++)
		{
			g[8][i][m] = tw*(w[8] + w[6]) - g[6][i][m];
			g[7][i][m] = tw*(w[7] + w[5]) - g[5][i][m];
			g[4][i][m] = tw*(w[4] + w[2]) - g[2][i][m];
			g[1][i][m] = tw*(w[1] + w[3]) - g[3][i][m];
		}
		for ( i = 0; i <= n; i++)
		{
			g[1][i][0] = g[1][i][1];
			g[2][i][0] = g[2][i][1];
			g[3][i][0] = g[3][i][1];
			g[4][i][0] = g[4][i][1];
			g[5][i][0] = g[5][i][1];
			g[6][i][0] = g[6][i][1];
			g[7][i][0] = g[7][i][1];
			g[8][i][0] = g[8][i][1];
		}
	}
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			velocity[i][j] = sqrt(u[i][j] * u[i][j] + v[i][j] * v[i][j]);
		}
	}
	ofstream fout;
	fout.open("Data.dat", ios::app);
	fout << "TITLE = \"Data\"\nvariables = X,Y,U,V,Velocity,T\nZone t=\"data\"\nI=101,J=101,F=POINT" << endl;
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			fout << x[i] << "\t" << y[j] << "\t" << u[i][j] << "\t" << v[i][j] << "\t" << velocity[i][j] << "\t" << th[i][j] << endl;
		}
	}
	fout.close();
	return 0;
}


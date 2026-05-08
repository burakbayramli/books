#include <float.h>
#include <iostream>
#include <math.h>
#include<fstream>
using namespace std;


int main()
{
	const int n = 100, m = 100, mstep = 40000;
	float f[9][n + 1][m + 1], feq[9][n + 1][m + 1], rho[n + 1][m + 1], w[9], cx[9], cy[9], u[n + 1][m + 1], v[n + 1][m + 1], x[n + 1], y[m + 1], velocity[n + 1][m + 1];
	float uo, rhoo, rhon, dx, dy, dt, alpha, Re, omega, t1, t2, ssum, usum, vsum;
	int i, j, k, kk;
	uo = 0.10, rhoo = 5.00, dx = 1.0, dy = dx, dt = 1.0, alpha = 0.01, Re = uo*m / alpha, omega = 1.0 / (3.0 * alpha + 0.5);
	//权重系数
	w[0] = 4.0 / 9;
	for (i = 1; i <= 4; i++)
	{
		w[i] = 1.0 / 9;
	}
	for (i = 5; i <= 8; i++)
	{
		w[i] = 1.0 / 36;
	}
	x[0] = 0.0;
	for (i = 1; i <= n; i++)
	{
		x[i] = x[i - 1] + dx;
	}
	y[0] = 0.0;
	for (j = 1; j <= m; j++)
	{
		y[j] = y[j - 1] + dy;
	}
	cx[0] = 0;
	cx[1] = 1;
	cx[2] = 0;
	cx[3] = -1;
	cx[4] = 0;
	cx[5] = 1;
	cx[6] = -1;
	cx[7] = -1;
	cx[8] = 1;
	cy[0] = 0;
	cy[1] = 0;
	cy[2] = 1;
	cy[3] = 0;
	cy[4] = -1;
	cy[5] = 1;
	cy[6] = 1;
	cy[7] = -1;
	cy[8] = -1;
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			rho[i][j] = rhoo;
			u[i][j] = 0.0;
			v[i][j] = 0.0;
		}
	}
	for (i = 1; i <= n - 1; i++)
	{
		u[i][m] = uo;
		v[i][m] = 0.0;
	}
	//源程序未给f赋初值
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			for (k = 0; k <= 8; k++)
			{
				f[k][i][j] = w[k] * rho[i][j];
			}
		}
	}
	//主循环
	for (kk = 1; kk <= mstep; kk++)
	{
		//碰撞
		for (i = 0; i <= n; i++)
		{
			for (j = 0; j <= m; j++)
			{
				t1 = u[i][j] * u[i][j] + v[i][j] * v[i][j];
				for (k = 0; k <= 8; k++)
				{
					t2 = u[i][j] * cx[k] + v[i][j] * cy[k];
					feq[k][i][j] = rho[i][j] * w[k] * (1.0 + 3.0*t2 + 4.50*t2*t2 - 1.5*t1);
					f[k][i][j] = omega*feq[k][i][j] + (1 - omega)*f[k][i][j];
				}
			}
		}
		//流动
		for (j = 0; j <= m; j++)
		{
			for (i = n; i > 0; i--)
			{
				f[1][i][j] = f[1][i - 1][j];
			}
			for (i = 0; i <= n - 1; i++)
			{
				f[3][i][j] = f[3][i + 1][j];
			}
		}
		for (j = m; j > 0; j--)
		{
			for (i = 0; i <= n; i++)
			{
				f[2][i][j] = f[2][i][j - 1];
			}
			for (i = n; i > 0; i--)
			{
				f[5][i][j] = f[5][i - 1][j - 1];
			}
			for (i = 0; i <= n - 1; i++)
			{
				f[6][i][j] = f[6][i + 1][j - 1];
			}
		}
		for (j = 0; j <= m - 1; j++)
		{
			for (i = 0; i <= n; i++)
			{
				f[4][i][j] = f[4][i][j + 1];
			}
			for (i = 0; i <= n - 1; i++)
			{
				f[7][i][j] = f[7][i + 1][j + 1];
			}
			for (i = n; i > 0; i--)
			{
				f[8][i][j] = f[8][i - 1][j + 1];
			}
		}
		//边界条件
		//除y=m，反弹边界
		for (j = 0; j <= m; j++)
		{
			f[1][0][j] = f[3][0][j];
			f[5][0][j] = f[7][0][j];
			f[8][0][j] = f[6][0][j];
			f[3][n][j] = f[1][n][j];
			f[7][n][j] = f[5][n][j];
			f[6][n][j] = f[8][n][j];
		}
		for (i = 0; i <= n; i++)
		{
			f[2][i][0] = f[4][i][0];
			f[5][i][0] = f[7][i][0];
			f[6][i][0] = f[8][i][0];
		}
		//y=m，移动的盖子
		for (i = 1; i <= n - 1; i++)
		{
			rhon = f[0][i][m] + f[1][i][m] + f[3][i][m] + 2 * (f[2][i][m] + f[6][i][m] + f[5][i][m]);
			f[4][i][m] = f[2][i][m];
			f[8][i][m] = f[6][i][m] + rhon*uo / 6.0;
			f[7][i][m] = f[5][i][m] - rhon*uo / 6.0;
		}
		for (j = 0; j <= m; j++)
		{
			for (i = 0; i <= n; i++)
			{
				ssum = 0.0;
				for (k = 0; k <= 8; k++)
				{
					ssum = ssum + f[k][i][j];
				}
				rho[i][j] = ssum;
			}
		}
		for (i = 1; i <= n; i++)
		{
			rho[i][m] = f[0][i][m] + f[1][i][m] + f[3][i][m] + 2 * (f[2][i][m] + f[6][i][m] + f[5][i][m]);
		}
		for (i = 1; i <= n; i++)
		{
			for (j = 1; j <= m - 1; j++)
			{
				usum = 0.0;
				vsum = 0.0;
				for (k = 0; k <= 8; k++)
				{
					usum = usum + f[k][i][j] * cx[k];
					vsum = vsum + f[k][i][j] * cy[k];
				}
				u[i][j] = usum / rho[i][j];
				v[i][j] = vsum / rho[i][j];
			}
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
	fout << "TITLE = \"Data\"\nvariables = X,Y,U,V,Velocity\nZone t=\"data\"\nI=101,J=101,F=POINT" << endl;
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			fout << x[i] << "\t" << y[j] << "\t" << u[i][j] << "\t" << v[i][j] << "\t" << velocity[i][j] << endl;
		}
	}
	fout.close();
	return 0;
}




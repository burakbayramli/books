#include <float.h>
#include <iostream>
#include <math.h>
#include<fstream>
using namespace std;

void streaming(int n, int m, float f[9][1001][51])
{
	int i, j;
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
}

void collision(float u[1001][51], float v[1001][51], float f[9][1001][51], float rho[1001][51], float w[9], float cx[9], float cy[9],int n,int m, float omega)
{
	float feq[9][1001][51];
	float t1, t2;
	int i, j, k;
	for ( j = 0; j <= m; j++)
	{
		for ( i = 0; i <= n; i++)
		{
			t1 = u[i][j] * u[i][j] + v[i][j] * v[i][j];
			for ( k = 0; k <= 8; k++)
			{
				t2 = u[i][j] * cx[k] + v[i][j] * cy[k];
				feq[k][i][j] = rho[i][j] * w[k] * (1.0 + 3.0*t2 + 4.50*t2*t2 - 1.50*t1);
				f[k][i][j] = omega*feq[k][i][j] + (1.0 - omega)*f[k][i][j];
			}
		}
	}
}

void collisiont(float u[1001][51], float v[1001][51], float f[9][1001][51], float rho[1001][51], float w[9], float cx[9], float cy[9], int n, int m, float omegat)
{
	float feq[9][1001][51];
	float t;
	int i, j, k;
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			for (k = 0; k <= 8; k++)
			{
				t = u[i][j] * cx[k] + v[i][j] * cy[k];
				feq[k][i][j] = rho[i][j] * w[k] * (1.0 + 3.0*t);
				f[k][i][j] = omegat*feq[k][i][j] + (1.0 - omegat)*f[k][i][j];
			}
		}
	}
}

void sfbound(float f[9][1001][51], float uo, int n, int m)
{
	int i, j;
	float rhow;
	//西入口
	for ( j = 0; j <= m; j++)
	{
		rhow = f[0][0][j] + f[2][0][j] + f[4][0][j] + 2 * (f[3][0][j] + f[6][0][j] + f[7][0][j]) / (1.0 - uo);
		f[1][0][j] = f[3][0][j] + 2 * rhow*uo / 3.0;
		f[5][0][j] = f[7][0][j] + rhow*uo / 6.0;
		f[8][0][j] = f[6][0][j] + rhow*uo / 6.0;
	}
	//北反弹
	for ( i = 0; i <= n; i++)
	{
		f[4][i][m] = f[2][i][m];
		f[8][i][m] = f[6][i][m];
		f[7][i][m] = f[5][i][m];
	}
	//南反弹
	for ( i = 0; i <= n; i++)
	{
		f[2][i][0] = f[4][i][0];
		f[5][i][0] = f[7][i][0];
		f[6][i][0] = f[8][i][0];
	}
	//东出口开放边界
	for ( j = 1; j <= m-1; j++)
	{
		f[3][n][j] = 2 * f[3][n - 1][j] - f[3][n - 2][j];
		f[7][n][j] = 2 * f[7][n - 1][j] - f[7][n - 2][j];
		f[6][n][j] = 2 * f[6][n - 1][j] - f[6][n - 2][j];
	}
}

void gbound(float g[9][1001][51], float w[9], float tw, int n, int m)
{
	int i, j;
	//左边界，T=0.0
	for ( j = 0; j <= m; j++)
	{
		g[1][0][j] = -g[3][0][j];
		g[5][0][j] = -g[7][0][j];
		g[8][0][j] = -g[6][0][j];
	}
	//右边界，开放
	for ( j = 0; j <= m; j++)
	{
		g[6][n][j] = 2 * g[6][n - 1][j] - g[6][n - 2][j];
		g[3][n][j] = 2 * g[3][n - 1][j] - g[3][n - 2][j];
		g[7][n][j] = 2 * g[7][n - 1][j] - g[7][n - 2][j];
		g[2][n][j] = 2 * g[2][n - 1][j] - g[2][n - 2][j];
		g[0][n][j] = 2 * g[0][n - 1][j] - g[0][n - 2][j];
		g[1][n][j] = 2 * g[1][n - 1][j] - g[1][n - 2][j];
		g[4][n][j] = 2 * g[4][n - 1][j] - g[4][n - 2][j];
		g[5][n][j] = 2 * g[5][n - 1][j] - g[5][n - 2][j];
		g[8][n][j] = 2 * g[8][n - 1][j] - g[8][n - 2][j];
	}
	//上边界，T=tw=1.0
	for ( i = 0; i <= n; i++)
	{
		g[8][i][m] = tw*(w[8] + w[6]) - g[6][i][m];
		g[4][i][m] = tw*(w[4] + w[2]) - g[2][i][m];
		g[7][i][m] = tw*(w[7] + w[5]) - g[5][i][m];
	}
	//下边界，T=tw=1.0
	for ( i = 0; i <= n; i++)
	{
		g[2][i][0] = tw*(w[2] + w[4]) - g[4][i][0];
		g[6][i][0] = tw*(w[6] + w[8]) - g[8][i][0];
		g[5][i][0] = tw*(w[5] + w[7]) - g[7][i][0];
	}
}

void rhouv(float f[9][1001][51], float rho[1001][51], float u[1001][51], float v[1001][51], float cx[9], float cy[9], int n, int m)
{
	int i, j, k;
	float ssum, usum, vsum;
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
	for ( j = 1; j <= m; j++)
	{
		v[n][j] = 0.0;
	}
}

void tcalcu(float g[9][1001][51], float th[1001][51], int n, int m)
{
	int i, j, k;
	float ssumt;
	for ( j = 0; j <= m; j++)
	{
		for ( i = 0; i <= n; i++)
		{
			ssumt = 0.0;
			for ( k = 0; k <= 8; k++)
			{
				ssumt = ssumt + g[k][i][j];
			}
			th[i][j] = ssumt;
		}
	}
}

void xy(float x[1001], float y[51], float dx, float dy, int n, int m)
{
	int i, j;
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
}

void Q9(float w[9], float cx[9], float cy[9])
{
	int i;
	w[0] = 4.0 / 9.0;
	for ( i = 1; i <= 4; i++)
	{
		w[i] = 1.0 / 9.0;
	}
	for ( i = 5; i <= 8; i++)
	{
		w[i] = 1.0 / 36.0;
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
}

void init(float rho[1001][51], float th[1001][51], float f[9][1001][51], float g[9][1001][51], float u[1001][51], float v[1001][51], int n, int m, float uo,float rhoo, float w[9])
{
	int i, j, k;
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			rho[i][j] = rhoo;
			u[i][j] = 0.0;
			v[i][j] = 0.0;
			for (k = 0; k <= 8; k++)
			{
				f[k][i][j] = w[k] * rho[i][j];
			}
		}
	}
	for (j = 1; j <= m - 1; j++)
	{
		u[0][j] = uo;
		v[0][j] = 0.0;
	}
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			th[i][j] = 0.0;
			for (k = 0; k <= 8; k++)
			{
				g[k][i][j] = w[k] * th[i][j];
			}
		}
	}
}

int main()
{
	const int n = 1000, m = 50, mstep=10000;
	float f[9][n + 1][m + 1], g[9][n + 1][m + 1], rho[n + 1][m + 1], th[n + 1][m + 1], w[9], cx[9], cy[9], u[n + 1][m + 1], v[n + 1][m + 1], x[n + 1], y[m + 1], velocity[n + 1][m + 1];
	float uo, sumvelo, rhoo, dx, dy, dt, tw, visco, Pr, alpha, Re, omega, omegat;
	int i, j, kk;
	uo = 0.12;
	sumvelo = 0.0;
	rhoo = 5.0;
	dx = 1.0, dy = dx, dt = 1.0;
	tw = 1.0;
	visco = 0.038;
	Pr = 3.8;
	alpha = visco / Pr;
	Re = uo*m / alpha;
	omega = 1.0 / (3.0*visco + 0.5);
	omegat = 1.0 / (3.0*alpha + 0.5);
	xy(x, y, dx, dy, n, m);
	Q9(w, cx, cy);
	init(rho, th, f, g, u, v, n, m, uo, rhoo, w);
	//主循环
	for ( kk = 1; kk <= mstep; kk++)
	{
		collision(u, v, f, rho, w, cx, cy, n, m, omega);
		streaming(n, m, f);
		sfbound(f, uo, n, m);
		rhouv(f, rho, u, v, cx, cy, n, m);
		tcalcu(g, th, n, m);
		collisiont(u, v, g, th, w, cx, cy, n, m, omegat);
		streaming(n, m, g);
		gbound(g, w, tw, n, m);
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
	fout << "TITLE = \"Data\"\nvariables = X,Y,U,V,Velocity,T\nZone t=\"data\"\nI=1001,J=51,F=POINT" << endl;
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


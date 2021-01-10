#include <float.h>
#include <iostream>
#include <math.h>
#include<fstream>
using namespace std;

void streaming(int n, int m, float f[9][101][101])
{
	int i, j;
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
}

void collision(float u[101][101], float v[101][101], float f[9][101][101], float rho[101][101], float w[9], float cx[9], float cy[9], int n, int m, float omega, float th[101][101], float gbeta, float tref)
{
	float feq[9][101][101];
	float t1, t2, force;
	int i, j, k;
	tref = 0.5;
	for ( i = 0; i <= n; i++)
	{
		for ( j = 0; j <= m; j++)
		{
			t1 = u[i][j] * u[i][j] + v[i][j] * v[i][j];
			for ( k = 0; k <= 8; k++)
			{
				t2 = u[i][j] * cx[k] + v[i][j] * cy[k];
				force = 3 * w[k] * gbeta *(th[i][j] - tref)* cy[k] * rho[i][j];
				if (i==0||i==n)
				{
					force = 0.0;
				}
				if (j==0||j==m)
				{
					force = 0.0;
				}
				feq[k][i][j] = rho[i][j] * w[k] * (1.0 + 3.0*t2 + 4.50*t2*t2 - 1.50*t1);
				f[k][i][j] = omega*feq[k][i][j] + (1 - omega)*f[k][i][j] + force;
			}
		}
	}
	
}

void collisiont(float u[101][101], float v[101][101], float f[9][101][101], float rho[101][101], float w[9], float cx[9], float cy[9], int n, int m, float omegat)
{
	float feq[9][101][101];
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

void bounce(float f[9][101][101], int n, int m)
{
	int i, j;
	//西入口
	for (j = 0; j <= m; j++)
	{
		f[1][0][j] = f[3][0][j];
		f[5][0][j] = f[7][0][j];
		f[8][0][j] = f[6][0][j];
	}
	//北反弹
	for (i = 0; i <= n; i++)
	{
		f[4][i][m] = f[2][i][m];
		f[8][i][m] = f[6][i][m];
		f[7][i][m] = f[5][i][m];
	}
	//南反弹
	for (i = 0; i <= n; i++)
	{
		f[2][i][0] = f[4][i][0];
		f[5][i][0] = f[7][i][0];
		f[6][i][0] = f[8][i][0];
	}
	//东反弹
	for (j = 0; j <= m; j++)
	{
		f[3][n][j] = f[1][n][j];
		f[7][n][j] = f[5][n][j];
		f[6][n][j] = f[8][n][j];
	}
}

void gbound(float g[9][101][101], float w[9], float tw, int n, int m)
{
	int i, j, k;
	//左边界，T=tw=1.0
	for (j = 0; j <= m; j++)
	{
		g[1][0][j] = tw*(w[1] + w[3]) - g[3][0][j];
		g[5][0][j] = tw*(w[5] + w[7]) - g[7][0][j];
		g[8][0][j] = tw*(w[8] + w[6]) - g[6][0][j];
	}
	//右边界，T=0.0
	for (j = 0; j <= m; j++)
	{
		g[6][n][j] = -g[8][n][j];
		g[3][n][j] = -g[1][n][j];
		g[7][n][j] = -g[5][n][j];
	}
	//上边界，绝热
	for (i = 1; i <= n-1; i++)
	{
		for ( k = 0; k <= 8; k++)
		{
			g[k][i][m] = g[k][i][m - 1];
		}
	}
	//下边界，T=tw=1.0
	for (i = 1; i <= n-1; i++)
	{
		for (k = 0; k <= 8; k++)
		{
			g[k][i][0] = g[k][i][1];
		}
	}
}

void rhouv(float f[9][101][101], float rho[101][101], float u[101][101], float v[101][101], float cx[9], float cy[9], int n, int m)
{
	int i, j, k;
	float ssum, usum, vsum;
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
	for (i = 0; i <= n; i++)
	{
		for (j = 0; j <= m; j++)
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

void tcalcu(float g[9][101][101], float th[101][101], int n, int m)
{
	int i, j, k;
	float ssumt;
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			ssumt = 0.0;
			for (k = 0; k <= 8; k++)
			{
				ssumt = ssumt + g[k][i][j];
			}
			th[i][j] = ssumt;
		}
	}
}

void xy(float x[101], float y[101], float dx, float dy, int n, int m)
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
	for (i = 1; i <= 4; i++)
	{
		w[i] = 1.0 / 9.0;
	}
	for (i = 5; i <= 8; i++)
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

void init(float rho[101][101], float th[101][101], float f[9][101][101], float g[9][101][101], float u[101][101], float v[101][101], int n, int m, float rhoo, float w[9], float tref)
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
	for (j = 0; j <= m; j++)
	{
		for (i = 0; i <= n; i++)
		{
			th[i][j] = tref;
			for (k = 0; k <= 8; k++)
			{
				g[k][i][j] = w[k] * th[i][j];
			}
		}
	}
}

int main()
{
	const int n = 100, m = 100, mstep = 15000;
	float f[9][n + 1][m + 1], g[9][n + 1][m + 1], rho[n + 1][m + 1], th[n + 1][m + 1], w[9], cx[9], cy[9], u[n + 1][m + 1], v[n + 1][m + 1], x[n + 1], y[m + 1], velocity[n + 1][m + 1];
	float uo, sumvelo, rhoo, dx, dy, dt, tw, visco, Pr, alpha, Re, omega, omegat, ra, gbeta, tref;
	int i, j, kk;
	uo = 0.0;
	sumvelo = 0.0;
	rhoo = 6.00;
	dx = 1.0, dy = dx, dt = 1.0;
	tw = 1.0;
	tref = 0.5;
	ra = 1.0e5;
	visco = 0.02;
	Pr = 0.71;
	alpha = visco / Pr;
	Re = uo*m / alpha;
	gbeta = ra*visco*alpha / (m*m*m);
	omega = 1.0 / (3.0*visco + 0.5);
	omegat = 1.0 / (3.0*alpha + 0.5);
	xy(x, y, dx, dy, n, m);
	Q9(w, cx, cy);
	init(rho, th, f, g, u, v, n, m, rhoo, w, tref);
	//主循环
	for (kk = 1; kk <= mstep; kk++)
	{
		collision(u, v, f, rho, w, cx, cy, n, m, omega, th,gbeta,tref);
		streaming(n, m, f);
		bounce(f, n, m);
		rhouv(f, rho, u, v, cx, cy, n, m);
		collisiont(u, v, g, th, w, cx, cy, n, m,omegat);
		streaming(n, m, g);
		gbound(g, w, tw, n, m);
		tcalcu(g, th, n, m);
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


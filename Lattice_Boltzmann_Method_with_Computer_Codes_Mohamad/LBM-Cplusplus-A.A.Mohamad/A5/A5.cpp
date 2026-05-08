#include <float.h>
#include <iostream>
#include <math.h>
#include<fstream>
using namespace std;

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

void init(float rho[101][101], float f[9][101][101], float u[101][101], float v[101][101], int n, int m, float rhoo, float uo, float w[9])
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
	for ( i = 1; i <= n-1; i++)
	{
		u[i][m] = uo;
		v[i][m] = 0.0;
	}
}

void collision(float u[101][101], float v[101][101], float f[9][101][101], float rho[101][101], float w[9], float cx[9], float cy[9], int n, int m, float omega, float tm[9][9], float tminv[9][9], float stmiv[9][9], float sm[9])
{
	float feq[9][101][101], fmom[9][101][101], fmeq[9][101][101];
	float t1,t2,suma,sumb;
	int i, j, l, k;
	//计算平衡时间
	for (i = 0; i <= n; i++)
	{
		for (j = 0; j <= m; j++)
		{
			t1 = u[i][j] * u[i][j] + v[i][j] * v[i][j];
			t2 = u[i][j] * u[i][j] - v[i][j] * v[i][j];
			fmeq[0][i][j] = rho[i][j];
			fmeq[1][i][j] = rho[i][j] * (-2.0 + 3.0*rho[i][j] * t1);
			fmeq[2][i][j] = rho[i][j] * (1.0 - 3.0*rho[i][j] * t1);
			fmeq[3][i][j] = rho[i][j] * u[i][j];
			fmeq[4][i][j] = -rho[i][j] * u[i][j];
			fmeq[5][i][j] = rho[i][j] * v[i][j];
			fmeq[6][i][j] = -rho[i][j] * v[i][j];
			fmeq[7][i][j] = rho[i][j] * t2;
			fmeq[8][i][j] = rho[i][j] * u[i][j] * v[i][j];
		}
	}
	//计算时间
	for ( j = 0; j <= m; j++)
	{
		for ( i = 0; i <= n; i++)
		{
			for ( k = 0; k <= 8; k++)
			{
				suma = 0.0;
				for ( l = 0; l <= 8; l++)
				{
					suma = suma + tm[k][l] * f[l][i][j];
				}
				fmom[k][i][j] = suma;
			}
		}
	}
	//计算时间空间中的碰撞
	for ( j = 0; j <= m; j++)
	{
		for ( i = 0; i <= n; i++)
		{
			for ( k = 0; k <= 8; k++)
			{
				sumb = 0.0;
				for ( l = 0; l <= 8; l++)
				{
					sumb = sumb + stmiv[k][l] * (fmom[l][i][j] - fmeq[l][i][j]);
				}
				f[k][i][j] = f[k][i][j] - sumb;
			}
		}
	}
}

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

void sfbound(float f[9][101][101], int n, int m, float uo)
{
	int i, j;
	float rhon;
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
	for ( i = 1; i <= n-1; i++)
	{
		rho[i][m] = f[0][i][m] + f[1][i][m] + f[3][i][m] + 2 * (f[2][i][m] + f[6][i][m] + f[5][i][m]);
	}
	for (i = 0; i <= n; i++)
	{
		for (j = 0; j <= m-1; j++)
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

int main()
{
	const int n = 100, m = 100, mstep=100000;
	int i, j, l, kk;
	float f[9][n + 1][m + 1], rho[n + 1][m + 1], w[9], cx[9], cy[9], u[n + 1][m + 1], v[n + 1][m + 1], stmiv[9][9], ev[9][9], x[n + 1], y[m + 1], velocity[n+1][m+1];
	float dx, dy, dt, a1, sumcc, uo, rhoo, alpha, omega, Re, tau;
	dx = 1.0, dy = dx, dt = 1.0;
	xy(x, y, dx, dy, n, m);
	Q9(w, cx, cy);
	static float tm[9][9] = { { 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 }, { -4.0, -1.0, -1.0, -1.0, -1.0, 2.0, 2.0 ,2.0 ,2.0 }, { 4.0, -2.0, -2.0, -2.0, -2.0, 1.0, 1.0, 1.0, 1.0 }, { 0.0, 1.0, 0.0, -1.0, 0.0, 1.0, -1.0, -1.0, 1.0 }, { 0.0, -2.0, 0.0, 2.0, 0.0, 1.0, -1.0, -1.0, 1.0 }, { 0.0, 0.0, 1.0, 0.0, -1.0, 1.0, 1.0, -1.0, -1.0 }, { 0.0, 0.0, -2.0, 0.0, 2.0, 1.0, 1.0, -1.0, -1.0 }, { 0.0, 1.0, -1.0, 1.0, -1.0, 0.0, 0.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, -1.0, 1.0, -1.0 } };
	a1 = 1.0 / 36.0;
	static float tminv[9][9] = { { 4 * a1, -4 * a1, 4 * a1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }, { 4 * a1, -a1, -2 * a1, 6 * a1, -6 * a1, 0.0, 0.0, 9 * a1, 0.0 }, { 4 * a1, -a1, -2 * a1, 0.0, 0.0, 6 * a1, -6 * a1, -9 * a1, 0.0 }, { 4 * a1, -a1, -2 * a1, -6 * a1, 6 * a1, 0.0, 0.0, 9 * a1, 0.0 },{ 4 * a1, -a1, -2 * a1, 0.0, 0.0, -6 * a1, 6 * a1, -9 * a1, 0.0 }, {4 * a1, 2 * a1, a1, 6 * a1, 3 * a1, 6 * a1, 3 * a1, 0.0, 9 * a1 },{ 4 * a1, 2 * a1, a1, -6 * a1, -3 * a1, 6 * a1, 3 * a1, 0.0, -9 * a1 }, { 4 * a1, 2 * a1, a1, -6 * a1, -3 * a1, -6 * a1, -3 * a1, 0.0, 9 * a1 }, { 4 * a1, 2 * a1, a1, 6 * a1, 3 * a1, -6 * a1, -3 * a1, 0.0, -9 * a1 } };
	/*ofstream arrayout;
	arrayout.open("arrayout.dat");
	for ( i = 0; i <= 8; i++)
	{
		for ( j = 0; j <= 8; j++)
		{
			arrayout << tminv[i][j]/a1 << "\t";
		}
		arrayout << endl;
	}*/
	for ( i = 0; i <= 8; i++)
	{
		for ( j = 0; j <= 8; j++)
		{
			sumcc = 0.0;
			for ( l = 0; l <= 8; l++)
			{
				sumcc = sumcc + tminv[i][l] * tm[l][j];
			}
			ev[i][j] = sumcc;
		}
	}
	uo = 0.05, rhoo = 1.00, dx = 1.0, dy = dx, dt = 1.0;
	alpha = 0.001;
	Re = uo*m / alpha;
	omega = 1.0 / (3.0*alpha + 0.5);
	tau = 1.0 / omega;
	static float sm[9] = { 1.0, 1.4, 1.4, 1.0, 1.2, 1.0, 1.2, tau, tau };
	for ( i = 0; i <= 8; i++)
	{
		for ( j = 0; j <= 8; j++)
		{
			stmiv[i][j] = tminv[i][j] * sm[j];
		}
	}
	init(rho, f, u, v, n, m, rhoo, uo, w);
	for ( kk = 1; kk <= mstep; kk++)
	{
		collision(u, v, f, rho, w, cx, cy, n, m, omega, tm, tminv, stmiv, sm);
		streaming(n, m, f);
		sfbound(f, n, m, uo);
		rhouv(f, rho, u, v, cx, cy, n, m);
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


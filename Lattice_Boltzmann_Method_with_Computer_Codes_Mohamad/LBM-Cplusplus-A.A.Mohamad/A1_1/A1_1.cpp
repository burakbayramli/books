#include <float.h>
#include <iostream>
#include <math.h>
#include<fstream>
using namespace std;



int main()
{
	const int m = 100;
	float f1[m], f2[m], rho[m], feq[m], x[m] = { 0.0 };
	const float dt = 1.0, dx = 1.0;
	int i, kk;
	for (i = 0; i < m - 1; i++) {
		x[i + 1] = x[i] + dx;
	}
	float csq = dx*dx / dt*dt;
	float alpha = 0.25;
	float omega = 1.0 / (alpha / (dt*csq) + 0.5);
	int mstep = 200;
	float twall = 1.0;
	i = 0;
	for (i = 0; i < m; i++) {
		rho[i] = 0.0;
		f1[i] = 0.5*rho[i];
		f2[i] = 0.5*rho[i];
	}
	for (kk = 1; kk <= mstep; kk++)
	{
		/*collision process*/
		for (i = 0; i < m; i++)
		{
			feq[i] = 0.5*rho[i];
			f1[i] = (1 - omega)*f1[i] + omega*feq[i];
			f2[i] = (1 - omega)*f2[i] + omega*feq[i];
		}
		/*streaming process*/
		for (i = 1; i <= m - 1; i++)
		{
			f1[m - i] = f1[m - i - 1];
			f2[i - 1] = f2[i];
		}
		/*boundary condition*/
		f1[0] = twall - f2[0];
		f1[m-1] = f1[m - 2];
		f2[m-1] = f2[m - 2];
		for (i = 0; i < m; i++)
		{
			rho[i] = f1[i] + f2[i];
		}
	}
	ofstream fout;
	fout.open("data.dat", ios::app);
	fout << "TITLE =  \"A1.1\" \nvariables = \"x\", \"rho\"\nZone I = 1,J = 100 F=POINT" << endl;
	for (i = 0; i < m; i++)
	{
		/*ofstream fout;
		fout.open("data.dat",ios::app);*/
		fout << x[i] << "\t" << rho[i] << endl;
	}
	fout.close();
	return 0;

}

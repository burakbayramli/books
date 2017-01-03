#include <cmath>
#include "nr.h"
using namespace std;

void NR::fdjac(Vec_IO_DP &x, Vec_I_DP &fvec, Mat_O_DP &df,
	void vecfunc(Vec_I_DP &, Vec_O_DP &))
{
	const DP EPS=1.0e-8;
	int i,j;
	DP h,temp;

	int n=x.size();
	Vec_DP f(n);
	for (j=0;j<n;j++) {
		temp=x[j];
		h=EPS*fabs(temp);
		if (h == 0.0) h=EPS;
		x[j]=temp+h;
		h=x[j]-temp;
		vecfunc(x,f);
		x[j]=temp;
		for (i=0;i<n;i++)
			df[i][j]=(f[i]-fvec[i])/h;
	}
}

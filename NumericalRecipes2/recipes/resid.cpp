#include "nr.h"

void NR::resid(Mat_O_DP &res, Mat_I_DP &u, Mat_I_DP &rhs)
{
	int i,j;
	DP h,h2i;

	int n=u.nrows();
	h=1.0/(n-1);
	h2i=1.0/(h*h);
	for (j=1;j<n-1;j++)
		for (i=1;i<n-1;i++)
			res[i][j] = -h2i*(u[i+1][j]+u[i-1][j]+u[i][j+1]
				+u[i][j-1]-4.0*u[i][j])+rhs[i][j];
	for (i=0;i<n;i++)
		res[i][0]=res[i][n-1]=res[0][i]=res[n-1][i]=0.0;
}

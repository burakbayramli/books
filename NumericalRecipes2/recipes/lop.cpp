#include "nr.h"

void NR::lop(Mat_O_DP &out, Mat_I_DP &u)
{
	int i,j;
	DP h,h2i;

	int n=u.nrows();
	h=1.0/(n-1);
	h2i=1.0/(h*h);
	for (j=1;j<n-1;j++)
		for (i=1;i<n-1;i++)
			out[i][j]=h2i*(u[i+1][j]+u[i-1][j]+u[i][j+1]+u[i][j-1]-
				4.0*u[i][j])+u[i][j]*u[i][j];
	for (i=0;i<n;i++)
		out[i][0]=out[i][n-1]=out[0][i]=out[n-1][i]=0.0;
}

#include "nr.h"

void NR::relax2(Mat_IO_DP &u, Mat_I_DP &rhs)
{
	int i,ipass,isw,j,jsw=1;
	DP foh2,h,h2i,res;

	int n=u.nrows();
	h=1.0/(n-1);
	h2i=1.0/(h*h);
	foh2 = -4.0*h2i;
	for (ipass=0;ipass<2;ipass++,jsw=3-jsw) {
		isw=jsw;
		for (j=1;j<n-1;j++,isw=3-isw) {
			for (i=isw;i<n-1;i+=2) {
				res=h2i*(u[i+1][j]+u[i-1][j]+u[i][j+1]+u[i][j-1]-
					4.0*u[i][j])+u[i][j]*u[i][j]-rhs[i][j];
				u[i][j] -= res/(foh2+2.0*u[i][j]);
			}
		}
	}
}

#include "nr.h"

void NR::relax(Mat_IO_DP &u, Mat_I_DP &rhs)
{
	int i,ipass,isw,j,jsw=1;
	DP h,h2;

	int n=u.nrows();
	h=1.0/(n-1);
	h2=h*h;
	for (ipass=0;ipass<2;ipass++,jsw=3-jsw) {
		isw=jsw;
		for (j=1;j<n-1;j++,isw=3-isw)
			for (i=isw;i<n-1;i+=2)
				u[i][j]=0.25*(u[i+1][j]+u[i-1][j]+u[i][j+1]
					+u[i][j-1]-h2*rhs[i][j]);
	}
}

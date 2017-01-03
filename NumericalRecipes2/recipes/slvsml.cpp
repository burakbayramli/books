#include "nr.h"

void NR::slvsml(Mat_O_DP &u, Mat_I_DP &rhs)
{
	int i,j;
	DP h=0.5;

	for (i=0;i<3;i++)
		for (j=0;j<3;j++)
			u[i][j]=0.0;
	u[1][1] = -h*h*rhs[1][1]/4.0;
}

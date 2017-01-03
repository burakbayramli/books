#include "nr.h"

void NR::matsub(Mat_I_DP &a, Mat_I_DP &b, Mat_O_DP &c)
{
	int i,j;

	int n=a.nrows();
	for (j=0;j<n;j++)
		for (i=0;i<n;i++)
			c[i][j]=a[i][j]-b[i][j];
}

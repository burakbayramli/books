#include "nr.h"

void NR::copy(Mat_O_DP &aout, Mat_I_DP &ain)
{
	int i,j;

	int n=ain.nrows();
	for (i=0;i<n;i++)
		for (j=0;j<n;j++)
			aout[j][i]=ain[j][i];

}

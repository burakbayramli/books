#include <cmath>
#include "nr.h"
using namespace std;

void NR::choldc(Mat_IO_DP &a, Vec_O_DP &p)
{
	int i,j,k;
	DP sum;

	int n=a.nrows();
	for (i=0;i<n;i++) {
		for (j=i;j<n;j++) {
			for (sum=a[i][j],k=i-1;k>=0;k--) sum -= a[i][k]*a[j][k];
			if (i == j) {
				if (sum <= 0.0)
					nrerror("choldc failed");
				p[i]=sqrt(sum);
			} else a[j][i]=sum/p[i];
		}
	}
}

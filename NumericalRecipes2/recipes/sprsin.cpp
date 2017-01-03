#include <cmath>
#include "nr.h"
using namespace std;

void NR::sprsin(Mat_I_DP &a, const DP thresh, Vec_O_DP &sa, Vec_O_INT &ija)
{
	int i,j,k;

	int n=a.nrows();
	int nmax=sa.size();
	for (j=0;j<n;j++) sa[j]=a[j][j];
	ija[0]=n+1;
	k=n;
	for (i=0;i<n;i++) {
		for (j=0;j<n;j++) {
			if (fabs(a[i][j]) >= thresh && i != j) {
				if (++k > nmax) nrerror("sprsin: sa and ija too small");
				sa[k]=a[i][j];
				ija[k]=j;
			}
		}
		ija[i+1]=k+1;
	}
}

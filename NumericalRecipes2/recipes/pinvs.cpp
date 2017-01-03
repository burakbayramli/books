#include <cmath>
#include "nr.h"
using namespace std;

void NR::pinvs(const int ie1, const int ie2, const int je1, const int jsf,
	const int jc1, const int k, Mat3D_O_DP &c, Mat_IO_DP &s)
{
	int jpiv,jp,je2,jcoff,j,irow,ipiv,id,icoff,i;
	DP pivinv,piv,dum,big;

	const int iesize=ie2-ie1;
	Vec_INT indxr(iesize);
	Vec_DP pscl(iesize);
	je2=je1+iesize;
	for (i=ie1;i<ie2;i++) {
		big=0.0;
		for (j=je1;j<je2;j++)
			if (fabs(s[i][j]) > big) big=fabs(s[i][j]);
		if (big == 0.0)
			nrerror("Singular matrix - row all 0, in pinvs");
		pscl[i-ie1]=1.0/big;
		indxr[i-ie1]=0;
	}
	for (id=0;id<iesize;id++) {
		piv=0.0;
		for (i=ie1;i<ie2;i++) {
			if (indxr[i-ie1] == 0) {
				big=0.0;
				for (j=je1;j<je2;j++) {
					if (fabs(s[i][j]) > big) {
						jp=j;
						big=fabs(s[i][j]);
					}
				}
				if (big*pscl[i-ie1] > piv) {
					ipiv=i;
					jpiv=jp;
					piv=big*pscl[i-ie1];
				}
			}
		}
		if (s[ipiv][jpiv] == 0.0)
			nrerror("Singular matrix in routine pinvs");
		indxr[ipiv-ie1]=jpiv+1;
		pivinv=1.0/s[ipiv][jpiv];
		for (j=je1;j<=jsf;j++) s[ipiv][j] *= pivinv;
		s[ipiv][jpiv]=1.0;
		for (i=ie1;i<ie2;i++) {
			if (indxr[i-ie1] != jpiv+1) {
				if (s[i][jpiv] != 0.0) {
					dum=s[i][jpiv];
					for (j=je1;j<=jsf;j++)
						s[i][j] -= dum*s[ipiv][j];
					s[i][jpiv]=0.0;
				}
			}
		}
	}
	jcoff=jc1-je2;
	icoff=ie1-je1;
	for (i=ie1;i<ie2;i++) {
		irow=indxr[i-ie1]+icoff;
		for (j=je2;j<=jsf;j++) c[irow-1][j+jcoff][k]=s[i][j];
	}
}

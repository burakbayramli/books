#include <cmath>
#include "nr.h"
using namespace std;

void NR::sor(Mat_I_DP &a, Mat_I_DP &b, Mat_I_DP &c, Mat_I_DP &d, Mat_I_DP &e,
	Mat_I_DP &f, Mat_IO_DP &u, const DP rjac)
{
	const int MAXITS=1000;
	const DP EPS=1.0e-13;
	int j,l,n,ipass,jsw,lsw;
	DP anorm,anormf=0.0,omega=1.0,resid;

	int jmax=a.nrows();
	for (j=1;j<jmax-1;j++)
		for (l=1;l<jmax-1;l++)
			anormf += fabs(f[j][l]);
	for (n=0;n<MAXITS;n++) {
		anorm=0.0;
		jsw=1;
		for (ipass=0;ipass<2;ipass++) {
			lsw=jsw;
			for (j=1;j<jmax-1;j++) {
				for (l=lsw;l<jmax-1;l+=2) {
					resid=a[j][l]*u[j+1][l]+b[j][l]*u[j-1][l]
						+c[j][l]*u[j][l+1]+d[j][l]*u[j][l-1]
						+e[j][l]*u[j][l]-f[j][l];
					anorm += fabs(resid);
					u[j][l] -= omega*resid/e[j][l];
				}
				lsw=3-lsw;
			}
			jsw=3-jsw;
			omega=(n == 0 && ipass == 0 ? 1.0/(1.0-0.5*rjac*rjac) :
				1.0/(1.0-0.25*rjac*rjac*omega));
		}
		if (anorm < EPS*anormf) return;
	}
	nrerror("MAXITS exceeded");
}

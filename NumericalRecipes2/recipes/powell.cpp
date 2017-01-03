#include <cmath>
#include "nr.h"
using namespace std;

void NR::powell(Vec_IO_DP &p, Mat_IO_DP &xi, const DP ftol, int &iter,
	DP &fret, DP func(Vec_I_DP &))
{
	const int ITMAX=200;
	const DP TINY=1.0e-25;
	int i,j,ibig;
	DP del,fp,fptt,t;

	int n=p.size();
	Vec_DP pt(n),ptt(n),xit(n);
	fret=func(p);
	for (j=0;j<n;j++) pt[j]=p[j];
	for (iter=0;;++iter) {
		fp=fret;
		ibig=0;
		del=0.0;
		for (i=0;i<n;i++) {
			for (j=0;j<n;j++) xit[j]=xi[j][i];
			fptt=fret;
			linmin(p,xit,fret,func);
			if (fptt-fret > del) {
				del=fptt-fret;
				ibig=i+1;
			}
		}
		if (2.0*(fp-fret) <= ftol*(fabs(fp)+fabs(fret))+TINY) {
			return;
		}
		if (iter == ITMAX) nrerror("powell exceeding maximum iterations.");
		for (j=0;j<n;j++) {
			ptt[j]=2.0*p[j]-pt[j];
			xit[j]=p[j]-pt[j];
			pt[j]=p[j];
		}
		fptt=func(ptt);
		if (fptt < fp) {
			t=2.0*(fp-2.0*fret+fptt)*SQR(fp-fret-del)-del*SQR(fp-fptt);
			if (t < 0.0) {
				linmin(p,xit,fret,func);
				for (j=0;j<n;j++) {
					xi[j][ibig-1]=xi[j][n-1];
					xi[j][n-1]=xit[j];
				}
			}
		}
	}
}

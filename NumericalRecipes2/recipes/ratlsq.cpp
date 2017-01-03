#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

void NR::ratlsq(DP fn(const DP), const DP a, const DP b, const int mm,
	const int kk, Vec_O_DP &cof, DP &dev)
{
	const int NPFAC=8,MAXIT=5;
	const DP BIG=1.0e30,PIO2=1.570796326794896619;
	int i,it,j,ncof,npt;
	DP devmax,e,hth,power,sum;

	ncof=mm+kk+1;
	npt=NPFAC*ncof;
	Vec_DP bb(npt),coff(ncof),ee(npt),fs(npt),w(ncof),wt(npt),xs(npt);
	Mat_DP u(npt,ncof),v(ncof,ncof);
	dev=BIG;
	for (i=0;i<npt;i++) {
		if (i < (npt/2)-1) {
			hth=PIO2*i/(npt-1.0);
			xs[i]=a+(b-a)*SQR(sin(hth));
		} else {
			hth=PIO2*(npt-i)/(npt-1.0);
			xs[i]=b-(b-a)*SQR(sin(hth));
		}
		fs[i]=fn(xs[i]);
		wt[i]=1.0;
		ee[i]=1.0;
	}
	e=0.0;
	for (it=0;it<MAXIT;it++) {
		for (i=0;i<npt;i++) {
			power=wt[i];
			bb[i]=power*(fs[i]+SIGN(e,ee[i]));
			for (j=0;j<mm+1;j++) {
				u[i][j]=power;
				power *= xs[i];
			}
			power = -bb[i];
			for (j=mm+1;j<ncof;j++) {
				power *= xs[i];
				u[i][j]=power;
			}
		}
		svdcmp(u,w,v);
		svbksb(u,w,v,bb,coff);
		devmax=sum=0.0;
		for (j=0;j<npt;j++) {
			ee[j]=ratval(xs[j],coff,mm,kk)-fs[j];
			wt[j]=fabs(ee[j]);
			sum += wt[j];
			if (wt[j] > devmax) devmax=wt[j];
		}
		e=sum/npt;
		if (devmax <= dev) {
			for (j=0;j<ncof;j++) cof[j]=coff[j];
			dev=devmax;
		}
		cout << " ratlsq iteration= " << it;
		cout << "  max error= " << setw(10) << devmax << endl;
	}
}

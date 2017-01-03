#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

void NR::linbcg(Vec_I_DP &b, Vec_IO_DP &x, const int itol, const DP tol,
	const int itmax, int &iter, DP &err)
{
	DP ak,akden,bk,bkden=1.0,bknum,bnrm,dxnrm,xnrm,zm1nrm,znrm;
	const DP EPS=1.0e-14;
	int j;

	int n=b.size();
	Vec_DP p(n),pp(n),r(n),rr(n),z(n),zz(n);
	iter=0;
	atimes(x,r,0);
	for (j=0;j<n;j++) {
		r[j]=b[j]-r[j];
		rr[j]=r[j];
	}
	//atimes(r,rr,0);
	if (itol == 1) {
		bnrm=snrm(b,itol);
		asolve(r,z,0);
	}
	else if (itol == 2) {
		asolve(b,z,0);
		bnrm=snrm(z,itol);
		asolve(r,z,0);
	}
	else if (itol == 3 || itol == 4) {
		asolve(b,z,0);
		bnrm=snrm(z,itol);
		asolve(r,z,0);
		znrm=snrm(z,itol);
	} else nrerror("illegal itol in linbcg");
	cout << fixed << setprecision(6);
	while (iter < itmax) {
		++iter;
		asolve(rr,zz,1);
		for (bknum=0.0,j=0;j<n;j++) bknum += z[j]*rr[j];
		if (iter == 1) {
			for (j=0;j<n;j++) {
				p[j]=z[j];
				pp[j]=zz[j];
			}
		} else {
			bk=bknum/bkden;
			for (j=0;j<n;j++) {
				p[j]=bk*p[j]+z[j];
				pp[j]=bk*pp[j]+zz[j];
			}
		}
		bkden=bknum;
		atimes(p,z,0);
		for (akden=0.0,j=0;j<n;j++) akden += z[j]*pp[j];
		ak=bknum/akden;
		atimes(pp,zz,1);
		for (j=0;j<n;j++) {
			x[j] += ak*p[j];
			r[j] -= ak*z[j];
			rr[j] -= ak*zz[j];
		}
		asolve(r,z,0);
		if (itol == 1)
			err=snrm(r,itol)/bnrm;
		else if (itol == 2)
			err=snrm(z,itol)/bnrm;
		else if (itol == 3 || itol == 4) {
			zm1nrm=znrm;
			znrm=snrm(z,itol);
			if (fabs(zm1nrm-znrm) > EPS*znrm) {
				dxnrm=fabs(ak)*snrm(p,itol);
				err=znrm/fabs(zm1nrm-znrm)*dxnrm;
			} else {
				err=znrm/bnrm;
				continue;
			}
			xnrm=snrm(x,itol);
			if (err <= 0.5*xnrm) err /= xnrm;
			else {
				err=znrm/bnrm;
				continue;
			}
		}
		cout << "iter=" << setw(4) << iter+1 << setw(12) << err << endl;
		if (err <= tol) break;
	}
}

#include "nr.h"

namespace {
	void mg(const int j, Mat_IO_DP &u, Mat_I_DP &rhs, Vec_Mat_DP_p &rho,
		DP &trerr)
	{
		using namespace NR;
		const int NPRE=1,NPOST=1;
		const DP ALPHA=0.33;
		int jpost,jpre,nc,nf;
		DP dum=-1.0;

		nf=u.nrows();
		nc=(nf+1)/2;
		Mat_DP temp(nf,nf);
		if (j == 0) {
			matadd(rhs,*rho[j],temp);
			slvsm2(u,temp);
		} else {
			Mat_DP v(nc,nc),ut(nc,nc),tau(nc,nc),tempc(nc,nc);
			for (jpre=0;jpre<NPRE;jpre++) {
				if (trerr < 0.0) {
					matadd(rhs,*rho[j],temp);
					relax2(u,temp);
				}
				else
					relax2(u,*rho[j]);
			}
			rstrct(ut,u);
			copy(v,ut);
			lop(tau,ut);
			lop(temp,u);
			if (trerr < 0.0)
				matsub(temp,rhs,temp);
			rstrct(tempc,temp);
			matsub(tau,tempc,tau);
			if (trerr > 0.0)
				trerr=ALPHA*anorm2(tau);
			mg(j-1,v,tau,rho,dum);
			matsub(v,ut,tempc);
			interp(temp,tempc);
			matadd(u,temp,u);
			for (jpost=0;jpost<NPOST;jpost++) {
				if (trerr < 0.0) {
					matadd(rhs,*rho[j],temp);
					relax2(u,temp);
				}
				else
					relax2(u,*rho[j]);
			}
		}
	}
}

void NR::mgfas(Mat_IO_DP &u, const int maxcyc)
{
	int j,jcycle,ng=0,ngrid,nn;
	DP res,trerr;
	Mat_DP *uj,*uj1;

	int n=u.nrows();
	nn=n;
	while (nn >>= 1) ng++;
	if ((n-1) != (1 << ng))
		nrerror("n-1 must be a power of 2 in mgfas.");
	Vec_Mat_DP_p rho(ng);
	nn=n;
	ngrid=ng-1;
	rho[ngrid]=new Mat_DP(nn,nn);
	copy(*rho[ngrid],u);
	while (nn > 3) {
		nn=nn/2+1;
		rho[--ngrid]=new Mat_DP(nn,nn);
		rstrct(*rho[ngrid],*rho[ngrid+1]);
	}
	nn=3;
	uj=new Mat_DP(nn,nn);
	slvsm2(*uj,*rho[0]);
	for (j=1;j<ng;j++) {
		nn=2*nn-1;
		uj1=uj;
		uj=new Mat_DP(nn,nn);
		Mat_DP temp(nn,nn);
		interp(*uj,*uj1);
		delete uj1;
		for (jcycle=0;jcycle<maxcyc;jcycle++) {
			trerr=1.0;
			mg(j,*uj,temp,rho,trerr);
			lop(temp,*uj);
			matsub(temp,*rho[j],temp);
			res=anorm2(temp);
			if (res < trerr) break;
		}
	}
	copy(u,*uj);
	delete uj;
	for (j=0;j<ng;j++)
		delete rho[j];
}

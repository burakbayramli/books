#include "nr.h"

namespace {
	void mg(int j, Mat_IO_DP &u, Mat_I_DP &rhs)
	{
		using namespace NR;
		const int NPRE=1,NPOST=1;
		int jpost,jpre,nc,nf;

		nf=u.nrows();
		nc=(nf+1)/2;
		if (j == 0)
			slvsml(u,rhs);
		else {
			Mat_DP res(nc,nc),v(0.0,nc,nc),temp(nf,nf);
			for (jpre=0;jpre<NPRE;jpre++)
				relax(u,rhs);
			resid(temp,u,rhs);
			rstrct(res,temp);
			mg(j-1,v,res);
			addint(u,v,temp);
			for (jpost=0;jpost<NPOST;jpost++)
				relax(u,rhs);
		}
	}
}

void NR::mglin(Mat_IO_DP &u, const int ncycle)
{
	int j,jcycle,ng=0,ngrid,nn;
	Mat_DP *uj,*uj1;

	int n=u.nrows();

	nn=n;
	while (nn >>= 1) ng++;
	if ((n-1) != (1 << ng))
		nrerror("n-1 must be a power of 2 in mglin.");
	Vec_Mat_DP_p rho(ng);
	nn=n;
	ngrid=ng-1;
	rho[ngrid] = new Mat_DP(nn,nn);
	copy(*rho[ngrid],u);
	while (nn > 3) {
		nn=nn/2+1;
		rho[--ngrid]=new Mat_DP(nn,nn);
		rstrct(*rho[ngrid],*rho[ngrid+1]);
	}
	nn=3;
	uj=new Mat_DP(nn,nn);
	slvsml(*uj,*rho[0]);
	for (j=1;j<ng;j++) {
		nn=2*nn-1;
		uj1=uj;
		uj=new Mat_DP(nn,nn);
		interp(*uj,*uj1);
		delete uj1;
		for (jcycle=0;jcycle<ncycle;jcycle++)
			mg(j,*uj,*rho[j]);
	}
	copy(u,*uj);
	delete uj;
	for (j=0;j<ng;j++)
		delete rho[j];
}

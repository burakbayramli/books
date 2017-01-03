#include <cmath>
#include "nr.h"
using namespace std;

void NR::period(Vec_I_DP &x, Vec_I_DP &y, const DP ofac, const DP hifac,
	Vec_O_DP &px, Vec_O_DP &py, int &nout, int &jmax, DP &prob)
{
	const DP TWOPI=6.283185307179586476;
	int i,j;
	DP ave,c,cc,cwtau,effm,expy,pnow,pymax,s,ss,sumc,sumcy,sums,sumsh,
		sumsy,swtau,var,wtau,xave,xdif,xmax,xmin,yy,arg,wtemp;

	int n=x.size();
	int np=px.size();
	Vec_DP wi(n),wpi(n),wpr(n),wr(n);
	nout=0.5*ofac*hifac*n;
	if (nout > np) nrerror("output arrays too short in period");
	avevar(y,ave,var);
	if (var == 0.0) nrerror("zero variance in period");
	xmax=xmin=x[0];
	for (j=0;j<n;j++) {
		if (x[j] > xmax) xmax=x[j];
		if (x[j] < xmin) xmin=x[j];
	}
	xdif=xmax-xmin;
	xave=0.5*(xmax+xmin);
	pymax=0.0;
	pnow=1.0/(xdif*ofac);
	for (j=0;j<n;j++) {
		arg=TWOPI*((x[j]-xave)*pnow);
		wpr[j]= -2.0*SQR(sin(0.5*arg));
		wpi[j]=sin(arg);
		wr[j]=cos(arg);
		wi[j]=wpi[j];
	}
	for (i=0;i<nout;i++) {
		px[i]=pnow;
		sumsh=sumc=0.0;
		for (j=0;j<n;j++) {
			c=wr[j];
			s=wi[j];
			sumsh += s*c;
			sumc += (c-s)*(c+s);
		}
		wtau=0.5*atan2(2.0*sumsh,sumc);
		swtau=sin(wtau);
		cwtau=cos(wtau);
		sums=sumc=sumsy=sumcy=0.0;
		for (j=0;j<n;j++) {
			s=wi[j];
			c=wr[j];
			ss=s*cwtau-c*swtau;
			cc=c*cwtau+s*swtau;
			sums += ss*ss;
			sumc += cc*cc;
			yy=y[j]-ave;
			sumsy += yy*ss;
			sumcy += yy*cc;
			wr[j]=((wtemp=wr[j])*wpr[j]-wi[j]*wpi[j])+wr[j];
			wi[j]=(wi[j]*wpr[j]+wtemp*wpi[j])+wi[j];
		}
		py[i]=0.5*(sumcy*sumcy/sumc+sumsy*sumsy/sums)/var;
		if (py[i] >= pymax) pymax=py[jmax=i];
		pnow += 1.0/(ofac*xdif);
	}
	expy=exp(-pymax);
	effm=2.0*nout/ofac;
	prob=effm*expy;
	if (prob > 0.01) prob=1.0-pow(1.0-expy,effm);
}

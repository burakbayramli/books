#include "nr.h"


namespace {
	unsigned char lobyte(unsigned short x)
	{return (x & 0xff);}

	unsigned char hibyte(unsigned short x)
	{return ((x >> 8) & 0xff);}
}

void NR::mpadd(Vec_O_UCHR &w, Vec_I_UCHR &u, Vec_I_UCHR &v)
{
	int j,n_min,p_min;
	unsigned short ireg=0;

	int n=u.size();
	int m=v.size();
	int p=w.size();
	n_min=MIN(n,m);
	p_min=MIN(n_min,p-1);
	for (j=p_min-1;j>=0;j--) {
		ireg=u[j]+v[j]+hibyte(ireg);
		w[j+1]=lobyte(ireg);
	}
	w[0]=hibyte(ireg);
	if (p > p_min+1)
		for (j=p_min+1;j<p;j++) w[j]=0;
}

void NR::mpsub(int &is, Vec_O_UCHR &w, Vec_I_UCHR &u, Vec_I_UCHR &v)
{
	int j,n_min,p_min;
	unsigned short ireg=256;

	int n=u.size();
	int m=v.size();
	int p=w.size();
	n_min=MIN(n,m);
	p_min=MIN(n_min,p);
	for (j=p_min-1;j>=0;j--) {
		ireg=255+u[j]-v[j]+hibyte(ireg);
		w[j]=lobyte(ireg);
	}
	is=hibyte(ireg)-1;
	if (p > p_min)
		for (j=p_min;j<p;j++) w[j]=0;
}

void NR::mpsad(Vec_O_UCHR &w, Vec_I_UCHR &u, const int iv)
{
	int j;
	unsigned short ireg;

	int n=u.size();
	int p=w.size();
	ireg=256*iv;
	for (j=n-1;j>=0;j--) {
		ireg=u[j]+hibyte(ireg);
		if (j+1 < p) w[j+1]=lobyte(ireg);
	}
	w[0]=hibyte(ireg);
	for (j=n+1;j<p;j++) w[j]=0;
}

void NR::mpsmu(Vec_O_UCHR &w, Vec_I_UCHR &u, const int iv)
{
	int j;
	unsigned short ireg=0;

	int n=u.size();
	int p=w.size();
	for (j=n-1;j>=0;j--) {
		ireg=u[j]*iv+hibyte(ireg);
		if (j < p-1) w[j+1]=lobyte(ireg);
	}
	w[0]=hibyte(ireg);
	for (j=n+1;j<p;j++) w[j]=0;
}

void NR::mpsdv(Vec_O_UCHR &w, Vec_I_UCHR &u, const int iv, int &ir)
{
	int i,j,p_min;

	int n=u.size();
	int p=w.size();
	p_min=MIN(n,p);
	ir=0;
	for (j=0;j<p_min;j++) {
		i=256*ir+u[j];
		w[j]=(unsigned char) (i/iv);
		ir=i % iv;
	}
	if (p > p_min)
		for (j=p_min;j<p;j++) w[j]=0;
}

void NR::mpneg(Vec_IO_UCHR &u)
{
	int j;
	unsigned short ireg=256;

	int n=u.size();
	for (j=n-1;j>=0;j--) {
		ireg=255-u[j]+hibyte(ireg);
		u[j]=lobyte(ireg);
	}
}

void NR::mpmov(Vec_O_UCHR &u, Vec_I_UCHR &v)
{
	int j,n_min;

	int n=u.size();
	int m=v.size();
	n_min=MIN(n,m);
	for (j=0;j<n_min;j++) u[j]=v[j];
	if (n > n_min)
		for(j=n_min;j<n-1;j++) u[j]=0;
}

void NR::mplsh(Vec_IO_UCHR &u)
{
	int j;

	int n=u.size();
	for (j=0;j<n-1;j++) u[j]=u[j+1];
	u[n-1]=0;
}

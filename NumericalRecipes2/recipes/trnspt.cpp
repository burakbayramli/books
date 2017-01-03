#include "nr.h"

void NR::trnspt(Vec_IO_INT &iorder, Vec_I_INT &n)
{
	int m1,m2,m3,nn,j,jj;

	int ncity=iorder.size();
	Vec_INT jorder(ncity);
	m1=(n[1]-n[0]+ncity) % ncity;
	m2=(n[4]-n[3]+ncity) % ncity;
	m3=(n[2]-n[5]+ncity) % ncity;
	nn=0;
	for (j=0;j<=m1;j++) {
		jj=(j+n[0]) % ncity;
		jorder[nn++]=iorder[jj];
	}
	for (j=0;j<=m2;j++) {
		jj=(j+n[3]) % ncity;
		jorder[nn++]=iorder[jj];
	}
	for (j=0;j<=m3;j++) {
		jj=(j+n[5]) % ncity;
		jorder[nn++]=iorder[jj];
	}
	for (j=0;j<ncity;j++)
		iorder[j]=jorder[j];
}

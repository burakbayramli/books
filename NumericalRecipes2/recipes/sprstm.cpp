#include <cmath>
#include "nr.h"
using namespace std;

void NR::sprstm(Vec_I_DP &sa, Vec_I_INT &ija, Vec_I_DP &sb, Vec_I_INT &ijb,
	const DP thresh, Vec_O_DP &sc, Vec_O_INT &ijc)
{
	int i,ijma,ijmb,j,k,ma,mb,mbb;
	DP sum;

	if (ija[0] != ijb[0]) nrerror("sprstm: sizes do not match");
	int nmax=sc.size();
	ijc[0]=k=ija[0];
	for (i=0;i<ija[0]-1;i++) {
		for (j=0;j<ijb[0]-1;j++) {
			if (i == j) sum=sa[i]*sb[j]; else sum=0.0e0;
			mb=ijb[j];
			for (ma=ija[i];ma<ija[i+1];ma++) {
				ijma=ija[ma];
				if (ijma == j) sum += sa[ma]*sb[j];
				else {
					while (mb < ijb[j+1]) {
						ijmb=ijb[mb];
						if (ijmb == i) {
							sum += sa[i]*sb[mb++];
							continue;
						} else if (ijmb < ijma) {
							mb++;
							continue;
						} else if (ijmb == ijma) {
							sum += sa[ma]*sb[mb++];
							continue;
						}
						break;
					}
				}
			}
			for (mbb=mb;mbb<ijb[j+1];mbb++) {
				if (ijb[mbb] == i) sum += sa[i]*sb[mbb];
			}
			if (i == j) sc[i]=sum;
			else if (fabs(sum) > thresh) {
				if (k > nmax-1) nrerror("sprstm: sc and ijc too small");
				sc[k]=sum;
				ijc[k++]=j;
			}
		}
		ijc[i+1]=k;
	}
}

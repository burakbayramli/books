#include "nr.h"

void NR::sprstp(Vec_I_DP &sa, Vec_I_INT &ija, Vec_O_DP &sb, Vec_O_INT &ijb)
{
	int j,jl,jm,jp,ju,k,m,n2,noff,inc,iv;
	DP v;

	n2=ija[0];
	for (j=0;j<n2-1;j++) sb[j]=sa[j];
	Vec_INT ija_vec((int *) &ija[n2],ija[n2-1]-ija[0]);
	Vec_INT ijb_vec(&ijb[n2],ija[n2-1]-ija[0]);
	indexx(ija_vec,ijb_vec);
	for (j=n2,k=0;j < ija[n2-1];j++,k++) {
		ijb[j]=ijb_vec[k];
	}
	jp=0;
	for (k=ija[0];k<ija[n2-1];k++) {
		m=ijb[k]+n2;
		sb[k]=sa[m];
		for (j=jp;j<ija[m]+1;j++)
			ijb[j]=k;
		jp=ija[m]+1;
		jl=0;
		ju=n2-1;
		while (ju-jl > 1) {
			jm=(ju+jl)/2;
			if (ija[jm] > m) ju=jm; else jl=jm;
		}
		ijb[k]=jl;
	}
	for (j=jp;j<n2;j++) ijb[j]=ija[n2-1];
	for (j=0;j<n2-1;j++) {
		jl=ijb[j+1]-ijb[j];
		noff=ijb[j];
		inc=1;
		do {
			inc *= 3;
			inc++;
		} while (inc <= jl);
		do {
			inc /= 3;
			for (k=noff+inc;k<noff+jl;k++) {
				iv=ijb[k];
				v=sb[k];
				m=k;
				while (ijb[m-inc] > iv) {
					ijb[m]=ijb[m-inc];
					sb[m]=sb[m-inc];
					m -= inc;
					if (m-noff+1 <= inc) break;
				}
				ijb[m]=iv;
				sb[m]=v;
			}
		} while (inc > 1);
	}
}

#include "nr.h"

void NR::sprspm(Vec_I_DP &sa, Vec_I_INT &ija, Vec_I_DP &sb, Vec_I_INT &ijb,
	Vec_O_DP &sc, Vec_I_INT &ijc)
{
	int i,ijma,ijmb,j,m,ma,mb,mbb,mn;
	DP sum;

	if (ija[0] != ijb[0] || ija[0] != ijc[0])
		nrerror("sprspm: sizes do not match");
	for (i=0;i<ijc[0]-1;i++) {
		m=i+1;
		j=m-1;
		mn=ijc[i];
		sum=sa[i]*sb[i];
		for (;;) {
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
			sc[m-1]=sum;
			sum=0.0;
			if (mn >= ijc[i+1]) break;
			j=ijc[(m= ++mn)-1];
		}
	}
}

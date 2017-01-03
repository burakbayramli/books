#include "nr.h"

void NR::quadct(const DP x, const DP y, Vec_I_DP &xx, Vec_I_DP &yy, DP &fa,
	DP &fb, DP &fc, DP &fd)
{
	int k,na,nb,nc,nd;
	DP ff;

	int nn=xx.size();
	na=nb=nc=nd=0;
	for (k=0;k<nn;k++) {
		if (yy[k] > y)
			xx[k] > x ? ++na : ++nb;
		else
			xx[k] > x ? ++nd : ++nc;
	}
	ff=1.0/nn;
	fa=ff*na;
	fb=ff*nb;
	fc=ff*nc;
	fd=ff*nd;
}

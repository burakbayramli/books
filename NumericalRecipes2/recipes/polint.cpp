#include <cmath>
#include "nr.h"
using namespace std;

void NR::polint(Vec_I_DP &xa, Vec_I_DP &ya, const DP x, DP &y, DP &dy)
{
	int i,m,ns=0;
	DP den,dif,dift,ho,hp,w;

	int n=xa.size();
	Vec_DP c(n),d(n);
	dif=fabs(x-xa[0]);
	for (i=0;i<n;i++) {
		if ((dift=fabs(x-xa[i])) < dif) {
			ns=i;
			dif=dift;
		}
		c[i]=ya[i];
		d[i]=ya[i];
	}
	y=ya[ns--];
	for (m=1;m<n;m++) {
		for (i=0;i<n-m;i++) {
			ho=xa[i]-x;
			hp=xa[i+m]-x;
			w=c[i+1]-d[i];
			if ((den=ho-hp) == 0.0) nrerror("Error in routine polint");
			den=w/den;
			d[i]=hp*den;
			c[i]=ho*den;
		}
		y += (dy=(2*(ns+1) < (n-m) ? c[ns+1] : d[ns--]));
	}
}

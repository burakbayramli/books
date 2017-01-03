#include "nr.h"

void NR::crank(Vec_IO_DP &w, DP &s)
{
	int j=1,ji,jt;
	DP t,rank;

	int n=w.size();
	s=0.0;
	while (j < n) {
		if (w[j] != w[j-1]) {
			w[j-1]=j;
			++j;
		} else {
			for (jt=j+1;jt<=n && w[jt-1]==w[j-1];jt++);
			rank=0.5*(j+jt-1);
			for (ji=j;ji<=(jt-1);ji++)
				w[ji-1]=rank;
			t=jt-j;
			s += (t*t*t-t);
			j=jt;
		}
	}
	if (j == n) w[n-1]=n;
}

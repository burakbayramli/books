#include <cmath>
#include "nr.h"
using namespace std;

namespace {
	inline DP alen(const DP a, const DP b, const DP c, const DP d)
	{
		return sqrt((b-a)*(b-a)+(d-c)*(d-c));
	}
}

DP NR::revcst(Vec_I_DP &x, Vec_I_DP &y, Vec_I_INT &iorder, Vec_IO_INT &n)
{
	int j,ii;
	DP de;
	Vec_DP xx(4),yy(4);

	int ncity=x.size();
	n[2]=(n[0]+ncity-1) % ncity;
	n[3]=(n[1]+1) % ncity;
	for (j=0;j<4;j++) {
		ii=iorder[n[j]];
		xx[j]=x[ii];
		yy[j]=y[ii];
	}
	de = -alen(xx[0],xx[2],yy[0],yy[2]);
	de -= alen(xx[1],xx[3],yy[1],yy[3]);
	de += alen(xx[0],xx[3],yy[0],yy[3]);
	de += alen(xx[1],xx[2],yy[1],yy[2]);
	return de;
}

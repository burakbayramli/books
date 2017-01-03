#include <cmath>
#include "nr.h"
using namespace std;

namespace {
	inline DP alen(const DP a, const DP b, const DP c, const DP d)
	{
		return sqrt((b-a)*(b-a)+(d-c)*(d-c));
	}
}

DP NR::trncst(Vec_I_DP &x, Vec_I_DP &y, Vec_I_INT &iorder, Vec_IO_INT &n)
{
	int j,ii;
	DP de;
	Vec_DP xx(6),yy(6);

	int ncity=x.size();
	n[3]=(n[2]+1) % ncity;
	n[4]=(n[0]+ncity-1) % ncity;
	n[5]=(n[1]+1) % ncity;
	for (j=0;j<6;j++) {
		ii=iorder[n[j]];
		xx[j]=x[ii];
		yy[j]=y[ii];
	}
	de = -alen(xx[1],xx[5],yy[1],yy[5]);
	de -= alen(xx[0],xx[4],yy[0],yy[4]);
	de -= alen(xx[2],xx[3],yy[2],yy[3]);
	de += alen(xx[0],xx[2],yy[0],yy[2]);
	de += alen(xx[1],xx[3],yy[1],yy[3]);
	de += alen(xx[4],xx[5],yy[4],yy[5]);
	return de;
}

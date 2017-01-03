#include <iostream>
#include <iomanip>
#include <cmath>
#include <cstdlib>
#include "nr.h"
using namespace std;

namespace {
	inline DP alen(const DP a, const DP b, const DP c, const DP d)
	{
		return sqrt((b-a)*(b-a)+(d-c)*(d-c));
	}
}

void NR::anneal(Vec_I_DP &x, Vec_I_DP &y, Vec_IO_INT &iorder)
{
	const DP TFACTR=0.9;
	bool ans;
	int i,i1,i2,idec,idum,j,k,nn,nover,nlimit,nsucc;
	static Vec_INT n(6);
	unsigned long iseed;
	DP path,de,t;

	int ncity=x.size();
	nover=100*ncity;
	nlimit=10*ncity;
	path=0.0;
	t=0.5;
	for (i=0;i<ncity-1;i++) {
		i1=iorder[i];
		i2=iorder[i+1];
		path += alen(x[i1],x[i2],y[i1],y[i2]);
	}
	i1=iorder[ncity-1];
	i2=iorder[0];
	path += alen(x[i1],x[i2],y[i1],y[i2]);
	idum = -1;
	iseed=111;
	cout << fixed << setprecision(6);
	for (j=0;j<100;j++) {
		nsucc=0;
		for (k=0;k<nover;k++) {
			do {
				n[0]=int(ncity*ran3(idum));
				n[1]=int((ncity-1)*ran3(idum));
				if (n[1] >= n[0]) ++n[1];
				nn=(n[0]-n[1]+ncity-1) % ncity;
			} while (nn<2);
			idec=irbit1(iseed);
			if (idec == 0) {
				n[2]=n[1]+int(abs(nn-1)*ran3(idum))+1;
				n[2] %= ncity;
				de=trncst(x,y,iorder,n);
				ans=metrop(de,t);
				if (ans) {
					++nsucc;
					path += de;
					trnspt(iorder,n);
				}
			} else {
				de=revcst(x,y,iorder,n);
				ans=metrop(de,t);
				if (ans) {
					++nsucc;
					path += de;
					reverse(iorder,n);
				}
			}
			if (nsucc >= nlimit) break;
		}
		cout << endl << "T = " << setw(12) << t;
		cout << "	 Path Length = " << setw(12) << path << endl;
		cout << "Successful Moves: " << nsucc << endl;
		t *= TFACTR;
		if (nsucc == 0) return;
	}
}

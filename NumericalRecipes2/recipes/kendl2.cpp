#include <cmath>
#include "nr.h"
using namespace std;

void NR::kendl2(Mat_I_DP &tab, DP &tau, DP &z, DP &prob)
{
	int k,l,nn,mm,m2,m1,lj,li,kj,ki;
	DP svar,s=0.0,points,pairs,en2=0.0,en1=0.0;

	int i=tab.nrows();
	int j=tab.ncols();
	nn=i*j;
	points=tab[i-1][j-1];
	for (k=0;k<=nn-2;k++) {
		ki=(k/j);
		kj=k-j*ki;
		points += tab[ki][kj];
		for (l=k+1;l<=nn-1;l++) {
			li=l/j;
			lj=l-j*li;
			mm=(m1=li-ki)*(m2=lj-kj);
			pairs=tab[ki][kj]*tab[li][lj];
			if (mm != 0) {
				en1 += pairs;
				en2 += pairs;
				s += (mm > 0 ? pairs : -pairs);
			} else {
				if (m1 != 0) en1 += pairs;
				if (m2 != 0) en2 += pairs;
			}
		}
	}
	tau=s/sqrt(en1*en2);
	svar=(4.0*points+10.0)/(9.0*points*(points-1.0));
	z=tau/sqrt(svar);
	prob=erfcc(fabs(z)/1.4142136);
}

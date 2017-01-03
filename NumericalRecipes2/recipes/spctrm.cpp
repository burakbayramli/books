#include <fstream>
#include <cmath>
#include "nr.h"
using namespace std;

namespace {
	inline DP window(const int j, const DP a, const DP b)
	{
		return 1.0-fabs((j-a)*b);     // Bartlett
		// return 1.0;                // Square
		// return 1.0-SQR((j-a)*b);   // Welch
	}
}

void NR::spctrm(ifstream &fp, Vec_O_DP &p, const int k, const bool ovrlap)
{
	int mm,m4,kk,joffn,joff,j2,j;
	DP w,facp,facm,sumw=0.0,den=0.0;

	int m=p.size();
	mm=m << 1;
	m4=mm << 1;
	Vec_DP w1(m4),w2(m);
	facm=m;
	facp=1.0/m;
	for (j=0;j<mm;j++)
		sumw += SQR(window(j,facm,facp));
	for (j=0;j<m;j++) p[j]=0.0;
	if (ovrlap)
		for (j=0;j<m;j++)
			fp >> w2[j];
	for (kk=0;kk<k;kk++) {
		for (joff=0;joff<2;joff++) {
			if (ovrlap) {
				for (j=0;j<m;j++) w1[joff+j+j]=w2[j];
				for (j=0;j<m;j++)
					fp >> w2[j];
				joffn=joff+mm-1;
				for (j=0;j<m;j++) w1[joffn+j+j+1]=w2[j];
			} else {
				for (j=joff;j<m4;j+=2)
					fp >> w1[j];
			}
		}
		for (j=0;j<mm;j++) {
			j2=j+j;
			w=window(j,facm,facp);
			w1[j2+1] *= w;
			w1[j2] *= w;
		}
		four1(w1,1);
		p[0] += (SQR(w1[0])+SQR(w1[1]));
		for (j=1;j<m;j++) {
			j2=j+j;
			p[j] += (SQR(w1[j2+1])+SQR(w1[j2])
				+SQR(w1[m4-j2+1])+SQR(w1[m4-j2]));
		}
		den += sumw;
	}
	den *= m4;
	for (j=0;j<m;j++) p[j] /= den;
}

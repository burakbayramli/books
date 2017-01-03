#include <cmath>
#include "nr.h"
using namespace std;

void NR::kstwo(Vec_IO_DP &data1, Vec_IO_DP &data2, DP &d, DP &prob)
{
	int j1=0,j2=0;
	DP d1,d2,dt,en1,en2,en,fn1=0.0,fn2=0.0;

	int n1=data1.size();
	int n2=data2.size();
	sort(data1);
	sort(data2);
	en1=n1;
	en2=n2;
	d=0.0;
	while (j1 < n1 && j2 < n2) {
		if ((d1=data1[j1]) <= (d2=data2[j2])) fn1=j1++/en1;
		if (d2 <= d1) fn2=j2++/en2;
		if ((dt=fabs(fn2-fn1)) > d) d=dt;
	}
	en=sqrt(en1*en2/(en1+en2));
	prob=probks((en+0.12+0.11/en)*d);
}

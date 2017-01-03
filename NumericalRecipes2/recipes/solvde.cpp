#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

void NR::solvde(const int itmax, const DP conv, const DP slowc,
	Vec_I_DP &scalv, Vec_I_INT &indexv, const int nb, Mat_IO_DP &y)
{
	int ic1,ic2,ic3,ic4,it,j,j1,j2,j3,j4,j5,j6,j7,j8,j9;
	int jc1,jcf,jv,k,k1,k2,km,kp,nvars;
	DP err,errj,fac,vmax,vz;

	int ne=y.nrows();
	int m=y.ncols();
	Vec_INT kmax(ne);
	Vec_DP ermax(ne);
	Mat3D_DP c(ne,ne-nb+1,m+1);
	Mat_DP s(ne,2*ne+1);
	k1=0; k2=m;
	nvars=ne*m;
	j1=0,j2=nb,j3=nb,j4=ne,j5=j4+j1;
	j6=j4+j2,j7=j4+j3,j8=j4+j4,j9=j8+j1;
	ic1=0,ic2=ne-nb,ic3=ic2,ic4=ne;
	jc1=0,jcf=ic3;
	for (it=0;it<itmax;it++) {
		k=k1;
		difeq(k,k1,k2,j9,ic3,ic4,indexv,s,y);
		pinvs(ic3,ic4,j5,j9,jc1,k1,c,s);
		for (k=k1+1;k<k2;k++) {
			kp=k;
			difeq(k,k1,k2,j9,ic1,ic4,indexv,s,y);
			red(ic1,ic4,j1,j2,j3,j4,j9,ic3,jc1,jcf,kp,c,s);
			pinvs(ic1,ic4,j3,j9,jc1,k,c,s);
		}
		k=k2;
		difeq(k,k1,k2,j9,ic1,ic2,indexv,s,y);
		red(ic1,ic2,j5,j6,j7,j8,j9,ic3,jc1,jcf,k2,c,s);
		pinvs(ic1,ic2,j7,j9,jcf,k2,c,s);
		bksub(ne,nb,jcf,k1,k2,c);
		err=0.0;
		for (j=0;j<ne;j++) {
			jv=indexv[j];
			errj=vmax=0.0;
			km=0;
			for (k=k1;k<k2;k++) {
				vz=fabs(c[jv][0][k]);
				if (vz > vmax) {
					vmax=vz;
					km=k+1;
				}
				errj += vz;
			}
			err += errj/scalv[j];
			ermax[j]=c[jv][0][km-1]/scalv[j];
			kmax[j]=km;
		}
		err /= nvars;
		fac=(err > slowc ? slowc/err : 1.0);
		for (j=0;j<ne;j++) {
			jv=indexv[j];
			for (k=k1;k<k2;k++)
			y[j][k] -= fac*c[jv][0][k];
		}
		cout << setw(8) << "Iter.";
		cout << setw(10) << "Error" << setw(10) <<  "FAC" << endl;
		cout << setw(6) << it;
		cout << fixed << setprecision(6) << setw(13) << err;
		cout << setw(12) << fac << endl;
		if (err < conv) return;
	}
	nrerror("Too many iterations in solvde");
}

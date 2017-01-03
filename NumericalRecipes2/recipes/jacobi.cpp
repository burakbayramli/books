#include <cmath>
#include "nr.h"
using namespace std;

namespace {
	inline void rot(Mat_IO_DP &a, const DP s, const DP tau, const int i,
		const int j, const int k, const int l)
	{
		DP g,h;

		g=a[i][j];
		h=a[k][l];
		a[i][j]=g-s*(h+g*tau);
		a[k][l]=h+s*(g-h*tau);
	}
}

void NR::jacobi(Mat_IO_DP &a, Vec_O_DP &d, Mat_O_DP &v, int &nrot)
{
	int i,j,ip,iq;
	DP tresh,theta,tau,t,sm,s,h,g,c;

	int n=d.size();
	Vec_DP b(n),z(n);
	for (ip=0;ip<n;ip++) {
		for (iq=0;iq<n;iq++) v[ip][iq]=0.0;
		v[ip][ip]=1.0;
	}
	for (ip=0;ip<n;ip++) {
		b[ip]=d[ip]=a[ip][ip];
		z[ip]=0.0;
	}
	nrot=0;
	for (i=1;i<=50;i++) {
		sm=0.0;
		for (ip=0;ip<n-1;ip++) {
			for (iq=ip+1;iq<n;iq++)
				sm += fabs(a[ip][iq]);
		}
		if (sm == 0.0)
			return;
		if (i < 4)
			tresh=0.2*sm/(n*n);
		else
			tresh=0.0;
		for (ip=0;ip<n-1;ip++) {
			for (iq=ip+1;iq<n;iq++) {
				g=100.0*fabs(a[ip][iq]);
				if (i > 4 && (fabs(d[ip])+g) == fabs(d[ip])
					&& (fabs(d[iq])+g) == fabs(d[iq]))
						a[ip][iq]=0.0;
				else if (fabs(a[ip][iq]) > tresh) {
					h=d[iq]-d[ip];
					if ((fabs(h)+g) == fabs(h))
						t=(a[ip][iq])/h;
					else {
						theta=0.5*h/(a[ip][iq]);
						t=1.0/(fabs(theta)+sqrt(1.0+theta*theta));
						if (theta < 0.0) t = -t;
					}
					c=1.0/sqrt(1+t*t);
					s=t*c;
					tau=s/(1.0+c);
					h=t*a[ip][iq];
					z[ip] -= h;
					z[iq] += h;
					d[ip] -= h;
					d[iq] += h;
					a[ip][iq]=0.0;
					for (j=0;j<ip;j++)
						rot(a,s,tau,j,ip,j,iq);
					for (j=ip+1;j<iq;j++)
						rot(a,s,tau,ip,j,j,iq);
					for (j=iq+1;j<n;j++)
						rot(a,s,tau,ip,j,iq,j);
					for (j=0;j<n;j++)
						rot(v,s,tau,j,ip,j,iq);
					++nrot;
				}
			}
		}
		for (ip=0;ip<n;ip++) {
			b[ip] += z[ip];
			d[ip]=b[ip];
			z[ip]=0.0;
		}
	}
	nrerror("Too many iterations in routine jacobi");
}

#include <cmath>
#include <complex>
#include "nr.h"
using namespace std;

void NR::hqr(Mat_IO_DP &a, Vec_O_CPLX_DP &wri)
{
	int nn,m,l,k,j,its,i,mmin;
	DP z,y,x,w,v,u,t,s,r,q,p,anorm;

	int n=a.nrows();
	anorm=0.0;
	for (i=0;i<n;i++)
		for (j=MAX(i-1,0);j<n;j++)
			anorm += fabs(a[i][j]);
	nn=n-1;
	t=0.0;
	while (nn >= 0) {
		its=0;
		do {
			for (l=nn;l>0;l--) {
				s=fabs(a[l-1][l-1])+fabs(a[l][l]);
				if (s == 0.0) s=anorm;
				if (fabs(a[l][l-1]) + s == s) {
					a[l][l-1] = 0.0;
					break;
				}
			}
			x=a[nn][nn];
			if (l == nn) {
				wri[nn--]=x+t;
			} else {
				y=a[nn-1][nn-1];
				w=a[nn][nn-1]*a[nn-1][nn];
				if (l == nn-1) {
					p=0.5*(y-x);
					q=p*p+w;
					z=sqrt(fabs(q));
					x += t;
					if (q >= 0.0) {
						z=p+SIGN(z,p);
						wri[nn-1]=wri[nn]=x+z;
						if (z != 0.0) wri[nn]=x-w/z;
					} else {
						wri[nn]=complex<DP>(x+p,z);
						wri[nn-1]=conj(wri[nn]);
					}
					nn -= 2;
				} else {
					if (its == 30) nrerror("Too many iterations in hqr");
					if (its == 10 || its == 20) {
						t += x;
						for (i=0;i<nn+1;i++) a[i][i] -= x;
						s=fabs(a[nn][nn-1])+fabs(a[nn-1][nn-2]);
						y=x=0.75*s;
						w = -0.4375*s*s;
					}
					++its;
					for (m=nn-2;m>=l;m--) {
						z=a[m][m];
						r=x-z;
						s=y-z;
						p=(r*s-w)/a[m+1][m]+a[m][m+1];
						q=a[m+1][m+1]-z-r-s;
						r=a[m+2][m+1];
						s=fabs(p)+fabs(q)+fabs(r);
						p /= s;
						q /= s;
						r /= s;
						if (m == l) break;
						u=fabs(a[m][m-1])*(fabs(q)+fabs(r));
						v=fabs(p)*(fabs(a[m-1][m-1])+fabs(z)+fabs(a[m+1][m+1]));
						if (u+v == v) break;
					}
					for (i=m;i<nn-1;i++) {
						a[i+2][i]=0.0;
						if (i != m) a[i+2][i-1]=0.0;
					}
					for (k=m;k<nn;k++) {
						if (k != m) {
							p=a[k][k-1];
							q=a[k+1][k-1];
							r=0.0;
							if (k+1 != nn) r=a[k+2][k-1];
							if ((x=fabs(p)+fabs(q)+fabs(r)) != 0.0) {
								p /= x;
								q /= x;
								r /= x;
							}
						}
						if ((s=SIGN(sqrt(p*p+q*q+r*r),p)) != 0.0) {
							if (k == m) {
								if (l != m)
								a[k][k-1] = -a[k][k-1];
							} else
								a[k][k-1] = -s*x;
							p += s;
							x=p/s;
							y=q/s;
							z=r/s;
							q /= p;
							r /= p;
							for (j=k;j<nn+1;j++) {
								p=a[k][j]+q*a[k+1][j];
								if (k+1 != nn) {
									p += r*a[k+2][j];
									a[k+2][j] -= p*z;
								}
								a[k+1][j] -= p*y;
								a[k][j] -= p*x;
							}
							mmin = nn < k+3 ? nn : k+3;
							for (i=l;i<mmin+1;i++) {
								p=x*a[i][k]+y*a[i][k+1];
								if (k != (nn)) {
									p += z*a[i][k+2];
									a[i][k+2] -= p*r;
								}
								a[i][k+1] -= p*q;
								a[i][k] -= p;
							}
						}
					}
				}
			}
		} while (l+1 < nn);
	}
}

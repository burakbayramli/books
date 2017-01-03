#include "nr.h"

void NR::cyclic(Vec_I_DP &a, Vec_I_DP &b, Vec_I_DP &c, const DP alpha,
	const DP beta, Vec_I_DP &r, Vec_O_DP &x)
{
	int i;
	DP fact,gamma;

	int n=a.size();
	if (n <= 2) nrerror("n too small in cyclic");
	Vec_DP bb(n),u(n),z(n);
	gamma = -b[0];
	bb[0]=b[0]-gamma;
	bb[n-1]=b[n-1]-alpha*beta/gamma;
	for (i=1;i<n-1;i++) bb[i]=b[i];
	tridag(a,bb,c,r,x);
	u[0]=gamma;
	u[n-1]=alpha;
	for (i=1;i<n-1;i++) u[i]=0.0;
	tridag(a,bb,c,u,z);
	fact=(x[0]+beta*x[n-1]/gamma)/
		(1.0+z[0]+beta*z[n-1]/gamma);
	for (i=0;i<n;i++) x[i] -= fact*z[i];
}

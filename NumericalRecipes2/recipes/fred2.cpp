#include "nr.h"

void NR::fred2(const DP a, const DP b, Vec_O_DP &t, Vec_O_DP &f, Vec_O_DP &w,
	DP g(const DP), DP ak(const DP, const DP))
{
	int i,j;
	DP d;

	int n=t.size();
	Mat_DP omk(n,n);
	Vec_INT indx(n);
	gauleg(a,b,t,w);
	for (i=0;i<n;i++) {
		for (j=0;j<n;j++)
			omk[i][j]=DP(i == j)-ak(t[i],t[j])*w[j];
		f[i]=g(t[i]);
	}
	ludcmp(omk,indx,d);
	lubksb(omk,indx,f);
}

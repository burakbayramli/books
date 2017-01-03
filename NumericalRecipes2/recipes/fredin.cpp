#include "nr.h"

DP NR::fredin(const DP x, const DP a, const DP b, Vec_I_DP &t, Vec_I_DP &f,
	Vec_I_DP &w, DP g(const DP), DP ak(const DP, const DP))
{
	int i;
	DP sum=0.0;

	int n=t.size();
	for (i=0;i<n;i++) sum += ak(x,t[i])*w[i]*f[i];
	return g(x)+sum;
}

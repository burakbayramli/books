#include "nr.h"

DP NR::factln(const int n)
{
	static DP a[101];

	if (n < 0) nrerror("Negative factorial in routine factln");
	if (n <= 1) return 0.0;
	if (n <= 100)
		return (a[n] != 0.0 ? a[n] : (a[n]=gammln(n+1.0)));
	else return gammln(n+1.0);
}

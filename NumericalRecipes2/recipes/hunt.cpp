#include "nr.h"

void NR::hunt(Vec_I_DP &xx, const DP x, int &jlo)
{
	int jm,jhi,inc;
	bool ascnd;

	int n=xx.size();
	ascnd=(xx[n-1] >= xx[0]);
	if (jlo < 0 || jlo > n-1) {
		jlo=-1;
		jhi=n;
	} else {
		inc=1;
		if (x >= xx[jlo] == ascnd) {
			if (jlo == n-1) return;
			jhi=jlo+1;
			while (x >= xx[jhi] == ascnd) {
				jlo=jhi;
				inc += inc;
				jhi=jlo+inc;
				if (jhi > n-1) {
					jhi=n;
					break;
				}
			}
		} else {
			if (jlo == 0) {
				jlo=-1;
				return;
			}
			jhi=jlo--;
			while (x < xx[jlo] == ascnd) {
				jhi=jlo;
				inc <<= 1;
				if (inc >= jhi) {
					jlo=-1;
					break;
				}
				else jlo=jhi-inc;
			}
		}
	}
	while (jhi-jlo != 1) {
		jm=(jhi+jlo) >> 1;
		if (x >= xx[jm] == ascnd)
			jlo=jm;
		else
			jhi=jm;
	}
	if (x == xx[n-1]) jlo=n-2;
	if (x == xx[0]) jlo=0;
}

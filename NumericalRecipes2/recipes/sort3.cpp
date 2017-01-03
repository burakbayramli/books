#include "nr.h"

void NR::sort3(Vec_IO_DP &ra, Vec_IO_DP &rb, Vec_IO_DP &rc)
{
	int j;

	int n=ra.size();
	Vec_INT iwksp(n);
	Vec_DP wksp(n);
	indexx(ra,iwksp);
	for (j=0;j<n;j++) wksp[j]=ra[j];
	for (j=0;j<n;j++) ra[j]=wksp[iwksp[j]];
	for (j=0;j<n;j++) wksp[j]=rb[j];
	for (j=0;j<n;j++) rb[j]=wksp[iwksp[j]];
	for (j=0;j<n;j++) wksp[j]=rc[j];
	for (j=0;j<n;j++) rc[j]=wksp[iwksp[j]];
}

#include "nr.h"

void NR::eclass(Vec_O_INT &nf, Vec_I_INT &lista, Vec_I_INT &listb)
{
	int l,k,j;

	int n=nf.size();
	int m=lista.size();
	for (k=0;k<n;k++) nf[k]=k;
	for (l=0;l<m;l++) {
		j=lista[l];
		while (nf[j] != j) j=nf[j];
		k=listb[l];
		while (nf[k] != k) k=nf[k];
		if (j != k) nf[j]=k;
	}
	for (j=0;j<n;j++)
		while (nf[j] != nf[nf[j]]) nf[j]=nf[nf[j]];
}

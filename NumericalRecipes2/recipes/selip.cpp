#include "nr.h"

DP NR::selip(const int k, Vec_I_DP &arr)
{
	const int M=64;
	const DP BIG=1.0e30;
	int i,j,jl,jm,ju,kk,mm,nlo,nxtmm;
	DP ahi,alo,sum;
	Vec_INT isel(M+2);
	Vec_DP sel(M+2);

	int n=arr.size();
	if (k < 0 || k > n-1) nrerror("bad input to selip");
	kk=k;
	ahi=BIG;
	alo = -BIG;
	for (;;) {
		mm=nlo=0;
		sum=0.0;
		nxtmm=M+1;
		for (i=0;i<n;i++) {
			if (arr[i] >= alo && arr[i] <= ahi) {
				mm++;
				if (arr[i] == alo) nlo++;
				if (mm <= M) sel[mm-1]=arr[i];
				else if (mm == nxtmm) {
					nxtmm=mm+mm/M;
					sel[(i+2+mm+kk) % M]=arr[i];
				}
				sum += arr[i];
			}
		}
		if (kk < nlo) {
			return alo;
		}
		else if (mm < M+1) {
			shell(mm,sel);
			ahi = sel[kk];
			return ahi;
		}
		sel[M]=sum/mm;
		shell(M+1,sel);
		sel[M+1]=ahi;
		for (j=0;j<M+2;j++) isel[j]=0;
		for (i=0;i<n;i++) {
			if (arr[i] >= alo && arr[i] <= ahi) {
				jl=0;
				ju=M+2;
				while (ju-jl > 1) {
					jm=(ju+jl)/2;
					if (arr[i] >= sel[jm-1]) jl=jm;
					else ju=jm;
				}
				isel[ju-1]++;
			}
		}
		j=0;
		while (kk >= isel[j]) {
			alo=sel[j];
			kk -= isel[j++];
		}
		ahi=sel[j];
	}
}

#include "nr.h"

void NR::piksr2(Vec_IO_DP &arr, Vec_IO_DP &brr)
{
	int i,j;
	DP a,b;

	int n=arr.size();
	for (j=1;j<n;j++) {
		a=arr[j];
		b=brr[j];
		i=j;
		while (i > 0 && arr[i-1] > a) {
			arr[i]=arr[i-1];
			brr[i]=brr[i-1];
			i--;
		}
		arr[i]=a;
		brr[i]=b;
	}
}

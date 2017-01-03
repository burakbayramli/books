#include "nr.h"

void NR::piksrt(Vec_IO_DP &arr)
{
	int i,j;
	DP a;

	int n=arr.size();
	for (j=1;j<n;j++) {
		a=arr[j];
		i=j;
		while (i > 0 && arr[i-1] > a) {
			arr[i]=arr[i-1];
			i--;
		}
		arr[i]=a;
	}
}

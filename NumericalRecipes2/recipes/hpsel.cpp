#include "nr.h"

void NR::hpsel(Vec_I_DP &arr, Vec_O_DP &heap)
{
	int i,j,k;

	int m=heap.size();
	int n=arr.size();
	if (m > n/2 || m < 1) nrerror("probable misuse of hpsel");
	for (i=0;i<m;i++) heap[i]=arr[i];
	sort(heap);
	for (i=m;i<n;i++) {
		if (arr[i] > heap[0]) {
			heap[0]=arr[i];
			for (j=0;;) {
				k=(j << 1)+1;
				if (k > m-1) break;
				if (k != (m-1) && heap[k] > heap[k+1]) k++;
				if (heap[j] <= heap[k]) break;
				SWAP(heap[k],heap[j]);
				j=k;
			}
		}
	}
}

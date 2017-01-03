#include "nr.h"

void NR::daub4(Vec_IO_DP &a, const int n, const int isign)
{
	const DP C0=0.4829629131445341,C1=0.8365163037378079,
		C2=0.2241438680420134,C3=-0.1294095225512604;
	int nh,i,j;

	if (n < 4) return;
	Vec_DP wksp(n);
	nh=n >> 1;
	if (isign >= 0) {
		for (i=0,j=0;j<n-3;j+=2,i++) {
			wksp[i]=C0*a[j]+C1*a[j+1]+C2*a[j+2]+C3*a[j+3];
			wksp[i+nh]=C3*a[j]-C2*a[j+1]+C1*a[j+2]-C0*a[j+3];
		}
		wksp[i]=C0*a[n-2]+C1*a[n-1]+C2*a[0]+C3*a[1];
		wksp[i+nh]=C3*a[n-2]-C2*a[n-1]+C1*a[0]-C0*a[1];
	} else {
		wksp[0]=C2*a[nh-1]+C1*a[n-1]+C0*a[0]+C3*a[nh];
		wksp[1]=C3*a[nh-1]-C0*a[n-1]+C1*a[0]-C2*a[nh];
		for (i=0,j=2;i<nh-1;i++) {
			wksp[j++]=C2*a[i]+C1*a[i+nh]+C0*a[i+1]+C3*a[i+nh+1];
			wksp[j++]=C3*a[i]-C0*a[i+nh]+C1*a[i+1]-C2*a[i+nh+1];
		}
	}
	for (i=0;i<n;i++) a[i]=wksp[i];
}

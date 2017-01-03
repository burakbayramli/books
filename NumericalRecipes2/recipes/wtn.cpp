#include "nr.h"

void NR::wtn(Vec_IO_DP &a, Vec_I_INT &nn, const int isign,
	void wtstep(Vec_IO_DP &, const int, const int))
{
	int idim,i1,i2,i3,k,n,nnew,nprev=1,nt,ntot=1;

	int ndim=nn.size();
	for (idim=0;idim<ndim;idim++) ntot *= nn[idim];
	for (idim=0;idim<ndim;idim++) {
		n=nn[idim];
		Vec_DP wksp(n);
		nnew=n*nprev;
		if (n > 4) {
			for (i2=0;i2<ntot;i2+=nnew) {
				for (i1=0;i1<nprev;i1++) {
					for (i3=i1+i2,k=0;k<n;k++,i3+=nprev) wksp[k]=a[i3];
					if (isign >= 0) {
						for(nt=n;nt>=4;nt >>= 1)
							wtstep(wksp,nt,isign);
					} else {
						for(nt=4;nt<=n;nt <<= 1)
							wtstep(wksp,nt,isign);
					}
					for (i3=i1+i2,k=0;k<n;k++,i3+=nprev) a[i3]=wksp[k];
				}
			}
		}
		nprev=nnew;
	}
}

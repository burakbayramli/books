#include <cmath>
#include "nr.h"
using namespace std;

void NR::rlft3(Mat3D_IO_DP &data, Mat_IO_DP &speq, const int isign)
{
	int i1,i2,i3,j1,j2,j3,ii3,k1,k2,k3,k4;
	DP theta,wi,wpi,wpr,wr,wtemp;
	DP c1,c2,h1r,h1i,h2r,h2i;
	Vec_INT nn(3);

	int nn1=data.dim1();
	int nn2=data.dim2();
	int nn3=data.dim3();
	c1=0.5;
	c2= -0.5*isign;
	theta=isign*(6.28318530717959/nn3);
	wtemp=sin(0.5*theta);
	wpr= -2.0*wtemp*wtemp;
	wpi=sin(theta);
	nn[0]=nn1;
	nn[1]=nn2;
	nn[2]=nn3 >> 1;
	Vec_DP data_v(&data[0][0][0],nn1*nn2*nn3);
	if (isign == 1) {
		fourn(data_v,nn,isign);
		k1=0;
		for (i1=0;i1<nn1;i1++)
			for (i2=0,j2=0;i2<nn2;i2++,k1+=nn3) {
				speq[i1][j2++]=data_v[k1];
				speq[i1][j2++]=data_v[k1+1];
			}
	}
	for (i1=0;i1<nn1;i1++) {
		j1=(i1 != 0 ? nn1-i1 : 0);
		wr=1.0;
		wi=0.0;
		for (ii3=0;ii3<=(nn3>>1);ii3+=2) {
			k1=i1*nn2*nn3;
			k3=j1*nn2*nn3;
			for (i2=0;i2<nn2;i2++,k1+=nn3) {
				if (ii3 == 0) {
					j2=(i2 != 0 ? ((nn2-i2)<<1) : 0);
					h1r=c1*(data_v[k1]+speq[j1][j2]);
					h1i=c1*(data_v[k1+1]-speq[j1][j2+1]);
					h2i=c2*(data_v[k1]-speq[j1][j2]);
					h2r= -c2*(data_v[k1+1]+speq[j1][j2+1]);
					data_v[k1]=h1r+h2r;
					data_v[k1+1]=h1i+h2i;
					speq[j1][j2]=h1r-h2r;
					speq[j1][j2+1]=h2i-h1i;
				} else {
					j2=(i2 != 0 ? nn2-i2 : 0);
					j3=nn3-ii3;
					k2=k1+ii3;
					k4=k3+j2*nn3+j3;
					h1r=c1*(data_v[k2]+data_v[k4]);
					h1i=c1*(data_v[k2+1]-data_v[k4+1]);
					h2i=c2*(data_v[k2]-data_v[k4]);
					h2r= -c2*(data_v[k2+1]+data_v[k4+1]);
					data_v[k2]=h1r+wr*h2r-wi*h2i;
					data_v[k2+1]=h1i+wr*h2i+wi*h2r;
					data_v[k4]=h1r-wr*h2r+wi*h2i;
					data_v[k4+1]= -h1i+wr*h2i+wi*h2r;
				}
			}
			wr=(wtemp=wr)*wpr-wi*wpi+wr;
			wi=wi*wpr+wtemp*wpi+wi;
		}
	}
	if (isign == -1) fourn(data_v,nn,isign);
	k1=0;
	for (i1=0;i1<nn1;i1++)
		for (i2=0;i2<nn2;i2++)
			for (i3=0;i3<nn3;i3++) data[i1][i2][i3]=data_v[k1++];
}

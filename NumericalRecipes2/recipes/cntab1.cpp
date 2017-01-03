#include <cmath>
#include "nr.h"
using namespace std;

void NR::cntab1(Mat_I_INT &nn, DP &chisq, DP &df, DP &prob, DP &cramrv, DP &ccc)
{
	const DP TINY=1.0e-30;
	int i,j,nnj,nni,minij;
	DP sum=0.0,expctd,temp;

	int ni=nn.nrows();
	int nj=nn.ncols();
	Vec_DP sumi(ni),sumj(nj);
	nni=ni;
	nnj=nj;
	for (i=0;i<ni;i++) {
		sumi[i]=0.0;
		for (j=0;j<nj;j++) {
			sumi[i] += nn[i][j];
			sum += nn[i][j];
		}
		if (sumi[i] == 0.0) --nni;
	}
	for (j=0;j<nj;j++) {
		sumj[j]=0.0;
		for (i=0;i<ni;i++) sumj[j] += nn[i][j];
		if (sumj[j] == 0.0) --nnj;
	}
	df=nni*nnj-nni-nnj+1;
	chisq=0.0;
	for (i=0;i<ni;i++) {
		for (j=0;j<nj;j++) {
			expctd=sumj[j]*sumi[i]/sum;
			temp=nn[i][j]-expctd;
			chisq += temp*temp/(expctd+TINY);
		}
	}
	prob=gammq(0.5*df,0.5*chisq);
	minij = nni < nnj ? nni-1 : nnj-1;
	cramrv=sqrt(chisq/(sum*minij));
	ccc=sqrt(chisq/(chisq+sum));
}

#include <cmath>
#include "nr.h"
using namespace std;

void NR::qrupdt(Mat_IO_DP &r, Mat_IO_DP &qt, Vec_IO_DP &u, Vec_I_DP &v)
{
	int i,k;

	int n=u.size();
	for (k=n-1;k>=0;k--)
		if (u[k] != 0.0) break;
	if (k < 0) k=0;
	for (i=k-1;i>=0;i--) {
		rotate(r,qt,i,u[i],-u[i+1]);
		if (u[i] == 0.0)
			u[i]=fabs(u[i+1]);
		else if (fabs(u[i]) > fabs(u[i+1]))
			u[i]=fabs(u[i])*sqrt(1.0+SQR(u[i+1]/u[i]));
		else u[i]=fabs(u[i+1])*sqrt(1.0+SQR(u[i]/u[i+1]));
	}
	for (i=0;i<n;i++) r[0][i] += u[0]*v[i];
	for (i=0;i<k;i++)
		rotate(r,qt,i,r[i][i],-r[i+1][i]);
}

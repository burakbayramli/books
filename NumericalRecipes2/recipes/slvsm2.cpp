#include <cmath>
#include "nr.h"
using namespace std;

void NR::slvsm2(Mat_O_DP &u, Mat_I_DP &rhs)
{
	int i,j;
	DP disc,fact,h=0.5;

	for (i=0;i<3;i++)
		for (j=0;j<3;j++)
			u[i][j]=0.0;
	fact=2.0/(h*h);
	disc=sqrt(fact*fact+rhs[1][1]);
	u[1][1]= -rhs[1][1]/(fact+disc);
}

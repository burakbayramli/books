#include <cmath>
#include "nr.h"
using namespace std;

DP NR::anorm2(Mat_I_DP &a)
{
	int i,j;
	DP sum=0.0;

	int n=a.nrows();
	for (j=0;j<n;j++)
		for (i=0;i<n;i++)
			sum += a[i][j]*a[i][j];
	return sqrt(sum)/n;
}

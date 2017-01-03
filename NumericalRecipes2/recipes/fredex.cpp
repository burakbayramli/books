#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

int main(void)	// Program fredex
{
	const int N=40;
	const DP PI=3.141592653589793238;
	int j;
	DP d,x;
	Vec_INT indx(N);
	Vec_DP g(N);
	Mat_DP a(N,N);

	NR::quadmx(a);
	NR::ludcmp(a,indx,d);
	for (j=0;j<N;j++)
		g[j]=sin(j*PI/(N-1));
	NR::lubksb(a,indx,g);
	for (j=0;j<N;j++) {
		x=j*PI/(N-1);
		cout << fixed << setprecision(2) << setw(6) << (j+1);
		cout << setprecision(6) << setw(13) << x << setw(13) << g[j] << endl;
	}
	return 0;
}

#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

DP NR::ei(const DP x)
{
	const int MAXIT=100;
	const DP EULER=0.577215664901533;
	const DP EPS=numeric_limits<DP>::epsilon();
	const DP FPMIN=numeric_limits<DP>::min()/EPS;
	int k;
	DP fact,prev,sum,term;

	if (x <= 0.0) nrerror("Bad argument in ei");
	if (x < FPMIN) return log(x)+EULER;
	if (x <= -log(EPS)) {
		sum=0.0;
		fact=1.0;
		for (k=1;k<=MAXIT;k++) {
			fact *= x/k;
			term=fact/k;
			sum += term;
			if (term < EPS*sum) break;
		}
		if (k > MAXIT) nrerror("Series failed in ei");
		return sum+log(x)+EULER;
	} else {
		sum=0.0;
		term=1.0;
		for (k=1;k<=MAXIT;k++) {
			prev=term;
			term *= k/x;
			if (term < EPS) break;
			if (term < prev) sum += term;
			else {
				sum -= prev;
				break;
			}
		}
		return exp(x)*(1.0+sum)/x;
	}
}

// Auxiliary routine for sphfpt

#include "nrtypes.h"

extern int m,n;
extern DP c2,dx,gmma;

void derivs(DP x,DP y[],DP dydx[])
{
        dydx[0]=y[1];
        dydx[1]=(2.0*x*(m+1.0)*y[1]-(y[2]-c2*x*x)*y[0])/(1.0-x*x);
        dydx[2]=0.0;
}

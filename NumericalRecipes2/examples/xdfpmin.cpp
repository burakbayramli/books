#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine dfpmin

int nfunc,ndfunc;

DP func(Vec_I_DP &x)
{
        DP x1p2sqr=SQR(2.0+x[0]);

        nfunc++;
        return 10.0*SQR(SQR(x[1])*(3.0-x[0])-SQR(x[0])*(3.0+x[0]))+
            x1p2sqr/(1.0+x1p2sqr);
}

void dfunc(Vec_I_DP &x, Vec_O_DP &df)
{
        DP x1sqr=SQR(x[0]),x2sqr=SQR(x[1]),x1p2=x[0]+2.0;
        DP x1p2sqr=SQR(x1p2);

        ndfunc++;
        df[0]=20.0*(x2sqr*(3.0-x[0])-x1sqr*(3.0+x[0]))*
          (-x2sqr-6.0*x[0]-3.0*x1sqr)+2.0*x1p2/(1.0+x1p2sqr)
          -2.0*x1p2*x1p2sqr/SQR((1.0+x1p2sqr));
        df[1]=40.0*(x2sqr*(3.0-x[0])-x1sqr*(3.0+x[0]))*x[1]*(3.0-x[0]);
}

int main(void)
{
        const int NDIM=2;
        const DP GTOL=1.0e-4;
        int iter;
        DP fret;
        Vec_DP p(NDIM);

        cout << "True minimum is at (-2.0,+-0.89442719)" << endl;
        nfunc=ndfunc=0;
        p[0]=0.1; p[1]=4.2;
        cout << fixed << setprecision(4);
        cout << "Starting vector: (" << setw(7) << p[0];
        cout << "," << setw(7) << p[1] << ")" << endl;
        cout << fixed << setprecision(6);
        NR::dfpmin(p,GTOL,iter,fret,func,dfunc);
        cout << "Iterations: " << iter << endl;
        cout << "Func. evals: " << nfunc << endl;
        cout << "Deriv. evals: " << ndfunc << endl;
        cout << "Solution vector: (" << setw(9) << p[0];
        cout << "," << setw(9) << p[1] << ")" << endl;
        cout << "Func. value at solution " << setw(14) << fret << endl;
        return 0;
}

#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine cyclic

int main(void)
{
        const int N=20;
        int i,idum=(-23);
        DP d,alpha,beta;
        Vec_INT indx(N);
        Vec_DP a(N),b(N),c(N),r(N),x(N);
        Mat_DP aa(0.0,N,N);      // Initialize to 0.0

        for (i=0;i<N;i++) {
          aa[i][i]=(b[i]=NR::ran2(idum));
          r[i]=NR::ran2(idum);
        }
        for (i=0;i<N-1;i++) {
          aa[i+1][i]=(a[i+1]=NR::ran2(idum));
          aa[i][i+1]=(c[i]=NR::ran2(idum));
        }
        aa[N-1][0]=(alpha=NR::ran2(idum));
        aa[0][N-1]=(beta=NR::ran2(idum));
        NR::cyclic(a,b,c,alpha,beta,r,x);
        NR::ludcmp(aa,indx,d);
        NR::lubksb(aa,indx,r);
        cout << scientific << setprecision(6);
        for (i=0;i<N;i++) {
          cout << setw(4) << i;
          cout << setw(15) << (x[i]-r[i])/(x[i]+r[i]) << endl;
        }
        return 0;
}

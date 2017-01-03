#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine mprove

int main(void)
{
        const int N=5,NP=N;
        const DP a_d[NP*NP]=
          {1.0,2.0,3.0,4.0,5.0,
          2.0,3.0,4.0,5.0,1.0,
          1.0,1.0,1.0,1.0,1.0,
          4.0,5.0,1.0,2.0,3.0,
          5.0,1.0,2.0,3.0,4.0};
        const DP b_d[N]={1.0,1.0,1.0,1.0,1.0};
        int i,j,idum=(-13);
        DP d;
        Vec_INT indx(N);
        Vec_DP x(N),b(b_d,N);
        Mat_DP aa(N,N),a(a_d,N,N);

        for (i=0;i<N;i++) {
          x[i]=b[i];
          for (j=0;j<N;j++)
            aa[i][j]=a[i][j];
        }
        NR::ludcmp(aa,indx,d);
        NR::lubksb(aa,indx,x);
        cout << endl << "Solution vector for the equations:" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<N;i++) cout << setw(12) << x[i];
        cout << endl;
        // now phoney up x and let mprove fix it
        for (i=0;i<N;i++) x[i] *= (1.0+0.2*NR::ran3(idum));
        cout << endl << "Solution vector with noise added:" << endl;
        for (i=0;i<N;i++) cout << setw(12) << x[i];
        cout << endl;
        NR::mprove(a,aa,indx,b,x);
        cout << endl << "Solution vector recovered by mprove:" << endl;
        for (i=0;i<N;i++) cout << setw(12) << x[i];
        cout << endl;
        return 0;
}

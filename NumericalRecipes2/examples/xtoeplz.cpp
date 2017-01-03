#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine toeplz

int main(void)
{
        const int N=5,TWON1=2*N-1;
        int i,j;
        DP sum;
        Vec_DP r(TWON1),x(N),y(N);

        for (i=0;i<N;i++) y[i]=0.1*(i+1);
        for (i=0;i<TWON1;i++) r[i]=1.0/(i+1.0);
        NR::toeplz(r,x,y);
        cout << "Solution vector:" << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<N;i++)
          cout << setw(7) << "x[" << i << "] = " << setw(10) << x[i] << endl;
        cout << endl << "Test of solution:" << endl;
        cout << setw(13) << "mtrx*soln" << setw(13) << "original" << endl;
        for (i=0;i<N;i++) {
          sum=0.0;
          for (j=0;j<N;j++)
            sum += (r[N-1+i-j]*x[j]);
          cout << setw(12) << sum << setw(12) << y[i] << endl;
        }
        return 0;
}

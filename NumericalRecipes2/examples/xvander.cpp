#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine vander

int main(void)
{
        const int N=5;
        const DP x_d[N]={1.0,1.5,2.0,2.5,3.0};
        const DP q_d[N]={1.0,1.5,2.0,2.5,3.0};
        int i,j;
        DP sum=0.0;
        Vec_DP w(N),term(N),x(x_d,N),q(q_d,N);

        NR::vander(x,w,q);
        cout << endl << "Solution vector:" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<N;i++)
          cout << "w[" << i << "]=" << setw(12) << w[i] << endl;
        cout << endl << "Test of solution vector:" << endl;
        cout << setw(14) << "mtrx*sol'n" << setw(12) << "original" << endl;
        for (i=0;i<N;i++) {
          term[i]=w[i];
          sum += w[i];
        }
        cout << fixed << setprecision(4);
        cout << setw(12) << sum << setw(12) << q[0] << endl;
        for (i=1;i<N;i++) {
          sum=0.0;
          for (j=0;j<N;j++) {
            term[j] *= x[j];
            sum += term[j];
          }
          cout << setw(12) << sum << setw(12) << q[i] << endl;
        }
        return 0;
}

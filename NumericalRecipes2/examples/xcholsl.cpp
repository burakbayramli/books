#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine cholsl

int main(void)
{
        const int N=3;
        const DP a_d[N*N]=
          {100.0,15.0,0.01,
          15.0,2.3,0.01,
          0.01,0.01,1.0};
        const DP b_d[N]={0.4,0.02,99.0};
        int i,j,k;
        DP sum;
        Vec_DP p(N),x(N),b(b_d,N);
        Mat_DP a(a_d,N,N), atest(N,N), chol(N,N);

        NR::choldc(a,p);
        cout << "Original matrix:" << endl;
        cout << scientific << setprecision(6);
        for (i=0;i<N;i++) {
          for (j=0;j<N;j++) {
            if (i == j) chol[i][i]=p[i];
            else chol[i][j]=(i > j ? a[i][j] : 0.0);
            cout << setw(16) << a_d[i*N+j];
          }
          cout << endl;
        }
        cout << endl << "Product of Cholesky factors:" << endl;
        for (i=0;i<N;i++) {
          for (j=0;j<N;j++) {
            for (sum=0.0,k=0;k<N;k++) sum += chol[i][k]*chol[j][k];
            atest[i][j]=sum;
            cout << setw(16) << atest[i][j];
          }
          cout << endl;
        }
        cout << endl;
        cout << "Check solution vector:" << endl;
        NR::cholsl(a,p,b,x);
        for (i=0;i<N;i++) {
          for (sum=0.0,j=0;j<N;j++) sum += a_d[i*N+j]*x[j];
            p[i]=sum;
            cout << setw(16) << p[i] << setw(16) << b[i] << endl;
        }
        return 0;
}

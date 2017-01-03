#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine banmul

int main(void)
{
        const int M1=2,M2=1,NP=7,MP=(M1+1+M2);
        int i,j,k;
        Vec_DP ax(NP),b(NP),x(NP);
        Mat_DP a(NP,MP),aa(NP,NP);

        // Lower band
        for (i=0;i<M1;i++)
          for (j=i;j<NP;j++)
            a[j][i]=10.0*(j+1)+(i+1);
        // Diagonal
        for (j=0;j<NP;j++) a[j][M1]=j+1;
        // Upper band
        for (i=0;i<M2;i++)
          for (j=0;j<NP;j++) a[j][M1+i+1]=0.1*(j+1)+(i+1);
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) {
            k=i-M1;
            if (j>=MAX(0,k) && j<=MIN(M1+M2+k,NP-1))
              aa[i][j]=a[i][j-k];
            else aa[i][j]=0.0;
          }
        }
        for (i=0;i<NP;i++) x[i]=(i+1)/10.0;
        NR::banmul(a,M1,M2,x,b);
        for (i=0;i<NP;i++) {
          for (ax[i]=0.0,j=0;j<NP;j++) ax[i] += aa[i][j]*x[j];
        }
        cout << fixed << setprecision(4) << "Reference vector";
        cout << setw(18) << "banmul vector" << endl << endl;
        for (i=0;i<NP;i++)
          cout << setw(12) << ax[i] << setw(18) << b[i] << endl;
        return 0;
}

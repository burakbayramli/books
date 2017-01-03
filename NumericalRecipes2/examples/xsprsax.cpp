#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine sprsax

int main(void)
{
        const int NP=5;
        const int NMAX=2*NP*NP+1;
        const DP a_d[NP*NP]=
          {3.0,0.0,1.0,0.0,0.0,
          0.0,4.0,0.0,0.0,0.0,
          0.0,7.0,5.0,9.0,0.0,
          0.0,0.0,0.0,0.0,2.0,
          0.0,0.0,0.0,6.0,5.0};
        const DP x_d[NP]={1.0,2.0,3.0,4.0,5.0};
        int i,j;
        Vec_INT ija(NMAX);
        Vec_DP ax(NP),sa(NMAX),x(x_d,NP),b(NP);
        Mat_DP a(a_d,NP,NP);

        NR::sprsin(a,0.5,sa,ija);
        NR::sprsax(sa,ija,x,b);
        for (i=0;i<NP;i++)
          for (ax[i]=0.0,j=0;j<NP;j++) ax[i] += a[i][j]*x[j];
        cout << setw(16) << "Reference" << setw(23) << "sprsax result" << endl;
        cout << fixed << setprecision(2);
        for (i=0;i<NP;i++)
          cout << setw(13) << ax[i] << setw(21) << b[i] << endl;
        return 0;
}

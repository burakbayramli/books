#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine sprstx

int main(void)
{
        const int NP=5,NMAX=2*NP*NP+1;
        const DP a_d[NP*NP]=
          {3.0,0.0,1.0,0.0,0.0,
          0.0,4.0,0.0,0.0,0.0,
          0.0,7.0,5.0,9.0,0.0,
          0.0,0.0,0.0,0.0,2.0,
          0.0,0.0,0.0,6.0,5.0};
        const DP x_d[NP]={1.0,2.0,3.0,4.0,5.0};
        int i,j;
        Vec_INT ija(NMAX);
        Vec_DP x(x_d,NP),ax(NP),b(NP),sa(NMAX);
        Mat_DP a(a_d,NP,NP);

        NR::sprsin(a,0.5,sa,ija);
        NR::sprstx(sa,ija,x,b);
        for (i=0;i<NP;i++)
          for (ax[i]=0.0,j=0;j<NP;j++) ax[i] += a[j][i]*x[j];
        cout << setw(13) << "Reference" << setw(20) << "sprstx result" << endl;
        cout << fixed << setprecision(2);
        for (i=0;i<NP;i++)
          cout << setw(11) << ax[i] << setw(17) << b[i] << endl;
        return 0;
}

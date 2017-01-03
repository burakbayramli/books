#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine sprstp

int main(void)
{
        const int NP=5,NMAX=2*NP*NP+1;
        int i,j;
        const DP a_d[NP*NP]=
          {3.0,0.0,1.0,0.0,0.0,
          0.0,4.0,0.0,0.0,0.0,
          0.0,7.0,5.0,9.0,0.0,
          0.0,0.0,0.0,0.0,2.0,
          0.0,0.0,0.0,6.0,5.0};
        Vec_INT ija(NMAX),ijat(NMAX);
        Vec_DP sa(NMAX),sat(NMAX);
        Mat_DP at(NP,NP),a(a_d,NP,NP);

        NR::sprsin(a,0.5,sa,ija);
        NR::sprstp(sa,ija,sat,ijat);
        for (i=0;i<NP;i++)
          for (j=0;j<NP;j++) at[i][j]=0.0;
        for (i=0;i<NP;i++) {
          at[i][i]=sat[i];
          for (j=ijat[i];j<ijat[i+1];j++) at[i][ijat[j]]=sat[j];
        }
        cout << fixed << setprecision(2);
        cout << "Original Matrix" << endl;
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(6) << a[i][j];
          cout << endl;
        }
        cout << endl << "Transpose" << endl;
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(6) << at[i][j];
          cout << endl;
        }
        return 0;
}

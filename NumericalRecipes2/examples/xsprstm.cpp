#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine sprstm

int main(void)
{
        const int NP=5,NMAX=2*NP*NP+1;
        const DP THRESH=0.99;
        const DP a_d[NP*NP]=
          {1.0,0.5,0.0,0.0,0.0,
          0.5,2.0,0.5,0.0,0.0,
          0.0,0.5,3.0,0.5,0.0,
          0.0,0.0,0.5,4.0,0.5,
          0.0,0.0,0.0,0.5,5.0};
        const DP b_d[NP*NP]=
          {1.0,1.0,0.0,0.0,0.0,
          1.0,2.0,1.0,0.0,0.0,
          0.0,1.0,3.0,1.0,0.0,
          0.0,0.0,1.0,4.0,1.0,
          0.0,0.0,0.0,1.0,5.0};
        int i,j,k,msize;
        Vec_INT ija(NMAX),ijb(NMAX),ijbt(NMAX);
        Vec_DP sa(NMAX),sb(NMAX),sbt(NMAX);
        Mat_DP c(NP,NP),ab(NP,NP);
        Mat_DP a(a_d,NP,NP),b(b_d,NP,NP);

        NR::sprsin(a,0.5,sa,ija);
        NR::sprsin(b,0.5,sb,ijb);
        NR::sprstp(sb,ijb,sbt,ijbt);
        msize=ija[ija[0]-1];
        Vec_INT ijc(msize);
        Vec_DP sc(msize);
        NR::sprstm(sa,ija,sbt,ijbt,THRESH,sc,ijc);
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) {
            ab[i][j]=0.0;
            for (k=0;k<NP;k++) {
              ab[i][j]=ab[i][j]+a[i][k]*b[k][j];
            }
          }
        }
        cout << "Reference matrix:" << endl;
        cout << fixed << setprecision(2);
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(6) << ab[i][j];
          cout << endl;
        }
        cout << fixed << setprecision(6);
        cout << endl << "sprstm matrix (off-diag. elements of mag > ): ";
        cout << setw(10) << THRESH << endl;
        for (i=0;i<NP;i++)
          for (j=0;j<NP;j++) c[i][j]=0.0;
        for (i=0;i<NP;i++) {
          c[i][i]=sc[i];
          for (j=ijc[i];j<ijc[i+1];j++) c[i][ijc[j]]=sc[j];
        }
        cout << fixed << setprecision(2);
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(6) << c[i][j];
          cout << endl;
        }
        return 0;
}

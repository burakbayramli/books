#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine sprsin

int main(void)
{
        const int NP=5,NMAX=2*NP*NP+1;
        int i,j,msize;
        Vec_INT ija(NMAX);
        Vec_DP sa(NMAX);
        DP a_d[NP*NP]=
          {3.0,0.0,1.0,0.0,0.0,
          0.0,4.0,0.0,0.0,0.0,
          0.0,7.0,5.0,9.0,0.0,
          0.0,0.0,0.0,0.0,2.0,
          0.0,0.0,0.0,6.0,5.0};
        Mat_DP aa(0.0,NP,NP), a(a_d,NP,NP);

        NR::sprsin(a,0.5,sa,ija);
        msize=ija[ija[0]-1];
        sa[NP]=0.0;
        cout << "index" << setw(8) << "ija" << setw(12) << "sa" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<msize;i++) {
           cout << setw(4) << i << setw(8) << ija[i];
           cout << setw(16) << sa[i] << endl;
        }
        for (i=0;i<NP;i++) {
          aa[i][i]=sa[i];
          for (j=ija[i];j<ija[i+1];j++) aa[i][ija[j]]=sa[j];
        }
        cout << endl << "Original Matrix" << endl;
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(12) << a[i][j];
          cout << endl;
        }
        cout << endl << "Reconstructed Matrix" << endl;
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(12) << aa[i][j];
          cout << endl;
        }
        return 0;
}

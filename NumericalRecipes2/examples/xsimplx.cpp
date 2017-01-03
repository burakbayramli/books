#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine simplx

int main(void)
{
        const int N=4,M=4,NP=N+1,MP=M+2;
        const int M1=2,M2=1,M3=1;      // M1+M2+M3 = M
        const int NM1M2=N+M1+M2;
        int i,j,icase;
        DP c_d[MP*NP]=
          {0.0,1.0,1.0,3.0,-0.5,
          740.0,-1.0,0.0,-2.0,0.0,
          0.0,0.0,-2.0,0.0,7.0,
          0.5,0.0,-1.0,1.0,-2.0,
          9.0,-1.0,-1.0,-1.0,-1.0,
          0.0,0.0,0.0,0.0,0.0};
        string txt[NM1M2]=
          {"x0","x1","x2","x3","y0","y1","y2"};
        Vec_INT izrov(N),iposv(M);
        Mat_DP a(c_d,MP,NP);

        NR::simplx(a,M1,M2,M3,icase,izrov,iposv);
        if (icase == 1)
          cout << endl << "unbounded objective function" << endl;
        else if (icase == -1)
          cout << "no solutions satisfy constraints given" << endl;
        else {
          cout << endl << setw(11) << " ";
          for (i=0;i<N;i++)
            if (izrov[i] < NM1M2) cout << setw(10) << txt[izrov[i]];
          cout << endl << endl;
          cout << fixed << setprecision(3);
          for (i=0;i<=M;i++) {
            if (i == 0 || iposv[i-1] < NM1M2) {
              if (i > 0)
                cout << txt[iposv[i-1]];
              else
                cout << "  ";
              cout << setw(10) << a[i][0];
              for (j=1;j<=N;j++)
                if (izrov[j-1] < NM1M2)
                  cout << setw(10) << a[i][j];
              cout << endl;
            }
          }
        }
        return 0;
}

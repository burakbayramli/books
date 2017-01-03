#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine poidev

int main(void)
{
        const int N=20,NPTS=10000,ISCAL=200,LLEN=50;
        int i,j,klim,idum=(-13);
        Vec_INT dist(N+1);
        DP xm,dd;

        for (;;) {
          for (j=0;j<=N;j++) dist[j]=0;
          do {
            cout << "Mean of Poisson distribution (0.0<x<" << N << ".0) ";
            cout << "- Negative to end: " << endl;
            cin >> xm;
          } while (xm > N);
          if (xm < 0.0) break;
          for (i=0;i<NPTS;i++) {
            j=int(0.5+NR::poidev(xm,idum));
            if ((j >= 0) && (j <= N)) ++dist[j];
          }
          cout << fixed << setprecision(4);
          cout << "Poisson-distributed deviate, mean " << xm;
          cout << " of " << NPTS << " points" << endl;
          cout << setw(5) << "x" << setw(9) << "p(x)";
          cout << setw(11) << "graph:" << endl;
          for (j=0;j<=N;j++) {
            dd=DP(dist[j])/NPTS;
            klim=int(ISCAL*dd);
            if (klim > LLEN) klim=LLEN;
            string txt(klim,'*');
            cout << setw(6) << j << setw(9) << dd;
            cout << "  " << txt << endl;
          }
          cout << endl;
        }
        return 0;
}

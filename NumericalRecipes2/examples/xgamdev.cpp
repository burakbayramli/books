#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine gamdev

int main(void)
{
        const int N=20,NPTS=10000,ISCAL=200,LLEN=50;
        int i,ia,j,klim,idum=(-13);
        DP dd;
        Vec_INT dist(N+1);

        for (;;) {
          for (j=0;j<=N;j++) dist[j]=0;
          do {
            cout << endl << "Select order of Gamma distribution (n=1.." << N;
            cout << "), -1 to end" << endl;
            cin >> ia;
          } while (ia > N);
          if (ia < 0) break;
          for (i=0;i<NPTS;i++) {
            j=int(NR::gamdev(ia,idum));
            if ((j >= 0) && (j <= N)) ++dist[j];
          }
          cout << "gamma-distribution deviate, order " << ia;
          cout << " of " << NPTS << " points" << endl << endl;
          cout << setw(6) << "x" << setw(8) << "p(x)";
          cout << setw(10) << "graph:" << endl << endl;
          cout << fixed << setprecision(4);
          for (j=0;j<N;j++) {
            dd=DP(dist[j])/NPTS;
            klim=int(ISCAL*dd);
            if (klim > LLEN) klim=LLEN;
            string txt(klim,'*');
            cout << setw(6) << j << setw(8) << dd << " " << txt << endl;
          }
        }
        return 0;
}

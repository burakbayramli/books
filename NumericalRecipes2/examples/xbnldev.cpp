#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bnldev

int main(void)
{
        const int N=20, NPTS=1000, ISCAL=200, NN=100, LLEN=50;
        int i,j,klim,idum=(-133);
        DP pp,xm,dd;
        Vec_INT dist(N+1);

        for (;;) {
          for (j=0;j<=N;j++) dist[j]=0;
          do {
            cout << "Mean of binomial distribution (0.0 to ";
            cout << N << ".0)" << " - Negative to end: " << endl;
            cin >> xm;
            cout << endl;
          } while (xm > N);
          if (xm < 0.0) break;
          pp=xm/NN;
          for (i=0;i<NPTS;i++) {
            j=NR::bnldev(pp,NN,idum);
            if (j >= 0 && j <= N) ++dist[j];
          }
          cout << "Binomial-distributed deviate, mean " << xm << " of ";
          cout << NPTS << " points" << endl;
          cout << setw(4) << "x" << setw(9) << "p(x)";
          cout << setw(11) << "graph:" << endl << endl;
          cout << fixed << setprecision(3);
          for (j=0;j<N;j++) {
            dd=DP(dist[j])/NPTS;
            klim=int(ISCAL*dd+1);
            if (klim > LLEN) klim=LLEN;
            string txt(klim,'*');
            cout << setw(4) << j << setw(9) << dd << " " << txt << endl;
          }
          cout << endl;
        }
        return 0;
}

#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine gasdev

int main(void)
{
        const int N=20,NOVER2=N/2,NPTS=10000,ISCAL=400,LLEN=50;
        int i,j,klim,idum=(-13);
        DP dd,x;
        Vec_INT dist(N+1);

        for (j=0;j<=N;j++) dist[j]=0;
        for (i=0;i<NPTS;i++) {
          x=0.25*N*NR::gasdev(idum);
          j=int(x > 0 ? x+0.5 : x-0.5);
          if ((j >= -NOVER2) && (j <= NOVER2)) ++dist[j+NOVER2];
        }
        cout << "Normally distributed deviate of " << NPTS;
        cout << " points" << endl << endl;
        cout << setw(5) << "x" << setw(11) << "p(x)";
        cout << setw(10) << "graph:" << endl << endl;
        cout << fixed << setprecision(4);
        for (j=0;j<=N;j++) {
          dd=DP(dist[j])/NPTS;
          klim=int(ISCAL*dd);
          if (klim > LLEN) klim=LLEN;
          string txt(klim,'*');
          cout << setw(8) << j/(0.25*N) << setw(9) << dd << "  ";
          cout << txt << endl;
        }
        return 0;
}

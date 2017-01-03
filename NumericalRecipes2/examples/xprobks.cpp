#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine probks

int main(void)
{
        const int NPTS=20,ISCAL=40;
        const DP EPS=0.1;
        int i,j,jmax;
        DP alam,aval;
        string txt;

        cout << "probability function for kolmogorov-smirnov statistic";
        cout << endl << endl;
        cout << setw(7) << "lambda" << setw(11) << "value:";
        cout << setw(14) << "graph:" << endl;
        cout << fixed << setprecision(5);
        for (i=0;i<NPTS;i++) {
          txt="       ";
          alam=(i+1)*EPS;
          aval=NR::probks(alam);
          jmax=int(0.5+(ISCAL-1)*aval);
          for (j=0;j<ISCAL;j++) {
            if (j < jmax) txt += '*';
            else txt += ' ';
          }
          cout << setw(8) << alam << setw(11) << aval;
          cout << txt << endl;
        }
        return 0;
}

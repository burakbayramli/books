#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine cosft2

int main(void)
{
        const int NP=16;
        const DP EPS=1.0e-3, WIDTH=30.0, PI=3.141592653589793238;
        int i,j,nlim;
        DP big,per,scal,small;
        Vec_DP data(NP);

        for (;;) {
          cout << endl << "Period of cosine in channels (4-";
          cout << NP << ", or 0 to end)" << endl;
          cin >> per;
          cin.get();
          if (per <= 0.0) break;
          for (i=0;i<NP;i++)
            data[i]=cos(2.0*PI*(i+0.5)/per);
          NR::cosft2(data,1);
          big = -1.0e10;
          small=1.0e10;
          for (i=0;i<NP;i++) {
            if (data[i] < small) small=data[i];
            if (data[i] > big) big=data[i];
          }
          scal=WIDTH/(big-small);
          cout << fixed << setprecision(2);
          for (i=0;i<NP;i++) {
            nlim=int(0.5+scal*(data[i]-small)+EPS);
            cout << setw(4) << i << setw(7) << data[i] << " ";
            for (j=0;j<nlim+1;j++) cout << "*";
            cout << endl;
          }
          cout << "press RETURN to continue ..." << endl;
          cin.get();
          NR::cosft2(data,-1);
          big = -1.0e10;
          small=1.0e10;
          for (i=0;i<NP;i++) {
            if (data[i] < small) small=data[i];
            if (data[i] > big) big=data[i];
          }
          scal=WIDTH/(big-small);
          for (i=0;i<NP;i++) {
            nlim=int(0.5+scal*(data[i]-small)+EPS);
            cout << setw(4) << i << " ";
            for (j=0;j<nlim+1;j++) cout << "*";
            cout << endl;
          }
        }
        return 0;
}

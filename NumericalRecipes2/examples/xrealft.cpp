#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine realft

int main(void)
{
        const int NP=32;
        const DP EPS=1.0e-3,WIDTH=50.0,PI=3.141592653589793238;
        int i,j,n=NP/2,nlim;
        DP big,per,scal,small;
        Vec_DP data(NP),size(NP/2+1);

        for (;;) {
          cout << "Period of sinusoid in channels (2-";
          cout << NP << ", or 0 to end)" << endl;
          cin >> per;
          cin.get();
          if (per <= 0.0) break;
          for (i=0;i<NP;i++) data[i]=cos(2.0*PI*i/per);
          NR::realft(data,1);
          big = -1.0e10;
          for (i=1;i<n;i++) {
            size[i]=sqrt(SQR(data[2*i])+SQR(data[2*i+1]));
            if (size[i] > big) big=size[i];
          }
          size[0]=fabs(data[0]);
          if (size[0] > big) big=size[0];
          size[n]=fabs(data[1]);
          if (size[n] > big) big=size[n];
          scal=WIDTH/big;
          for (i=0;i<n;i++) {
            nlim=int(0.5+scal*size[i]+EPS);
            cout << setw(4) << i << " ";
            for (j=0;j<nlim+1;j++) cout << "*";
            cout << endl;
          }
          cout << "press RETURN to continue ..." << endl;
          cin.get();;
          NR::realft(data,-1);
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

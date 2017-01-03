#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine rzextr

Vec_DP *x_p;   // defining declaration
Mat_DP *d_p;

int main(void)
{
        const int NV=4,IMAXX=10;
        int i,j,iest;
        DP dum,xest;
        Vec_DP dy(NV),yest(NV),yz(NV);

        x_p=new Vec_DP(IMAXX);
        d_p=new Mat_DP(IMAXX,IMAXX);
        // Feed values from a rational function
        // fn(x)=(1-x+x**3)/(x+1)**(n+1)
        cout << fixed << setprecision(6);
        for (i=0;i<IMAXX;i++) {
          iest=i;
          xest=1.0/(i+1.0);
          dum=1.0-xest+xest*xest*xest;
          for (j=0;j<NV;j++) {
            dum /= (xest+1.0);
            yest[j]=dum;
          }
          NR::rzextr(iest,xest,yest,yz,dy);
          cout << endl << "iest = " << setw(3) << i;
          cout << "    xest = " << setw(8) << xest << endl;
          cout << "Extrap. function: ";
          for (j=0;j<NV;j++) cout << setw(12) << yz[j];
          cout << endl << "Estimated error:  ";
          for (j=0;j<NV;j++) cout << setw(12) << dy[j];
          cout << endl;
        }
        cout << endl << "Actual values: " << setw(15) << 1.0;
        cout << setw(12) << 1.0 << setw(12) << 1.0 << setw(12) << 1.0 << endl;
        delete d_p;
        delete x_p;
        return 0;
}

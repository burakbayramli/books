#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine ddpoly

int main(void)
{
        const int NC=5,ND=NC,NP=20;
        const DP c_d[NC+1]={-1.0,5.0,-10.0,10.0,-5.0,1.0};
        const string a[ND+1]={"polynomial:", "first deriv:",
          "second deriv:","third deriv:","fourth deriv:","fifth deriv:"};
        int i,j,k;
        DP x,pwr;
        Vec_DP c(c_d,NC+1),pd(ND+1);
        Mat_DP d(ND+1,NP);

        for (i=0;i<NP;i++) {
          x=0.1*(i+1);
          NR::ddpoly(c,x,pd);
          for (j=0;j<ND+1;j++) d[j][i]=pd[j];
        }
        cout << fixed << setprecision(6);
        for (i=0;i<ND+1;i++) {
          cout << i << "      " << a[i] << endl;
          cout << endl << setw(12) << "x" << setw(17) << "DDPOLY";
          cout << setw(15) << "actual" << endl;
          for (j=0;j<NP;j++) {
            x=0.1*(j+1);
            pwr=1.0;
            for (k=0;k<NC-i;k++) pwr *= x-1.0;
            cout << setw(15) << x << setw(15) << d[i][j];
            cout << setw(15) << ((NR::factrl(NC)/NR::factrl(NC-i))*pwr) << endl;
          }
          cout << "press ENTER to continue..." << endl;
          cin.get();
        }
        return 0;
}

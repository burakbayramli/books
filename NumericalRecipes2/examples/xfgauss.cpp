#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine fgauss

int main(void)
{
        const int NPT=3,NLIN=2,NA=3*NLIN;
        const DP a_d[NA]={3.0,0.2,0.5,1.0,0.7,0.3};
        int i,j;
        DP e1,e2,f,x,y;
        Vec_DP a(a_d,NA),dyda(NA),df(NA);

        cout << endl << setw(6) << "x" << setw(9) << "y";
        cout << setw(9) << "dyda1" << setw(8) << "dyda2";
        cout << setw(8) << "dyda3" << setw(8) << "dyda4";
        cout << setw(8) << "dyda5" << setw(8) << "dyda6" << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<NPT;i++) {
          x=0.3*(i+1);
          NR::fgauss(x,a,y,dyda);
          e1=exp(-SQR((x-a[1])/a[2]));
          e2=exp(-SQR((x-a[4])/a[5]));
          f=a[0]*e1+a[3]*e2;
          df[0]=e1;
          df[3]=e2;
          df[1]=a[0]*e1*2.0*(x-a[1])/(a[2]*a[2]);
          df[4]=a[3]*e2*2.0*(x-a[4])/(a[5]*a[5]);
          df[2]=a[0]*e1*2.0*SQR(x-a[1])/(a[2]*a[2]*a[2]);
          df[5]=a[3]*e2*2.0*SQR(x-a[4])/(a[5]*a[5]*a[5]);
          cout << "from FGAUSS" << endl;
          cout << setw(8) << x << setw(8) << y;
          for (j=0;j<6;j++) cout << setw(8) << dyda[j];
          cout << endl << "independent calc." << endl;
          cout << setw(8) << x << setw(8) << f;
          for (j=0;j<6;j++) cout << setw(8) << df[j];
          cout << endl << endl;
        }
        return 0;
}

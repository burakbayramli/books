#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine kstwo

int main(void)
{
        const int N1=1000,N2=2000;
        const DP EPS=0.1;
        int i,j;
        int idum=(-1357);
        DP d,factr,prob,varnce;
        Vec_DP data1(N1),data2(N2);

        for (i=0;i<N1;i++) data1[i]=NR::gasdev(idum);
        cout << setw(18) << "variance ratio" << setw(16) << "k-s statistic";
        cout << setw(15) << "probability" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<11;i++) {
          varnce=1.0+i*EPS;
          factr=sqrt(varnce);
          for (j=0;j<N2;j++)
            data2[j]=factr*NR::gasdev(idum);
          NR::kstwo(data1,data2,d,prob);
          cout << setw(15) << varnce << setw(16) << d;
          cout << setw(16) << prob << endl;
        }
        return 0;
}

#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine moment

int main(void)
{
        const int NPTS=5000,NBIN=100,NPPNB=NPTS+NBIN;
        const DP PI=3.141592653589793238;
        int i=0,k,nlim;
        DP adev,ave,curt,sdev,skew,vrnce,x;
        Vec_DP temp(NPPNB);

        for (x=PI/NBIN;x<=PI;x+=PI/NBIN) {
          nlim=int(0.5+sin(x)*PI/2.0*NPTS/NBIN);
          for (k=0;k<nlim;k++) temp[i++]=x;
        }
        Vec_DP data(i);
        for (k=0;k<i;k++)
          data[k]=temp[k];
        cout << "moments of a sinusoidal distribution" << endl << endl;
        NR::moment(data,ave,adev,sdev,vrnce,skew,curt);
        cout << setw(39) << "calculated" << setw(12) << "expected";
        cout << endl << endl;
        cout << fixed << setprecision(4);
        cout << "Mean :" << setw(18) << " ";
        cout << setw(13) << ave << setw(13) << PI/2.0 << endl;
        cout << "Average Deviation :" << setw(5) << " ";
        cout << setw(13) << adev << setw(13) << ((PI/2.0)-1.0) << endl;
        cout << "Standard Deviation :" << setw(4) << " ";
        cout << setw(13) << sdev << setw(13) << 0.683667 << endl;
        cout << "Variance :" << setw(14) << " ";
        cout << setw(13) << vrnce << setw(13) << 0.467401 << endl;
        cout << "Skewness :" << setw(14) << " ";
        cout << setw(13) << skew << setw(13) << 0.0 << endl;
        cout << "Kurtosis :" << setw(14) << " ";
        cout << setw(13) << curt << setw(13) << -0.806249 << endl;
        return 0;
}

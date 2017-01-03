#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine savgol

int main(void)
{
        const int NMAX=1000,NTEST=6;
        const int mtest[NTEST]={2,2,2,2,4,4};
        const int nltest[NTEST]={2,3,4,5,4,5};
        const int nrtest[NTEST]={2,1,0,5,4,5};
        const char *ans[NTEST]={
"                      -0.086  0.343  0.486  0.343 -0.086",
"               -0.143  0.171  0.343  0.371  0.257",
"         0.086 -0.143 -0.086  0.257  0.886",
" -0.084  0.021  0.103  0.161  0.196  0.207  0.196  0.161  0.103  0.021 -0.084",
"         0.035 -0.128  0.070  0.315  0.417  0.315  0.070 -0.128  0.035",
"  0.042 -0.105 -0.023  0.140  0.280  0.333  0.280  0.140 -0.023 -0.105  0.042"};
        int i,j,m,nl,np,nr;
        DP sum;
        Vec_DP c(NMAX);

        cout << "Sample Savitzky-Golay Coefficients" << endl;
        cout << fixed << setprecision(3);
        for (i=0;i<NTEST;i++) {
          m=mtest[i];
          nl=nltest[i];
          nr=nrtest[i];
          np=nl+nr+1;
          NR::savgol(c,np,nl,nr,0,m);
          sum=0.0;
          for (j=0;j<np;j++) sum += c[j];
          cout << endl << endl << "M, nl, nr: ";
          cout << m << " " << nl << " " << nr << endl;
          for (j=nl;j<5;j++) cout << "       ";
          for (j=nl;j>=0;j--) cout << setw(7) << c[j];
          for (j=0;j<nr;j++) cout << setw(7) << c[np-1-j];
          cout << endl;
          cout << "Sum = " << setw(7) << sum << endl;
          cout << "Compare answer:" << endl << ans[i] << endl;
        }
        return 0;
}

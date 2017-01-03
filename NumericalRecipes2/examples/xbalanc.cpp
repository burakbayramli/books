#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine balanc

int main(void)
{
        const int NP=5;
        int i,j;
        Vec_DP c(NP),r(NP);
        Mat_DP a(NP,NP);

        for (i=0;i<NP;i++)
          for (j=0;j<NP;j++)
            a[i][j] = (!(i & 1) && (j & 1) ? 100.0 : 1.0);
        // Write norms
        for (i=0;i<NP;i++) {
          r[i]=c[i]=0.0;
          for (j=0;j<NP;j++) {
            r[i] += fabs(a[i][j]);
            c[i] += fabs(a[j][i]);
          }
        }
        cout << "rows:" << endl;
        cout << fixed << setprecision(2);
        for (i=0;i<NP;i++) cout << setw(12) << r[i];
        cout << endl << "columns:" << endl;
        for (i=0;i<NP;i++) cout << setw(12) << c[i];
        cout << endl << endl << "***** Balancing matrix *****" << endl;
        NR::balanc(a);
        // Write norms
        for (i=0;i<NP;i++) {
          r[i]=c[i]=0.0;
          for (j=0;j<NP;j++) {
            r[i] += fabs(a[i][j]);
            c[i] += fabs(a[j][i]);
          }
        }
        cout << "rows:" << endl;
        for (i=0;i<NP;i++) cout << setw(12) << r[i];
        cout << endl << "columns:" << endl;
        for (i=0;i<NP;i++) cout << setw(12) << c[i];
        cout << endl;
        return 0;
}

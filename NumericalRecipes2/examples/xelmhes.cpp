#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine elmhes

int main(void)
{
        const int NP=5;
        const DP a_d[NP*NP]=
          {1.0,2.0,300.0,4.0,5.0,
          2.0,3.0,400.0,5.0,6.0,
          3.0,4.0,5.0,6.0,7.0,
          4.0,5.0,600.0,7.0,8.0,
          5.0,6.0,700.0,8.0,9.0};
        int i,j;
        Mat_DP a(a_d,NP,NP);

        cout << "***** original matrix *****" << endl;
        cout << scientific << setprecision(2);
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(12) << a[i][j];
          cout << endl;
        }
        cout << "***** balance matrix *****" << endl;
        NR::balanc(a);
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(12) << a[i][j];
          cout << endl;
        }
        cout << "***** reduce to hessenberg form *****" << endl;
        NR::elmhes(a);
        for (j=0;j<NP-2;j++)
          for (i=j+2;i<NP;i++) a[i][j]=0.0;
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(12) << a[i][j];
          cout << endl;
        }
        return 0;
}

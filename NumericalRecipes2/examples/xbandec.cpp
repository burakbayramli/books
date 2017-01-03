#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bandec

int main(void)
{
        int i,j,idum=(-1);
        DP d;
        Vec_DP x(7),b(7);
        Vec_INT indx(7);
        Mat_DP a(7,4),al(7,2);

        for (i=0;i<7;i++) {
          x[i]=NR::ran1(idum);
          for (j=0;j<4;j++) a[i][j]=NR::ran1(idum);
        }
        NR::banmul(a,2,1,x,b);
        cout << fixed << setprecision(6);
        for (i=0;i<7;i++)
          cout << i << setw(13) << b[i] << setw(13) << x[i] << endl;
        cout << endl;
        NR::bandec(a,2,1,al,indx,d);
        NR::banbks(a,2,1,al,indx,b);
        for (i=0;i<7;i++)
          cout << i << setw(13) << b[i] << setw(13) << x[i] << endl;
        return 0;
}

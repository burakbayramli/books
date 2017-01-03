#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine ks2d1s

int main(void)
{
        int idum,jtrial,j,n1,ntrial;
        DP d,factor,prob,u,v;

        for (;;) {
          cout << "How many points? (0 to end)" << endl;
          cin >> n1;
          if (n1 == 0) break;
          Vec_DP x1(n1),y1(n1);
          cout << "What factor nonlinearity (0.0 to 1.0)?" << endl;
          for (;;) {
            cin >> factor;
            if (factor < 0.0) {
              cout << "factor less than 0" << endl;
              continue;
            }
            if (factor > 1.0) {
              cout << "factor greater than 1" << endl;
              continue;
            }
            break;
          }
          cout << "How many trials?" << endl;
          cin >> ntrial;
          idum = -289-ntrial-n1;
          for (jtrial=0;jtrial<ntrial;jtrial++) {
            for (j=0;j<n1;j++) {
              u=NR::ran1(idum);
              u=u*((1.0-factor)+u*factor);
              x1[j]=2.0*u-1.0;
              v=NR::ran1(idum);
              v=v*((1.0-factor)+v*factor);
              y1[j]=2.0*v-1.0;
            }
            NR::ks2d1s(x1,y1,NR::quadvl,d,prob);
            cout << fixed << setprecision(6);
            cout << "d, prob= " << setw(12) << d;
            cout << scientific << setprecision(2);
            cout << setw(12) << prob << endl;
          }
          cout << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}

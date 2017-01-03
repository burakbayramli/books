#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine ks2d2s

int main(void)
{
        int idum,jtrial,j,n1,n2,ntrial;
        DP d,prob,shrink,u,v;

        cout << fixed << setprecision(6);
        for (;;) {
          cout << endl << "Input N1 N2 (or 0 0 to end)" << endl;
          cin >> n1 >> n2;
          if (n1 <= 0 || n2 <= 0) break;
          Vec_DP x1(n1),y1(n1),x2(n2),y2(n2);
          cout << "What shrinkage? (0.0 to 1.0)" << endl;
          cin >> shrink;
          cout << "How many trials?" << endl;
          cin >> ntrial;
          idum = -287-ntrial-n1-n2;
          for (jtrial=0;jtrial<ntrial;jtrial++) {
            for (j=0;j<n1;j++) {
              u=NR::gasdev(idum);
              v=NR::gasdev(idum)*shrink;
              x1[j]=u+v;
              y1[j]=u-v;
            }
            for (j=0;j<n2;j++) {
              u=NR::gasdev(idum)*shrink;
              v=NR::gasdev(idum);
              x2[j]=u+v;
              y2[j]=u-v;
            }
            NR::ks2d2s(x1,y1,x2,y2,d,prob);
            cout << fixed << setprecision(6);
            cout << "d, prob= " << setw(13) << d;
            cout << scientific << setprecision(2);
            cout << setw(13) << prob << endl;
          }
          cout << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}

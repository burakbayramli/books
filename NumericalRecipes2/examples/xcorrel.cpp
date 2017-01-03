#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine correl

int main(void)
{
        const int N=64;
        int i,j;
        DP cmp;
        Vec_DP data1(N),data2(N),ans(N);

        for (i=0;i<N;i++) {
          if ((i > N/2-N/8-1) && (i < N/2+N/8-1))
            data1[i]=1.0;
          else
            data1[i]=0.0;
          data2[i]=data1[i];
        }
        NR::correl(data1,data2,ans);
        // Calculate directly
        cout << setw(3) << "n" << setw(15) << "CORREL";
        cout << setw(19) << "direct calc." << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<=16;i++) {
          cmp=0.0;
          for (j=0;j<N;j++)
            cmp += data1[((i+j) % N)]*data2[j];
          cout << setw(3) << i << setw(16) << ans[i];
          cout << setw(16) << cmp << endl;
        }
        return 0;
}

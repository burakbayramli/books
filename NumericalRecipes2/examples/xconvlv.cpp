#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine convlv

int main(void)
{
        const int N=16;  //data array size
        const int M=9;   //response function dim. - must be odd
        int i,isign,j;
        DP cmp;
        Vec_DP data(N),respns(N),resp(M),ans(N);

        for (i=0;i<N;i++)
          if ((i >= N/2-N/8-1) && (i <= N/2+N/8-1))
            data[i]=1.0;
          else
            data[i]=0.0;
        for (i=0;i<M;i++) {
          if ((i > 1) && (i < 6))
            respns[i]=1.0;
          else
            respns[i]=0.0;
          resp[i]=respns[i];
        }
        isign=1;
        NR::convlv(data,resp,isign,ans);
        // compare with a direct convolution
        cout << setw(3) << "i" << setw(15) << "CONVLV";
        cout << setw(14) << "Expected" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<N;i++) {
          cmp=0.0;
          for (j=1;j<=M/2;j++) {
            cmp += data[(i-j+N) % N]*respns[j];
            cmp += data[(i+j) % N]*respns[M-j];
          }
          cmp += data[i]*respns[0];
          cout << setw(3) << i << setw(16) << ans[i];
          cout << setw(13) << cmp << endl;
        }
        return 0;
}

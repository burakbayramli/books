#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine twofft

void prntft(Vec_I_DP &data)
{
        int n;

        int nn=data.size()/2;
        cout << setw(4) << "n" << setw(14) << "real(n)";
        cout << setw(14) << "imag.(n)" << setw(13) << "real(N-n)";
        cout << setw(14) << "imag.(N-n)" << endl;
        cout << fixed << setprecision(6);
        cout << "   0" << setw(15) << data[0] << setw(13) << data[1];
        cout << setw(13) << data[0] << setw(13) << data[1] << endl;
        for (n=2;n<nn+1;n+=2) {
          cout << setw(4) << (n/2) << setw(15) << data[n];
          cout << setw(13) << data[n+1] << setw(13) << data[2*nn-n];
          cout << setw(13) << data[2*nn-n+1] << endl;
        }
        cout << " press return to continue ..." << endl;
        cin.get();
        return;
}

int main(void)
{
        const int N=32, N2=N+N, PER=8;
        const DP PI=3.141592653589793238;
        int i,isign;
        Vec_DP data1(N), data2(N), fft1(N2), fft2(N2);

        for (i=0;i<N;i++) {
          data1[i]=floor(0.5+cos((i+1)*2.0*PI/PER));
          data2[i]=floor(0.5+sin((i+1)*2.0*PI/PER));
        }
        NR::twofft(data1,data2,fft1,fft2);
        cout << "Fourier transform of first function:" << endl;
        prntft(fft1);
        cout << "Fourier transform of second function:" << endl;
        prntft(fft2);
        // Invert transform
        isign = -1;
        NR::four1(fft1,isign);
        cout << "inverted transform  =  first function:" << endl;
        prntft(fft1);
        NR::four1(fft2,isign);
        cout << "inverted transform  =  second function:" << endl;
        prntft(fft2);
        return 0;
}

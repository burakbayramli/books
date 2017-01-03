#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine four1

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
        for (n=2;n<=nn;n+=2) {
          cout << setw(4) << (n/2) << setw(15) << data[n];
          cout << setw(13) << data[n+1] << setw(13) << data[2*nn-n];
          cout << setw(13) << data[2*nn-n+1] << endl;
        }
        cout << "press return to continue ..." << endl;
        cin.get();
        return;
}

int main(void)
{
        const int NN=32, NN2=NN+NN;
        int i,isign;
        Vec_DP data(NN2),dcmp(NN2);

        cout << "h(t)=real-valued even-function" << endl;
        cout << "h(n)=h(N-n) and real?" << endl;
        for (i=0;i<NN2-1;i+=2) {
          data[i]=1.0/(SQR(DP(i-NN)/NN)+1.0);
          data[i+1]=0.0;
        }
        isign=1;
        NR::four1(data,isign);
        prntft(data);

        cout << "h(t)=imaginary-valued even-function" << endl;
        cout << "h(n)=h(N-n) and imaginary?" << endl;
        for (i=0;i<NN2-1;i+=2) {
          data[i+1]=1.0/(SQR(DP(i-NN)/NN)+1.0);
          data[i]=0.0;
        }
        isign=1;
        NR::four1(data,isign);
        prntft(data);

        cout << "h(t)=real-valued odd-function" << endl;
        cout << "h(n) = -h(N-n) and imaginary?" << endl;
        for (i=0;i<NN2-1;i+=2) {
          data[i]=(DP(i-NN)/NN)/(SQR(DP(i-NN)/NN)+1.0);
          data[i+1]=0.0;
        }
        data[0]=0.0;
        isign=1;
        NR::four1(data,isign);
        prntft(data);

        cout << "h(t)=imaginary-valued odd-function" << endl;
        cout << "h(n) = -h(N-n) and real?" << endl;
        for (i=0;i<NN2-1;i+=2) {
          data[i+1]=(DP(i-NN)/NN)/(SQR(DP(i-NN)/NN)+1.0);
          data[i]=0.0;
        }
        data[1]=0.0;
        isign=1;
        NR::four1(data,isign);
        prntft(data);
        // transform, inverse-transform test

        for (i=0;i<NN2-1;i+=2) {
          data[i]=1.0/(SQR(0.5*(i-NN)/NN)+1.0);
          dcmp[i]=data[i];
          data[i+1]=(0.25*(i-NN)/NN)*exp(-SQR(0.5*(i-NN)/NN));
          dcmp[i+1]=data[i+1];
        }

        isign=1;
        NR::four1(data,isign);
        isign = -1;
        NR::four1(data,isign);

        cout << setw(25) << "original data:";
        cout << setw(37) << "double fourier transform:" << endl;
        cout << endl << setw(4) << "k" << setw(16) << "real h(k)";
        cout << setw(13) << "imag h(k)" << setw(13) << "real h(k)";
        cout << setw(13) << "imag h(k)" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NN-1;i+=2) {
          cout << setw(4) << (i+2)/2 << setw(15) << dcmp[i];
          cout << setw(13) << dcmp[i+1] << setw(13) << data[i]/NN;
          cout << setw(13) << data[i+1]/NN << endl;
        }
        return 0;
}

#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine fourn

int main(void)
{
        const int NDIM=3,NDAT2=1024;
        int i,isign,j,k,l,ndum=2,idum=(-23);
        Vec_INT nn(NDIM);
        Vec_DP data1(NDAT2),data2(NDAT2);

        for (i=0;i<NDIM;i++) nn[i]=(ndum <<= 1);
        for (i=0;i<nn[2];i++)
          for (j=0;j<nn[1];j++)
            for (k=0;k<nn[0];k++) {
              l=k+j*nn[0]+i*nn[1]*nn[0];
              l=(l<<1);
              // real part of component
              data2[l]=data1[l]=2*NR::ran1(idum)-1;
              // imaginary part of component
              l++;
              data2[l]=data1[l]=2*NR::ran1(idum)-1;
            }
        isign=1;
        NR::fourn(data2,nn,isign);
        // here would be any processing to be done in Fourier space
        isign = -1;
        NR::fourn(data2,nn,isign);
        cout << "Double 3-dimensional transform" << endl << endl;
        cout << setw(22) << "Double transf." << setw(25) << "Original data";
        cout << setw(21) << "Ratio" << endl;
        cout << setw(10) << "real" << setw(14) << "imag.";
        cout << setw(12) << "real" << setw(14) << "imag.";
        cout << setw(12) << "real" << setw(14) << "imag." << endl << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<4;i++) {
          j=2*i+1;
          k=2*j+1;
          l=k+j*nn[0]+i*nn[1]*nn[0];
          l=(l<<1);
          cout << setw(12) << data2[l] << setw(13) << data2[l+1];
          cout << setw(12) << data1[l] << setw(13) << data1[l+1];
          cout << setw(14) << data2[l]/data1[l];
          cout << setw(13) << data2[l+1]/data1[l+1] << endl;
        }
        cout << endl << "The product of transform lengths is: ";
        cout << nn[0]*nn[1]*nn[2] << endl;
        return 0;
}

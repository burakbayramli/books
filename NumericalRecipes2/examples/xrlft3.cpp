#include <string>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

// Driver for routine rlft3

int compare(const string &str, Mat3D_I_DP &arr1, Mat3D_I_DP &arr2, const DP eps)
{
        const int IPRNT=20;
        int err=0,i1,i2,i3;
        DP a1,a2;

        int len1=arr1.dim1();
        int len2=arr1.dim2();
        int len3=arr1.dim3();
        cout << str << endl;
        cout << fixed << setprecision(6);
        for (i1=0;i1<len1;i1++)
          for (i2=0;i2<len2;i2++)
            for (i3=0;i3<len3;i3++) {
              a1=arr1[i1][i2][i3];
              a2=arr2[i1][i2][i3];
              if ((a2 == 0.0 && fabs(a1-a2) > eps)
                || (fabs((a1-a2)/a2) > eps)) {
                if (++err <= IPRNT)
                  cout << setw(3) << i1 << setw(3) << i2;
                cout << setw(3) << i3;
                cout << setw(13) << a1 << setw(13) << a2 << endl;
              }
            }
        return err;
}

int main(void)
{
        const int NX=32,NY=8,NZ=16;
        const DP EPS=1.e4*numeric_limits<DP>::epsilon();
        int err,i,j,k,nn1=NX,nn2=NY,nn3=NZ,idum=(-3);
        DP fnorm;
        Mat_DP speq1(nn1,nn2<<1);
        Mat3D_DP data1(nn1,nn2,nn3),data2(nn1,nn2,nn3);

        fnorm=DP(nn1)*DP(nn2)*DP(nn3)/2.0;
        for (i=0;i<nn1;i++)
          for (j=0;j<nn2;j++)
            for (k=0;k<nn3;k++)
              data2[i][j][k]=fnorm*(data1[i][j][k]=2*NR::ran1(idum)-1);
        NR::rlft3(data1,speq1,1);
        // here would be any processing in Fourier space
        NR::rlft3(data1,speq1,-1);
        err=compare("data",data1,data2,EPS);
        cout << scientific << setprecision(2);
        if (err != 0) {
          cout << "Comparison error at tolerance " << setw(10) << EPS << endl;
          cout << "Total number of errors is " << err << endl;
        } else {
          cout << "Data compares OK to tolerance " << setw(10) << EPS << endl;
        }
        return (err > 0 ? 1 : 0);
}

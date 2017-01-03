#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <strstream>
#include "nr.h"
using namespace std;

// Driver for routine cntab2

int main(void)
{
        const int NI=9,NMON=12;
        int i,j;
        string txt,txt2,fate[NI],mon;
        DP h,hx,hxgy,hy,hygx,uxgy,uxy,uygx;
        Mat_INT nmbr(NI,NMON);
        ifstream fp("table1.dat");

        if (fp.fail())
          NR::nrerror("Data file table1.dat not found");
        getline(fp,txt);
        getline(fp,txt2);
        getline(fp,txt);
        mon=txt.substr(16);
        getline(fp,txt);
        for (i=0;i<NI;i++) {
          getline(fp,txt);
          fate[i]=txt.substr(0,16);
          txt=txt.substr(16);
          istrstream sp(txt.c_str());
          for (j=0;j<12;j++) sp >> nmbr[i][j];
        }
        fp.close();
        cout << txt2 << endl << endl << setw(17) << " " << mon;
        cout << endl << endl;
        for (i=0;i<NI;i++) {
          cout << setw(15) << left << fate[i] << right;
          for (j=0;j<12;j++) cout << setw(5) << nmbr[i][j];
          cout << endl;
        }
        NR::cntab2(nmbr,h,hx,hy,hygx,hxgy,uygx,uxgy,uxy);
        cout << fixed << setprecision(4);
        cout << endl << "       entropy of table           ";
        cout << setw(10) << h << endl;
        cout << "       entropy of x-distribution  ";
        cout << setw(10) << hx << endl;
        cout << "       entropy of y-distribution  ";
        cout << setw(10) << hy << endl;
        cout << "       entropy of y given x       ";
        cout << setw(10) << hygx << endl;
        cout << "       entropy of x given y       ";
        cout << setw(10) << hxgy << endl;
        cout << "       dependency of y on x       ";
        cout << setw(10) << uygx << endl;
        cout << "       dependency of x on y       ";
        cout << setw(10) << uxgy << endl;
        cout << "       symmetrical dependency     ";
        cout << setw(10) << uxy << endl;
        return 0;
}

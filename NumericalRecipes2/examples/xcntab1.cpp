#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <strstream>
#include "nr.h"
using namespace std;

// Driver for routine cntab1

int main(void)
{
        const int NDAT=9,NMON=12;
        int i,j;
        DP ccc,chisq,cramrv,df,prob;
        string txt,txt2,fate[NDAT],mon;
        Mat_INT nmbr(NDAT,NMON);
        ifstream fp("table1.dat");

        if (fp.fail())
          NR::nrerror("Data file table1.dat not found");
        getline(fp,txt);
        getline(fp,txt2);
        getline(fp,txt);
        mon=txt.substr(16);
        getline(fp,txt);
        for (i=0;i<NDAT;i++) {
          getline(fp,txt);
          fate[i]=txt.substr(0,16);
          txt=txt.substr(16);
          istrstream sp(txt.c_str());
          for (j=0;j<12;j++) sp >> nmbr[i][j];
        }
        fp.close();
        cout << txt2 << endl;
        cout << endl << setw(17) << " " << mon;
        cout << endl << endl;
        for (i=0;i<NDAT;i++) {
          cout << setw(15) << left << fate[i] << right;
          for (j=0;j<12;j++) cout << setw(5) << nmbr[i][j];
          cout << endl;
        }
        NR::cntab1(nmbr,chisq,df,prob,cramrv,ccc);
        cout << fixed << setprecision(4);
        cout << endl << setw(20) << "chi-squared       ";
        cout << setw(20) << chisq << endl;
        cout << setw(20) << "degrees of freedom";
        cout << setw(20) << df << endl;
        cout << setw(20) << "probability       ";
        cout << setw(20) << prob << endl;
        cout << setw(20) << "cramer-v          ";
        cout << setw(20) << cramrv << endl;
        cout << setw(20) << "contingency coeff.";
        cout << setw(20) << ccc << endl;
        return 0;
}

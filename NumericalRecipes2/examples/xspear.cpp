#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine spear

int main(void)
{
        const int NDAT=20,NMON=12;
        int i,j;
        char temp[17];
        string txt,txt2,city[NDAT],mon[NMON];
        DP d,probd,probrs,rs,zd;
        Vec_DP data1(NDAT),data2(NDAT);
        Mat_DP rays(NDAT,NMON);
        ifstream fp("table2.dat");

        if (fp.fail())
          NR::nrerror("Data file table2.dat not found");
        getline(fp,txt);
        getline(fp,txt2);
        fp >> txt;
        for (i=0;i<NMON;i++) fp >> mon[i];
        getline(fp,txt);
        getline(fp,txt);
        for (i=0;i<NDAT;i++) {
          fp.get(temp,sizeof(temp));
          city[i]=temp;
          for (j=0;j<NMON;j++) fp >> rays[i][j];
          getline(fp,txt);
        }
        fp.close();
        cout << endl << setw(16) << " ";
        for (i=0;i<12;i++) cout << setw(5) << mon[i];
        cout << endl << endl;
        for (i=0;i<NDAT;i++) {
          cout << city[i];
          for (j=0;j<12;j++)
            cout << setw(5) << int(0.5+rays[i][j]);
          cout << endl;
        }
        cout << "press return to continue ..." << endl;
        cin.get();
        // Check temperature correlations between different months
        cout << endl << "Are sunny summer places also sunny winter places?";
        cout << endl << "Check correlation of sampled U.S. solar radiation";
        cout << endl << "(july with other months)" << endl << endl;
        cout << "month" << setw(10) << "d" << setw(15) << "st. dev.";
        cout << setw(12) << "probd" << setw(16) << "spearman-r";
        cout << setw(11) << "probrs" << endl << endl;
        for (i=0;i<NDAT;i++) data1[i]=rays[i][0];
        for (j=0;j<12;j++) {
          for (i=0;i<NDAT;i++) data2[i]=rays[i][j];
          NR::spear(data1,data2,d,zd,probd,rs,probrs);
          cout << fixed << setprecision(2);
          cout << setw(4) << mon[j] << setw(13) << d;
          cout << fixed << setprecision(6);
          cout << setw(13) << zd << setw(13) << probd << setw(14) << rs;
          cout << setw(13) << probrs << endl;
        }
        return 0;
}

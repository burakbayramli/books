#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine crank

int main(void)
{
        const int NDAT=20,NMON=12;
        string txt,txt2,city[NDAT],mon[NMON];
        char temp[17];
        int i,j;
        Vec_DP data(NDAT),order(NDAT),s(NMON);
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
          fp.get(temp,17);
          city[i]=temp;
          for (j=0;j<NMON;j++) fp >> rays[i][j];
          getline(fp,txt);
        }
        fp.close();
        cout << txt2 << endl << endl << setw(16) << " ";
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
        // Replace solar flux in each column by rank order
        for (j=0;j<12;j++) {
          for (i=0;i<NDAT;i++) {
            data[i]=rays[i][j];
            order[i]=i;
          }
          NR::sort2(data,order);
          NR::crank(data,s[j]);
          for (i=0;i<NDAT;i++)
            rays[int(0.5+order[i])][j]=data[i];
        }
        cout << txt2 << endl << endl << setw(16) << " ";
        for (i=0;i<12;i++) cout << setw(5) << mon[i];
        cout << endl << endl;
        for (i=0;i<NDAT;i++) {
          cout << city[i];
          for (j=0;j<12;j++) cout << setw(5) << int(0.5+rays[i][j]);
          cout << endl;
        }
        return 0;
}

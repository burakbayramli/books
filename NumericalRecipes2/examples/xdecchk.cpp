#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine decchk

int main(void)
{
        int j,k,l,n,nbad=0,ntot=0;
        bool iok,jok;
        char ch,chh;
        string lin;

        // test all jump transpositions of the form 86jlk41
        for (j=48;j<=57;j++) {
          for (k=48;k<=57;k++) {
            for (l=48;l<=57;l++) {
              lin="86xxx41";
              if (j != k) {
                ntot++;
                lin[2]=char(j);
                lin[3]=char(l);
                lin[4]=char(k);
                iok=NR::decchk(lin,ch);
                lin += ch;
                iok=NR::decchk(lin,chh);
                lin[2]=char(k);
                lin[4]=char(j);
                jok=NR::decchk(lin,chh);
                if (!iok || jok) nbad++;
              }
            }
          }
        }
        cout << "Total tries:" << setw(15) << " " << setw(3) << ntot << endl;
        cout << "Bad tries:" << setw(17) << " " << setw(3) << nbad << endl;
        cout << "Fraction good:" << setw(12) << " ";
        cout << fixed << setprecision(2);
        cout << setw(4) << DP(ntot-nbad)/ntot << endl;
        for (;;) {
          cout << "enter string terminated by <CR> (or just <CR> to end):";
          cout << endl;
          getline(cin,lin);
          n=lin.length();
          if (n == 0) break;
          iok=NR::decchk(lin,ch);
          lin += '-';
          lin += ch;
          jok=NR::decchk(lin,chh);
          cout << lin << " checks as " << (jok ? 'T' : 'F') << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}

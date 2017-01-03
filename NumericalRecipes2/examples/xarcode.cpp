#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routines arcmak and arcode

int main(void)
{
        const unsigned long MC=512,NWK=20;
        unsigned long nch,nrad,lcode,i,j,k;
        unsigned long lc,n,tmp,zero=0;
        string mess,ness,code;
        arithcode acode(NWK,NWK,MC+2);
        Vec_ULNG nfreq(256);
        ifstream fp("text.dat");

        if (fp.fail())
          NR::nrerror("Input file text.dat not found.");
        for (i=0;i<256;i++)
          nfreq[i]=0;
        while (!fp.eof()) {
          getline(fp,mess);
          for (k=0;k<mess.length();k++)
            if (mess[k] >= 32) nfreq[mess[k]-32]++;
        }
        fp.close();
        nch=96;
        nrad=256;
        // here is the initialization that constructs the code
        NR::arcmak(nfreq,nch,nrad,acode);
        // now ready to prompt for lines to encode
        for (;;) {
          cout << endl << "Enter a line:" << endl;
          getline(cin,mess);
          n=mess.length();
          if (n == 0) break;
          // shift from 256 character alphabet to 96 printing characters
          for (j=0;j<n;j++) mess[j] -= char(32);
          // message initialization
          lc=0;
          code.erase(); // characters are appended to what is in code
          NR::arcode(zero,code,lc,0,acode);
          // here we arithmetically encode mess(0:n-1)
          for (j=0;j<n;j++) {
            tmp=mess[j];
            NR::arcode(tmp,code,lc,1,acode);
          }
          // message termination
          NR::arcode(nch,code,lc,1,acode);
          cout << "Length of line input, coded= " << n << "  ";
          cout << lc << endl;
          // here we decode the message to get the original back
          lc=0;
          ness.erase();
          NR::arcode(zero,code,lc,0,acode);
          lcode=n+10;  // 10 characters more than should be needed
          for (j=0;j<lcode;j++) {
            NR::arcode(i,code,lc,-1,acode);
            if (i == nch) break;
            else ness += (unsigned char) (i+32);
          }
          if (j >= lcode) NR::nrerror("Arith. coding: Never get here");
          cout << "Decoded output:" << endl << ness << endl;
          if (j != n) cout << "Error! n decoded != n input." << endl;
        }
        return 0;
}

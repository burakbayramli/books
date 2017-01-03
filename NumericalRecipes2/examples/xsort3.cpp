#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine sort3

int main(void)
{
        const int NLEN=64;
        int i;
        string txt;
        string amsg,bmsg,cmsg;
        Vec_DP a(NLEN),b(NLEN),c(NLEN);
        ifstream fp("tarray.dat");

        amsg = "I'd rather have a bottle in front of me ";
        amsg += "than a frontal lobotomy.";
        cout << endl << "original message:" << endl << amsg << endl;
        // read array of random numbers
        if (fp.fail())
          NR::nrerror("Data file tarray.dat not found");
        getline(fp,txt);
        for (i=0;i<NLEN;i++) fp >> a[i];
        // create array b and array c
        for (i=0;i<NLEN;i++) {
          b[i]=i;
          c[i]=NLEN-1-i;
        }
        // sort array a while mixing b and c
        NR::sort3(a,b,c);
        // scramble message according to array b
        bmsg=amsg;       // another array of the same length
        for (i=0;i<NLEN;i++) bmsg[i]=amsg[int(b[i])];
        cout << endl << "scrambled message:" << endl << bmsg << endl;
        // unscramble according to array c
        cmsg=amsg;       // another array of the same length
        for (i=0;i<NLEN;i++) cmsg[int(c[i])]=bmsg[i];
        cout << endl << "mirrored message:" << endl << cmsg << endl;
        return 0;
}

#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine eclazz

bool equiv(const int i, const int j)
{
        return (i % 4) == (j % 4);
}

int main(void)
{
        const int N=15;
        int i,j,k,lclas=0,nclass;
        Vec_BOOL nflag(N);
        Vec_INT nf(N),nsav(N);

        NR::eclazz(nf,equiv);
        for (i=0;i<N;i++) nflag[i]=true;
        cout << endl << "Numbers from 0-" << (N-1) << " divided according to";
        cout << endl << "their value modulo 4:" << endl;
        for (i=0;i<N;i++) {
          nclass=nf[i];
          if (nflag[nclass]) {
            nflag[nclass]=false;
            lclas++;
            k=0;
            for (j=i;j<N;j++)
              if (nf[j] == nf[i]) nsav[k++]=j;
            cout << "Class " << setw(2) << lclas << ":      ";
            for (j=0;j<k;j++) cout << setw(3) << nsav[j];
            cout << endl;
          }
        }
        return 0;
}

#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine eclass

int main(void)
{
        const int M=11,N=15;
        const int lista_d[M]={0,4,4,1,5,1,6,10,2,3,11};
        const int listb_d[M]={4,8,12,5,9,13,2,6,14,7,3};
        int i,j,k,lclas=0,nclass;
        Vec_BOOL nflag(N);
        Vec_INT nf(N),nsav(N);
        Vec_INT lista(lista_d,M),listb(listb_d,M);

        NR::eclass(nf,lista,listb);
        for (i=0;i<N;i++) nflag[i]=true;
        cout << endl << "Numbers from 0-" << N-1 << " divided according to";
        cout << endl << "their value modulo 4:" << endl << endl;
        for (i=0;i<N;i++) {
          nclass=nf[i]-1;
          if (nflag[nclass]) {
            nflag[nclass]=false;
            k=0;
            for (j=i;j<N;j++)
              if (nf[j] == nf[i]) nsav[k++]=j;
            cout << "Class " << setw(2) << lclas << ":      ";
            for (j=0;j<k;j++) cout << setw(3) << nsav[j];
            cout << endl;
            lclas++;
          }
        }
        return 0;
}

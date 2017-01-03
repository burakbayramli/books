#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
#include "print_array.h"
using namespace std;

// Driver for routine rank

int main(void)
{
        const int NP=100;
        string txt;
        int i,j,k,l;
        Vec_INT indx(NP),irank(NP);
        Vec_DP b(10),a(NP);
        ifstream fp("tarray.dat");

        if (fp.fail())
          NR::nrerror("Data file tarray.dat not found");
        getline(fp,txt);
        for (i=0;i<NP;i++) fp >> a[i];
        NR::indexx(a,indx);
        NR::rank(indx,irank);
        cout << "original array is:" << endl;
        cout << fixed << setprecision(2);
        print_array(a,10,7);
        cout << "table of ranks is:" << endl;
        print_array(irank,10,7);
        cout << "press return to continue..." << endl;
        cin.get();
        cout << "array sorted according to rank table:" << endl;
        for (i=0;i<10;i++) {
          for (j=0;j<10;j++) {
            k=10*i+j;
            for (l=0;l<NP;l++)
              if (irank[l] == k) b[j]=a[l];
          }
          print_array(b,10,7);
        }
        return 0;
}

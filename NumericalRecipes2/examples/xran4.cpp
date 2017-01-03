#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine ran4

int main(void)
{
        const char *ansvax[4]={"0.275898","0.208204","0.034307","0.838676"};
        const char *ansiee[4]={"0.219120","0.849246","0.375290","0.457334"};
        int i;
        int idum[4]={-1,99,-99,99};
        Vec_DP random(4);

        cout << fixed << setprecision(6);
        for (i=0;i<4;i++) random[i]=NR::ran4(idum[i]);
        cout << endl << "ran4 gets values: ";
        for (i=0;i<4;i++) cout << setw(15) << random[i];
        cout << endl << "    IEEE answers: ";
        for (i=0;i<4;i++) cout << setw(15) << ansiee[i];
        cout << endl << "     VAX answers: ";
        for (i=0;i<4;i++) cout << setw(15) << ansvax[i];
        cout << endl;
        return 0;
}

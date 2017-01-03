#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine psdes

int main(void)
{
        const char *ans[4]={
          "0x604d1dce 0x509c0c23","0xd97f8571 0xa66cb41a",
          "0x7822309d 0x64300984","0xd7f376f0 0x59ba89eb"};
        unsigned long lword[4]={1,1,99,99};
        unsigned long irword[4]={1,99,1,99};
        int i;

        cout << hex;
        for (i=0;i<4;i++) {
          NR::psdes(lword[i],irword[i]);
          cout << "PSDES now calculates:       0x";
          cout << lword[i] << " 0x" << irword[i] << endl;
          cout << "Known correct answers are:  " << ans[i];
          cout << endl << endl;
        }
        return 0;
}

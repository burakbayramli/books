#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine df1dim

int ncom;   // defining declarations
DP (*nrfunc)(Vec_I_DP &);
Vec_DP *pcom_p,*xicom_p;
void (*nrdfun)(Vec_I_DP &, Vec_O_DP &);

void dfunc(Vec_I_DP &x, Vec_O_DP &df)
{
        int i;

        for (i=0;i<3;i++)
          df[i]=(x[i]-1.0)*(x[i]-1.0);
}

int main(void)
{
        const int NDIM=3;

        ncom=NDIM;
        pcom_p=new Vec_DP(ncom);
        xicom_p=new Vec_DP(ncom);
        Vec_DP &pcom=*pcom_p, &xicom=*xicom_p;
        nrdfun=dfunc;
        cout << endl << "Enter vector direction along which to" << endl;
        cout << "plot the function. Minimum is in the" << endl;
        cout << "direction 1.0 1.0 1.0 - enter x y z:" << endl << endl;
        pcom[0]=pcom[1]=pcom[2]=0.0;
        cin >> xicom[0] >> xicom[1] >> xicom[2];
        NR::scrsho(NR::df1dim);
        delete xicom_p;
        delete pcom_p;
        return 0;
}

#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine f1dim

int ncom;   // defining declarations
DP (*nrfunc)(Vec_I_DP &);
Vec_DP *pcom_p,*xicom_p;

DP func(Vec_I_DP &x)
{
        int i;
        DP f=0.0;

        for (i=0;i<3;i++)
          f += (x[i]-1.0)*(x[i]-1.0);
        return f;
}

int main(void)
{
        const int NDIM=3;

        ncom=NDIM;
        pcom_p=new Vec_DP(ncom);
        xicom_p=new Vec_DP(ncom);
        Vec_DP &pcom=*pcom_p,&xicom=*xicom_p;
        nrfunc=func;
        pcom[0]=pcom[1]=pcom[2]=0.0;
        cout << endl << "Enter vector direction along which to" << endl;
        cout << "plot the function. Minimum is in the" << endl;
        cout << "direction 1.0 1.0 1.0 - enter x y z:" << endl;
        cin >> xicom[0] >> xicom[1] >> xicom[2];
        NR::scrsho(NR::f1dim);
        delete xicom_p;
        delete pcom_p;
        return 0;
}

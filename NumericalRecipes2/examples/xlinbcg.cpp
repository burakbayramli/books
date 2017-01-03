#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine linbcg

Vec_INT *ija_p;
Vec_DP *sa_p;

int main(void)
{
        const int NP=20;
        const int ITOL=1,ITMAX=75;
        const DP TOL=1.0e-9;
        int i,ii,iter;
        DP err;
        Vec_DP b(NP),bcmp(NP),x(NP);
        const int NSIZE=59;
        const int ija_d[NSIZE]={
          21,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,
          58,59,1,0,2,1,3,2,4,3,5,4,6,5,7,6,8,7,9,8,10,9,11,10,12,11,
          13,12,14,13,15,14,16,15,17,16,18,17,19,18};
        const DP sa_d[NSIZE]={
          3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,
          3.0,3.0,3.0,3.0,0.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,
          2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,
          -2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0,2.0,-2.0};

        ija_p=new Vec_INT(ija_d,NSIZE);
        sa_p=new Vec_DP(sa_d,NSIZE);
        for (i=0;i<NP;i++) {
          x[i]=0.0;
          b[i]=1.0;
        }
        b[0]=3.0;
        b[NP-1] = -1.0;
        NR::linbcg(b,x,ITOL,TOL,ITMAX,iter,err);
        cout << scientific << setprecision(6);
        cout << "Estimated error: " << setw(16) << err << endl;
        cout << "Iterations needed: " << setw(6) << iter;
        cout << endl << "Solution vector:" << endl;
        for (ii=0;ii<NP/5;ii++) {
          for (i=5*ii;i<5*(ii+1);i++) cout << setw(15) << x[i];
          cout << endl;
        }
        for (i=0;i<(NP % 5);i++)
          cout << setw(12) << x[5*(NP/5)+i];
        cout << endl;
        NR::sprsax(*sa_p,*ija_p,x,bcmp);
        cout << endl << "press RETURN to continue..." << endl;
        cin.get();
        cout << "test of solution vector:" << endl;
        cout << setw(9) << "a*x" << setw(13) << "b" << endl;
        for (i=0;i<NP;i++)
          cout << setw(14) << bcmp[i] << setw(15) << b[i] << endl;
        delete sa_p;
        delete ija_p;
        return 0;
}

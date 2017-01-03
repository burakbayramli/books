#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine amebsa

int idum=(-64);

DP tfunk(Vec_I_DP &p)
{
        const int N=4;
        const DP RAD=0.3,AUG=2.0;
        const DP wid_d[N]={1.0,3.0,10.0,30.0};
        int j;
        DP q,r,sumd=0.0,sumr=0.0;
        Vec_DP wid(wid_d,N);

        for (j=0;j<N;j++) {
          q=p[j]*wid[j];
          r=DP(q >= 0 ? int(q+0.5) : -int(0.5-q));
          sumr += q*q;
          sumd += (q-r)*(q-r);
        }
        return 1+sumr*(1+(sumd > RAD*RAD ? AUG : AUG*sumd/(RAD*RAD)));
}

int main(void)
{
        const int NP=4, MP=5;
        const DP FTOL=1.0e-6;
        const DP xoff[NP]={10.0,10.0,10.0,10.0};
        int i,iiter,iter,j,jiter,nit;
        DP temptr,yb,ybb;
        Vec_DP x(NP),y(MP),pb(NP);
        Mat_DP p(MP,NP);

        for (i=0;i<MP;i++)
          for (j=0;j<NP;j++) p[i][j]=0.0;
        cout << fixed << setprecision(6);
        for (;;) {
          for (j=1;j<MP;j++) p[j][j-1]=1.0;
          for (i=0;i<MP;i++) {
            for (j=0;j<NP;j++) x[j]=(p[i][j] += xoff[j]);
            y[i]=tfunk(x);
          }
          yb=1.0e30;
          cout << "Input t, iiter (t=0 to end):" << endl;
          cin >> temptr >> iiter;
          if (temptr <= 0.0) break;
          ybb=1.0e30;
          nit=0;
          for (jiter=0;jiter<100;jiter++) {
            iter=iiter;
            temptr *= 0.8;
            NR::amebsa(p,y,pb,yb,FTOL,tfunk,iter,temptr);
            nit += iiter-iter;
            if (yb < ybb) {
              ybb=yb;
              cout << setw(6) << nit << setw(14) << temptr;
              for (j=0;j<NP;j++) cout << setw(10) << pb[j];
              cout << setw(15) << yb << endl;
            }
            if (iter > 0) break;
          }
          cout << endl << "Vertices of final 3-D simplex and";
          cout << " function values at the vertices:" << endl;
          cout << setw(3) << "i" << setw(10) << "w[i]" << setw(12) << "x[i]";
          cout << setw(12) << "y[i]" << setw(12) << "z[i]";
          cout << setw(14) << "function" << endl << endl;
          for (i=0;i<MP;i++) {
            cout << setw(3) << i;
            for (j=0;j<NP;j++) cout << setw(12) << p[i][j];
            cout << setw(15) << y[i] << endl;
          }
          cout << setw(3) << 99;
          for (j=0;j<NP;j++) cout << setw(12) << pb[j];
          cout << setw(15) << yb << endl << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}

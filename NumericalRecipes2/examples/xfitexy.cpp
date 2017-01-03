#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine fitexy

int main(void)
{
        const int NPT=30;
        int j,idum=(-1);
        DP a,b,chi2,q,sa,sb,siga,sigb;
        Vec_DP x(NPT),y(NPT),dx(NPT),dy(NPT),dz(NPT);

        for (j=0;j<NPT;j++) {
          dx[j]=0.1+NR::ran1(idum);
          dy[j]=0.1+NR::ran1(idum);
          dz[j]=0.0;
          x[j]=10.0+10.0*NR::gasdev(idum);
          y[j]=2.0*x[j]-5.0+dy[j]*NR::gasdev(idum);
          x[j] += dx[j]*NR::gasdev(idum);
        }
        cout << "Values of a,b,siga,sigb,chi2,q:" << endl;
        cout << "Fit with x and y errors gives:" << endl;
        NR::fitexy(x,y,dx,dy,a,b,siga,sigb,chi2,q);
        cout << fixed << setprecision(6);
        cout << setw(11) << a << setw(12) << b << setw(12) << siga;
        cout << setw(12) << sigb << setw(12) << chi2 << setw(12) << q;
        cout << endl << endl;
        cout << "Setting x errors to zero gives:" << endl;
        NR::fitexy(x,y,dz,dy,a,b,siga,sigb,chi2,q);
        cout << setw(11) << a << setw(12) << b << setw(12) << siga;
        cout << setw(12) << sigb << setw(12) << chi2 << setw(12) << q;
        cout << endl << endl;
        cout << "...to be compared with fit result:" << endl;
        NR::fit(x,y,dy,true,a,b,siga,sigb,chi2,q);
        cout << setw(11) << a << setw(12) << b << setw(12) << siga;
        cout << setw(12) << sigb << setw(12) << chi2 << setw(12) << q;
        cout << endl << endl;
        cout << "Setting y errors to zero gives:" << endl;
        NR::fitexy(x,y,dx,dz,a,b,siga,sigb,chi2,q);
        cout << setw(11) << a << setw(12) << b << setw(12) << siga;
        cout << setw(12) << sigb << setw(12) << chi2 << setw(12) << q;
        cout << endl << endl;
        cout << "...to be compared with fit result:" << endl;
        NR::fit(y,x,dx,true,a,b,siga,sigb,chi2,q);
        sa=sqrt(siga*siga+SQR(sigb*(a/b)))/b;
        sb=sigb/(b*b);
        cout << setw(11) << -a/b << setw(12) << 1.0/b << setw(12) << sa;
        cout << setw(12) << sb << setw(12) << chi2 << setw(12) << q;
        cout << endl << endl;
        return 0;
}

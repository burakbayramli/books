#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bcuint

int main(void)
{
        const DP xx_d[4]={0.0,2.0,2.0,0.0};
        const DP yy_d[4]={0.0,0.0,2.0,2.0};
        int i;
        DP ansy,ansy1,ansy2,ey,ey1,ey2;
        DP x1,x1l,x1u,x1x2,x2,x2l,x2u,xxyy;
        Vec_DP y(4),y1(4),y12(4),y2(4);
        Vec_DP xx(xx_d,4),yy(yy_d,4);

        x1l=xx[0];
        x1u=xx[1];
        x2l=yy[0];
        x2u=yy[3];
        for (i=0;i<4;i++) {
          xxyy=xx[i]*yy[i];
          y[i]=xxyy*xxyy;
          y1[i]=2.0*yy[i]*xxyy;
          y2[i]=2.0*xx[i]*xxyy;
          y12[i]=4.0*xxyy;
        }
        cout << endl << setw(6) << "x1" << setw(9) << "x2";
        cout << setw(8) << "y" << setw(12) << "expect";
        cout << setw(7) << "y1" << setw(11) << "expect";
        cout << setw(7) << "y2" << setw(11) << "expect" << endl << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<10;i++) {
          x2=(x1=0.2*(i+1));
          NR::bcuint(y,y1,y2,y12,x1l,x1u,x2l,x2u,x1,x2,ansy,ansy1,ansy2);
          x1x2=x1*x2;
          ey=x1x2*x1x2;
          ey1=2.0*x2*x1x2;
          ey2=2.0*x1*x1x2;
          cout << setw(8) << x1 << setw(9) << x2 << setw(9) << ansy;
          cout << setw(9) << ey << setw(9) << ansy1 << setw(9) << ey1;
          cout << setw(9) << ansy2 << setw(9) << ey2 << endl;
        }
        return 0;
}

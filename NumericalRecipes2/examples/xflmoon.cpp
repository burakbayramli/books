#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine flmoon

int main(void)
{
        const DP ZON=-5.0;
        const string phase[]={"new moon","first quarter",
          "full moon","last quarter"};
        int i,i1,i2,i3,id,im,iy,j1,j2,n,nph=2;
        DP timzon=ZON/24.0,frac,secs;

        cout << "Date of the next few phases of the moon" << endl;
        cout << "Enter today\'s date (e.g. 12 15 2001):  ";
        cin >> im >> id >> iy;
        cin.get();
        // Approximate number of full moons since january 1900
        n=int(12.37*(iy-1900+((im-0.5)/12.0)));
        j1=NR::julday(im,id,iy);
        NR::flmoon(n,nph,j2,frac);
        n += int((j1-j2)/29.53 + (j1 >= j2 ? 0.5 : -0.5));
        cout << endl << setw(10) << "date" << setw(20) << "time(EST)";
        cout << setw(10) << "phase" << endl << endl;
        for (i=0;i < 20;i++) {
          NR::flmoon(n,nph,j2,frac);
          frac=24.0*(frac+timzon);
          if (frac < 0.0) {
            --j2;
            frac += 24.0;
          }
          if (frac > 12.0) {
            ++j2;
            frac -= 12.0;
          } else
            frac += 12.0;
            i1=int(frac);
            secs=3600.0*(frac-i1);
            i2=int(secs/60.0);
            i3=int(secs-60*i2+0.5);
            NR::caldat(j2,im,id,iy);
            cout << setw(5) << im << setw(4) << id << setw(6) << iy;
            cout << setw(8) << i1 << ":" << setw(2) << i2 << ":";
            cout << setw(2) << i3 << "      " << phase[nph] << endl;
            if (nph == 3) {
              nph=0;
              ++n;
            } else
              ++nph;
        }
        return 0;
}

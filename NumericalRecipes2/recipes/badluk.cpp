#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

int main(void)	// Program badluk
{
	const int IYBEG=2000,IYEND=2100;
	const DP ZON=-5.0;
	int ic,icon,idwk,im,iyyy,jd,jday,n;
	DP timzon=ZON/24.0,frac;

	cout << endl << "Full moons on Friday the 13th from ";
	cout << setw(5) << IYBEG << " to " << setw(5) << IYEND << endl;
	for (iyyy=IYBEG;iyyy<=IYEND;iyyy++) {
		for (im=1;im<=12;im++) {
			jday=NR::julday(im,13,iyyy);
			idwk=int((jday+1) % 7);
			if (idwk == 5) {
				n=int(12.37*(iyyy-1900+(im-0.5)/12.0));
				icon=0;
				for (;;) {
					NR::flmoon(n,2,jd,frac);
					frac=24.0*(frac+timzon);
					if (frac < 0.0) {
						--jd;
						frac += 24.0;
					}
					if (frac > 12.0) {
						++jd;
						frac -= 12.0;
					} else
						frac += 12.0;
					if (jd == jday) {
						cout << endl << setw(2) << im;
						cout << "/13/" << setw(4) << iyyy << endl;
						cout << fixed << setprecision(1);
						cout << "Full moon" << setw(6) << frac;
						cout << " hrs after midnight (EST)" << endl;
						break;
					} else {
						ic=(jday >= jd ? 1 : -1);
						if (ic == (-icon)) break;
						icon=ic;
						n += ic;
					}
				}
			}
		}
	}
	return 0;
}

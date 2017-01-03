#include <cmath>
#include "nr.h"
using namespace std;

int NR::julday(const int mm, const int id, const int iyyy)
{
	const int IGREG=15+31*(10+12*1582);
	int ja,jul,jy=iyyy,jm;

	if (jy == 0) nrerror("julday: there is no year zero.");
	if (jy < 0) ++jy;
	if (mm > 2) {
		jm=mm+1;
	} else {
		--jy;
		jm=mm+13;
	}
	jul = int(floor(365.25*jy)+floor(30.6001*jm)+id+1720995);
	if (id+31*(mm+12*iyyy) >= IGREG) {
		ja=int(0.01*jy);
		jul += 2-ja+int(0.25*ja);
	}
	return jul;
}

#include <fstream>
#include "nr.h"
using namespace std;

void NR::fourew(Vec_FSTREAM_p &file, int &na, int &nb, int &nc, int &nd)
{
	int i;

	for (i=0;i<4;i++) (*file[i]).seekp(0);
	for (i=0;i<4;i++) (*file[i]).seekg(0);
	SWAP(file[1],file[3]);
	SWAP(file[0],file[2]);
	na=2;
	nb=3;
	nc=0;
	nd=1;
}

#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include "nr.h"
using namespace std;

// Driver for routine caldat

int main(void)
{
        const string name[]={"","january","february","march",
          "april","may","june","july","august","september",
          "october","november","december"};
        int i,id,idd,im,imm,iy,iyy,j,n;
        string txt;
        ifstream fp("dates1.dat");

        // Check whether caldat properly undoes the operation of julday
        if (fp.fail())
          NR::nrerror("Data file dates1.dat not found");
        getline(fp,txt);
        fp >> n;
        getline(fp,txt);
        cout << endl << setw(15) << "original date:";
        cout << setw(44) << "reconstructed date:" << endl;
        cout << setw(8) << "month" << setw(6) << "day" << setw(7) << "year";
        cout << setw(16) << "julian day" << setw(13) << "month";
        cout << setw(6) << "day" << setw(7) << "year" << endl << endl;
        for (i=0;i < n;i++) {
          fp >> im >> id >> iy;
          getline(fp,txt);
          j=NR::julday(im,id,iy);
          NR::caldat(j,imm,idd,iyy);
          cout << setw(10) << name[im] << setw(4) << id << setw(7) << iy;
          cout << setw(14) << j << setw(17) << name[imm];
          cout << setw(4) << idd << setw(7) << iyy << endl;
        }
        return 0;
}

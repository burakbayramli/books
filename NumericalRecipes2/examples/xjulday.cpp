#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include "nr.h"
using namespace std;

// Driver for routine julday

int main(void)
{
        const string name[]={"","january","february","march",
          "april","may","june","july","august","september",
          "october","november","december"};
        int i,id,im,iy,n;
        string txt;
        ifstream fp("dates1.dat");

        if (fp.fail())
          NR::nrerror("Data file dates1.dat not found");
        getline(fp,txt);
        fp >> n;
        getline(fp,txt);
        cout << endl << setw(5) << "month" << setw(9) << "day";
        cout << setw(7) << "year" << setw(13) << "julian day";
        cout << setw(10) << "event" << endl << endl;
        for (i=0;i < n;i++) {
          fp >> im >> id >> iy;
          getline(fp,txt);
          cout << setw(10) << name[im] << setw(4) << id;
          cout << setw(7) << iy << setw(10) << NR::julday(im,id,iy);
          cout << " " << txt << endl;
        }
        cout << endl << "Your choices: (negative to end)" << endl;
        cout << "month day year (e.g. 1 13 1905)" << endl;
        for (i=0;i < 20;i++) {
          cout << endl << "mm dd yyyy ?" << endl;
          cin >> im >> id >> iy;
          if (im < 0) return 0;
          cout << "julian day: " << NR::julday(im,id,iy) << endl;
        }
        return 0;
}

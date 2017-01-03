#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine covsrt

int main(void)
{
        const int MA=10,MFIT=5;
        int i,j;
        Vec_BOOL ia(MA);
        Mat_DP covar(MA,MA);

        for (i=0;i<MA;i++)
          for (j=0;j<MA;j++) {
            if ((i < MFIT) && (j < MFIT))
              covar[i][j]=i+j+1;
            else
              covar[i][j]=0.0;
          }
        cout << endl << "original matrix" << endl;
        cout << fixed << setprecision(1);
        for (i=0;i<MA;i++) {
          for (j=0;j<MA;j++) cout << setw(4) << covar[i][j];
          cout << endl;
        }
        cout << "press RETURN to continue..." << endl;
        cin.get();
        cout << endl << "Test #1 - full fitting" << endl;
        for (i=0;i<MA;i++) ia[i]=true;
        NR::covsrt(covar,ia,MA);
        for (i=0;i<MA;i++) {
          for (j=0;j<MA;j++) cout << setw(4) << covar[i][j];
          cout << endl;
        }
        cout << "press RETURN to continue..." << endl;
        cin.get();
        cout << endl << "Test #2 - spread" << endl;
        for (i=0;i<MA;i++)
          for (j=0;j<MA;j++) {
            covar[i][j]=0.0;
            if ((i < MFIT) && (j < MFIT)) covar[i][j]=i+j+1;
          }
        for (i=0;i<MA;i+=2) ia[i]=false;
        NR::covsrt(covar,ia,MFIT);
        for (i=0;i<MA;i++) {
          for (j=0;j<MA;j++) cout << setw(4) << covar[i][j];
          cout << endl;
        }
        return 0;
}

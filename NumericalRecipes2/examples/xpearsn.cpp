#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine pearsn

int main(void)
{
        const int N=10;
        const DP dose_d[N]=
          {56.1,64.1,70.0,66.6,82.0,91.3,90.0,99.7,115.3,110.0};
        const DP spore_d[N]=
          {0.11,0.40,0.37,0.48,0.75,0.66,0.71,1.20,1.01,0.95};
        int i;
        DP prob,r,z;
        Vec_DP dose(dose_d,N), spore(spore_d,N);

        cout << endl << "Effect of Gamma Rays on Man-in-the-Moon Marigolds";
        cout << endl;
        cout << setw(16) << "Count Rate (cpm)";
        cout << setw(24) << "Pollen Index" << endl;
        cout << fixed << setprecision(2);
        for (i=0;i<N;i++)
          cout << setw(10) << dose[i] << setw(26) << spore[i] << endl;
        NR::pearsn(dose,spore,r,prob,z);
        cout << endl << setw(30) << "PEARSN" << setw(17) << "Expected" << endl;
        cout << "Corr. Coeff." << setw(9) << " ";
        cout << fixed << setprecision(6);
        cout << setw(10) << r << setw(16) << 0.9069586 << endl;
        cout << "Probability" << setw(10) << " ";
        cout << setw(10) << prob << setw(16) << 0.2926505e-3 << endl;
        cout << "Fisher's z" << setw(11) << " ";
        cout << setw(10) << z << setw(16) << 1.510110 << endl;
        return 0;
}

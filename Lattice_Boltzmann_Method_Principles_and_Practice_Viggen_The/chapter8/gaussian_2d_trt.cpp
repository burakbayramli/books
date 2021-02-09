/* This code accompanies
 *   The Lattice Boltzmann Method: Principles and Practice
 *   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
 *   ISBN 978-3-319-44649-3 (Electronic) 
 *        978-3-319-44647-9 (Print)
 *   http://www.springer.com/978-3-319-44647-9
 *
 * This code is provided under the MIT license. See LICENSE.txt.
 *
 * Author: Alexandr Kuzmin
 * 
 */
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <cmath>
#include <vector>
#include <limits>

// Domain size
const int NX = 512;
const int NY = 512;

// Time steps
int N = 200;
int NOUTPUT = 10;

// Number of populations
const int NPOP = 9;

// Characteristics of Gaussian hill
double xInit = 200.0;
double yInit = 200.0;
double sigma = 10.0;

// Fields and populations
double g[NY][NX][NPOP], g2[NY][NX][NPOP];
double phase[NY][NX];

// Diffusion parameters
double ux_main = 0.1;
double uy_main = 0.1;

double lambda = 1.0 / 12.0;
double omegaminus = 1.95;
double omegaplus = 1.0 / (0.5 + lambda * omegaminus / (1.0 - 0.5 * omegaminus));

// Underlying lattice parameters
double weights[] = {4.0/9.0, 1.0/9.0, 1.0/9.0, 1.0/9.0, 1.0/9.0, 1.0/36.0, 1.0/36.0, 1.0/36.0, 1.0/36.0};
int cx[] = {0, 1, 0, -1, 0, 1, -1, -1, 1};
int cy[] = {0, 0, 1, 0, -1, 1, 1, -1, -1};
int complement[] = {0, 3, 4, 1, 2, 7, 8, 5, 6};

void writephase(std::string const & fName)
{
    std::string fullName = "gaussian_2d_trt/" + fName+ ".dat";
    std::ofstream fout(fullName.c_str());
    fout.precision(10);

    for (int iY = 0; iY < NY; ++iY)
    {
        for (int iX = 0; iX < NX; ++iX)
            fout << phase[iX][iY] << " ";
        fout << std::endl;
    }
}


void init()
{
    std::cout << "Omegaplus: " << omegaplus << " Omegaminus: " << omegaminus << std::endl;
    // Phase initialization prior any equilibrium functions calculations
    for(int iY = 0; iY < NY; ++iY)
        for(int iX = 0; iX < NX; ++iX)
        {
            double radius = sqrt((double(iX) - xInit) * (double(iX) - xInit) + 
                                 (double(iY) - yInit) * (double(iY) - yInit));
            if (radius <= 3.0 * sigma)
                phase[iY][iX] = exp(-0.5 * radius * radius / (sigma * sigma));
            else
                phase[iY][iX] = 0.0;
        }
        
    // Bulk nodes initialization
    for(int iY = 0; iY < NY; ++iY)
        for(int iX = 0; iX < NX; ++iX)
        {
            for (int iPOP = 0; iPOP < NPOP; iPOP++)
                g[iY][iX][iPOP] = weights[iPOP] * phase[iY][iX] * (1.0 + 
                                  3.0 * cx[iPOP] * ux_main + 3.0 * cy[iPOP] * uy_main + 
                                  4.5 * (cx[iPOP] * cx[iPOP] - 1.0 / 3.0) * ux_main * ux_main +
                                  9.0 * cx[iPOP] * cy[iPOP] * ux_main * uy_main +
                                  4.5 * (cy[iPOP] * cy[iPOP] - 1.0 / 3.0) * uy_main * uy_main);     
        }

}


void collide_bulk()
{
    // Construction of phase field prior all other calculations
    for(int iY = 0; iY < NY; ++iY)
        for(int iX = 0; iX < NX; ++iX)
        {
            phase[iY][iX] = 0.0;
            for(int iPop=0; iPop < NPOP; iPop++)
                phase[iY][iX] += g[iY][iX][iPop];    
        }

    for(int iY = 0; iY < NY; ++iY)
        for(int iX = 0; iX < NX; ++iX)
        {
            double geq[NPOP], geqplus[NPOP], geqminus[NPOP], gplus[NPOP], gminus[NPOP];
            for (int iPOP = 0; iPOP < NPOP; ++iPOP)
            {
                gplus[iPOP] = 0.5 * (g[iY][iX][iPOP] + g[iY][iX][complement[iPOP]]);
                gminus[iPOP] = 0.5 * (g[iY][iX][iPOP] - g[iY][iX][complement[iPOP]]);
                
                geq[iPOP] = weights[iPOP] * phase[iY][iX] * (1.0 + 
                            3.0 * cx[iPOP] * ux_main + 3.0 * cy[iPOP] * uy_main + 
                            4.5 * (cx[iPOP] * cx[iPOP] - 1.0 / 3.0) * ux_main * ux_main +
                            9.0 * cx[iPOP] * cy[iPOP] * ux_main * uy_main +
                            4.5 * (cy[iPOP] * cy[iPOP] - 1.0 / 3.0) * uy_main * uy_main);
              }
              for (int iPOP = 0; iPOP < NPOP; ++iPOP)
            {
                geqplus[iPOP] = 0.5 * (geq[iPOP] + geq[complement[iPOP]]);
                geqminus[iPOP] = 0.5 * (geq[iPOP] - geq[complement[iPOP]]);       
                g2[iY][iX][iPOP] = g[iY][iX][iPOP] - omegaplus * (gplus[iPOP] - geqplus[iPOP])
                                                     - omegaminus * (gminus[iPOP] - geqminus[iPOP]);    
         
            }
                          

        }
}


int main(int argc, char* argv[])
{
    init();
    
    for(int counter = 0; counter <= N; ++counter)
    {

        collide_bulk();

        //Streaming
        for(int iY = 0; iY < NY; ++iY)
            for(int iX = 0; iX < NX; ++iX)
                for(int iPOP=0; iPOP < NPOP; ++iPOP)
                {
                    int iX2 = (iX - cx[iPOP] + NX) % NX;
                    int iY2 = (iY - cy[iPOP] + NY) % NY;
    
                    g[iY][iX][iPOP]= g2[iY2][iX2][iPOP];
                }

        //Writing files
        if (counter%NOUTPUT==0)
        {
            std::cout<<counter<<"\n";
            std::stringstream filewritephase;
            std::stringstream counterconvert;
            counterconvert<<counter;
            filewritephase<<std::fixed;

            filewritephase<<"gaussian_2d_trt"<<std::string(6-counterconvert.str().size(),'0')<<counter;

            writephase(filewritephase.str());

        }
    
    
    }

    return 0;
}

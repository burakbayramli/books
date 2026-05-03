////////////////////////////////////////////////////////////////////////////////////////////////////
// 2D DSMC solver for steady-state problems, Template 01
//--------------------------------------------------------------------------------------------------
// The purpose of this template is to include random number generators and set initial seed
//--------------------------------------------------------------------------------------------------
// ME 491/591 Non-equilibrium gas dynamics, Spring 2017
// Alexey N. Volkov, Univesity of Alabama, avolkov1@ua.edu
////////////////////////////////////////////////////////////////////////////////////////////////////
#define _CRT_SECURE_NO_WARNINGS // Requred to suppress crt warnings under MS Visual Studio
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
//#include <conio.h> // This file does not exist under Linux
        
//--------------------------------------------------------------------------------------------------
// Mathematical constants
//--------------------------------------------------------------------------------------------------

        #define M_PI                    3.14159265358979323846

//--------------------------------------------------------------------------------------------------
// Random number generators
//--------------------------------------------------------------------------------------------------

        #include "BRNG.cxx"

        #include "DiscreteStdRNG.cxx"
        #include "DiscreteUniformRNG.cxx"
        #include "PoissonRNG.cxx"

        #include "RayleighRNG.cxx"
        #include "GaussianRNG.cxx"
        #include "ARMRNG.cxx"

        #include "Point2DUniformRectRNG.cxx"
        #include "Point2DUniformCircleRNG.cxx"

        #include "IsotropicVectorRNG.cxx"

//--------------------------------------------------------------------------------------------------
// Table of probabilities used as an example for standard generator of discrete random variables
//--------------------------------------------------------------------------------------------------
                
        #define PTSIZE  3
        double  X[PTSIZE] = { 0.1, -0.3, 6.0 };
        double  P[PTSIZE] = { 0.25, 0.6, 0.15 };

//--------------------------------------------------------------------------------------------------
// PDF of a randon variable used as an example for the acceptance and rejection method
//--------------------------------------------------------------------------------------------------

        double  R = 3.0;
        double  r_min = 0.0;
        double  r_max = R;

        double pdf_cyl ( double r ) ////////////////////////////////////////////////////////////////
        // This is the PDF for the polar radius for a point with uniform distribution inside a circle
        // of radius R, see slides 25 and 26 in Section 5.4.
        { //////////////////////////////////////////////////////////////////////////////////////////
                return 2.0 * r / R / R;
        } //////////////////////////////////////////////////////////////////////////////////////////

        double  pdf_max = pdf_cyl ( R );

//--------------------------------------------------------------------------------------------------
// Main function
//--------------------------------------------------------------------------------------------------

        int main ( int argc, char argv ) ///////////////////////////////////////////////////////////
        {

                // Set the initial seed for pseudo-random number generators
        time_t  t;
	        SetSeed ( unsigned ( time ( &t ) ), unsigned ( 362436069 ) );

                // Here we calculate cumulative probabilities for discrete random variable given by the table (X,P)
        double  Pcum[PTSIZE];
                Pcum[0] = P[0];
                for ( int i = 1; i < PTSIZE; i++ ) Pcum[i] = Pcum[i-1] + P[i];

                // Here we generate in a loop 10 realizations of various random variables
        int     Count = 10;
                do {
                        printf ( "\nPass %d\n", 10 - Count );
                        printf ( "Random number = %18.10e\n", brng () );
                        printf ( "Discrete variable = %18.10e\n", frand_discrete_std ( PTSIZE, X, Pcum ) );
                        printf ( "Discrete variable with uniform distribution = %d\n", irand_uniform ( 3 ) );
                        printf ( "Discrete variable with Poisson distribution = %d\n", irand_Poisson ( 10.0 ) );
                        printf ( "Continuous variable with Rayleigh distribution = %18.10e\n", frand_Rayleigh ( 300.0 ) );
                        printf ( "Continuous variable with Gaussian distribution = %18.10e\n", frand_Gaussian ( -300.0, 100.0 ) );
                        printf ( "Continuous variable with given pdf = %18.10e\n", frand_arm ( r_min, r_max, pdf_max, &pdf_cyl ) );
                        Count--;
                        //getch (); // This function does not work under Linux
                } while ( Count > 0 );

        } //////////////////////////////////////////////////////////////////////////////////////////
        
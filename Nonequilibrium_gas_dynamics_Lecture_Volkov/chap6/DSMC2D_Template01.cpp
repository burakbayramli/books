////////////////////////////////////////////////////////////////////////////////////////////////////
// 2D DSMC solver for steady-state problems, Template 01
//--------------------------------------------------------------------------------------------------
// The purpose of this template is to include random number generators and set initial seed
//--------------------------------------------------------------------------------------------------
// ME 491/591 Non-equilibrium gas dynamics, Spring 2017
// Alexey N. Volkov, Univesity of Alabama, avolkov1@ua.edu
////////////////////////////////////////////////////////////////////////////////////////////////////
#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
        
//--------------------------------------------------------------------------------------------------
// Mathematical and physical constants
//--------------------------------------------------------------------------------------------------

        #define M_PI                    3.14159265358979323846
        #define BOLTZMANN_CONSTANT      1.380662E-23                    // Boltzmann constant (J/K) 
        #define AVOGADRO_CONSTANT       6.022045E+23                    // Avogadro constant (1/mole)
        #define UNIT_DALTON             1.66056e-27                     // Atomic mass unit, Dalton (kg)

//--------------------------------------------------------------------------------------------------
// Random number generators
//--------------------------------------------------------------------------------------------------

        #include "BRNG.cxx"

        #include "DiscreteUniformRNG.cxx"
        #include "PoissonRNG.cxx"

        #include "RayleighRNG.cxx"
        #include "GaussianRNG.cxx"

        #include "Point2DUniformRectRNG.cxx"

        #include "IsotropicVectorRNG.cxx"

//--------------------------------------------------------------------------------------------------
// Main function
//--------------------------------------------------------------------------------------------------

        int main ( int argc, char **argv ) /////////////////////////////////////////////////////////
        {

                // Set the initial seed for pseudo-random number generators
        time_t  t;
	        SetSeed ( unsigned ( time ( &t ) ), unsigned ( 362436069 ) );

        } //////////////////////////////////////////////////////////////////////////////////////////
        
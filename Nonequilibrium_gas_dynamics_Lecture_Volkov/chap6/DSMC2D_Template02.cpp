////////////////////////////////////////////////////////////////////////////////////////////////////
// 2D DSMC solver for steady-state problems, Template 02
//--------------------------------------------------------------------------------------------------
// The purpose of this template is to implement a skeleton (time loop) of the DSMC algorithm
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
// Problem-specific constants and types of the DSMC algorithm
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
// Input parameters for the problem and method
//--------------------------------------------------------------------------------------------------

        //------------------------------------------------------------------------------------------
        // Input parameters of the DSMC algorithm, which are necessary to define the time loop
        //------------------------------------------------------------------------------------------

        double  Dt;                     // Time step (s)
        int     NStep;                  // Total number of time steps
        int     FirstSamplingStep;      // Number of the first sampling step
        int     SamplingPeriod;         // Period between sampling steps
        int     PrintPeriod;            // Period between printing of results

//--------------------------------------------------------------------------------------------------
// Calculated constants
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
// Dynamic variables of the DSMC process
//--------------------------------------------------------------------------------------------------

        //------------------------------------------------------------------------------------------
        // Time varables defining the current state of the simulation process
        //------------------------------------------------------------------------------------------

        int     Step;                   // Current step number
        double  Time;                   // Current time (s)

//--------------------------------------------------------------------------------------------------
// Major components of the DSMC algorithm
//--------------------------------------------------------------------------------------------------

        void Setup () //////////////////////////////////////////////////////////////////////////////
        {
        }

        void InitialConditions () //////////////////////////////////////////////////////////////////
        {
                Step = 0;
                Time = 0.0;
        }

        void MoveParticles () //////////////////////////////////////////////////////////////////////
        {
        }

        void BoundaryConditions () //////////////////////////////////////////////////////////////////
        {
        } 

        void Indexing () ///////////////////////////////////////////////////////////////////////////
        {
        }

        void CollideParticles () ////////////////////////////////////////////////////////////////////
        {
        }

        void Sampling () ////////////////////////////////////////////////////////////////////////////
        {
        }

        void Printing () ////////////////////////////////////////////////////////////////////////////
        {
        }

//--------------------------------------------------------------------------------------------------
// Main function
//--------------------------------------------------------------------------------------------------

        int main ( int argc, char **argv ) /////////////////////////////////////////////////////////
        {

                // Set the initial seed for pseudo-random number generators
        time_t  t;
	        SetSeed ( unsigned ( time ( &t ) ), unsigned ( 362436069 ) );

                // Setup of the computational algorithm: Calculation of all constants
                Setup ();

                // Set initial distribution of particles in the domain
                InitialConditions ();

                // DSMC time loop
                do {        
                        MoveParticles ();
                        BoundaryConditions ();        
                        Indexing ();
                        CollideParticles ();
                        if ( Step > FirstSamplingStep && Step % SamplingPeriod == 0 ) Sampling ();
                        Step++;
                        Time += Dt;
                        if ( Step > FirstSamplingStep && Step % PrintPeriod == 0 ) Printing ();
                } while ( Step < NStep );

                // Printing the final results                        
                Printing ();

        } //////////////////////////////////////////////////////////////////////////////////////////
        
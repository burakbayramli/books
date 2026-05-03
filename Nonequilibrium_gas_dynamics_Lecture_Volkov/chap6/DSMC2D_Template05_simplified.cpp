////////////////////////////////////////////////////////////////////////////////////////////////////
// 2D DSMC solver for steady-state problems, Template 05 (Simplified)
//--------------------------------------------------------------------------------------------------
// The purpose of this template is to implement cell structure, sampling, and printing.
//
// This simplified version of Template 5 is discussed in slides 24 and 25 of lecture notes (Section 6.3).
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

        //------------------------------------------------------------------------------------------
        // Constant and types defining the state variables
        //------------------------------------------------------------------------------------------

        #define DIM             2               // Spatial dimension of the problem
        #define MAX_PCL         1000000         // Maximum number of particles in the domain

        // This structure contains dynamic state variables for an individual simulated particle

        typedef struct pcl { ///////////////////////////////////////////////////////////////////////
                double  X[DIM];
                double  V[3];
        } PCL;

        //------------------------------------------------------------------------------------------
        // Constant and types defining the properties of the computtaional mesh of cells
        //------------------------------------------------------------------------------------------

        #define MAX_PCL_IN_CELL 1000            // Maximum number of particles in a cell
        #define MAX_CELL_X      200             // Maximum number of cells along X axis
        #define MAX_CELL_Y      200             // Maximum number of cells along X axis

        //------------------------------------------------------------------------------------------
        // Type defining the counters for macroscopic parameters in cells of the computtaional mesh
        //------------------------------------------------------------------------------------------

        // This structure contains counters for calculation of macroscopic parameters in a cell of the computational mesh

        typedef struct cell { //////////////////////////////////////////////////////////////////////
                double  CountNP;                // Particle number -> Number density
                double  CountV[3];              // Velocity vector V -> Macroscopic gas velocity
                double  CountV2;                // Velocity tensor V x V -> Density of internal energy
        } CELL;

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
        
        //------------------------------------------------------------------------------------------
        // Domain size
        //------------------------------------------------------------------------------------------

        double  X1;                     // Position of the left boundary of the domain (m)
        double  X2;                     // Position of the right boundary of the domain (m)
        double  Y1;                     // Position of the bottom boundary of the domain (m)
        double  Y2;                     // Position of the top boundary of the domain (m)
        double  DZ;                     // Domain (cell) size in z direction for 2D problem (m)

        //------------------------------------------------------------------------------------------
        // Parameters, which define discretization of the domain into a mesh of cells
        //------------------------------------------------------------------------------------------

        int     NX;                     // Number of cells along X axis
        int     NY;                     // Number of cells along Y axis

//--------------------------------------------------------------------------------------------------
// Calculated constants
//--------------------------------------------------------------------------------------------------

        double  DX, DY;                 // Cell sizes (m)
        double  DV;                     // Cell volume (m^3)
        double  Weight;                 // Statistical weight of simulated particles
        double  MoleculeMass;           // Mass of a molecule (kg)

//--------------------------------------------------------------------------------------------------
// Dynamic variables of the DSMC process
//--------------------------------------------------------------------------------------------------

        //------------------------------------------------------------------------------------------
        // Time variables defining the current state of the simulation process
        //------------------------------------------------------------------------------------------

        int     Step;                   // Current step number
        double  Time;                   // Current time (s)

        //------------------------------------------------------------------------------------------
        // State variables defining the current state of all simulated particles
        //------------------------------------------------------------------------------------------

        int     NP;                     // Current number of particles in the domain
        PCL     P[MAX_PCL];             // Array of simulated particles                                

        //------------------------------------------------------------------------------------------
        // Indices of particles in cells of the computational mesh
        //------------------------------------------------------------------------------------------

        int     NPC[MAX_CELL_X][MAX_CELL_Y]; // Numbers of particles in cells
        int     IPC[MAX_CELL_X][MAX_CELL_Y][MAX_PCL_IN_CELL]; // Indices of particles in cells

        //------------------------------------------------------------------------------------------
        // Counters for sampling of macroscopic gas parameters
        //------------------------------------------------------------------------------------------

        int     SampleStep;                     // Number of sampled steps
        double  SampleTime;                     // Duration of sampled steps (s)
        CELL    C[MAX_CELL_X][MAX_CELL_Y];      // Sample counters in cells

//--------------------------------------------------------------------------------------------------
// Major components of the DSMC algorithm
//--------------------------------------------------------------------------------------------------

        void Setup () //////////////////////////////////////////////////////////////////////////////
        {
                DX = ( X2 - X1 ) / NX;
                DY = ( Y2 - Y1 ) / NY;
                DV = DX * DY * DZ;
        }

        void InitialConditions () //////////////////////////////////////////////////////////////////
        {
                Step = 0;
                Time = 0.0;
                NP = 0;

                // Set to zero all counters of macroscopic properties
                SampleStep = 0;
                memset ( C, 0, sizeof C );
        }

        void MoveParticles () //////////////////////////////////////////////////////////////////////
        {
                for ( int i = 0; i < NP; i++ )
                        for ( int m = 0; m < DIM; m++ ) P[i].X[m] += Dt * P[i].V[m];
        }

        void BoundaryConditions () //////////////////////////////////////////////////////////////////
        {
        } 

        void Indexing () ///////////////////////////////////////////////////////////////////////////
        {
                // Set initial number of particles in every cell to zero
                memset ( NPC, 0, sizeof NPC );
                // Distribute particles between cells
                for ( int i = 0; i < NP; i++ ) {
                        // Calculate indices of the cell
                        int k = int ( ( P[i].X[0] - X1 ) / DX );
                        int l = int ( ( P[i].X[1] - Y1 ) / DY );
                        // Add particle to the cell
                        IPC[k][l][NPC[k][l]++] = i;
                }
        }

        void CollideParticles () ////////////////////////////////////////////////////////////////////
        {
        }
        
        #define sqr3( V ) ( V[0] * V[0] + V[1] * V[1] + V[2] * V[2] )

        void Sampling () ////////////////////////////////////////////////////////////////////////////
        {
                SampleStep++;
                for ( int k = 0; k < NX; k++ )
                        for ( int l = 0; l < NY; l++ ) {
                                C[k][l].CountNP += NPC[k][l];
                                for ( int i = 0; i < NPC[k][l]; i++ ) {
                                        C[k][l].CountV2 += sqr3 ( P[IPC[k][l][i]].V );
                                        for ( int m = 0; m < 3; m++ ) 
                                                C[k][l].CountV[m] += P[IPC[k][l][i]].V[m];
                                }
                        }
        }

        void Printing () ////////////////////////////////////////////////////////////////////////////
        {
        FILE    *F = fopen ( "DSMC2D_Field.plt", "wt" );
                fprintf ( F, "VARIABLES = \"X\" \"Y\" \"N\" \"Ux\" \"Uy\" \"E\"\n" );
                fprintf ( F, "ZONE I=%d J=%d\n", NX, NY );
                for ( int k = 0; k < NY; k++ ) 
                        for ( int l = 0; l < NX; l++ ) {
                                double X = X1 + ( k + 0.5 ) * DX;
                                double Y = Y1 + ( l + 0.5 ) * DY;
                                double N = Weight * C[k][l].CountNP / SampleStep / DV;
                                double U[3];
                                for ( int m = 0; m < 3; m++ ) 
                                        U[m] = C[k][l].CountV[m] / C[k][l].CountNP;
                                double E = Weight * MoleculeMass * C[k][l].CountV2 / 2.0 / SampleStep / DV - N * MoleculeMass * sqr3 ( U ) / 2.0;
                                fprintf ( F, "%18.10e %18.10e %18.10e %18.10e %18.10e %18.10e\n", X, Y, N, U[0], U[1], E );
                        }
                fclose ( F );
        }

//--------------------------------------------------------------------------------------------------
// Main function
//--------------------------------------------------------------------------------------------------

        int main ( int argc, char argv ) ///////////////////////////////////////////////////////////
        {

                // Set the initial seed for pseudo-random number generators
        time_t  t;
	        SetSeed ( unsigned ( time ( &t ) ), unsigned ( 362436069 ) );

                // Setup of the computational algorithm
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
        
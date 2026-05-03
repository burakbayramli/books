////////////////////////////////////////////////////////////////////////////////////////////////////
// 2D DSMC solver for steady-state problems, Template 07
//--------------------------------------------------------------------------------------------------
// The purpose of this template is to implement diffuse scaterring of gas molecules on the wing 
// surface.
//
// The boundary conditions at the body surface are embedded into the function MoveParticles.
//
// This version of the code can be used in order to simulate free moleucular flow past the wing, 
// since collisions between gas molecules are not accounted for yet.
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

        void vrand_MB ( double *v, double m, double *u, double T ) /////////////////////////////////
        // This function generates random veocity vector from Maxwell-Boltzmann distribution.
        // It was adopted from solution of problem 6 in homework 4.
        { //////////////////////////////////////////////////////////////////////////////////////////
        double  RT = BOLTZMANN_CONSTANT * T / m;
                v[0] = frand_Gaussian ( u[0], RT ); 
                v[1] = frand_Gaussian ( u[1], RT ); 
                v[2] = frand_Gaussian ( u[2], RT ); 
        }

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
                double  CountVV[3][3];          // Velocity tensor V x V -> Stress tensor
                double  CountV2V[3];            // Vector of V^2 * V -> Heat flux vector
                double  CountNC;                // Collsion number -> Collsion frequency, M.F.T. and M.F.P.
        } CELL;

//--------------------------------------------------------------------------------------------------
// Input parameters for the problem and method
//--------------------------------------------------------------------------------------------------

        //------------------------------------------------------------------------------------------
        // Input parameters of the DSMC algorithm, which are necessary to define the time loop
        //------------------------------------------------------------------------------------------

        double  Dt                      = 1.0e-06;      // Time step (s)
        int     NStep                   = 100000;       // Total number of time steps
        int     FirstSamplingStep       = 10000;        // Number of the first sampling step
        int     SamplingPeriod          = 1;            // Period between sampling steps
        int     PrintPeriod             = 10000;        // Period between printing of results
        
        //------------------------------------------------------------------------------------------
        // Domain size
        //------------------------------------------------------------------------------------------

        double  X1      = -1.0;         // Position of the left boundary of the domain (m)
        double  X2      = 1.0;          // Position of the right boundary of the domain (m)
        double  Y1      = -1.0;         // Position of the bottom boundary of the domain (m)
        double  Y2      = 1.0;          // Position of the top boundary of the domain (m)
        double  DZ      = 0.1;          // Domain (cell) size in z direction for 2D problem (m)

        //------------------------------------------------------------------------------------------
        // Parameters, which define discretization of the domain into a mesh of cells
        //------------------------------------------------------------------------------------------

        int     NX      = 100;          // Number of cells along X axis
        int     NY      = 100;          // Number of cells along Y axis

        //------------------------------------------------------------------------------------------
        // Parameters defining the number of simulated particles
        //------------------------------------------------------------------------------------------

        double  NPCFree = 10;           // Average number of particles in a cell of the free stream
        double  DL      = 0.1;          // Size of the auxiliary domains introduces to implement inflow of particles from the free stream (m)

        //------------------------------------------------------------------------------------------
        // Parameters of the problem
        //------------------------------------------------------------------------------------------

        // Gas properties
        double  MolarMass = 0.040;      // Molar mass of gas (kg/mole)

        // Free stream properties
        double  MaFree = 4.0;           // Free stream Mach number
        double  PFree = 0.1;            // Free stream pressure (Pa)
        double  TFree = 200.0;          // Free stream temperature (K)

        // Body properties
        double  WingX = -0.25;          // X coordinate of the wing leading edge (m)
        double  WingY = 0.0;            // Y coordinate of the wing leading edge (m)
        double  WingLength = 0.5;       // Length of the wing (m)
        double  AttackAngle = 30.0;     // Angle of attack (degree)
        double  Tw = 300.0;             // Temperature of the wing surface (K)

//--------------------------------------------------------------------------------------------------
// Calculated constants
//--------------------------------------------------------------------------------------------------

        double  DX, DY;                 // Cell sizes (m)
        double  DV;                     // Cell volume (m^3)
        double  Weight;                 // Statistical weight of simulated particles
        double  MoleculeMass;           // Mass of a molecule (kg)
        
        double  NFree;                  // Number desnity in the free stream (1/m^3)
        double  UFree;                  // Velocity in the free stream (m/s)
        double  UxFree, UyFree;         // X and Y components of the gas velocity vector in the free stream  

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
// This function is the major component of initial conditions and free stream boundary condistions
// in the problem under consideration. It adds to the particle list (NP,P) new particles uniformly distributed
// in the rectangle [X1,X2]x[Y1,Y2] with number density Ngas and Maxwell-Boltzmann velocity distribution
// with macroscopic velocty (Ugasx,Ugasy) and temperature Tgas
//--------------------------------------------------------------------------------------------------

        void GenerateNewParticles ( double X1, double Y1, double X2, double Y2, double Ngas, double Ugasx, double Ugasy, double Tgas, int MoveFlag )
        {
        double  V = ( X2 - X1 ) * ( Y2 - Y1 ) * DZ; // Volume of the rectangular region (m^3)
        double  Nnew_avg = Ngas * V / Weight; // Average number of particles to be added
        int     Nnew; // Random number of particles to be added
                if ( Nnew_avg > 20.0 ) { // Number if large; use standard method for discrete variables
                        Nnew = int ( Nnew_avg );
                        if ( brng () < Nnew_avg - Nnew ) Nnew++; 
                } else { // Number is not large; use Poisson distribution
                        Nnew = irand_Poisson ( Nnew_avg );
                }          
                // Check that the particle list has enought space for new particles
                if ( NP + Nnew > MAX_PCL ) {
                        printf ( "ERROR in [GenerateNewParticles]: Not enough space for new particles\n" );
                        return;
                }
                // Now we add new particles one by one
        double  Ugas[3] = { Ugasx, Ugasy, 0.0 };
                for ( int i = NP; i < NP + Nnew; i++ ) {
                        // Generate position
                        v2rand_uniform_rect ( P[i].X[0], P[i].X[1], X1, X2, Y1, Y2 );
                        // Generate velocity
                        vrand_MB ( P[i].V, MoleculeMass, Ugas, Tgas );
                        if ( MoveFlag == 1 ) { // Move particle during time step
                                P[i].X[0] += Dt * P[i].V[0];
                                P[i].X[1] += Dt * P[i].V[1];
                        }
                }
                NP += Nnew;
        }

//--------------------------------------------------------------------------------------------------
// This function implements diffuse scattering from a surface with normal along axis Y, 
// so Y component of the unit normal vector si equal to eigher +1 or -1.
//--------------------------------------------------------------------------------------------------

        void DiffuseScattering ( double *v, double m, double Tw, double Ny ) ///////////////////////
        {
        double  RTw = BOLTZMANN_CONSTANT * Tw / m;
                v[0] = frand_Gaussian ( 0.0, RTw ); 
                v[1] = Ny * frand_Rayleigh ( sqrt ( RTw ) ); 
                v[2] = frand_Gaussian ( 0.0, RTw ); 
        } 

//--------------------------------------------------------------------------------------------------
// Major components of the DSMC algorithm
//--------------------------------------------------------------------------------------------------

        void Setup () //////////////////////////////////////////////////////////////////////////////
        {
                MoleculeMass = MolarMass / AVOGADRO_CONSTANT; // Mass of a molecule (kg)
                NFree = PFree / ( BOLTZMANN_CONSTANT * TFree ); // Number desnity in the free stream (1/m^3)
                UFree = MaFree * sqrt ( ( 5.0 / 3.0 ) * BOLTZMANN_CONSTANT * TFree / MoleculeMass ); // Velocity in the free stream (m/s)
                UxFree = UFree * cos ( M_PI * AttackAngle / 180.0 ); // Z component of the gas velocity vector in the free stream
                UyFree = - UFree * sin ( M_PI * AttackAngle / 180.0 ); // Y component of the gas velocity vector in the free stream  

                DX = ( X2 - X1 ) / NX;
                DY = ( Y2 - Y1 ) / NY;
                DV = DX * DY * DZ;

                Weight = DV * NFree / NPCFree; // Statistical weight of simulated particles
        }

        void InitialConditions () //////////////////////////////////////////////////////////////////
        {
                Step = 0;
                Time = 0.0;
                NP = 0;

                // Set to zero all counters of macroscopic properties
                SampleStep = 0;
                SampleTime = 0.0;
                memset ( C, 0, sizeof C );

                // Here we distribute initial particles according to the equilibrium distribution in the free stream
                GenerateNewParticles ( X1, Y1, X2, Y2, NFree, UxFree, UyFree, TFree, 0 );
        }

        void MoveParticles () //////////////////////////////////////////////////////////////////////
        {
                for ( int i = 0; i < NP; i++ ) {
                        double X0 = P[i].X[0]; 
                        double Y0 = P[i].X[1]; 
                        for ( int m = 0; m < DIM; m++ ) P[i].X[m] += Dt * P[i].V[m];
                        // Here we implement diffuse scattering of gas molecules from the wing surface
                        if ( ( Y0 - WingY ) * ( P[i].X[1] - WingY ) < 0.0 ) {
                                // Linear interpolation to point Y = WingY
                                double Xw = ( X0 * ( WingY - P[i].X[1] ) + P[i].X[0] * ( Y0 - WingY ) ) / ( Y0 - P[i].X[1] );
                                if ( Xw > WingX && Xw < WingX + WingLength ) { // Molecule interacts with the wing during the time step
                                        // Linear interpolation of the time of scattering
                                        double Dt1 = Dt - Dt * ( Y0 - WingY ) / ( Y0 - P[i].X[1] );
                                        // Generate velocity vector of the reflected molecule
                                        DiffuseScattering ( P[i].V, MoleculeMass, Tw, ( Y0 - WingY > 0 ) ? 1.0 : ( -1.0 )  );
                                        // Move the reflected molecule
                                        P[i].X[0] = Xw + Dt1 * P[i].V[0];
                                        P[i].X[1] = WingY + Dt1 * P[i].V[1];
                                }
                        }
                }
        }

        void BoundaryConditions () //////////////////////////////////////////////////////////////////
        {
                // Here we generate new particles at the external boundaries of the computational domain
                // where the distribution function of molecules entering domain is the free stream
                // equilibrium distribution function
                // Left boundary
                GenerateNewParticles ( X1 - DL, Y1 - DL, X1, Y2 + DL, NFree, UxFree, UyFree, TFree, 1 );
                // Right boundary
                GenerateNewParticles ( X2, Y1 - DL, X2 + DL, Y2 + DL, NFree, UxFree, UyFree, TFree, 1 );
                // Bottom boundary
                GenerateNewParticles ( X1, Y1 - DL, X2, Y1, NFree, UxFree, UyFree, TFree, 1 );
                // Top boundary
                GenerateNewParticles ( X1, Y2, X2, Y2 + DL, NFree, UxFree, UyFree, TFree, 1 );

                // Here we remove all particles that are located outside the domain
                for ( int i = 0; i < NP; ) {
                        if ( P[i].X[0] <= X1 || P[i].X[0] >= X2 || P[i].X[1] <= Y1 || P[i].X[1] >= Y2 ) {
                                // Particle i is outside the domain, so we replace P[i] with P[NP-1]
                                if ( i < NP - 1 ) memmove ( &P[i], &P[NP-1], sizeof ( PCL ) );
                                NP--; 
                        } else {
                                i++;
                        }
                }
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
                SampleTime += Dt;
                for ( int k = 0; k < NX; k++ )
                        for ( int l = 0; l < NY; l++ ) 
                                for ( int i = 0; i < NPC[k][l]; i++ ) {
                                        C[k][l].CountNP += 1.0;
                                        double V2 = sqr3 ( P[IPC[k][l][i]].V );
                                        for ( int m = 0; m < 3; m++ ) {
                                                C[k][l].CountV[m] += P[IPC[k][l][i]].V[m];
                                                C[k][l].CountV2V[m] += V2 * P[IPC[k][l][i]].V[m];
                                                for ( int n = 0; n < 3; n++ )
                                                        C[k][l].CountVV[m][n] += P[IPC[k][l][i]].V[m] * P[IPC[k][l][i]].V[n];
                                        }
                        }
        }

        void Printing () ////////////////////////////////////////////////////////////////////////////
        {
        FILE    *F = fopen ( "DSMC2D_Field.plt", "wt" );
                fprintf ( F, "VARIABLES = \"X\" \"Y\" \"N\" \"Ux\" \"Uy\" \"Tx\" \"Ty\" \"T\"\n" );
                fprintf ( F, "ZONE I=%d J=%d\n", NX, NY );
                for ( int k = 0; k < NY; k++ ) 
                        for ( int l = 0; l < NX; l++ ) {
                                double X = X1 + ( k + 0.5 ) * DX;
                                double Y = Y1 + ( l + 0.5 ) * DY;
                                double N = Weight * C[k][l].CountNP / SampleStep / DV;  // Number density
                                double U[3], Txyz[3];
                                for ( int m = 0; m < 3; m++ ) {
                                        U[m] = C[k][l].CountV[m] / C[k][l].CountNP; // Components of the macroscopic gas velocity vector
                                        Txyz[m] = MoleculeMass * ( C[k][l].CountVV[m][m] / C[k][l].CountNP - U[m] * U[m] ) / BOLTZMANN_CONSTANT; // Temperature analogues characterizing energy of chaotic motion of a single degree of freedom
                                }
                                double T = ( Txyz[0] + Txyz[1] + Txyz[2] ) / 3.0; // Temperature
                                fprintf ( F, "%18.10e %18.10e %18.10e %18.10e %18.10e %18.10e %18.10e %18.10e\n", X, Y, N, U[0], U[1], Txyz[0], Txyz[1], T );
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
                        if ( Step % 100 == 0 ) printf ( "Time step %d, Number of particles %d\n", Step, NP );
                } while ( Step < NStep );

                // Printing the final results                        
                Printing ();

        } //////////////////////////////////////////////////////////////////////////////////////////
        
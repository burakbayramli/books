/*
  Copyright 2017-2020 Airinnova AB and the PyTornado authors

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/

/*******************************************************************************
HEADER FILE FOR VORTEX-LATTICE METHOD CODE

Authors:
* Alessandro Gastaldi
* Aaron Dettmann
*******************************************************************************/

/*
    NOTES:
    Changes were made to the data structures of C-TORNADO:

    * Structs and corresponding typedefs have same name now
    * Instances of data structs have complete names now, e.g. "lattice", "results"

    * Removed geometrystruct: geometric parameters defined in Python

    * Lattice.XYZ renamed to lattice.P
    * Panel areas now stored in lattice.A

    * State.AS renamed to state.airspeed
    * State.betha renamed to state.beta

    * CG now stored in refstruct
    * RP now stored in refstruct

    * Changed resultstruct: now includes gamma, inwash, forces, coefficients
*/

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION

// Python- and NumPy-C-API
#include <Python.h>
#include <numpy/arrayobject.h>

#include <float.h>
#include <iostream>
#include <stdexcept>

const int NUM_COORD = 3;  // Number of coordinates per point (x, y, z)
const int NUM_P = 4;      // Number of points per panel (P, Q, R, S)
const int NUM_V = 4;      // Number of points per horseshoe (V1, V2, V3, V4)

const double PI = 3.14159265358979323846;

const int ERR_SUCCESS = 0;
const int ERR_TYPE = -10;
const int ERR_KEY = -20;
const int ERR_VALUE = -30;
const int ERR_ATTRIBUTE = -40;

// Lattice geometry data
typedef struct latticestruct
{
    double* P;                 // (num_pan * 4 * 3) 1D array of panel corner points
    double* V;                 // (num_pan * 4 * 3) 1D array of panel vortex segment points
    double* C;                 // (num_pan * 3) 1D array of panel collocation points
    double* BoundLegMidpoint;  // (num_pan * 3) 1D array of bound leg midpoints
    double* N;                 // (num_pan * 3) 1D array of panel normals

    double* A;                 // (num_pan * 1) 1D array of panel surface areas
    double EPSILON;            // Small number

} latticestruct;

// Flight state data
typedef struct statestruct
{
    double airspeed;  // Airspeed [m/s]
    double rho;       // Density [kg/m^3]

    double alpha;     // Pitch angle [degree]
    double beta;      // Yaw angle [degree]

    double P;         // Pitch rate [1/s]
    double Q;         // Yaw rate [1/s]
    double R;         // Roll rate [1/s]
} statestruct;

// Reference values
typedef struct refstruct
{
    double* CG;    // (x, y, z) Center of gravity
    double* RP;    // (x, y, z) Reference point

    double area;   // Reference surface area
    double span;   // Reference semi-span length
    double chord;  // Reference chord length
} refstruct;

// vortex-lattice method solver data
typedef struct resultstruct
{
    double* gamma;     // (num_pan * 1) 1D array of panel vortex strengths

    double* iw_x;      // (num_pan * 1) 1D array of panel x-velocities
    double* iw_y;      // (num_pan * 1) 1D array of panel y-velocities
    double* iw_z;      // (num_pan * 1) 1D array of panel z-velocities
    double* iw_mag;    // (num_pan * 1) 1D array of panel velocity magnitudes

    double* fpan_x;    // (num_pan * 1) 1D array of panel x-forces
    double* fpan_y;    // (num_pan * 1) 1D array of panel y-forces
    double* fpan_z;    // (num_pan * 1) 1D array of panel z-forces
    double* fpan_mag;  // (num_pan * 1) 1D array of panel force magnitudes

    double* cp;        // (num_pan * 1) 1D array of panel pressure coefficients

    double ftot_x;     // Total x-force
    double ftot_y;     // Total y-force
    double ftot_z;     // Total z-force

    double Cx;         // Total x-force coefficient
    double Cy;         // Total y-force coefficient
    double Cz;         // Total z-force coefficient

    double ftot_D;     // Total lift force
    double ftot_C;     // Total side force
    double ftot_L;     // Total drag force

    double CD;         // Total lift force coefficient
    double CC;         // Total side force coefficient
    double CL;         // Total drag force coefficient

    double mtot_x;     // Total x-moment
    double mtot_y;     // Total y-moment
    double mtot_z;     // Total z-moment

    double Cl;         // Total rolling  moment coefficient
    double Cm;         // Total pitching moment coefficient
    double Cn;         // Total yawing   moment coefficient
} resultstruct;

// Lattice quality information
typedef struct infostruct
{
    long int num_wng;   // Number of wings
    long int num_seg;   // Number of segments
    long int num_ctr;   // Number of controls
    long int num_str;   // Number of panel strips
    long int num_pan;   // Number of panels

    double area_min;    // Minimum panel area
    double area_max;    // Maximum panel area
    double area_avg;    // Average panel area
    double aspect_min;  // Minimum panel aspect ratio
    double aspect_max;  // Maximum panel aspect ratio
    double aspect_avg;  // Average panel aspect ratio
} infostruct;

// Main function for lattice generation
void vlm_lattice( latticestruct* lattice,
                  infostruct* info,
                  statestruct* state,
                  double* pts,
                  long int* sym,
                  long int* pan);

// Main function for downwash factor calculation
void vlm_downwash( latticestruct* lattice,
                   double* dw,
                   long int num_pan );

// Main function for calculation of rhs from tangential flow condition
void vlm_boundary( latticestruct* lattice,
                   statestruct* state,
                   refstruct* refs,
                   double* rhs,
                   long int num_pan );

// Main function for calculation of results
void vlm_results( latticestruct* lattice,
                  statestruct* state,
                  refstruct* refs,
                  resultstruct* results,
                  long int num_pan );

// Main function for calculation of aerodynamic coefficients
void vlm_coeffs( latticestruct* lattice,
                 statestruct* state,
                 refstruct *refs,
                 resultstruct* results );

// Miscellaneous functions
double* normalise_vector(double* v);
double deg2rad(double deg);
double* get_freestream_direction(double* freestream_dir, double alpha, double beta);

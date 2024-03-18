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
RIGHT-HAND SIDE (RHS) FOR VORTEX-LATTICE METHOD (NEUMANN CONDITION)

Authors:
* Alessandro Gastaldi
* Aaron Dettmann
*******************************************************************************/

#include "c_vlm.h"

void vlm_boundary(latticestruct* lattice, statestruct* state,
                  refstruct* refs, double* rhs, long int num_pan)
{
    // Using Drela's sign conventions (Drela, 2014)

    double alpha = deg2rad(state->alpha);
    double beta = deg2rad(state->beta);

    // Orientation of the far-field flow
    double freestream_dir[3];
    get_freestream_direction(freestream_dir, alpha, beta);

    // Velocity of aircraft opposite to freestream direction
    double vel_wind_x = -state->airspeed*freestream_dir[0];
    double vel_wind_y = -state->airspeed*freestream_dir[1];
    double vel_wind_z = -state->airspeed*freestream_dir[2];

    // Loop over all panels
    for (int i = 0 ; i < num_pan ; ++i)
    {
        // index_i = 0, 3, 6, 9, ...
        int index_i = i*NUM_COORD;

        // Lever arm w.r.t. center of gravity (vector pointing from CG to collocation point)
        double arm_x = lattice->C[index_i    ] - refs->CG[0];
        double arm_y = lattice->C[index_i + 1] - refs->CG[1];
        double arm_z = lattice->C[index_i + 2] - refs->CG[2];

        // Velocity component due to aircraft rotation (compare Drela)
        // Cross product: "Omega x r_ic", where Omega = (P, Q, R)^T and r_ic = (arm_x, arm_y, arm_z)^T
        double vel_rot_x = state->Q*arm_z - state->R*arm_y;
        double vel_rot_y = state->R*arm_x - state->P*arm_z;
        double vel_rot_z = state->P*arm_y - state->Q*arm_x;

        // Local body velocity as a result of freestream flow and rotation
        double vel_x = vel_wind_x + vel_rot_x;
        double vel_y = vel_wind_y + vel_rot_y;
        double vel_z = vel_wind_z + vel_rot_z;

        rhs[i] = vel_x*lattice->N[index_i    ]
               + vel_y*lattice->N[index_i + 1]
               + vel_z*lattice->N[index_i + 2];
    }
}

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
COMPUTE THE AERODYNAMIC INFLUENCE COEFFICIENT MATRIX (DOWNWASH FACTORS)

Authors:
* Alessandro Gastaldi
* Aaron Dettmann
*******************************************************************************/

#include "c_vlm.h"

void vlm_downwash(latticestruct* lattice, double* dw, long int num_pan)
{
    // Outer loop over all panels
    for (int i = 0 ; i < num_pan ; ++i)
    {
        // Index of panel i at which velocity is induced ("index of receiving panel")
        int index_i = i*NUM_COORD;

        // Inner loop over all panels
    	for (int j = 0 ; j < num_pan ; ++j)
        {
            // Index of downwash factor A_ij
            // 0, 1, 2, ..., N, N+1, N+2, ..., 2N, 2N+1, 2N+2, ... (N: number of panels)
            int index_ij = i*num_pan + j;

            // Induced velocity ("downwash")
            double dw_x = 0.0;
            double dw_y = 0.0;
            double dw_z = 0.0;

            // Iterate over the three legs of horseshoe j (k = 0, 1, 2)
            // Each horseshoe vortex leg must be considered
            for (int k = 0 ; k < NUM_V - 1 ; ++k)
            {
                // Indices of current vortex filament endpoints
                // * Loop 1 (k=0): V1 and V2 --> 'left trailing leg'
                // * Loop 2 (k=1): V2 and V3 --> 'bound leg'
                // * Loop 3 (k=2): V3 and V4 --> 'right trailing leg'
                int index_leg_p1 = j*NUM_V*NUM_COORD + k*NUM_COORD;
                int index_leg_p2 = j*NUM_V*NUM_COORD + (k + 1)*NUM_COORD;

                // ----- CALCULATE VECTORS R0, R1, R2 -----
                double r1_x = lattice->C[index_i    ] - lattice->V[index_leg_p1    ];
                double r1_y = lattice->C[index_i + 1] - lattice->V[index_leg_p1 + 1];
                double r1_z = lattice->C[index_i + 2] - lattice->V[index_leg_p1 + 2];
                double r1_mag = sqrt(r1_x*r1_x + r1_y*r1_y + r1_z*r1_z);

                double r2_x = lattice->C[index_i    ] - lattice->V[index_leg_p2    ];
                double r2_y = lattice->C[index_i + 1] - lattice->V[index_leg_p2 + 1];
                double r2_z = lattice->C[index_i + 2] - lattice->V[index_leg_p2 + 2];
                double r2_mag = sqrt(r2_x*r2_x + r2_y*r2_y + r2_z*r2_z);

                // Dotproduct r1*r2
                double r1r2_dot = r1_x*r2_x + r1_y*r2_y + r1_z*r2_z;

                // ----- CROSS-PRODUCT R1 x R2 -----
                double rcross_x = r1_y*r2_z - r1_z*r2_y;
                double rcross_y = r1_z*r2_x - r1_x*r2_z;
                double rcross_z = r1_x*r2_y - r1_y*r2_x;

                // ----- INDUCED VELOCITY FOR UNIT VORTEX STRENGTH -----
                double v_c = (1/(4.0*PI))*(1/(r1r2_dot + r1_mag*r2_mag*(1 + lattice->EPSILON)))*(1/r1_mag + 1/r2_mag);
                double v_x = v_c*rcross_x;
                double v_y = v_c*rcross_y;
                double v_z = v_c*rcross_z;

                // ----- DOWNWASH FACTOR -----
                // Collect contribution of current vortex segment
                dw_x += v_x;
                dw_y += v_y;
                dw_z += v_z;
            }

            // Reminder: normal vector must be normalised
            dw[index_ij] = dw_x*lattice->N[index_i    ]
                         + dw_y*lattice->N[index_i + 1]
                         + dw_z*lattice->N[index_i + 2];
        }
    }
}

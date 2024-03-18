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
CALCULATION OF RESULTS FROM VORTEX-LATTICE METHOD

Authors:
* Alessandro Gastaldi
* Aaron Dettmann
*******************************************************************************/

/*
    NOTES:
    * for performance, calculates inwash in same iteration as downwash
    * for performance, calculates coefficient of pressure also
    * panel count now provided as argument

    If i-th value used only at i-th iteration of loop, arrays are not efficient!
    Allocating instead doubles that change at every iteration is enough.
*/

#include "c_vlm.h"

// ===== INWASH =====
void vlm_results(latticestruct* lattice, statestruct* state,
                 refstruct* refs, resultstruct* results, long int num_pan)
{
    double alpha = deg2rad(state->alpha);
    double beta = deg2rad(state->beta);

    double freestream_dir[3];
    get_freestream_direction(freestream_dir, alpha, beta);

    // Velocity of aircraft opposite to freestream direction
    double vel_wind_x = -state->airspeed*freestream_dir[0];
    double vel_wind_y = -state->airspeed*freestream_dir[1];
    double vel_wind_z = -state->airspeed*freestream_dir[2];

    // Make sure global forces/moments initialised with '0', the loads are added up below
    results->ftot_x = 0.;
    results->ftot_y = 0.;
    results->ftot_z = 0.;

    results->mtot_x = 0.;
    results->mtot_y = 0.;
    results->mtot_z = 0.;

    for (int i = 0 ; i < num_pan ; ++i)
    {
        // Index of receiving panel i
        int index_i = i*NUM_COORD;

        // Indices of inner and outer ends of bound vortex filament
        int index_VA = i*NUM_V*NUM_COORD + 1*NUM_COORD;
        int index_VB = i*NUM_V*NUM_COORD + 2*NUM_COORD;

        // Midpoint of bound vortex filament
        double P_x = lattice->BoundLegMidpoint[index_i    ];
        double P_y = lattice->BoundLegMidpoint[index_i + 1];
        double P_z = lattice->BoundLegMidpoint[index_i + 2];

        results->iw_x[i] = 0.0;
        results->iw_y[i] = 0.0;
        results->iw_z[i] = 0.0;
        results->iw_mag[i] = 0.0;

        for (int j = 0 ; j < num_pan ; ++j)
        {
            double dw_x = 0.0;
            double dw_y = 0.0;
            double dw_z = 0.0;

            for (int k = 0 ; k < NUM_V - 1 ; ++k)
            {
                // Indices of current vortex filament endpoints
                int index_V1 = j*NUM_V*NUM_COORD + k*NUM_COORD;
                int index_V2 = j*NUM_V*NUM_COORD + (k + 1)*NUM_COORD;

                // CALCULATE VECTORS R0, R1, R2 ******************************/
                double r1_x = P_x - lattice->V[index_V1    ];
                double r1_y = P_y - lattice->V[index_V1 + 1];
                double r1_z = P_z - lattice->V[index_V1 + 2];
                double r1_mag = sqrt(r1_x*r1_x + r1_y*r1_y + r1_z*r1_z);

                double r2_x = P_x - lattice->V[index_V2    ];
                double r2_y = P_y - lattice->V[index_V2 + 1];
                double r2_z = P_z - lattice->V[index_V2 + 2];
                double r2_mag = sqrt(r2_x*r2_x + r2_y*r2_y + r2_z*r2_z);

                // Dotproduct r1*r2
                double r1r2_dot = r1_x*r2_x + r1_y*r2_y + r1_z*r2_z;

                /* CROSS-PRODUCT R1 x R2 *************************************/
                double rcross_x = r1_y*r2_z - r1_z*r2_y;
                double rcross_y = r1_z*r2_x - r1_x*r2_z;
                double rcross_z = r1_x*r2_y - r1_y*r2_x;

                /* INDUCED VELOCITY FOR UNIT VORTEX STRENGTH *****************/
                double v_c = (1/(4.0*PI))*(1/(r1r2_dot + r1_mag*r2_mag*(1 + lattice->EPSILON)))*(1/r1_mag + 1/r2_mag);
                double v_x = v_c*rcross_x;
                double v_y = v_c*rcross_y;
                double v_z = v_c*rcross_z;

                // ===== DOWNWASH FACTOR =====
                // Collect downwash contribution of vortex segments
                dw_x += v_x;
                dw_y += v_y;
                dw_z += v_z;
            }

            results->iw_x[i] += dw_x*results->gamma[j];
            results->iw_y[i] += dw_y*results->gamma[j];
            results->iw_z[i] += dw_z*results->gamma[j];

            // Collect inwash contribution of emitting panels
            results->iw_mag[i] = sqrt( results->iw_x[i]*results->iw_x[i]
                                     + results->iw_y[i]*results->iw_y[i]
                                     + results->iw_z[i]*results->iw_z[i] );
        }

        // Vector of bound horseshoe vortex leg L = (L_x, L_y, L_z)^T
        // L is the vector from "a" to "b" (compare Drela, 2014)
        double L_x = lattice->V[index_VB    ] - lattice->V[index_VA    ];
        double L_y = lattice->V[index_VB + 1] - lattice->V[index_VA + 1];
        double L_z = lattice->V[index_VB + 2] - lattice->V[index_VA + 2];

        /* FLOW VELOCITY AT RECEIVING PANEL i ********************************/
        double arm_x = P_x - refs->CG[0];
        double arm_y = P_y - refs->CG[1];
        double arm_z = P_z - refs->CG[2];

        // Velocity component due to aircraft rotation (compare Drela)
        double vel_rot_x = state->Q*arm_z - state->R*arm_y;
        double vel_rot_y = state->R*arm_x - state->P*arm_z;
        double vel_rot_z = state->P*arm_y - state->Q*arm_x;

        // See Drela (6.42)
        double vel_x = results->iw_x[i] - (vel_wind_x + vel_rot_x);
        double vel_y = results->iw_y[i] - (vel_wind_y + vel_rot_y);
        double vel_z = results->iw_z[i] - (vel_wind_z + vel_rot_z);

        // Absolute aerodynamic forces per panel in [Newton] (Kutta-Joukowski theorem)
        double f_x = state->rho*(vel_y*L_z - vel_z*L_y)*results->gamma[i];
        double f_y = state->rho*(vel_z*L_x - vel_x*L_z)*results->gamma[i];
        double f_z = state->rho*(vel_x*L_y - vel_y*L_x)*results->gamma[i];

        // ===== AERODYNAMIC FORCES =====
        // Save panel forces
        results->fpan_x[i] = f_x;
        results->fpan_y[i] = f_y;
        results->fpan_z[i] = f_z;
        results->fpan_mag[i] = sqrt(f_x*f_x + f_y*f_y + f_z*f_z);

        // Collect contribution of panel i to total forces
        results->ftot_x += f_x;
        results->ftot_y += f_y;
        results->ftot_z += f_z;

        // ===== AERODYNAMIC MOMENTS =====
        // Moments are computed with respect to a given reference point (can be different to CG)
        arm_x = P_x - refs->RP[0];
        arm_y = P_y - refs->RP[1];
        arm_z = P_z - refs->RP[2];

        // Aerodynamic moments on panel i
        double m_x = arm_y*f_z - arm_z*f_y;
        double m_y = arm_z*f_x - arm_x*f_z;
        double m_z = arm_x*f_y - arm_y*f_x;

        // Collect contribution of panel i to total moments
        results->mtot_x += m_x;
        results->mtot_y += m_y;
        results->mtot_z += m_z;

        // Normal force acting on panel i
        // Reminder: normal vector must be normalised
        double fpan_n = f_x*lattice->N[index_i    ]
                      + f_y*lattice->N[index_i + 1]
                      + f_z*lattice->N[index_i + 2];

        // Dynamic pressure
        double q = 0.5*state->rho*(state->airspeed*state->airspeed);

        results->cp[i] = fpan_n/(q*lattice->A[i]);
    }

    // Once global loads are computed, get corresponding coefficients
    vlm_coeffs(lattice, state, refs, results);
}

// ===== COEFFICIENTS =====
void vlm_coeffs(latticestruct* lattice, statestruct* state,
                refstruct* refs, resultstruct* results)
{
    // Body- to wind-axes transformation matrix
    double B2W[3][3];

    double alpha = deg2rad(state->alpha);
    double beta = deg2rad(state->beta);

    double sin_a = sin(alpha);
    double cos_a = cos(alpha);
    double sin_b = sin(beta);
    double cos_b = cos(beta);

    // Dynamic pressure
    double q = 0.5*state->rho*(state->airspeed*state->airspeed);

    // Avoid division by 0
    if (q == 0)
    {
        q = 1;
        std::cerr << "WARNING: Dynamic pressure was 0 (set to 1 instead)!\n";
    }

    // Force coefficients
    results->Cx = results->ftot_x/(q*refs->area);
    results->Cy = results->ftot_y/(q*refs->area);
    results->Cz = results->ftot_z/(q*refs->area);

    // Rotation matrix (see Drela)
    // Transforms loads from global coordinate system (geometry axes) to wind axes
    B2W[0][0] = cos_b*cos_a;
    B2W[0][1] = -sin_b;
    B2W[0][2] = cos_b*sin_a;

    B2W[1][0] = sin_b*cos_a;
    B2W[1][1] = cos_b;
    B2W[1][2] = sin_b*sin_a;

    B2W[2][0] = -sin_a;
    B2W[2][1] = 0.0;
    B2W[2][2] = cos_a;

    results->ftot_D = B2W[0][0]*results->ftot_x + B2W[0][1]*results->ftot_y + B2W[0][2]*results->ftot_z;
    results->ftot_C = B2W[1][0]*results->ftot_x + B2W[1][1]*results->ftot_y + B2W[1][2]*results->ftot_z;
    results->ftot_L = B2W[2][0]*results->ftot_x + B2W[2][1]*results->ftot_y + B2W[2][2]*results->ftot_z;

    // Lift, drag and side force coefficients
    results->CL = results->ftot_L/(q*refs->area);
    results->CD = results->ftot_D/(q*refs->area);
    results->CC = results->ftot_C/(q*refs->area);

    // Roll, pitch and yaw moments coefficients
    // Note: The body fixed moments are being used
    results->Cl = results->mtot_x/(q*refs->area*refs->span);
    results->Cm = results->mtot_y/(q*refs->area*refs->chord);
    results->Cn = results->mtot_z/(q*refs->area*refs->span);
}

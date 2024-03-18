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
GEOMETRY DISCRETISATION ROUTINE

Authors:
* Alessandro Gastaldi
* Aaron Dettmann
*******************************************************************************/

#include "c_vlm.h"

void vlm_lattice(latticestruct* lattice, infostruct* info, statestruct* state,
                 double* pts, long int* sym, long int* pan)
{
    // NOTE:
    // * alpha{1,2,3,4}, beta{1,2,3,4} and gamma{1,2,3,4} are geometric properties
    // * Do NOT confuse with angle of attack, side slip angle or vortex strength!
    double alpha1;
    double alpha2;
    double alpha3;
    double alpha4;

    double beta1;
    double beta2;
    double beta3;
    double beta4;

    double gamma1;
    double gamma2;
    double gamma3;
    double gamma4;

    // Get freestream direction
    double alpha = deg2rad(state->alpha);
    double beta = deg2rad(state->beta);
    double freestream_dir[3];
    get_freestream_direction(freestream_dir, alpha, beta);

    // Large number (=infinity) used to place the end points of the horseshoe trailing legs
    const int infty = 10000;

    // Horseshoe vortex trailing leg direction
    double hv_trail_leg_dir[3];
    hv_trail_leg_dir[0] = freestream_dir[0]*infty;
    hv_trail_leg_dir[1] = freestream_dir[1]*infty;
    hv_trail_leg_dir[2] = freestream_dir[2]*infty;

    // Initialise lattice properties
    info->area_min = +DBL_MAX;
    info->area_max = -DBL_MAX;
    info->area_avg = 0.0;
    info->aspect_min = +DBL_MAX;
    info->aspect_max = -DBL_MAX;
    info->aspect_avg = 0.0;

    // Index for first panel of each subarea in lattice
    int start_pan = 0;

    // Each subarea is divided into panels separately
    for (int index_seg = 0 ; index_seg < info->num_seg ; ++index_seg)
    {
        // Number of panels per subarea
        int pan_s = pan[index_seg*2    ];  // Number of spanwise panels
        int pan_c = pan[index_seg*2 + 1];  // Number of chordwise panels
        int pan_seg = pan_s*pan_c;         // Total number of panels per subarea

        // Panel size in fraction of subarea span and chord (linear division)
        double ds = 1.0/pan_s;
        double dc = 1.0/pan_c;

        // Indices of x-coordinate of subarea corner points
        int index_A = index_seg*NUM_P*NUM_COORD;
        int index_B = index_seg*NUM_P*NUM_COORD + 1*NUM_COORD;
        int index_C = index_seg*NUM_P*NUM_COORD + 2*NUM_COORD;
        int index_D = index_seg*NUM_P*NUM_COORD + 3*NUM_COORD;

        int num_symmetries = 0;

        // Loop once more if symmetry is 1, 2 or 3
        if (sym[index_seg] > 0)
        {
            num_symmetries = 1;
        }

        for (int symmetry = 0 ; symmetry < 1 + num_symmetries ; ++symmetry)
        {
            if (symmetry == 1 && sym[index_seg] == 2)
            {
                // Re-order vertices (positive normal)
                index_B = index_seg*NUM_P*NUM_COORD;
                index_A = index_seg*NUM_P*NUM_COORD + 1*NUM_COORD;
                index_D = index_seg*NUM_P*NUM_COORD + 2*NUM_COORD;
                index_C = index_seg*NUM_P*NUM_COORD + 3*NUM_COORD;
            }
            else if (symmetry == 1 && sym[index_seg] == 3)
            {
                // Re-order vertices (positive normal)
                index_D = index_seg*NUM_P*NUM_COORD;
                index_C = index_seg*NUM_P*NUM_COORD + 1*NUM_COORD;
                index_B = index_seg*NUM_P*NUM_COORD + 2*NUM_COORD;
                index_A = index_seg*NUM_P*NUM_COORD + 3*NUM_COORD;
            }

            // Transformation coefficients from subarea c, s-coordinates to x-coordinate
            alpha1 = pts[index_A];
            alpha2 = pts[index_B] - pts[index_A];
            alpha3 = pts[index_D] - pts[index_A];
            alpha4 = pts[index_A] - pts[index_B] + pts[index_C] - pts[index_D];

            // Transformation coefficients from subarea c, s-coordinates to y-coordinate
            beta1 = pts[index_A + 1];
            beta2 = pts[index_B + 1] - pts[index_A + 1];
            beta3 = pts[index_D + 1] - pts[index_A + 1];
            beta4 = pts[index_A + 1] - pts[index_B + 1] + pts[index_C + 1] - pts[index_D + 1];

            // Transformation coefficients from subarea c, s-coordinates to z-coordinate
            gamma1 = pts[index_A + 2];
            gamma2 = pts[index_B + 2] - pts[index_A + 2];
            gamma3 = pts[index_D + 2] - pts[index_A + 2];
            gamma4 = pts[index_A + 2] - pts[index_B + 2] + pts[index_C + 2] - pts[index_D + 2];

            if (symmetry == 1 && sym[index_seg] == 1)
            {
                // Invert z-coordinate
                gamma1 = -gamma1;
                gamma2 = -gamma2;
                gamma3 = -gamma3;
                gamma4 = -gamma4;
            }
            else if (symmetry == 1 && sym[index_seg] == 2)
            {
                // Invert y-coordinate
                beta1 = -beta1;
                beta2 = -beta2;
                beta3 = -beta3;
                beta4 = -beta4;
            }
            else if (symmetry == 1 && sym[index_seg] == 3)
            {
                // Invert x-coordinate
                alpha1 = -alpha1;
                alpha2 = -alpha2;
                alpha3 = -alpha3;
                alpha4 = -alpha4;
            }

            for (int i = 0 ; i < pan_c ; ++i)
            {
                for (int j = 0 ; j < pan_s ; ++j)
                {
                    // Index to current panel in subarea
                    int index_pan = j*pan_c + i;

                    // Edges of current panel in subarea s, c-coordinates
                    double s_min = ds*j;
                    double s_max = ds*(j + 1);
                    double c_min = dc*i;
                    double c_max = dc*(i + 1);

                    // ===== COMPUTE PANEL CORNER POINTS =====
                    // Indices to x-coord of panel corner points
                    int index_P = (start_pan + index_pan)*NUM_P*NUM_COORD;
                    int index_Q = (start_pan + index_pan)*NUM_P*NUM_COORD + 1*NUM_COORD;
                    int index_R = (start_pan + index_pan)*NUM_P*NUM_COORD + 2*NUM_COORD;
                    int index_S = (start_pan + index_pan)*NUM_P*NUM_COORD + 3*NUM_COORD;

                    lattice->P[index_P    ] = alpha1 + alpha2*s_min + alpha3*c_min + alpha4*s_min*c_min;
                    lattice->P[index_P + 1] = beta1  + beta2*s_min  + beta3*c_min  + beta4*s_min*c_min;
                    lattice->P[index_P + 2] = gamma1 + gamma2*s_min + gamma3*c_min + gamma4*s_min*c_min;

                    lattice->P[index_Q    ] = alpha1 + alpha2*s_max + alpha3*c_min + alpha4*s_max*c_min;
                    lattice->P[index_Q + 1] = beta1  + beta2*s_max  + beta3*c_min  + beta4*s_max*c_min;
                    lattice->P[index_Q + 2] = gamma1 + gamma2*s_max + gamma3*c_min + gamma4*s_max*c_min;

                    lattice->P[index_R    ] = alpha1 + alpha2*s_max + alpha3*c_max + alpha4*s_max*c_max;
                    lattice->P[index_R + 1] = beta1  + beta2*s_max  + beta3*c_max  + beta4*s_max*c_max;
                    lattice->P[index_R + 2] = gamma1 + gamma2*s_max + gamma3*c_max + gamma4*s_max*c_max;

                    lattice->P[index_S    ] = alpha1 + alpha2*s_min + alpha3*c_max + alpha4*s_min*c_max;
                    lattice->P[index_S + 1] = beta1  + beta2*s_min  + beta3*c_max  + beta4*s_min*c_max;
                    lattice->P[index_S + 2] = gamma1 + gamma2*s_min + gamma3*c_max + gamma4*s_min*c_max;

                    // ===== COMPUTE PANEL VORTEX FILAMENT ENDS =====
                    // Indices to x-coord of panel vortex filament ends
                    int index_V1 = (start_pan + index_pan)*NUM_V*NUM_COORD;
                    int index_V2 = (start_pan + index_pan)*NUM_V*NUM_COORD + 1*NUM_COORD;
                    int index_V3 = (start_pan + index_pan)*NUM_V*NUM_COORD + 2*NUM_COORD;
                    int index_V4 = (start_pan + index_pan)*NUM_V*NUM_COORD + 3*NUM_COORD;

                    // ===== HORSESHOE GEOMETRY =====
                    for (int k = 0; k < 3; ++k) {
                        // First compute V2 and V3, then V1 and V4 (order of assignment)
                        lattice->V[index_V2 + k] = 0.75*lattice->P[index_P + k] + 0.25*lattice->P[index_S + k];
                        lattice->V[index_V3 + k] = 0.75*lattice->P[index_Q + k] + 0.25*lattice->P[index_R + k];
                        lattice->V[index_V1 + k] = lattice->V[index_V2 + k] + hv_trail_leg_dir[k];
                        lattice->V[index_V4 + k] = lattice->V[index_V3 + k] + hv_trail_leg_dir[k];
                    }

                    // ===== COMPUTE PANEL COLLOCATION POINTS =====
                    // Index to x-coord of panel collocation point and normal vector
                    int index_N = (start_pan + index_pan)*NUM_COORD;

                    lattice->C[index_N    ] = 0.125*(lattice->P[index_P    ] + lattice->P[index_Q    ])
                                            + 0.375*(lattice->P[index_R    ] + lattice->P[index_S    ]);

                    lattice->C[index_N + 1] = 0.125*(lattice->P[index_P + 1] + lattice->P[index_Q + 1])
                                            + 0.375*(lattice->P[index_R + 1] + lattice->P[index_S + 1]);

                    lattice->C[index_N + 2] = 0.125*(lattice->P[index_P + 2] + lattice->P[index_Q + 2])
                                            + 0.375*(lattice->P[index_R + 2] + lattice->P[index_S + 2]);

                    // ===== COMPUTE BOUND LEG MIDPOINT =====
                    for (int k = 0; k < 3; ++k) {
                        lattice->BoundLegMidpoint[index_N + k] = 0.5*(lattice->V[index_V2 + k] + lattice->V[index_V3 + k]);
                    }

                    // ===== COMPUTE PANEL NORMAL VECTOR (at collocation point) =====
                    double vec1_x = lattice->C[index_N    ] - 0.50*lattice->P[index_P    ] - 0.50*lattice->P[index_Q    ];
                    double vec1_y = lattice->C[index_N + 1] - 0.50*lattice->P[index_P + 1] - 0.50*lattice->P[index_Q + 1];
                    double vec1_z = lattice->C[index_N + 2] - 0.50*lattice->P[index_P + 2] - 0.50*lattice->P[index_Q + 2];

                    double vec2_x = lattice->C[index_N    ] - 0.75*lattice->P[index_R    ] - 0.25*lattice->P[index_Q    ];
                    double vec2_y = lattice->C[index_N + 1] - 0.75*lattice->P[index_R + 1] - 0.25*lattice->P[index_Q + 1];
                    double vec2_z = lattice->C[index_N + 2] - 0.75*lattice->P[index_R + 2] - 0.25*lattice->P[index_Q + 2];

                    double n_x = vec1_y*vec2_z - vec1_z*vec2_y;
                    double n_y = vec1_z*vec2_x - vec1_x*vec2_z;
                    double n_z = vec1_x*vec2_y - vec1_y*vec2_x;
                    double norm = sqrt(n_x*n_x + n_y*n_y + n_z*n_z);

                    lattice->N[index_N    ] = n_x/norm;
                    lattice->N[index_N + 1] = n_y/norm;
                    lattice->N[index_N + 2] = n_z/norm;

                    // ===== COMPUTE PANEL PROPERTIES =====
                    double span_x = 0.5*( lattice->P[index_Q    ] - lattice->P[index_P    ]
                                        + lattice->P[index_R    ] - lattice->P[index_S    ] );

                    double span_y = 0.5*( lattice->P[index_Q + 1] - lattice->P[index_P + 1]
                                        + lattice->P[index_R + 1] - lattice->P[index_S + 1] );

                    double span_z = 0.5*( lattice->P[index_Q + 2] - lattice->P[index_P + 2]
                                        + lattice->P[index_R + 2] - lattice->P[index_S + 2] );

                    // Panel span through centre
                    double span = sqrt(span_x*span_x + span_y*span_y + span_z*span_z);

                    double chord_x = 0.5*( lattice->P[index_S    ] - lattice->P[index_P   ]
                                         + lattice->P[index_R    ] - lattice->P[index_Q   ] );

                    double chord_y = 0.5*( lattice->P[index_S + 1] - lattice->P[index_P + 1]
                                         + lattice->P[index_R + 1] - lattice->P[index_Q + 1] );

                    double chord_z = 0.5*( lattice->P[index_S + 2] - lattice->P[index_P + 2]
                                         + lattice->P[index_R + 2] - lattice->P[index_Q + 2] );

                    // Panel chord through centre
                    double chord = sqrt(chord_x*chord_x + chord_y*chord_y + chord_z*chord_z);
                    double area = span*chord;
                    double aspect = span/chord;

                    if (area > info->area_max) { info->area_max = area; }
                    if (area < info->area_min) { info->area_min = area; }
                    if (aspect > info->aspect_max) { info->aspect_max = aspect; }
                    if (aspect < info->aspect_min) { info->aspect_min = aspect; }

                    // Update lattice metrics
                    info->area_avg += area;
                    info->aspect_avg += aspect;

                    // Panel area
                    lattice->A[start_pan + index_pan] = area;
                }
            }
            start_pan += pan_seg;
        }
    }

    // Finally compute the average area and the average aspect
    info->area_avg /= info->num_pan;
    info->aspect_avg /= info->num_pan;
}

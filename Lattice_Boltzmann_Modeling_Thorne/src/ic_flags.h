//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// ic_flags.h
//
//  - IC preprocessor flags for lb2d_prime.
//

#ifndef IC_FLAGS_H
#define IC_FLAGS_H

// IC_* flags are for initial conditions.  Used in switch statement in
// init_problem() in latmat.c .  Set the initial_condition parameter
// in params.in .
#define IC_UNIFORM_RHO_A          1
#define IC_UNIFORM_RHO_B          2
#define IC_UNIFORM_RHO_IN         3
#define IC_BUBBLE                 4
#define IC_DIAGONAL               5
#define IC_2X2_CHECKERS           6
#define IC_STATIC                 7
#define IC_RECTANGLE              8
#define IC_DOT                    9
#define IC_WOLF_GLADROW_DIFFUSION 10
#define IC_YIN_YANG               11
#define IC_HYDROSTATIC            12
#define IC_READ_FROM_FILE         99

#endif /* IC_FLAGS_H */

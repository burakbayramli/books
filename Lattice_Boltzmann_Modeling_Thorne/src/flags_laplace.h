//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// flags.h
//
//  - Preprocessor flags for lb2d_prime.
//

// Set VERBOSITY_LEVEL to correspond to how deep into nested loops to
// print debug and related output.  Stuff will be printed down to
// loops nested VERBOSITY_LEVEL-1 deep. For example,
//
//   VERBOSITY_LEVEL 0 ==> Nothing is printed, even outside of loops.
//   VERBOSITY_LEVEL 1 ==> Only stuff outside of loops is printed.
//   VERBOSITY_LEVEL 2 ==> Stuff inside the first level of loops is printed.
//   VERBOSITY_LEVEL 3 ==> Stuff inside the second level of loops is printed.
//
#define VERBOSITY_LEVEL 1

// If SAY_HI is on, some routines will display "hi" and "bye" messages
// to stdout.
#define SAY_HI 0

// NUM_FLUID_COMPONENTS specifies the number of fluid components.
#define NUM_FLUID_COMPONENTS 2

// If NUM_FLUID_COMPONENTS is 2, the second component can be the sigma
// component for solute (or thermal) transport as in Inamuro & Yoshino
// by turning on INAMURO_SIGMA_COMPONENT .
#define INAMURO_SIGMA_COMPONENT 0

// Simulate POROUS_MEDIA via a solid density parameter
// as proposed by Dardis and McCloskey,
// Phys Rev E, 57, 4, 4834-4837, 1998
#define POROUS_MEDIA 0

// When there are two (or more) fluid components, a single velocity is
// sometimes (always?) used to compute the equilibrium distribution
// function.  This single velocity will be called upr, and the
// STORE_U_COMPOSITE flag will toggle its use.
#define STORE_U_COMPOSITE 1 && NUM_FLUID_COMPONENTS==2 && !INAMURO_SIGMA_COMPONENT

// If DO_NOT_STORE_SOLIDS is on, then only the nodes necessary to flow are
// stored.  In this case, extra storage is needed for geometry information
// (e.g. node neighbors).  If the ratio of fluid nodes to solid nodes is
// small (<~.7), this results in lower storage requirements.
#define DO_NOT_STORE_SOLIDS 0

// NON_LOCAL_FORCES toggles any mechanisms for computing and storing
// non-local (interaction) forces.
#define NON_LOCAL_FORCES 1

// BC_XXX flags are for boundary conditions.
// Used to set struct bc_struct::bc_type.
// Should be powers of two so that multiple boundary types can be
// checked easily via bitwise and (&).
// For instance, bc_type & BC_SOLID_NODE & BC_CONSTANT_CONCENTRATION.
#define BC_SOLID_NODE     0x00000001
#define BC_FLUID_NODE     0x00000000
#define BC_FILM_NODE      0x40000000
#define BC_PRESSURE_N_IN  0x00000010
#define BC_PRESSURE_S_IN  0x00000020
#define BC_PRESSURE_E_IN  0x00000040
#define BC_PRESSURE_W_IN  0x00000080
#define BC_PRESSURE_N_OUT 0x00000100
#define BC_PRESSURE_S_OUT 0x00000200
#define BC_PRESSURE_E_OUT 0x00000400
#define BC_PRESSURE_W_OUT 0x00000800
#define BC_VELOCITY_N_IN  0x00001000
#define BC_VELOCITY_S_IN  0x00002000
#define BC_VELOCITY_E_IN  0x00004000
#define BC_VELOCITY_W_IN  0x00008000
#define BC_VELOCITY_N_OUT 0x00010000
#define BC_VELOCITY_S_OUT 0x00020000
#define BC_VELOCITY_E_OUT 0x00040000
#define BC_VELOCITY_W_OUT 0x00080000

// Dumping the density and velocity data to files can be time consuming and
// take up a lot of disk space.  If all that is needed is the BMP files, then
// turn WRITE_MACRO_VAR_DAT_FILES off to save time and space.
#define WRITE_MACRO_VAR_DAT_FILES 1

// Usually the density and velocity are written only for the active nodes
// and in a way designed for post-processing.  Additional files with the
// variables written in a readable grid of all lattice nodes will be
// generated when WRITE_RHO_AND_U_TO_TXT is on.  This is done in an
// inefficient way and is intended only for debugging purposes on tiny
// lattices.  Note that if WRITE_MACRO_VAR_DAT_FILES is off, this flag
// has no effect.
#define WRITE_RHO_AND_U_TO_TXT 0

// WRITE_PDF_DAT_FILES is analogous to WRITE_MACRO_VAR_DAT_FILES.
#define WRITE_PDF_DAT_FILES 0

// WRITE_PDF_TO_TXT is analogous to WRITE_RHO_AND_U_TO_TXT.
#define WRITE_PDF_TO_TXT 0

// Value used to represent an INACTIVE_NODE .  This is used in the list
// of neighbors ( struct node_struct::nn).  It is also used in the
// map from (i,j) space onto n index space in rho2bmp() and u2bmp().
#define INACTIVE_NODE -1

// Negative densities (f_a) generally signify impending doom.  The code
// will die "gracefully" when this happens if PUKE_NEGATIVE_DENSITIES is on.
// Might want to turn this off to boost performance on big, long runs that
// are expected to survive without such instabilities.
#define PUKE_NEGATIVE_DENSITIES 0

// Turn one of these on for coloring of the solids in bmp files.
#define SOLID_COLOR_IS_CHECKERBOARD 0
#define SOLID_COLOR_IS_BLACK        1

#define DELAY    0
#define END_GRAV 2000

// A single white pixel will be placed in at the (0,0) lattice node if
// MARK_ORIGIN_FOR_REFERENCE is turned on.  This is good for assisting with the
// problem of tracking orientation of the results between regimes (e.g. C, BMP,
// Matlab...).
#define MARK_ORIGIN_FOR_REFERENCE 0

#define PERTURBATIONS 0

// If WRITE_CHEN_DAT_FILES is on, the code will output old style chen_*.dat
// files to be processed by the old lb_rho_v*.m matlab scripts.
#define WRITE_CHEN_DAT_FILES 0


// IC_* flags are for initial conditions.  Used in switch statement in
// init_problem() in latmat.c .  Set the initial_condition parameter
// in params.in .
#define IC_UNIFORM_RHO_A          1
#define IC_UNIFORM_RHO_IN         2
#define IC_BUBBLE                 3
#define IC_YIN_YANG               4
#define IC_DIAGONAL               5
#define IC_2X2_CHECKERS           6
#define IC_STATIC                 7
#define IC_RECTANGLE              8
#define IC_DOT                    9
#define IC_WOLF_GLADROW_DIFFUSION 10

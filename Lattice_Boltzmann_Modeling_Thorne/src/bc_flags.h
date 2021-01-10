//##############################################################################
//
// Copyright (C), 2005, Michael Sukop and Danny Thorne
//
// bc_flags.h
//
//  - BC preprocessor flags for lb2d_prime.
//

#ifndef BC_FLAGS_H
#define BC_FLAGS_H

// BC_XXX flags are for boundary conditions.
// Used to set struct bc_struct::bc_type.
// Should be powers of two so that multiple boundary types can be
// checked easily via bitwise and (&).
// For instance, bc_type & BC_SOLID_NODE & BC_SLIP_NODE to specify
// a slip boundary. (Non-slip is the default for solid boundaries.)
#define BC_SOLID_NODE     0x00000001
#define BC_FLUID_NODE     0x00000000
#define BC_SLIP_NODE      0x80000000
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

#endif /* BC_FLAGS_H */

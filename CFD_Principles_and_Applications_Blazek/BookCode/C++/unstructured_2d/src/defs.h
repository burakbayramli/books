/// @file defs.h
///
/// Global definitions, structures and macros.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 1, 2014
//
//=============================================================================
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//
//*****************************************************************************

#ifndef DEFS_H_INCLUDED
#define DEFS_H_INCLUDED

#include <cmath>
#include <cfloat>

  /// Types of fluid flow
  enum class FlowType { External, Internal };

  /// Kind of equations solved
  enum class Equations { Euler, NavierStokes };

  /// Kind of time-stepping
  enum class TimeStepping { Local, Global };

// floating point type (SGLPREC=single precision, otherwise double) ***********

#ifdef SGLPREC
  typedef float  REAL;        /**< floating-point value */
  #define EPSGLO FLT_EPSILON  /**< tolerance for a small number */
  #define ABS    fabsf
  #define SQRT   sqrtf
  #define SIN    sinf
  #define COS    cosf
  #define POW    powf
  #define TAN    tanf
  #define ATAN2  atan2f
  #define LOG10  log10f
#else
  typedef double REAL;        /**< floating-point value */
  #define EPSGLO DBL_EPSILON  /**< tolerance for a small number */
  #define ABS    fabs
  #define SQRT   sqrt
  #define SIN    sin
  #define COS    cos
  #define POW    pow
  #define TAN    tan
  #define ATAN2  atan2
  #define LOG10  log10
#endif

// general structures of geometry and flow variables **************************

  /// Coordinates of a grid node
  typedef struct T_NODE
  {
    REAL x,  /**< x-coordinate */
         y;  /**< y-coordinate */
  } NODE;

  /// Conserved variables. All quantities are in SI units.
  typedef struct T_CONSVARS
  {
    REAL dens,  /**< density */
         xmom,  /**< x-momentum (density*u) */
         ymom,  /**< y-momentum (density*v) */
         ener;  /**< total energy per unit volume (density*E) */
  } CONSVARS;

  /// Primitive variables. All quantities are in SI units.
  typedef struct T_PRIMVARS
  {
    REAL dens,   /**< density */
         uvel,   /**< u-velocity */
         vvel,   /**< v-velocity */
         press;  /**< static pressure */
  } PRIMVARS;

  /// Dependent variables (inviscid flow). All quantities are in SI units.
  typedef struct T_DEPVARS
  {
    REAL press,  /**< static pressure (p) */
         temp,   /**< static temperature (T) */
         csoun,  /**< speed of sound (c) */
         gamma,  /**< ratio of specific heats (gamma) */
         cpgas;  /**< specific heat coeff. at const. pressure (c<sub>p</sub>) */
  } DEPVARS;

  /// Additional dependent variables for viscous flow. All quantities are in SI units.
  typedef struct T_VISCDEPVARS
  {
    REAL mue,   /**< dynamic viscosity coefficient (µ) */
         cond;  /**< heat conductivity coefficient (k) */
  } VISCDEPVARS;

// general constants **********************************************************

#define PI   3.1415926535897932
#define PI2  1.5707963267948966
#define RAD  1.7453292519943296e-2

// function macros ************************************************************

#define MAX(a,b)  (((a) > (b)) ? (a) : (b))
#define MIN(a,b)  (((a) < (b)) ? (a) : (b))
#define SIGN(a,b) (((b) < (0)) ? (-a) : (a))

#endif // DEFS_H_INCLUDED

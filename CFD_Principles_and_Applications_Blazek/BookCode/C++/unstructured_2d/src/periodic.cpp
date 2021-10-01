/// @file periodic.cpp
///
/// Addition of variables at periodic boundaries.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 18, 2014
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

#include "bndConds.h"

/// Adds both parts of a conserved variables at all periodic boundaries.
/// This function is utilized for residuals in TimeDiscr::Solve and
/// in TimeDiscr::Irsmoo.
///
/// @param geometry  geometrical data
/// @param var       variable of CONSVARS type
///
void BndConds::Periodic( const Geometry &geometry, CONSVARS var[] )
{
  int i, j, ib, ibn, ibegn, iendn;

  ibegn = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendn = geometry.ibound[ib].bnodeIndex;
    if (geometry.btype[ib]>=700 && geometry.btype[ib]<800)
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i = geometry.bnode[ibn].node;
        j = geometry.bnode[ibn].dummy;
        var[i].dens += var[j].dens;
        var[i].xmom += var[j].xmom;
        var[i].ymom += var[j].ymom;
        var[i].ener += var[j].ener;
        var[j].dens  = var[i].dens;
        var[j].xmom  = var[i].xmom;
        var[j].ymom  = var[i].ymom;
        var[j].ener  = var[i].ener;
      }
    }
    ibegn = iendn + 1;
  }
}

//*****************************************************************************

/// Adds both parts of a primitive variables at all periodic boundaries.
/// This function is utilized for gradients in SpaceDiscr::Gradients.
///
/// @param geometry  geometrical data
/// @param var       variable of PRIMVARS type
///
void BndConds::Periodic( const Geometry &geometry, PRIMVARS var[] )
{
  int i, j, ib, ibn, ibegn, iendn;

  ibegn = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendn = geometry.ibound[ib].bnodeIndex;
    if (geometry.btype[ib]>=700 && geometry.btype[ib]<800)
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i = geometry.bnode[ibn].node;
        j = geometry.bnode[ibn].dummy;
        var[i].dens  += var[j].dens;
        var[i].uvel  += var[j].uvel;
        var[i].vvel  += var[j].vvel;
        var[i].press += var[j].press;
        var[j].dens   = var[i].dens;
        var[j].uvel   = var[i].uvel;
        var[j].vvel   = var[i].vvel;
        var[j].press  = var[i].press;
      }
    }
    ibegn = iendn + 1;
  }
}

//*****************************************************************************

/// Adds both parts of two REAL variables at all periodic boundaries. This
/// function is utilized for gradients of T in SpaceDiscr::GradientsVisc.
///
/// @param geometry  geometrical data
/// @param var1      first variable
/// @param var2      second variable
///
void BndConds::Periodic( const Geometry &geometry, REAL var1[], REAL var2[] )
{
  int i, j, ib, ibn, ibegn, iendn;

  ibegn = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendn = geometry.ibound[ib].bnodeIndex;
    if (geometry.btype[ib]>=700 && geometry.btype[ib]<800)
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i = geometry.bnode[ibn].node;
        j = geometry.bnode[ibn].dummy;
        var1[i] += var1[j];
        var2[i] += var2[j];
        var1[j]  = var1[i];
        var2[j]  = var2[i];
      }
    }
    ibegn = iendn + 1;
  }
}

//*****************************************************************************

/// Adds both parts of an integer variable at all periodic boundaries. This
/// function is utilized for number of contributions in TimeDiscr::Irsmoo.
///
/// @param geometry  geometrical data
/// @param var       variable of int type
///
void BndConds::Periodic( const Geometry &geometry, int var[] )
{
  int i, j, ib, ibn, ibegn, iendn;

  ibegn = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendn = geometry.ibound[ib].bnodeIndex;
    if (geometry.btype[ib]>=700 && geometry.btype[ib]<800)
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i = geometry.bnode[ibn].node;
        j = geometry.bnode[ibn].dummy;
        var[i] += var[j];
        var[j]  = var[i];
      }
    }
    ibegn = iendn + 1;
  }
}
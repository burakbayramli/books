/// @file getNumberBoundNodes.cpp
///
/// Number of nodes at given boundary.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 27, 2014
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

#include "geometry.h"

/// Returns maximum number of boundary nodes for given range of boundary types.
///
/// @param btypeFrom  starting value of the boundary type(s)
/// @param btypeTo    ending value of the boundary type(s)
/// @return  number of nodes
///
int Geometry::GetNumberBoundNodes( int btypeFrom, int btypeTo ) const
{
  int ib, ibegn, iendn, nBnodesBtype=0;

  ibegn = 0;
  for (ib=0; ib<nSegs; ib++)
  {
    iendn = ibound[ib].bnodeIndex;
    if (btype[ib]>=btypeFrom && btype[ib]<=btypeTo)
      nBnodesBtype = MAX(nBnodesBtype,(iendn-ibegn+1));
    ibegn = iendn + 1;
  }

  return nBnodesBtype;
}
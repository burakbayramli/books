/// @file computeMetrics.cpp
///
/// Computation of the complete grid metrics.
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

/// Computes face vectors and cell volumes; checks their values.
/// Sets coordinates of dummy nodes equal to those of boundary nodes.
/// Corrects face vectors at symmetry boundaries.
/// Computes projections of control volumes on coordinate axes.
///
/// @exception std::runtime_error  failed logic check
/// @exception std::bad_alloc      failed memory allocation
///
void Geometry::ComputeMetrics()
{
  // allocate all memory

  sij   = new NODE[nEdges];
  vol   = new REAL[nNodes];
  sbf   = new NODE[nBfaces];
  sproj = new NODE[nNodes];

  // face vectors and volumes

  FaceVectorsVolumes();

  // dispose of temporary edge list

  DeleteTmpElist();

  // face vectors and volumes along boundaries; dummy nodes

  FaceVectorsVolumesBound();

  // check grid metrics

  CheckMetrics();

  // face vectors at symmetry boundaries

  FaceVectorsSymm();

  // volume projections

  VolumeProjections();
}
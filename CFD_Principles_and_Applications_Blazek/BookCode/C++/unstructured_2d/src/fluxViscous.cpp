/// @file fluxViscous.cpp
///
/// Computation of the viscous fluxes.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 19, 2014
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

#include "spaceDiscr.h"

/// Computes viscous fluxes for the Navier-Stokes equations and adds
/// them to the dissipation variable (diss). Gradients at the faces
/// of the control volumes are obtained by a modified averaging of
/// node-based gradients (see Section 5.4.2).
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param beta        dissipation-blending coefficient
///
void SpaceDiscr::FluxViscous( const Geometry &geometry, const FluidProps &fluidProps, REAL beta )
{
  int  i, j, ie;
  REAL two3=2.0/3.0, ui, uj, vi, vj, uav, vav, mav, kav;
  REAL txn, tyn, ds2, rds, duxa, duya, dvxa, dvya, dtxa, dtya;
  REAL duds, dvds, dtds, dudt, dvdt, dtdt;
  REAL duxf, duyf, dvxf, dvyf, dtxf, dtyf;
  REAL tauxx, tauxy, tauyy, phix, phiy;
  REAL fv[3];

  CONSVARS       *cv     = fluidProps.cv;
  DEPVARS        *dv     = fluidProps.dv;
  VISCDEPVARS    *dvLam  = fluidProps.dvLam;
  NODE           *sij    = geometry.sij;
  NODE           *coords = geometry.coords;
  Geometry::EDGE *edge   = geometry.edge;

  // interior edges ***********************************************************

  for (ie=0; ie<geometry.nedInt; ie++)
  {
    i = edge[ie].i;
    j = edge[ie].j;

    // average of flow variables

    ui  = cv[i].xmom/cv[i].dens;
    uj  = cv[j].xmom/cv[j].dens;
    vi  = cv[i].ymom/cv[i].dens;
    vj  = cv[j].ymom/cv[j].dens;
    uav = 0.5*(ui+uj);
    vav = 0.5*(vi+vj);
    mav = 0.5*(dvLam[i].mue +dvLam[j].mue );
    kav = 0.5*(dvLam[i].cond+dvLam[j].cond);

    // tangential vector (normalized)

    txn = coords[j].x - coords[i].x;
    tyn = coords[j].y - coords[i].y;
    ds2 = txn*txn + tyn*tyn;
    rds = 1.0/SQRT(ds2);
    txn = txn*rds;
    tyn = tyn*rds;

    // average of gradients

    duxa = 0.5*(gradx[i].uvel+gradx[j].uvel);
    duya = 0.5*(grady[i].uvel+grady[j].uvel);
    dvxa = 0.5*(gradx[i].vvel+gradx[j].vvel);
    dvya = 0.5*(grady[i].vvel+grady[j].vvel);
    dtxa = 0.5*(gradTx[i]    +gradTx[j]    );
    dtya = 0.5*(gradTy[i]    +gradTy[j]    );

    // divided difference

    duds = rds*(uj-ui);
    dvds = rds*(vj-vi);
    dtds = rds*(dv[j].temp-dv[i].temp);

    // tangential component - divided difference

    dudt = duxa*txn + duya*tyn - duds;
    dvdt = dvxa*txn + dvya*tyn - dvds;
    dtdt = dtxa*txn + dtya*tyn - dtds;

    // face gradients (Eq. (5.73))

    duxf = duxa - dudt*txn;
    duyf = duya - dudt*tyn;
    dvxf = dvxa - dvdt*txn;
    dvyf = dvya - dvdt*tyn;
    dtxf = dtxa - dtdt*txn;
    dtyf = dtya - dtdt*tyn;

    // viscous fluxes

    tauxx = two3*mav*(2.0*duxf-dvyf);
    tauyy = two3*mav*(2.0*dvyf-duxf);
    tauxy =      mav*(    duyf+dvxf);
    phix  = uav*tauxx + vav*tauxy + kav*dtxf;
    phiy  = uav*tauxy + vav*tauyy + kav*dtyf;

    fv[0] = sij[ie].x*tauxx + sij[ie].y*tauxy;
    fv[1] = sij[ie].x*tauxy + sij[ie].y*tauyy;
    fv[2] = sij[ie].x*phix  + sij[ie].y*phiy;

    // edge contributions to dissipation

    diss[i].xmom += fv[0]*beta;
    diss[i].ymom += fv[1]*beta;
    diss[i].ener += fv[2]*beta;

    diss[j].xmom -= fv[0]*beta;
    diss[j].ymom -= fv[1]*beta;
    diss[j].ener -= fv[2]*beta;
  }

  // edges to dummy nodes *****************************************************

  for (ie=geometry.nedInt; ie<geometry.nEdges; ie++)
  {
    i = edge[ie].i;
    j = edge[ie].j;

    // average of variables

    uav = 0.5*(cv[i].xmom/cv[i].dens+cv[j].xmom/cv[j].dens);
    vav = 0.5*(cv[i].ymom/cv[i].dens+cv[j].ymom/cv[j].dens);
    mav = 0.5*(dvLam[i].mue +dvLam[j].mue );
    kav = 0.5*(dvLam[i].cond+dvLam[j].cond);

    // viscous fluxes

    tauxx = two3*mav*(2.0*gradx[i].uvel-grady[i].vvel);
    tauyy = two3*mav*(2.0*grady[i].vvel-gradx[i].uvel);
    tauxy =      mav*(    grady[i].uvel+gradx[i].vvel);
    phix  = uav*tauxx + vav*tauxy + kav*gradTx[i];
    phiy  = uav*tauxy + vav*tauyy + kav*gradTy[i];

    fv[0] = sij[ie].x*tauxx + sij[ie].y*tauxy;
    fv[1] = sij[ie].x*tauxy + sij[ie].y*tauyy;
    fv[2] = sij[ie].x*phix  + sij[ie].y*phiy;

    diss[i].xmom += fv[0]*beta;
    diss[i].ymom += fv[1]*beta;
    diss[i].ener += fv[2]*beta;
  }
}
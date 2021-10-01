/// @file outputSurfaces.cpp
///
/// Output of flow variables at the surfaces in Vis2D format.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 5, 2014
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

#include <iomanip>
#include <sstream>
#include <stdexcept>
#include "output.h"

using namespace std;

/// Writes out selected values at walls and symmetry boundaries in Vis2D format.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param bndConds    boundary conditions
/// @param spaceDiscr  spatial discretization
/// @param iter        current iteration
/// @exception         std::runtime_error  file cannot be opened
///
void Output::Surfaces( const Geometry &geometry, const FluidProps &fluidProps,
                       const BndConds &bndConds, const SpaceDiscr &spaceDiscr,
                       int iter ) const
{
  int  itype, ibegf, iendf, ibegn, iendn, ibf1, ibf2, nquant, nsurfs;
  int  i, ib, ibf, ibn, m;
  NODE du, dv;
  REAL rrho, u, v, e, press, temp, c, ptot, ttot, mach, machis,
       ptloss, pratio, ptotinf, gam1, ggm1;
  REAL cf, cp, visc, sx, sy, ds, sxn, syn, grdnx, grdny, grdnn,
       dvdnx, dvdny, dvdna, sgn;

  ptotinf = 0.;
  if (external)
  {
    gam1    = fluidProps.gamma - 1.0;
    ggm1    = fluidProps.gamma/gam1;
    ptotinf = bndConds.pinf*POW((1.0+0.5*gam1*bndConds.machinf*bndConds.machinf),ggm1);
  }

  // open file

  stringstream str;
  str << right << setw(5) << setfill('0') << iter;
  string fname = fnameSurf + str.str() + ".v2d";

  ofstream stream( fname,ios::binary | ios::trunc,_SH_DENYRW );
  if (stream.fail()) throw runtime_error( "could not open plot file (surfaces) for writing." );

  // header

  nquant = 0;
  for (m=0; m<MXQUANT; m++) if (varOn[m]) nquant++;

  nsurfs = 0;  // no. of surfaces to output
  for (ib=0; ib<geometry.nSegs; ib++)
  {
    itype = geometry.btype[ib];
    if ((itype>=300 && itype<600) || (itype>=800 && itype<900)) nsurfs++;
  }

  stream << title << endl
         << "1" << endl
         << "Boundaries" << endl
         << nsurfs << " " << nquant+2 << endl
         << "x [m]" << endl
         << "y [m]" << endl;

  // names of variables

  for (m=0; m<MXQUANT; m++)
  {
    if (varOn[m]) stream << varName[m] << endl;
  }

  //  compute quantities and write them out

  stream << scientific << setprecision(8);

  ibegf = 0;
  ibegn = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendf = geometry.ibound[ib].bfaceIndex;
    iendn = geometry.ibound[ib].bnodeIndex;
    itype = geometry.btype[ib];

    if ((itype>=300 && itype<600) || (itype>=800 && itype<900))
    {
      stream << iendn-ibegn+1 << " 0" << endl
             << "0 0 0" << endl
             << geometry.bname[ib] << endl;

      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i = geometry.bnode[ibn].node;

        stream << geometry.coords[i].x << " "
           << geometry.coords[i].y;

        rrho  = 1.0/fluidProps.cv[i].dens;
        u     = fluidProps.cv[i].xmom*rrho;
        v     = fluidProps.cv[i].ymom*rrho;
        e     = fluidProps.cv[i].ener*rrho;
        press = fluidProps.dv[i].press;
        temp  = fluidProps.dv[i].temp;
        c     = fluidProps.dv[i].csoun;
        gam1  = fluidProps.dv[i].gamma - 1.0;
        ggm1  = fluidProps.dv[i].gamma/gam1;
        mach  = SQRT(u*u+v*v)/c;
        ttot  = (e+press*rrho)/fluidProps.dv[i].cpgas;
        ptot  = press*POW(ttot/temp,ggm1);
        if (external)
        {
          ptloss = 1.0 - ptot/ptotinf;
          pratio = ptotinf/press;
          cp     = 2.0*(bndConds.pinf-press)/(bndConds.rhoinf*bndConds.qinf*bndConds.qinf);
        }
        else
        {
          ptloss = 1.0 - ptot/bndConds.ptinl;
          pratio = bndConds.ptinl/press;
          cp     = 2.0*((bndConds.p12rat*bndConds.pout)-press)/
                        (fluidProps.refRho*fluidProps.refVel*fluidProps.refVel);
        }
        machis = (POW(pratio,1.0/ggm1)-1.0)*2.0/gam1;
        machis = MAX(machis, 0.0);
        machis = SQRT(machis);

        if (fluidProps.equsType == Equations::NavierStokes)
        {
          ibf1 = -1;
          ibf2 = -1;
          for (ibf=ibegf; ibf<=iendf; ibf++)
          {
            if (geometry.bface[ibf].node1 == i)
            {
              if (ibf1 < 0)
                ibf1 = ibf;
              else
                ibf2 = ibf;
            }
            if (geometry.bface[ibf].node2 == i)
            {
              if (ibf1 < 0)
                ibf1 = ibf;
              else
                ibf2 = ibf;
            }
          }
          if (ibf2 < 0) ibf2 = ibf1;
          visc  = fluidProps.dvLam[i].mue;
          sx    = -0.5*(geometry.sbf[ibf1].x+geometry.sbf[ibf2].x);   // to point inside
          sy    = -0.5*(geometry.sbf[ibf1].y+geometry.sbf[ibf2].y);
          ds    = SQRT(sx*sx+sy*sy);
          sxn   = sx/ds;
          syn   = sy/ds;
          spaceDiscr.GetGradU( i,du );
          spaceDiscr.GetGradV( i,dv );
          grdnx = du.x*sxn + du.y*syn;
          grdny = dv.x*sxn + dv.y*syn;
          grdnn = grdnx*sxn + grdny*syn;
          dvdnx = grdnx - grdnn*sxn;
          dvdny = grdny - grdnn*syn;
          if (grdnx > grdny)       // to get somehow the main flow
            sgn = SIGN(1.0,grdnx);
          else
            sgn = SIGN(1.0,grdny);
          dvdna = SQRT(dvdnx*dvdnx+dvdny*dvdny);
          if (external)
            cf = (2.0*sgn*visc*dvdna)/(bndConds.rhoinf*bndConds.qinf*bndConds.qinf);
          else
            cf = (2.0*sgn*visc*dvdna)/(fluidProps.refRho*fluidProps.refVel*fluidProps.refVel);
        }
        else
        {
          visc = 0.0;
          cf   = 0.0;
        }

        for (m=0; m<MXQUANT; m++)
        {
          if (varOn[m])
          {
            switch (m)
            {
              case 0:   // density
                stream << " " << fluidProps.cv[i].dens;
                break;
              case 1:   // u-velocity
                stream << " " << u;
                break;
              case 2:   // v-velocity
                stream << " " << v;
                break;
              case 3:   // static pressure
                stream << " " << press;
                break;
              case 4:   // total pressure
                stream << " " << ptot;
                break;
              case 5:   // static temperature
                stream << " " << temp;
                break;
              case 6:   // total temperature
                stream << " " << ttot;
                break;
              case 7:   // local Mach number
                stream << " " << mach;
                break;
              case 8:   // isentropic Mach number
                stream << " " << machis;
                break;
              case 9:   // total pressure loss
                stream << " " << ptloss;
                break;
              case 10:  // laminar viscosity coefficient
                stream << " " << visc;
                break;
              case 11:  // skin friction coefficient
                stream << " " << cf;
                break;
              case 12:  // pressure coefficient
                stream << " " << cp;
                break;
            }
          }
        }
        stream << endl;

      } // ibn
    }   // itype

    ibegf = iendf + 1;
    ibegn = iendn + 1;
  } // boundary

  stream.close();
}

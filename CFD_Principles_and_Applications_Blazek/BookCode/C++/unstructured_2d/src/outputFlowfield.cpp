/// @file outputFlowfield.cpp
///
/// Output of the flow field in Vis2D format.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 4, 2014
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

/// Writes out selected quantities for the whole flow field in Vis2D format.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param bndConds    boundary conditions
/// @param iter        current iteration
/// @exception         std::runtime_error  file cannot be opened
///
void Output::Flowfield( const Geometry &geometry, const FluidProps &fluidProps,
                        const BndConds &bndConds, int iter ) const
{
  int  nquant, i, m;
  REAL rrho, u, v, e, press, temp, c, mach, ttot, ptot, machis,
       ptloss, pratio, ptotinf, gam1, ggm1, visc;

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
  string fname = fnameFlow + str.str() + ".v2d";

  ofstream stream( fname,ios::binary | ios::trunc,_SH_DENYRW );
  if (stream.fail()) throw runtime_error( "could not open plot file (flow field) for writing." );

  // header

  nquant = 0;
  for (m=0; m<MXQFIELD; m++) if (varOn[m]) nquant++;

  stream << title << endl
         << "1" << endl
         << "Flow Field" << endl
         << "1 " << nquant+2 << endl
         << "x [m]" << endl
         << "y [m]" << endl;

  // names of variables

  for (m=0; m<MXQFIELD; m++)
  {
    if (varOn[m]) stream << varName[m] << endl;
  }

  // number of data points and grid elements

  stream << "0 0" << endl
         << geometry.nndInt << " " << geometry.nTria << " 0" << endl
         << "Unstructured" << endl;

  //  compute quantities and write them out

  stream << scientific << setprecision(8);

  for (i=0; i<geometry.nndInt; i++)
  {
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
    }
    else
    {
      ptloss = 1.0 - ptot/bndConds.ptinl;
      pratio = bndConds.ptinl/press;
    }
    machis = (POW(pratio,1.0/ggm1)-1.0)*2.0/gam1;
    machis = MAX(machis, 0.0);
    machis = SQRT(machis);

    if (fluidProps.equsType == Equations::NavierStokes)
      visc = fluidProps.dvLam[i].mue;
    else
      visc = 0.0;

    for (m=0; m<MXQFIELD; m++)
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
        }
      }
    }

    stream << endl;
  } // nodes

  // write node indexes

  for (i=0; i<geometry.nTria; i++)
  {
    stream << geometry.tria[i].node[0] << " "
           << geometry.tria[i].node[1] << " "
           << geometry.tria[i].node[2] << endl;
  }

  stream.close();
}

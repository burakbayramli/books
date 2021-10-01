/// @file userInput.cpp
///
/// Reading in and processing of user input.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 2, 2014
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

#include <cstdlib>
#include <stdexcept>
#include <sstream>
#include "streamIO.h"
#include "userInput.h"

using namespace std;

/// Opens user input file, reads and processes the data.
///
/// @param fileName  path and name of the user-input file
/// @param solver    settings related to the solver
/// @param output    settings related to the plot output
/// @exception       std::runtime_error  problem during the reading
///
void UserInput::Read( const char *fileName, Solver &solver, Output &output )
{
  string str;

  // open file

  ifstream stream( fileName );
  if (stream.fail()) throw runtime_error( "could not open user input file." );

  // title and file names

  str = ReadLine( stream );
  output.title = ReadLine( stream );

  str = ReadLine( stream );
  str = ReadLine( stream );
  solver.geometry.fnameGrid = ReadLine( stream );

  str = ReadLine( stream );
  str = ReadLine( stream );
  output.fnameFlow = ReadLine( stream );

  str = ReadLine( stream );
  str = ReadLine( stream );
  output.fnameSurf = ReadLine( stream );

  str = ReadLine( stream );
  str = ReadLine( stream );
  output.fnameConv = ReadLine( stream );

  str = ReadLine( stream );
  str = ReadLine( stream );
  solver.fnameRsti = ReadLine( stream );

  str = ReadLine( stream );
  str = ReadLine( stream );
  solver.fnameRsto = ReadLine( stream );

  // Physics - general

  str = ReadLine( stream );
  str = ReadLine( stream );
  str = ReadLine( stream );
  
  str = ReadLine( stream );
  if (str=="e" || str=="E")
    solver.bndConds.flowType = FlowType::External;
  else
    solver.bndConds.flowType = FlowType::Internal;
  
  str = ReadLine( stream );
  if (str=="e" || str=="E")
    solver.fluidProps.equsType = Equations::Euler;
  else
    solver.fluidProps.equsType = Equations::NavierStokes;
  
  str = ReadLine( stream ); solver.fluidProps.gamma  = stod( str );
  str = ReadLine( stream ); solver.fluidProps.cpgas  = stod( str );
  str = ReadLine( stream ); solver.fluidProps.renum  = stod( str );
  str = ReadLine( stream ); solver.fluidProps.refVel = stod( str );
  str = ReadLine( stream ); solver.fluidProps.refRho = stod( str );
  str = ReadLine( stream ); solver.fluidProps.prlam  = stod( str );

  // Physics - external flow

  str = ReadLine( stream );
  str = ReadLine( stream );
  str = ReadLine( stream );

  str = ReadLine( stream ); solver.bndConds.machinf = stod( str );
  str = ReadLine( stream ); solver.bndConds.alpha   = stod( str );
  str = ReadLine( stream ); solver.bndConds.pinf    = stod( str );
  str = ReadLine( stream ); solver.bndConds.tinf    = stod( str );

  // Physics - internal flow

  str = ReadLine( stream );
  str = ReadLine( stream );
  str = ReadLine( stream );

  str = ReadLine( stream ); solver.bndConds.ptinl   = stod( str );
  str = ReadLine( stream ); solver.bndConds.ttinl   = stod( str );
  str = ReadLine( stream ); solver.bndConds.betainl = stod( str );
  str = ReadLine( stream ); solver.bndConds.pout    = stod( str );
  str = ReadLine( stream ); solver.bndConds.betaout = stod( str );
  str = ReadLine( stream ); solver.bndConds.p12rat  = stod( str );

  // Geometrical reference values

  str = ReadLine( stream );
  str = ReadLine( stream );
  str = ReadLine( stream );

  str = ReadLine( stream ); solver.geometry.xref = stod( str );
  str = ReadLine( stream ); solver.geometry.yref = stod( str );
  str = ReadLine( stream ); solver.geometry.cref = stod( str );

  // Iteration control

  str = ReadLine( stream );
  str = ReadLine( stream );
  str = ReadLine( stream );

  str = ReadLine( stream ); solver.maxIter = stoi( str );
  str = ReadLine( stream ); solver.outStep = stoi( str );
  str = ReadLine( stream ); solver.convTol = stod( str );
  str = ReadLine( stream ); solver.restUse = (str=="y" || str=="Y");

  // Numerical parameters

  str = ReadLine( stream );
  str = ReadLine( stream );
  str = ReadLine( stream );

  str = ReadLine( stream ); solver.timeDiscr.cfl      = stod( str );
  str = ReadLine( stream ); solver.timeDiscr.epsIrs   = stod( str );
  str = ReadLine( stream ); solver.timeDiscr.nIterIrs = stoi( str );

  str = ReadLine( stream );
  if (str=="l" || str=="L")
    solver.timeDiscr.timeStepping = TimeStepping::Local;
  else
    solver.timeDiscr.timeStepping = TimeStepping::Global;

  str = ReadLine( stream ); solver.precond.switchedOn = (str=="y" || str=="Y");
  str = ReadLine( stream ); solver.precond.preCoeff   = stod( str );
  str = ReadLine( stream ); solver.spaceDiscr.order   = stoi( str );
  str = ReadLine( stream ); solver.spaceDiscr.limfac  = stod( str );
  str = ReadLine( stream ); solver.spaceDiscr.epsEntr = stod( str );
  str = ReadLine( stream ); solver.bndConds.vortCorr  = (str=="y" || str=="Y");

  str = ReadLine( stream ); solver.timeDiscr.nrk = stoi( str );
  ReadVector( stream,solver.timeDiscr.nrk,solver.timeDiscr.ark );
  ReadVector( stream,solver.timeDiscr.nrk,solver.timeDiscr.betrk );
  ReadVector( stream,solver.timeDiscr.nrk,solver.timeDiscr.dissipOn );

  // Quantities to plot (Y=yes, N=no)

  str = ReadLine( stream );
  str = ReadLine( stream );
  str = ReadLine( stream );

  for (int i=0; i<Output::MXQUANT; i++)
  {
    str = ReadLine( stream ); output.varOn[i] = (str=="y" || str=="Y");
  }

  // close file

  stream.close();
}

//*****************************************************************************

/// Prints user-input parameters for checking purposes.
///
/// @param solver  settings related to the solver
/// @param output  settings related to the plot output
///
void UserInput::Print( const Solver &solver, const Output &output )
{
  int    i;
  string str;

  cout << endl << output.title << endl << endl;

  // Physics - general

  cout << "#" << endl << "# Physics - general" << endl;
  cout << "# -----------------" << endl;

  if (solver.bndConds.flowType == FlowType::External)   str = " E"; else str = " I";
  PrintValue( str,"E=external flow, I=internal flow" );
  if (solver.fluidProps.equsType == Equations::Euler) str = " E"; else str = " N";
  PrintValue( str,"E=Euler, N=Navier-Stokes" );

  PrintValue( solver.fluidProps.gamma,  "ratio of specific heats" );
  PrintValue( solver.fluidProps.cpgas,  "specific heat coeff. at const. pressure [J/kgK]" );
  PrintValue( solver.fluidProps.renum,  "Reynolds number" );
  PrintValue( solver.fluidProps.refVel, "reference velocity [m/s]" );
  PrintValue( solver.fluidProps.refRho, "reference density [kg/m^3]" );
  PrintValue( solver.fluidProps.refVisc,"laminar viscosity [kg/ms]" );
  PrintValue( solver.fluidProps.prlam,  "laminar Prandtl number" );

  // Physics - external/internal flow

  if (solver.bndConds.flowType == FlowType::External)
  {
    cout << "#" << endl << "# Physics - external flow" << endl;
    cout << "# -----------------------" << endl;
    PrintValue( solver.bndConds.machinf  ,"Mach-number at infinity" );
    PrintValue( solver.bndConds.alpha/RAD,"angle of attack [deg]" );
    PrintValue( solver.bndConds.pinf     ,"static pressure at infinity [Pa]" );
    PrintValue( solver.bndConds.tinf     ,"static temperature at infinity [K]" );
  }
  else
  {
    cout << "#" << endl << "# Physics - internal flow" << endl;
    cout << "# -----------------------" << endl;
    PrintValue( solver.bndConds.ptinl      ,"total pressure at inlet [Pa]" );
    PrintValue( solver.bndConds.ttinl      ,"total temperature at inlet [K]" );
    PrintValue( solver.bndConds.betainl/RAD,"flow angle at inlet (with x-axis) [deg]" );
    PrintValue( solver.bndConds.pout       ,"static pressure at outlet [Pa]" );
    PrintValue( solver.bndConds.betaout/RAD,"approx. flow angle at outlet (with x-axis) [deg]" );
    PrintValue( solver.bndConds.p12rat     ,"approx. ratio of inlet to outlet static pressure" );
  }

  // Geometrical reference values

  cout << "#" << endl << "# Geometrical reference values" << endl;
  cout << "# ----------------------------" << endl;

  PrintValue( solver.geometry.xref,"x-coordinate of reference point (moment coefficient) [m]" );
  PrintValue( solver.geometry.yref,"y-coordinate             - '' -" );
  PrintValue( solver.geometry.cref,"reference or cord length [m]" );

  // Iteration control

  cout << "#" << endl << "# Iteration control" << endl;
  cout << "# -----------------" << endl;

  PrintValue( solver.maxIter,"max. number of iterations" );
  PrintValue( solver.outStep,"number of iterations between solution dumps" );
  PrintValue( solver.convTol,"2-norm of density change to stop the iteration" );

  if (solver.restUse) str = " Y"; else str == " N";
  PrintValue( str,"use previous solution for restart (Y=yes, N=no)" );

  // Numerical parameters

  cout << "#" << endl << "# Numerical parameters" << endl;
  cout << "# --------------------" << endl;

  PrintValue( solver.timeDiscr.cfl     ,"CFL-number" );
  PrintValue( solver.timeDiscr.epsIrs  ,"coefficient of implicit residual smoothing (<=0 - no smoothing)" );
  PrintValue( solver.timeDiscr.nIterIrs,"number of Jacobi iterations for residual smoothing" );

  if (solver.timeDiscr.timeStepping == TimeStepping::Local) str = " L"; else str = " G";
  PrintValue( str,"L=local, G=global time-stepping" );
  if (solver.precond.switchedOn) str = " Y"; else str = " N";
  PrintValue( str,"low Mach-number preconditioning (Y/N)" );

  PrintValue( solver.precond.preCoeff  ,"preconditioning parameter K" );
  PrintValue( solver.spaceDiscr.order  ,"1st-order (1) / 2nd-order (2) Roe scheme" );
  PrintValue( solver.spaceDiscr.limfac ,"limiter coefficient (Roe scheme)" );
  PrintValue( solver.spaceDiscr.epsEntr,"entropy correction coefficient (Roe scheme)" );

  if (solver.bndConds.vortCorr) str = " Y"; else str = " N";
  PrintValue( str,"correction of far-field due to single vortex (external flow)" );

  PrintValue( solver.timeDiscr.nrk,"number of Runge-Kutta stages (steady flow only)" );
  for (i=0; i<solver.timeDiscr.nrk; i++)
  {
    cout << setw(11) << right << solver.timeDiscr.ark[i];
    if (i < (solver.timeDiscr.nrk-1)) cout << ", ";
  }
  cout << endl;
  for (i=0; i<solver.timeDiscr.nrk; i++)
  {
    cout << setw(11) << right << solver.timeDiscr.betrk[i];
    if (i < (solver.timeDiscr.nrk-1)) cout << ", ";
  }
  cout << endl;
  for (i=0; i<solver.timeDiscr.nrk; i++)
  {
    cout << setw(11) << left << solver.timeDiscr.dissipOn[i];
    if (i < (solver.timeDiscr.nrk-1)) cout << ", ";
  }
  cout << endl;

  // Quantities to plot (Y=yes, N=no)

  cout << "#" << endl << "# Quantities to plot" << endl;
  cout << "# ------------------" << endl;

  for (i=0; i<Output::MXQUANT; i++)
  {
    if (output.varOn[i]) cout << " Y"; else cout << " N";
    cout << "   # " << output.GetVarName( i ) << endl;
  }

  str.clear();
  str.insert( 0,85,'-' );
  cout << endl << str << endl << endl;
}

//*****************************************************************************

/// Reads a vector of encoded boolean values from file (values <=0 mean false).
/// It is assumed that the values are in one line, and are separated by ",".
///
/// @param stream  file stream
/// @param nVals   number of values to read (dimension of the vector)
/// @return        vect[]
/// @exception     std::runtime_error  problem with the stream
///
void UserInput::ReadVector( ifstream &stream, int nVals, bool vect[] )
{
  int    ival;
  string str;
  string::size_type ic;

  if (stream.good())
  {
    str = ReadLine( stream );
    for (int i=0; i<nVals; i++)
    {
      stringstream(str) >> ival;
      vect[i] = (ival > 0);
      ic = str.find_first_of( ',' );
      if (ic != string::npos) str.erase( str.begin(),str.begin()+ic+1 );
    }
  }
  else throw runtime_error( "could not read vector of booleans from user input file." );
}

//*****************************************************************************

/// Reads a vector of REAL values from file. It is assumed that the values
/// are in one line, and are separated by ",".
///
/// @param stream  file stream
/// @param nVals   number of values to read (dimension of the vector)
/// @return        vect[]
/// @exception     std::runtime_error  problem with the stream
///
void UserInput::ReadVector( ifstream &stream, int nVals, REAL vect[] )
{
  string str;
  string::size_type ic;

  if (stream.good())
  {
    str = ReadLine( stream );
    for (int i=0; i<nVals; i++)
    {
      stringstream(str) >> vect[i];
      ic = str.find_first_of( ',' );
      if (ic != string::npos) str.erase( str.begin(),str.begin()+ic+1 );
    }
  }
  else throw runtime_error( "could not read vector of reals from user input file." );
}

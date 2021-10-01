/// @file main.cpp
///
/// Solution of 2-D Euler- and Navier-Stokes Equations
/// on Unstructured Triangular Grids.
///
//  Features:
//  ~~~~~~~~~
//  # unstructured finite-volume scheme of median-dual type
//  # triangular elements only
//  # ideal gas model (other models possible)
//  # laminar flow (viscosity computed by Sutherland`s law)
//  # Roe's flux-difference splitting scheme, Venkatakrishnan's limiter
//  # explicit multistage time-stepping scheme (Runge-Kutta type)
//  # preconditioning for low Mach numbers
//  # global or local time steps
//  # central implicit residual smoothing
//  # characteristic boundary conditions for external and internal flows
//  # special initial solution for compressor and turbine blades
//
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: September 22, 2014
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

#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <stdexcept>
#include "error.h"
#include "output.h"
#include "solver.h"
#include "userInput.h"

using namespace std;
void Info();

/// Main program of the flow solver.
///
/// @param argc   number of command line arguments
/// @param argv[] list of arguments
/// @return       EXIT_SUCCESS or an error code
///
int main( int argc, char *argv[] )
{
  Output  output;
  Solver  solver;
  string  str;
  clock_t startTime, endTime;

  cout << endl
       << " *************************************************" << endl
       << " *                                               *" << endl
       << " *   2-D FLOW ON UNSTRUCTURED TRIANGULAR GRIDS   *" << endl
       << " *                                               *" << endl
       << " *  (c) Jiri Blazek, CFD Consulting & Analysis   *" << endl
       << " *                 www.cfd-ca.de                 *" << endl
       << " *                                               *" << endl
       << " *          Version 1.0 from 09/22/2014          *" << endl
       << " *                                               *" << endl
       << " *************************************************" << endl << endl;

  if (argc == 2)
  {
    try
    {
      UserInput::Read( argv[1],solver,output );
    }
    catch (exception &e)
    {
      Error::Message( e.what() );
    }
  }
  else Info();

  // initialize some constants

  solver.InitConstants();
  output.StoreFlowType( solver.bndConds.flowType );

  // print input parameters for checking

  UserInput::Print( solver,output );

  // read grid dimensions, coordinates, and triangle nodes

  cout << " Reading grid data ..." << endl << endl;

  try
  {
    solver.geometry.ReadGrid();
  }
  catch (exception &e)
  {
    Error::Message( e.what() );
  }

  // generate edge list

  cout << " Generating edge list ..." << endl << endl;

  try
  {
    solver.geometry.GenerateEdgelist();
  }
  catch (exception &e)
  {
    Error::Message( e.what() );
  }

  // print some statistics

  cout << " No. of interior nodes: " << solver.geometry.nndInt << endl;
  cout << " No. of dummy nodes   : " << solver.geometry.nNodes-solver.geometry.nndInt << endl;
  cout << " No. of grid cells    : " << solver.geometry.nTria << endl;
  cout << " No. of interior edges: " << solver.geometry.nedInt << endl;
  cout << " Total number of edges: " << solver.geometry.nEdges << endl;
  cout << " No. of boundary faces: " << solver.geometry.nBfaces << endl;
  cout << " No. of boundary nodes: " << solver.geometry.nBnodes << endl << endl;
  
  // compute face vectors & cell volumes and check them;
  // set coordinates of dummy nodes = those of boundary nodes;
  // correct face vectors at symmetry boundaries;
  // compute projections of control volumes

  cout << " Computing metrics ..." << endl << endl;

  try
  {
    solver.geometry.ComputeMetrics();
  }
  catch (exception &e)
  {
    Error::Message( e.what() );
  }

  // allocate remaining memory in SpaceDiscr and TimeDiscr
  
  cout << " Allocating remaining memory ..." << endl << endl;

  try
  {
    solver.bndConds.AllocateMemory( solver.geometry );
    solver.fluidProps.AllocateMemory( solver.geometry.nNodes );
    solver.spaceDiscr.AllocateMemory( solver.fluidProps.equsType,solver.geometry.nNodes );
    solver.timeDiscr.AllocateMemory( solver.geometry.nNodes );
  }
  catch (exception &e)
  {
    Error::Message( e.what() );
  }

  // read / initialize flow field

  if (solver.restUse)
  {
    cout << " Reading initial solution ..." << endl << endl;
    try
    {
      solver.ReadSolution();
    }
    catch (exception &e)
    {
      Error::Message( e.what() );
    }
  }
  else
  {
    cout << " Guessing initial solution ..." << endl << endl;
    solver.InitSolution();
  }

  solver.fluidProps.DependentVarsAll( solver.geometry.nNodes );
  if (!solver.restUse)
    solver.bndConds.BoundaryConditions( solver.geometry,solver.fluidProps,solver.precond );

  // compute limiter reference values

  solver.spaceDiscr.LimiterRefvals( solver.geometry,solver.fluidProps,solver.bndConds );

  // open file for convergence history

  try
  {
    output.OpenConvergence();
  }
  catch (exception &e)
  {
    Error::Message( e.what() );
  }

  // ***************************************************************************
  // iterate until steady state solution or max. number of iterations is reached
  // ***************************************************************************

  if (solver.bndConds.flowType == FlowType::External)
  {
    str.clear();
    str.insert( 0,83,'-' );
    cout << str << endl
         << " step      resid        resmax      i-res      cl            cd            cm"
         << endl << str << endl;
  }
  else
  {
    str.clear();
    str.insert( 0,69,'-' );
    cout << str << endl
         << " step      resid        resmax      i-res    mass flow    mass ratio"
         << endl << str << endl;
  }

  if (!solver.restUse) solver.iter = 0;
  startTime = clock();

  do
  {
    solver.iter++;
    solver.timeDiscr.Solve( solver.geometry,solver.fluidProps,solver.bndConds,
                            solver.spaceDiscr,solver.precond );
    solver.Convergence( output );

    if (solver.iter%solver.outStep == 0)
    {
      try
      {
        cout << endl << " Writing plot files ..." << endl;
        output.Surfaces( solver.geometry,solver.fluidProps,solver.bndConds,
                         solver.spaceDiscr,solver.iter );
        output.Flowfield( solver.geometry,solver.fluidProps,solver.bndConds,
                          solver.iter );
      }
      catch (exception &e)
      {
        Error::Message( e.what() );
      }
    }
  } while (!solver.Converged());

  endTime = clock();

  // ***************************************************************************

  str.clear();
  str.insert( 0,85,'-' );
  cout << endl << str << endl << endl;

  // close file with convergence history

  output.CloseConvergence();

  // output the results

  if (solver.iter%solver.outStep != 0)
  {
    try
    {
      cout << " Writing plot files ..." << endl << endl;
      output.Surfaces( solver.geometry,solver.fluidProps,solver.bndConds,
                       solver.spaceDiscr,solver.iter );
      output.Flowfield( solver.geometry,solver.fluidProps,solver.bndConds,
                        solver.iter );
    }
    catch (exception &e)
    {
      Error::Message( e.what() );
    }
  }

  // store solution for restart

  cout << " Writing solution file ..." << endl << endl;
  try
  {
    solver.WriteSolution();
  }
  catch (exception &e)
  {
    Error::Message( e.what() );
  }

  cout << " Finished in " << fixed << setprecision(1)
       << (endTime-startTime)/((double)CLOCKS_PER_SEC)
       << " seconds." << endl << endl;

  return(EXIT_SUCCESS);
}

//*****************************************************************************

/// Displays program usage.
///
void Info()
{
  printf("Usage:\n");
  printf("unstruct2d <input_file>\n\n");

  exit(EXIT_FAILURE);
}

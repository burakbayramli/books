!> @file main.f90
!!
!! Solution of 2-D Euler- and Navier-Stokes Equations
!! on Structured, Body-Fitted Grids.
!
!  Features:
!  ~~~~~~~~~
!  # structured, finite-volume scheme of cell-centered type
!  # single-block topology only
!  # ideal gas model
!  # laminar flow (viscosity computed by Sutherland`s law)
!  # central differences with Jameson's artificial dissipation
!  # upwind flux-difference splitting scheme due to Roe (MUSCL interpolation)
!  # explicit multistage time-stepping scheme (Runge-Kutta)
!  # preconditioning for low Mach numbers
!  # global or local time steps
!  # central implicit residual smoothing
!  # characteristic boundary conditions for external and internal flows
!  # special initial solution for compressor and turbine blades
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Version 1.0 from September 2, 2014
!
! *****************************************************************************
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!
! *****************************************************************************

!> Main program of the flow solver.
!!
program Struct2D

  use ModDataTypes
  use ModControl
  use ModFiles
  use ModGeometry
  use ModPhysics
  use ModPlotQuant
  use ModInterfaces
  implicit none

! local variables
  character(chrlen) :: fname          ! filename (user input, convergence)
  integer           :: errFlag        ! error flag
  real(rtype), allocatable :: work(:) ! work space (used inside Solver)

! *****************************************************************************

  write(*,*) " "
  write(*,*) "*************************************************"
  write(*,*) "*                                               *"
  write(*,*) "*   2-D FLOW ON STRUCTURED, BODY-FITTED GRIDS   *"
  write(*,*) "*                                               *"
  write(*,*) "*  (c) Jiri Blazek, CFD Consulting & Analysis   *"
  write(*,*) "*                 www.cfd-ca.de                 *"
  write(*,*) "*                                               *"
  write(*,*) "*          Version 1.0 from 09/02/2014          *"
  write(*,*) "*                                               *"
  write(*,*) "*************************************************"
  write(*,*) " "

! read name of input file from command line

  call Getarg( 1,fname )
  IF (Len_trim(fname) == 0) call Usage

! set names of quantities which can be written out

  cquant( 1) = "density [kg/m^3]" ! density
  cquant( 2) = "u [m/s]"          ! u-velocity
  cquant( 3) = "v [m/s]"          ! v-velocity
  cquant( 4) = "p [Pa]"           ! static pressure
  cquant( 5) = "p-tot [Pa]"       ! total pressure
  cquant( 6) = "T [K]"            ! static temperature
  cquant( 7) = "T-tot [K]"        ! total temperature
  cquant( 8) = "M"                ! local Mach-number
  cquant( 9) = "M-isen"           ! isentropic Mach-number
  cquant(10) = "pt-loss"          ! total pressure loss
  cquant(11) = "visc-lam"         ! laminar viscosity
  cquant(12) = "cf"               ! friction coefficient (boundaries only)
  cquant(13) = "-Cp"              ! pressure coefficient (boundaries only)

! read input parameters

  call ReadParams( fname )

! initialize some constants

  call InitConstants

! print input parameters for checking

  call PrintParams

! set no. of equations (rho, rho*u, rho*v, rho*E, ...);
! set no. of dependent variables (p, T, c, gamma, cpgas, ...)

  nconv = 4
  ndepv = 5
  if (kequs == "N") then
    ndepv = ndepv + 2   ! laminar viscosity, heat conduction coeff.
  endif

! read grid topology and allocate memory

  write(*,"(A,/)") " Reading grid topology ..."
  call ReadTopology

  write(*,"(A,/)") " Allocating memory ..."
  call AllocateMemory

  allocate( work(2*((imax+1)*(jmax+1))),stat=errFlag )
  if (errFlag /= 0) call ErrorMessage( "cannot allocate memory for work space" )

! read grid coordinates

  write(*,"(A,/)") " Reading grid coordinates ..."
  call ReadGrid

! compute metrics, cell volumes

  write(*,"(A)") " Computing metrics ..."
  call InitMetrics

! read / initialize flow field

  if (lrest == "Y") then
    write(*,"(A,/)") " Reading initial solution ..."
    call ReadSolution
  else
    write(*,"(A,/)") " Guessing initial solution ..."
    call InitSolution
  endif

  call DependentVarsAll

  if (lrest /= "Y") call BoundaryConditions( work )

! compute limiter reference values

  call LimiterRefvals

! open file for convergence history

  write(fname,"(A)") Trim(fnConv)//".v2d"
  open(unit=ifConv, file=fname, status="unknown", action="write", iostat=errFlag)
  if (errFlag /= 0) call ErrorMessage( "cannot open convergence file" )
  if (kflow == "E") then
    write(ifConv,1000) Trim(title)
  else
    write(ifConv,1010) Trim(title)
  endif

! -----------------------------------------------------------------------------
! iterate until steady state solution or max. number of iterations is reached
! -----------------------------------------------------------------------------

  if (kflow == "E") then
    write(*,1005)
  else
    write(*,1015)
  endif

  if (lrest /= "Y") iter = 0

  do
    iter = iter + 1
    call Solver( work )
    call Convergence

    if (Mod(iter,outstep) == 0) then
      write(*,"(/,A)") " Writing plot files ..."
      call PlotFlow
      call PlotSurfaces
    endif

    if (iter>=maxiter .or. drho<=convtol) exit
  enddo

! -----------------------------------------------------------------------------
! close file for convergence history

  close(unit=ifConv)

! output the results

  write(*,"(/,80('-'))")

  if (Mod(iter,outstep) /= 0) then
    write(*,"(/,A)") " Writing plot files ..."
    call PlotFlow
    call PlotSurfaces
  endif

! store solution for restart

  write(*,"(/,A)") " Writing solution file ..."
  call WriteSolution

  write(*,"(/,' Finished.',/)")

1000  format(A,/,"1",/,"Convergence History",/,"1 8",/, &
             "step",/,"resid",/,"resmax",/,"i-res",/,"j-res",/,"cl",/, &
             "cd",/,"cm",/,"-1 0",/,"0 0 0",/,"Structured")
1005  format(80("-"),/, &
             " step",5X,"resid",7X,"resmax",4X,"i-res",2X,"j-res", &
             6X,"cl",10X,"cd",10X,"cm",/,80("-"))
1010  format(A,/,"1",/,"Convergence History",/,"1 7",/, &
             "step",/,"resid",/,"resmax",/,"i-res",/,"j-res",/, &
             "mass-flow",/,"mass-ratio",/,"-1 0",/,"0 0 0",/,"Structured")
1015  format(70("-"),/, &
             " step",5X,"resid",7X,"resmax",4X,"i-res",2X,"j-res", &
             3X,"mass flow",3X,"mass ratio",/,70("-"))

! *****************************************************************************

contains

  subroutine Usage
    write(*,"(/,A,/)") "Usage:"
    write(*,"(A,/)")   "Struct2D <input file>"
    stop
  end subroutine Usage

end program Struct2D

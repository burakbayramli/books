!> @file printParams.f90
!!
!! Print out of user-input parameters.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: February 26, 2014
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

!> Prints user-input parameters for checking purposes.
!!
subroutine PrintParams

  use ModControl
  use ModFiles
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModPlotQuant
  implicit none

! local variables
  integer :: i

! *****************************************************************************

  write(*,1000) Trim( title )

! physics - general

  write(*,1005)
  write(*,1040) kflow,"E=external flow, I=internal flow"
  write(*,1040) kequs,"E=Euler, N=Navier-Stokes"
  write(*,1045) gamma,"ratio of specific heats"
  write(*,1045) cpgas,"specific heat coefficient (p=const.)"
  write(*,1045) renum,"Reynolds number"
  write(*,1045) refvel,"reference velocity"
  write(*,1045) refrho,"reference density"
  write(*,1045) refvisc,"laminar viscosity"
  write(*,1045) prlam,"laminar Prandtl number"

! physics - external/internal flow

  if (kflow=="e" .or. kflow=="E") then
    write(*,1010)
    write(*,1045) machinf,"Mach-number at infinity"
    write(*,1045) alpha*rad,"angle of attack [deg]"
    write(*,1045) pinf,"static pressure at infinity [Pa]"
    write(*,1045) tinf,"static temperature at infinity [K]"
  else
    write(*,1015)
    write(*,1045) ptinl,"total pressure at inlet [Pa]"
    write(*,1045) ttinl,"total temperature at inlet [K]"
    write(*,1045) betainl*rad,"flow angle at inlet (with x-axis) [deg]"
    write(*,1045) pout,"static pressure at outlet [Pa]"
    write(*,1045) betaout*rad,"approx. flow angle at outlet (with x-axis) [deg]"
    write(*,1045) p12rat,"approx. ratio of inlet to outlet static pressure"
  endif

! physics - injection boundary

  write(*,1017)
  write(*,1045) minject,"mass flow rate [kg/m^2*s]"
  write(*,1045) tinject,"injection temperature [K]"

! geometrical reference values

  write(*,1020)
  write(*,1045) xref,"x-coordinate of reference point (moment coefficient) [m]"
  write(*,1045) yref,"y-coordinate             - '' -"
  write(*,1045) cref,"reference or cord length [m]"

! iteration control

  write(*,1025)
  write(*,1050) maxiter,"max. number of iterations"
  write(*,1050) outstep,"number of iterations between solution dumps"
  write(*,1045) convtol,"2-norm of density change to stop the iteration"
  write(*,1040) lrest,"use previous solution for restart (Y=yes, N=no)"

! numerical parameters

  write(*,1030)
  write(*,1045) cfl,"CFL-number"
  write(*,1045) epsirs,"coefficient of implicit residual smoothing (<=0 - no smoothing)"
  write(*,1040) ktimst,"L=local, G=global time-stepping"
  write(*,1040) kprecond,"low Mach-number preconditioning (Y/N)"
  write(*,1045) precoeff,"preconditioning parameter K"
  write(*,1040) kdissip,"central scheme (C) or Roe upwind scheme (R)"
  write(*,1045) vis2,"artificial dissipation coefficient - k2"
  if (kdissip=="c" .or. kdissip=="C") then
    write(*,1045) 1.D0/vis4,"artificial dissipation coefficient - 1/k4"
  else
    write(*,1045) 0.D0,"artificial dissipation coefficient - 1/k4"
  endif
  write(*,1055) iorder,"1st-order (1) / 2nd-order (2) Roe scheme"
  write(*,1045) limfac,"limiter coefficient (Roe scheme)"
  write(*,1045) epsentr,"entropy correction coefficient (Roe scheme)"
  write(*,1040) lvort,"correction of far-field due to single vortex (external flow)"
  write(*,1055) iextrapol,"pressure extrapolation to solid walls"
  write(*,1045) maxwchg,"max. rel. change of rho and rho*E at slip walls"
  write(*,1045) maxichg,"max. rel. change of rho and rho*E at injection boundaries"
  write(*,1055) nrk,"number of Runge-Kutta stages (steady flow only)"
  write(*,1060) (ark  (i), i=1,nrk)
  write(*,1060) (betrk(i), i=1,nrk)
  write(*,1065) (ldiss(i), i=1,nrk)

! quantities to plot

  write(*,1035)
  do i=1,mxquant
    write(*,1041) lquant(i),Trim( cquant(i) )
  enddo
  write(*,1070)

! formats

1000  format(/,A,/)
1005  format("#",/,"# Physics - general",/,"# ",17("-"))
1010  format("#",/,"# Physics - external flow",/,"# ",23("-"))
1015  format("#",/,"# Physics - internal flow",/,"# ",23("-"))
1017  format("#",/,"# Physics - injection boundary",/,"# ",28("-"))
1020  format("#",/,"# Geometrical reference values",/,"# ",28("-"))
1025  format("#",/,"# Iteration control",/,"# ",17("-"))
1030  format("#",/,"# Numerical parameters",/,"# ",20("-"))
1035  format("#",/,"# Quantities to plot",/,"# ",18("-"))
1040  format(2X,A1,12X,"# ",A)
1041  format(2X,A1,2X,"# ",A)
1045  format(1X,1PE11.4,3X,"# ",A)
1050  format(2X,I6,7X,"# ",A)
1055  format(2X,I1,12X,"# ",A)
1060  format(5(2X,F6.4))
1065  format(5(2X,I6))
1070  format(80("-"),/)

end subroutine PrintParams

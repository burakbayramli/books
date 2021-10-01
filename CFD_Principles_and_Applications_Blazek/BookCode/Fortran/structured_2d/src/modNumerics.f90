!> @file modNumerics.f90
!!
!! Variables and switches controlling the numerical procedure.
!
! *****************************************************************************
!
!  (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: February 27, 2014
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

!> Variables related to the numerics.
!!
module ModNumerics

  use ModDataTypes
  implicit none

  character(1) :: ktimst,  & !< switch between local (="L") and global (="G") time-stepping
                  lvort,   & !< far-field vortex correction ("Y"=yes, "N"=no)
                  kdissip, & !< central scheme (="C") or Roe's upwind scheme (="R")
                  kprecond   !< low Mach-number preconditioning ("Y"=yes, "N"=no)

  integer :: iorder,    & !< order of Roe's upwind scheme (1; 2: kappa=1/3 MUSCL scheme)
             nrk,       & !< number of stages (Runge-Kutta scheme); max. = 5
             iextrapol, & !< pressure extrapolation to solid walls (2- or 3-point formula)
             ldiss(5)     !< dissipation evaluation per stage (0=no, 1=yes)

  real(rtype) :: cfl,      & !< CFL-number
                 epsirs,   & !< coefficient of implicit residual smoothing
                 vis2,     & !< 2nd-order dissipation coefficient (central scheme)
                 vis4,     & !< 4th-order dissipation coefficient (central scheme)
                 limfac,   & !< limiter coefficient (Roe's upwind scheme)
                 epsentr,  & !< entropy correction coefficient (Roe's upwind scheme)
                 precoeff, & !< preconditioning parameter K (low Mach numbers)
                 ark(5),   & !< stage coefficients
                 betrk(5)    !< dissipation-blending coefficients

  real(rtype) :: maxwchg !< max. relative change of density and density*E at slip walls\n
                         !! @details
                         !! When the relative change exceeds this value, extrapolation
                         !! to the dummy cells at walls is reduced to 0th-order.
  real(rtype) :: maxichg !< max. relative change of density and density*E at injection boundaries\n
                         !! @details
                         !! When the relative change exceeds this value, extrapolation
                         !! to the dummy cells at injection boundaries is reduced to
                         !! 0th-order.

  real(rtype) :: volref    !< reference volume\n
                           !! @details
                           !! Parameter is required for the computation of limiter
                           !! functions (higher-order Roe scheme).
  real(rtype) :: limref(4) !< reference values of density, u, v and pressure\n
                           !! @details
                           !! Parameter is required for the computation of limiter
                           !! functions (higher-order Roe scheme).

  real(rtype), allocatable :: cvold(:,:,:), & !< conservative variables from previous time step
                              diss(:,:,:),  & !< artificial dissipation
                              rhs(:,:,:),   & !< residual (right-hand side)
                              sri(:,:),     & !< spectral radii in i-direction
                              srj(:,:),     & !< spectral radii in j-direction
                              epsij(:,:,:), & !< coefficients of implicit residual smoothing (x-, y-direction)
                              tstep(:,:)      !< time steps (without the CFL-number)

  real(rtype), allocatable :: dui(:,:,:) !< 1st differences of primitive variables\n
                              !! @details
                              !! Values of density, u, v, and pressure in the
                              !! i-direction at the cell face I-1/2 (higher-order
                              !! Roe scheme).
  real(rtype), allocatable :: duj(:,:,:) !< 1st differences of primitive variables\n
                              !! @details
                              !! Values of density, u, v, and pressure in the
                              !! j-direction at the cell face J-1/2 (higher-order
                              !! Roe scheme).

  real(rtype), allocatable :: gradfi(:,:,:) !< gradients of velocity components and temperature\n
                              !! @details
                              !! Gradients with respect to the x- and y-coordinates at
                              !! the cell faces I-1/2 (required for viscous flow).
  real(rtype), allocatable :: gradfj(:,:,:) !< gradients of velocity components and temperature\n
                              !! @details
                              !! Gradients with respect to the x- and y-coordinates at
                              !! the cell faces J-1/2 (required for viscous flow).

  real(rtype) :: pi, & !< 3.14...
                 rad   !< 180./pi

end module ModNumerics

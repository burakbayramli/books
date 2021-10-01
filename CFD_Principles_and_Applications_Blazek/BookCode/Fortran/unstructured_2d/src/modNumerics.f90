!> @file modNumerics.f90
!!
!! Variables and switches controlling the numerical procedure.
!
! *****************************************************************************
!
!  (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: May 20, 2014
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
                  kprecond   !< low Mach-number preconditioning ("Y"=yes, "N"=no)

  integer :: nedges, & !< total number of edges (including edges between boundary and dummy nodes)
             nedint, & !< number of edges excluding those to dummy nodes
             iorder, & !< order of Roe's upwind scheme (1 or 2)
             nitirs, & !< number of Jacobi iterations (implicit residual smoothing)
             nrk,    & !< number of stages (Runge-Kutta scheme); max. = 5
             ldiss(5)  !< dissipation evaluation per stage (0=no, 1=yes)

  integer, allocatable :: edge(:,:) !< edge list (node i, node j)\n
    !! @details
    !! For ie > nedint, edge(*,ie) represents the edge from a boundary node
    !! (i) to a dummy node (used at inlet, outlet and far-field boundaries).

  real(rtype) :: cfl,      & !< CFL-number
                 epsirs,   & !< coefficient of implicit residual smoothing
                 limfac,   & !< limiter coefficient (Roe's upwind scheme)
                 epsentr,  & !< entropy correction coefficient (Roe's upwind scheme)
                 precoeff, & !< preconditioning parameter K (low Mach numbers)
                 ark(5),   & !< stage coefficients
                 betrk(5)    !< dissipation-blending coefficients

  real(rtype) :: volref    !< reference volume\n
                           !! @details
                           !! Parameter is required for the computation of limiter
                           !! functions (higher-order Roe scheme).
  real(rtype) :: limref(4) !< reference values of density, u, v and pressure\n
                           !! @details
                           !! Parameter is required for the computation of limiter
                           !! functions (higher-order Roe scheme).

  real(rtype), allocatable :: cvold(:,:), & !< conservative variables from previous time step
                              diss(:,:),  & !< artificial dissipation
                              rhs(:,:),   & !< residual (right-hand side)
                              lim(:,:),   & !< values of the limiter function (density, u, v, pressure)
                              tstep(:)      !< time steps (without the CFL-number)

  real(rtype), allocatable :: gradx(:,:) !< gradients of density, velocity components,
                              !! pressure and temperature with respect to the x-coordinate
  real(rtype), allocatable :: grady(:,:) !< gradients of density, velocity components,
                              !! pressure and temperature with respect to the y-coordinate

  real(rtype) :: pi, & !< 3.14...
                 rad   !< 180./pi

end module ModNumerics

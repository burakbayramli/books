!> @file modPlotQuant.f90
!!
!! Quantities to be written out and plotted (flow-field, surface(s),
!! forces & moments, convergence history).
!
! *****************************************************************************
!
!  (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
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

!> Variables related to plot output.
!!
module ModPlotQuant

  use ModDataTypes
  implicit none

  integer, parameter :: mxquant =13, & !< total number of plot variables
                        mxqfield=11    !< no. of plot variables in the field (cf and Cp only at the boundaries)

  character(chrlen) :: title           !< title of the simulation case
  character(chrlen) :: cquant(mxquant) !< names of plot variables
  character(1)      :: lquant(mxquant) !< on/off switches of the plot variables

  real(rtype) :: drho,  & !< change of the density residual (convergence criterion)
                 drho1, & !< initial change of the density residual (used for normalization)
                 cl,    & !< lift coefficient (pressure forces only; external flow)
                 cd,    & !< drag coefficient (pressure forces only; external flow)
                 cm,    & !< pitching moment coefficient wrp. to the reference point
                 mflow, & !< average mass flow rate (internal flow)
                 mfratio  !< ratio of mass flow at outlet to mass flow at inlet

end module ModPlotQuant

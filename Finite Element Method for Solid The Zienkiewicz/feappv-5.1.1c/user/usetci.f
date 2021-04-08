!$Id:$
      subroutine usetci(dt,theta,gtan,cc1,cc2,cc3)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Set parameters to perform updates for given time step

!     Inputs :
!       dt       - Current time increment
!       theta(3) - Command line parameters

!     Outputs:
!       gtan(3)  - Parmeters for: 1 - stiffness; 2 - damping; 3 - mass
!                  e.g., S = K*gtan(1) + C*gtan(2) + M*gtan(3)
!       cc1      - Parameter to update solution  at t_n+1
!       cc2      - Parameter to update increment at t_n+1
!       cc3      - Parameter to update specified boundary displacement
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real (kind=8) :: dt,cc1,cc2,cc3,theta(3),gtan(3)

      save

      end subroutine usetci

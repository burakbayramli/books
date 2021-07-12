c$Id:$
      subroutine usetci(dt,theta,gtan,cc1,cc2,cc3)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: Set parameters to perform updates for given time step

c     Inputs :
c       dt       - Current time increment
c       theta(3) - Command line parameters

c     Outputs:
c       gtan(3)  - Parmeters for: 1 - stiffness; 2 - damping; 3 - mass
c                  e.g., S = K*gtan(1) + C*gtan(2) + M*gtan(3)
c       cc1      - Parameter to update solution  at t_n+1
c       cc2      - Parameter to update increment at t_n+1
c       cc3      - Parameter to update specified boundary displacement
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real*8    dt,cc1,cc2,cc3,theta(3),gtan(3)

      save

      end

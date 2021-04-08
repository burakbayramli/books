!$Id:$
      subroutine plview(q,vmin,vmax)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Shows plot viewpoint for perspectives

!      Inputs:
!         q(3,1)    - View point location
!         vmin(3)   - Minimum coordinates of mesh
!         vmax(3)   - Maximum coordinates of mesh

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i
      real (kind=8) :: qmax, absmax, rnorm, xx, yy

      real (kind=8) :: q(3,3),vmin(3),vmax(3), bmax(3), view(3)

      save

!     Vectors - 1

      call dplot( 0.9854650d0, 0.8825000d0, 3)
      call dplot( 1.1091850d0, 0.8825000d0, 2)
      call dplot( 1.1014525d0, 0.8902325d0, 2)
      call dplot( 1.1014525d0, 0.8747675d0, 2)
      call dplot( 1.1091850d0, 0.8825000d0, 2)

!     Vectors - 1

      call dplot( 1.1401150d0, 0.8825000d0, 3)
      call dplot( 1.2638350d0, 0.8825000d0, 2)
      call dplot( 1.2561025d0, 0.8902325d0, 2)
      call dplot( 1.2561025d0, 0.8747675d0, 2)
      call dplot( 1.2638350d0, 0.8825000d0, 2)

!     Vectors - 2

      call dplot( 1.0473250d0, 0.8306400d0, 3)
      call dplot( 1.0473250d0, 0.9443600d0, 2)
      call dplot( 1.0395925d0, 0.9366275d0, 2)
      call dplot( 1.0550575d0, 0.9366275d0, 2)
      call dplot( 1.0473250d0, 0.9443600d0, 2)

!     Vectors - 3

      call dplot( 1.2012750d0, 0.8306400d0, 3)
      call dplot( 1.2012750d0, 0.9443600d0, 2)
      call dplot( 1.1935425d0, 0.9366275d0, 2)
      call dplot( 1.2090075d0, 0.9366275d0, 2)
      call dplot( 1.2012750d0, 0.9443600d0, 2)

!     Label = 1

      call dplot( 1.0669175d0, 0.8925000d0, 3)
      call plabl(1)
      call dplot( 1.2269175d0, 0.8925000d0, 3)
      call plabl(1)

!     Label = 2

      call dplot( 1.0095925d0, 0.9471800d0, 3)
      call plabl(2)

!     Label = 3

      call dplot( 1.1635425d0, 0.9471800d0, 3)
      call plabl(3)

      qmax    = max(abs(q(1,1)) , abs(q(2,1)) , abs(q(3,1)) )
      bmax(1) = (vmax(1) - vmin(1))/2.0d0
      bmax(2) = (vmax(2) - vmin(2))/2.0d0
      bmax(3) = (vmax(3) - vmin(3))/2.0d0
      absmax  = max(qmax , abs(bmax(1)) , abs(bmax(2)) , abs(bmax(3)) )
      rnorm   = 0.06186d0/absmax
      do i = 1,3
        view(i) = rnorm*q(i,1)
        bmax(i) = rnorm*bmax(i)
      end do

!     Plot box of body limits - 12 -plane

      xx = 1.047325d0 - bmax(1)
      yy = 0.882500d0 - bmax(2)
      call ppbox( xx , yy, 2.d0*bmax(1), 2.d0*bmax(2), 3)

!     Plot box of body limits - 13 -plane

      xx = 1.201275d0 - bmax(1)
      call ppbox( xx , yy, 2.d0*bmax(1), 2.d0*bmax(2), 3)

!     Plot eyes - 12 -plane

      call ppeye( view(1) + 1.047325d0 , view (2) + 0.8825000d0 )

!     Plot eyes - 13 -plane

      call ppeye( view(1) + 1.201275d0 , view (3) + 0.8825000d0 )

      end subroutine plview

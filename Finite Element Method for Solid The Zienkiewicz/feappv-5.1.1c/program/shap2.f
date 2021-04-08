!$Id:$
      subroutine shap2(s,t,shp,ix,nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Adds quadratic functions to quadrilaterals for any
!               non-zero mid-side or central node

!      Inputs:
!         s,t      - Natural coordinates
!         ix(*)    - List of nodes attached to element (0 = no node)
!         nel      - Maximum number of local node on element <= 9

!      Outputs:
!         shp(3,*) - Shape functions and derivatives w/r natural coords
!                    shp(1,i) = dN_i/dxi_1
!                    shp(2,i) = dN_i/dxi_2
!                    shp(3,i) = N_i
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i, j, k, l, nel
      real (kind=8) :: s, t, s2, t2

      integer       :: ix(nel)
      real (kind=8) :: shp(3,nel)

      save

      s2 = (1.d0-s*s)*0.5d0
      t2 = (1.d0-t*t)*0.5d0

      do i = 5,9
        do j = 1,3
          shp(j,i) = 0.0d0
        end do
      end do

!     Midside nodes (serendipity)

      if(ix(5).ne.0) then
        shp(1,5) = -s*(1.d0-t)
        shp(2,5) = -s2
        shp(3,5) = s2*(1.d0-t)
      endif
      if(nel.lt.6) go to 100
      if(ix(6).ne.0) then
        shp(1,6) = t2
        shp(2,6) = -t*(1.d0+s)
        shp(3,6) = t2*(1.d0+s)
      endif
      if(nel.lt.7) go to 100
      if(ix(7).ne.0) then
        shp(1,7) = -s*(1.d0+t)
        shp(2,7) = s2
        shp(3,7) = s2*(1.d0+t)
      endif
      if(nel.lt.8) go to 100
      if(ix(8).ne.0) then
        shp(1,8) = -t2
        shp(2,8) = -t*(1.d0-s)
        shp(3,8) = t2*(1.d0-s)
      endif

!     Interior node (lagrangian)

      shp(1,9) = -4.d0*s*t2
      shp(2,9) = -4.d0*t*s2
      shp(3,9) =  4.d0*s2*t2
      if(nel.lt.9 .or. ix(9).eq.0) go to 100

!     Correct edge nodes for interior node (lagrangian)

      do j= 1,3
        do i = 1,4
          shp(j,i) = shp(j,i) - 0.25d0*shp(j,9)
        end do
        do i = 5,8
          if(ix(i).ne.0) shp(j,i) = shp(j,i) - 0.5d0*shp(j,9)
        end do
      end do

!     Correct corner nodes for presence of midside nodes

100   k = 8
      do i = 1,4
        l = i + 4
        do j = 1,3
          shp(j,i) = shp(j,i) - 0.5d0*(shp(j,k)+shp(j,l))
        end do
        k = l
      end do

      end subroutine shap2

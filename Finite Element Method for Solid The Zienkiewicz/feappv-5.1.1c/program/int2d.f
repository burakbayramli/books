!$Id:$
      subroutine int2d(l,lint,sg)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Form Gauss points and weights for two dimensions

!      Inputs:
!         l       - Number of points/direction

!      Outputs:
!         lint    - Total number of points
!         sg(3,*) - Array of points and weights
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'eldata.h'

      integer       :: i,j,k,l,lint, lr(9),lz(9),lw(9)
      real (kind=8) :: g,h, third, sg(3,*),ss(5),ww(5)

      data      lr/-1,1,1,-1,0,1,0,-1,0/,lz/-1,-1,1,1,-1,0,1,0,0/
      data      lw/4*25,4*40,64/
      data      third / 0.3333333333333333d0 /

!     Set number of total points

      lint = l*l

!     5 pt. integration

      if(l.eq.0) then

        lint = 5
        g    = sqrt(0.6d0)
        do i = 1,4
          sg(1,i) = g*lr(i)
          sg(2,i) = g*lz(i)
          sg(3,i) = 5.d0/9.d0
        end do

        sg(1,5) = 0.0d0
        sg(2,5) = 0.0d0
        sg(3,5) = 16.d0/9.d0

!     1x1 integration

      elseif(l.eq.1) then
        sg(1,1) = 0.d0
        sg(2,1) = 0.d0
        if(nel.eq.3) sg(2,1) = -third
        sg(3,1) = 4.d0

!     2x2 integration

      elseif(l.eq.2) then
        g = sqrt(third)
        do i = 1,4
          sg(1,i) = g*lr(i)
          sg(2,i) = g*lz(i)
          sg(3,i) = 1.d0
        end do

!     3x3 integration

      elseif(l.eq.3) then
        g = sqrt(0.6d0)
        h = 1.d0/81.d0
        do i = 1,9
          sg(1,i) = g*lr(i)
          sg(2,i) = g*lz(i)
          sg(3,i) = h*lw(i)
        end do

!     4x4 integration

      elseif(l.eq.4) then
        g     = sqrt(4.8d0)
        h     = third/g
        ss(1) = sqrt((3.d0+g)/7.d0)
        ss(4) = - ss(1)
        ss(2) = sqrt((3.d0-g)/7.d0)
        ss(3) = -ss(2)
        ww(1) = 0.5d0 - h
        ww(2) = 0.5d0 + h
        ww(3) = 0.5d0 + h
        ww(4) = 0.5d0 - h
        i = 0
        do j = 1,4
          do k = 1,4
            i = i + 1
            sg(1,i) = ss(k)
            sg(2,i) = ss(j)
            sg(3,i) = ww(j)*ww(k)
          end do
        end do

!     5x5 integration

      elseif(l.eq.5) then

        g     =  sqrt(1120.d0)
        ss(1) =  sqrt((70.d0 + g)/126.d0)
        ss(2) =  sqrt((70.d0 - g)/126.d0)
        ss(3) =  0.0d0
        ss(4) = -ss(2)
        ss(5) = -ss(1)

        ww(1) =  (21.d0*g + 117.6d0)/(g*(70.d0 + g))
        ww(2) =  (21.d0*g - 117.6d0)/(g*(70.d0 - g))
        ww(3) =  2.d0*(1.d0 - ww(1) - ww(2))
        ww(4) =  ww(2)
        ww(5) =  ww(1)

        i = 0
        do j = 1,5
          do k = 1,5
            i = i + 1
            sg(1,i) = ss(k)
            sg(2,i) = ss(j)
            sg(3,i) = ww(j)*ww(k)
          end do
        end do

      endif

      end subroutine int2d

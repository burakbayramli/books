!$Id:$
      subroutine quadr2d(d,stiff)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: 2-D Quadrature formulae

!      Inputs:
!         d(*)      - Element parameters
!         stiff     - Flag for stiffness or mass order

!      Outputs:
!         lint      - Number of quadrature points
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'debugs.h'
      include   'eldata.h'
      include   'pmod2d.h'
      include   'pconstant.h'
      include   'qudshp.h'
      include   'sdata.h'
      include   'vem_data.h'

      include   'pointer.h'
      include   'comblk.h'

      logical       :: stiff
      integer       :: l
      real (kind=8) :: d(*)

      save

!     Compute Gauss quadrature points and weights for 2-d elements

      quad   = .false.
      vemfl  = .false.

      if(nint(d(189)).eq.8) then        ! VEM 2-d elements

        vemfl = .true.

        if(stiff) then
          call vem_proj2d(hr(np(44)), 3)
        else
          call vem_proj2d(hr(np(44)), 5)
        endif

        lint = ltot

      elseif(nel.eq.3) then             ! 3-node triangle
        if(d(182).gt.0.0d0) then
          call tint2dn(nel,lint,el2)
        else
          if(stype.le.2 .and. stiff) then
            l =  1
          else
            l = -3
          endif
          call tint2d (l,lint,el2)
        endif
        npm  = 1
        nvn  = 3
      elseif(nel.eq.6 .or. nel.eq.7 ) then
        if(d(182).gt.0.0d0) then
          call tint2dn(nel,lint,el2)
        else
          if(stiff) then
            l = 7 ! -6 ! 3
          else
            l = 7 ! -6 ! 7
          endif
          call tint2d (l,lint,el2)
        endif
        if(nel.eq.6) then           ! 6-node triangle
          npm = 1
        else                        ! 7-node triangle
          npm = 3
        endif
        nvn =  3
      else                          ! Quadrilaterals
        l = min(5,nint(d(5)))
        if(nel.le.4) then           ! 4-node quadrilateral
          npm = 1
          if(l.eq.0) l = 2
        elseif(nel.le.9) then       ! 8 & 9-node quadrilateral
          npm = 3
          if(l.eq.0) l = 3
        else                        ! 16-node quadrilateral
          npm = 6
          if(l.eq.0) l = 4
        endif
        nvn =  4
        if(nint(d(182)).gt.0) then
          call int2dn(nel,lint,sg2)
        else
          call int2d(l,lint,sg2)
        endif
        quad = .true.
      endif

      end subroutine quadr2d

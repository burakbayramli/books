!$Id:$
      subroutine uplagm(du,ulagr,lagbc,ie,ix,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Accumlate Lagrange multiplier unknowns

!      Inputs:
!         du(*)      - Increment to solution
!         lagbc(*)   - Lagrange multiplier boundary conditions
!         ie(nie,*)  - Element group control data
!         ix(nen1,*) - Element connection data
!         isw        - Update: 1=initialize, 2=update, 3=back up step

!      Outputs:
!         ulagr(ndl,3,*) - Lagrange multiplier values
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'gltran.h'
      include   'sdata.h'
      include   'iofile.h'

      integer       :: isw, i, m,ma,mm, n
      integer       :: lagbc(ndl,numel)
      integer       :: ie(nie,*),ix(nen1,*)
      real (kind=8) :: ulagr(ndl,3,numel),du(*)

      save

      select case (isw)

!     Initialize
      case(1)

        do n = 1,numel
          ma = ix(nen1,n)
          if(ma.gt.0) then
            do m = 1,nummat
              if(ie(nie-2,m).eq.ma) then
                do i = 1,ie(nie-8,m)
                  ulagr(i,2,n) = 0.0d0
                  ulagr(i,3,n) = 0.0d0
                end do ! i
              endif
            end do ! m
          endif
        end do ! n

!     Update
      case(2)

!       Loop over elements
        do n = 1,numel
          ma = ix(nen1,n)
          if(ma.gt.0) then
            do m = 1,nummat
              if(ie(nie-2,m).eq.ma) then
                do i = 1,ie(nie-8,m)
!                 Check for active equation
                  mm = lagbc(i,n)
                  if(mm.gt.0) then
                    ulagr(i,1,n) = ulagr(i,1,n) + du(mm)*gtan(1)
                    ulagr(i,2,n) = ulagr(i,2,n) + du(mm)*gtan(1)
                    ulagr(i,3,n) = du(mm)*gtan(1)
                  endif
                end do ! i
              endif
            end do ! m
          endif
        end do ! n

!     Back up solution
      case(3)

        do n = 1,numel
          ma = ix(nen1,n)
          if(ma.gt.0) then
            do m = 1,nummat
              if(ie(nie-2,m).eq.ma) then
                do i = 1,ie(nie-8,m)
                  ulagr(i,1,n) = ulagr(i,1,n) - ulagr(i,2,n)
                  ulagr(i,2,n) = 0.0d0
                  ulagr(i,3,n) = 0.0d0
                end do ! i
              endif
            end do ! m
          endif
        end do ! n

      end select

      end subroutine uplagm

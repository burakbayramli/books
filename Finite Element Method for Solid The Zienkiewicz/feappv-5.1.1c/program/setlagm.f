!$Id:$
      subroutine setlagm(lagbc,ix, ie)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set element lagrange multiplier equations

!      Inputs:
!        ix(nen1,*) - Element connection array
!        ie(nie,*)  - Element control data

!      Outputs:
!        lagbc(*)   - Lagrange multiplier equation numbers
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'sdata.h'

      integer :: m,ma, n,i, nlag
      integer :: lagbc(ndl,numel,2),ix(nen1,*), ie(nie,*)

      save

      nlag       = 0
      do n = 1,numel
        ma = ix(nen1,n)
        if(ma.gt.0) then
          do m = 1,nummat
            if(ie(nie-2,m).eq.ma) then
              if(ie(nie-8,m).gt.0) then
                ix(nen+4,n) = n
                do i = 1,ie(nie-8,m)
                  if(lagbc(i,n,2).eq.0) then
                    nlag         = nlag + 1
                    lagbc(i,n,1) = nlag
                  else
                    lagbc(i,n,1) = 0
                  endif
                end do ! i
              endif
            endif
          end do ! m
        endif
      end do ! n

!     Add to number of equations
      neq = neq + nlag

      end subroutine setlagm

      logical function setlagf(ix,ie)

      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'sdata.h'

      integer        :: n, m,ma
      integer        :: ix(nen1,*),ie(nie,*)

      save

!     Test for lagrange multipliers in elements
      setlagf = .false.
      do n = 1,numel
        ma = ix(nen1,n)
        if(ma.gt.0) then
          do m = 1,nummat
            if(ie(nie-2,m).eq.ma) then

!             Check for number of multiplers
              if(ie(nie-8,m).gt.0) then
                setlagf = .true.
                return
              endif
            endif
          end do ! m
        endif
      end do ! n

      end function setlagf

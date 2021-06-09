!$Id:$
      subroutine setndl(ix, ie)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set maximum number element equations

!      Inputs:
!        ix(nen1,*) - Element connection array
!        ie(nie,*)  - Element control data

!      Outputs:
!        ndl        - Size of equation arrays
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'debugs.h'
      include   'cdata.h'
      include   'cdat1.h'
      include   'sdata.h'

      integer :: m,ma, n, ilagm
      integer :: ix(nen1,*), ie(nie,*)

      save

      ndl = 0
      do n = 1,numel
        ilagm = 0
        ma = ix(nen1,n)
        if(ma.gt.0) then
          do m = 1,nummat
            if(ie(nie-2,m).eq.ma) then
              ilagm = ilagm + ie(nie-8,m)
              ndl   = max(ndl,ilagm)
            endif
          end do ! m
        endif
      end do ! n

      if(debug) write(*,*) 'NDL =',ndl

      end subroutine setndl

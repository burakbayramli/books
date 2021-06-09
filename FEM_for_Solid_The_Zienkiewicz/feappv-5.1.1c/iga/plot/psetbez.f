!$Id:$
      subroutine psetbez(xb,wb, xbez,wbez, ixbez, nll,nd_bez,nmat,
     &                   ne_bez,ma)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute Bezier mesh connections from T-spline inputs

!      Inputs:
!         xb(ndm,*)  -
!         wb(*)      -
!         nd_bez     -
!         ne_bez     = Number of element

!      Working
!         nmat(*)    - Mark material number for nodes

!      Outputs:
!         xbez(ndm,*)-
!         wbez(*)    -
!         ixbez(*,*) - Bezier connection data
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'sdata.h'

      integer    ixbez(nnpl+1,*), nmat(*), nll
      real*8     xb(ndm,*),wb(*), xbez(ndm,*), wbez(*)

      logical    pflag
      integer    ma, nd_bez, nn, i,ne_bez
      real*8     ptol, xb3

      data       ptol / 1.d-6 /

      if(ndm.eq.2) then
        xb3   = 0.0d0
      endif

      do i = 1,nll
        pflag = .true.
        do nn = 1,numpln
          if(nmat(nn).eq.ma) then
            if(ndm.eq.3) then
              xb3 = abs(xb(3,i) - xbez(3,nn))
            endif
            if(max(abs(xb(1,i)-xbez(1,nn)),
     &             abs(xb(2,i)-xbez(2,nn)),xb3) .lt. ptol) then
              pflag = .false.
              exit
            endif
          endif
        end do ! nn

        if(pflag) then
          numpln = numpln + 1
          if(numpln.gt.nd_bez) then
            write(*,*) ' Number of computed Bezier points too large'
            write(*,*) ' NUMPLN = ',numpln,' ND_BEZ =',nd_bez,
     &                 ' ELEMENT = ',ne_bez
            call plstop(.true.)
          endif
          do nn = 1,ndm
            xbez(nn,numpln) = xb(nn,i)
          end do ! nn
          wbez(numpln) = wb(i)
          ixbez(i,ne_bez)   = numpln
          nmat(numpln)      = ma
        else
          ixbez(i,ne_bez)   = nn
        endif
      end do ! i
      ixbez(nnpl+1,ne_bez)  = ma

      end

!$Id:$
      subroutine plknots(lkdim,lknots,ii,nurnp,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Local knot vector inputs

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters
!         prt       - Flag, output if true

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'
      include   'print.h'

      integer    ii,nurnp,ndm, lkdim(*)
      real*8     lknots(ii,nurnp)

      character  fmt1*36, fmt2*29
      logical    errck, pinput
      integer    node, n,i
      real*8     td(16)

      data       fmt1 /'(4x,a4,4(i4,a5,3x)/(8x,4(i4,a5,3x)))' /
      data       fmt2 /'(i8,1p,4e12.4/(8x,1p,4e12.4))' /

      write(fmt1( 8: 8),'(i1)') lkdim(1)+2
      write(fmt2( 8: 8),'(i1)') lkdim(1)+2
      write(fmt1(24:24),'(i1)') lkdim(2)+2
      write(fmt2(22:22),'(i1)') lkdim(2)+2

      call prtitl(prth)
      write(iow,fmt1) 'Node',((i,'-Knot',i=1,lkdim(n)+2),n=1,ndm)
      do n = 1,nurnp
        errck = pinput(td,min(ii+1,16))
        node  = nint(td(1))
        do i = 1,min(15,ii)
          lknots(i,node) = td(i+1)
        end do ! i
        if(ii.gt.15) then
          errck = pinput(td,16)
          do i = 16,min(31,ii)
            lknots(i,node) = td(i-15)
          end do ! i
          if(ii.gt.31) then
            errck = pinput(td,16)
            do i = 32,min(47,ii)
              lknots(i,node) = td(i-31)
            end do ! i
          endif
        endif
        write(iow,fmt2) node,(lknots(i,node),i=1,ii)
      end do ! n

      end

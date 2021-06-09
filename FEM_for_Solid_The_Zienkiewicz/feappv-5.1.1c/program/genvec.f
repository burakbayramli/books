!$Id:$
      subroutine genvec(ndm,xdm,x,carg,prt,prth,err,prtz)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Generate real data arrays by linear interpolation

!      Inputs:
!         ndm       - Number of data items/node
!         xdm       - Dimension of x array
!         carg(*)   - Header identifier
!         prt       - Print generated data if true
!         prth      - Print title/header data if true
!         prtz      - Print zero data if true

!      Outputs:
!         x(xdm,*)  - Generated data
!         err       - Error flag, true if error occurs
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cblktr.h'
      include  'cdata.h'
      include  'dstars.h'
      include  'iofile.h'
      include  'trdata.h'

      include  'pointer.h'
      include  'comblk.h'

      character (len=15) :: cd
      character          :: carg*(*)

      logical       :: prt,prth,prtz,prv,prx,err
      logical       :: errck, pinput, pcomp, norec
      integer       :: i,j,l,n, ii,il,is, lg,ng,ndm,xdm,mct, nmn,nmx
      real (kind=8) :: xli, x(xdm,*),xl(50),xs(50),td(16)

      save

      norec = .true.
      err = .false.
      cd  = carg
      prv = pcomp(cd,' coo',4)
      mct = 0
      nmn = numnp
      nmx = 0
      n   = 0
      ng  = 0
100   l   = n
      lg  = ng

!     Call input routine - values returned in td and then moved

1001  if(ior.lt.0) then
        write(*,2010)
      endif
      il = min(ndm+2,16)
      errck = pinput(td,il)
      if(errck) go to 1001
      n  = nint(td(1))
      ng = nint(td(2))
      do j = 1,min(ndm,14)
        xs(j) = td(j+2)
      end do ! j
      if(ndm.gt.14 .and. n.gt.0) then
        do ii = 1,(ndm+2)/16
          is = il+1
          il = min(is+15,ndm+2)
101       errck = pinput(td,il-is+1)
          if(errck) go to 101
          do j = 1,il-is+1
            xs(j+is-3) = td(j)
          end do ! j
        end do ! ii
      endif
      if(n.gt.0) then
        n = n + starnd
      endif
      if(n.gt.numnp) then
        write(iow,3001) n,cd
        if(ior.lt.0) then
          write(*,3001) n,cd
        endif
      endif
      if(n.le.0.or.n.gt.numnp) go to 104
      norec = .false.

!     Duplicate node input on succeeding records

      if(n.eq.l) then
        if(ior.lt.0) then
          write(*,3002) n
          go to 1001
        endif
        write(iow,3002) n,cd
        call plstop(.true.)
      endif

      nmn = min(n,nmn)
      nmx = max(n,nmx)
      if(ndm.gt.1) then
        if(prv) then
          do i = 1,ndm
            xl(i) = xr(i)
          end do ! i
        else
          do i = 1,ndm
            xl(i) = 0.0d0
          end do ! i
        endif
        do i = 1,min(3,ndm)
          do j = 1,min(3,ndm)
            xl(i) = xl(i) + tr(i,j)*xs(j)
          end do ! j
        end do ! i
        do i = 4,ndm
          xl(i) = xs(i)
        end do ! i
      else
        xl(1) = xs(1)
      endif
      do i = 1,ndm
        x(i,n) = xl(i)
      end do ! i
      if(prv) then
        mr(np(190)+n-1) = 0
        nio       = max(nio,n)
      endif
      if(lg.ne.0) then
        lg  =  sign(lg,n-l)
        xli = (abs(n-l+lg)-1)/abs(lg)
        do i = 1,ndm
          xl(i) = (x(i,n)-x(i,l))/xli
        end do ! i
102     l = l + lg
        if((n-l)*lg.le.0) go to 100
        if(l.le.0.or.l.gt.numnp) go to 103
        do i = 1,ndm
          x(i,l) = x(i,l-lg) + xl(i)
        end do ! i
        if(prv) mr(np(190)+l-1) = 0
        go to 102
103     write(iow,3000) l,cd
        if(ior.lt.0) write(  *,3000) l,cd
        err = .true.
      endif
      go to 100

!     Output data

104   if(norec) then
        write(  *,4001) cd(1:5)
        write(iow,4001) cd(1:5)
        return
      endif
      if(.not.prt) return
      do j = nmn,nmx
        if(prtz) go to 106
        do l = 1,ndm
          if(x(l,j).ne.0.0d+00) go to 106
        end do ! l
        go to 108
106     mct = mct - 1
        if(prv) then
          prx = mr(np(190)+j-1).ge.0
        else
          prx = .true.
        endif
        if(mct.le.0) then
          mct = 50
          call prtitl(prth)
          write(iow,2000) cd,(l,cd,l=1,ndm)
          if(ior.lt.0) then
            write(*,2000) cd,(l,cd,l=1,ndm)
          endif
        endif
        if(prx) write(iow,2009) j,(x(l,j),l=1,ndm)
        if(ior.lt.0) then
          if(prx) write(*,2009) j,(x(l,j),l=1,ndm)
        endif
108     continue
      end do ! j

!     Formats

2000  format(5x,'Nodal',a//6x,'node',6(i5,a6)/(10x,6(i5,a6)))

2009  format(i10,1p,6e11.3:/(10x,1p,6e11.3))

2010  format(' Input: node#, inc., values'/3x,'>',$)

3000  format(' *ERROR* GENVEC: Attempt to generate node',i5,' in ',a)

3001  format(' *ERROR* GENVEC: Attempt to input node',i5,', terminate',
     &       ' input of nodes in ',a)

3002  format(' *ERROR* GENVEC: Attempt to input duplicate node',i5:,
     &       ', terminate input of nodes in ',a)

4001  format(' *WARNING* GENVEC: No data found for a ->',a5,
     &       ' <- data set.')

      end subroutine genvec

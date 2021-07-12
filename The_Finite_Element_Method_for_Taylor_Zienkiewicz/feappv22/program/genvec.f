c$Id:$
      subroutine genvec(ndm,xdm,x,carg,prt,prth,err,prtz)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Generate real data arrays by linear interpolation

c      Inputs:
c         ndm       - Number of data items/node
c         xdm       - Dimension of x array
c         carg(*)   - Header identifier
c         prt       - Print generated data if true
c         prth      - Print title/header data if true
c         prtz      - Print zero data if true

c      Outputs:
c         x(ndm,*)  - Generated data
c         err       - Error flag, true if error occurs
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cblktr.h'
      include  'cdata.h'
      include  'iofile.h'
      include  'trdata.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   prt,prth,prtz,prv,prx,err,errck, pinput, pcomp
      character cd*15, carg*(*)
      integer   i,j,l,n, ii,il,is, lg,ng,ndm,xdm,mct
      integer   nmn,nmx
      integer   nty
      real*8    xli

      real*8    x(xdm,*),xl(50),xs(50),td(16)

      save

      err = .false.
      cd  = carg
      prv = pcomp(cd,' coo',4)
      mct = 0
      nmn = numnp
      nmx = 0
      nty = np(49) - 1
      n   = 0
      ng  = 0
100   l   = n
      lg  = ng

c     Call input routine - values returned in td and then moved

1001  if(ior.lt.0) write(*,2010)
      il = min(ndm+2,16)
      errck = pinput(td,il)
      if(errck) go to 1001
      n  = td(1)
      ng = td(2)
      do j = 1,min(ndm,14)
        xs(j) = td(j+2)
      end do
      if(ndm.gt.14 .and. n.gt.0) then
        do ii = 1,(ndm+2)/16
          is = il+1
          il = min(is+15,ndm+2)
1002      errck = pinput(td,il-is+1)
          if(errck) go to 1002
          do j = 1,il-is+1
            xs(j+is-3) = td(j)
          end do
        end do
      endif
      if(n.gt.numnp) then
        write(iow,3001) n,cd
        if(ior.lt.0) then
          write(*,3001) n,cd
        endif
      endif
      if(n.le.0.or.n.gt.numnp) go to 105

c     Duplicate node input on succeeding records

      if(n.eq.l) then
        if(ior.lt.0) then
          write(*,3002) n
          go to 1001
        endif
        write(iow,3002) n,cd
        call plstop()
      endif

      nmn = min(n,nmn)
      nmx = max(n,nmx)
      if(ndm.gt.1) then
        if(prv) then
          do i = 1,ndm
            xl(i) = xr(i)
          end do
        else
          do i = 1,ndm
            xl(i) = 0.0d0
          end do
        endif
        do i = 1,min(3,ndm)
          do j = 1,min(3,ndm)
            xl(i) = xl(i) + tr(i,j)*xs(j)
          end do
        end do
        do i = 4,ndm
          xl(i) = xs(i)
        end do
      else
        xl(1) = xs(1)
      endif
      do i = 1,ndm
        x(i,n) = xl(i)
      end do
      if(prv) then
        mr(nty+n) = 0
        nio       = max(nio,n)
      endif
      if(lg) 102,100,102
102   lg  =  sign(lg,n-l)
      xli = (abs(n-l+lg)-1)/abs(lg)
      do i = 1,ndm
        xl(i) = (x(i,n)-x(i,l))/xli
      end do
103   l = l + lg
      if((n-l)*lg.le.0) go to 100
      if(l.le.0.or.l.gt.numnp) go to 104
      do i = 1,ndm
        x(i,l) = x(i,l-lg) + xl(i)
      end do
      if(prv) mr(nty+l) = 0
      go to 103
104   write(iow,3000) l,cd
      if(ior.lt.0) write(  *,3000) l,cd
      err = .true.
      go to 100
105   if(.not.prt) return
      do j = nmn,nmx
        if(prtz) go to 106
        do l = 1,ndm
          if(x(l,j).ne.0.0d+00) go to 106
        end do
        go to 108
106     mct = mct - 1
        if(prv) then
          prx = mr(nty+j).ge.0
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
      end do

c     Formats

2000  format(5x,'Nodal',a//6x,'node',6(i5,a6)/(10x,6(i5,a6)))

2009  format(i10,1p,6e11.3:/(10x,1p,6e11.3))

2010  format(' Input: node#, inc., values'/3x,'>',$)

3000  format(' *ERROR* Attempt to generate node',i5,' in ',a)

3001  format(' *ERROR* Attempt to input node',i5,', terminate',
     &       ' input of nodes in ',a)

3002  format(' *ERROR* Attempt to input duplicate node',i5:,
     &       ', terminate input of nodes in ',a)


      end

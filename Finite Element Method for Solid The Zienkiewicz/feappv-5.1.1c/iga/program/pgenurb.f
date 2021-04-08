!$Id:$
      subroutine pgenurb(ndm,xdm,x,nurb,carg,prt,prth,err,prtz,nwtfl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Generate real nurb arrays by linear interpolation

!      Inputs:
!         ndm       - Number of data items/node
!         xdm       - Dimension of x array
!         carg(*)   - Header identifier
!         prt       - Print generated data if true
!         prth      - Print title/header data if true
!         prtz      - Print zero data if true
!         nwtfl     - Inputs from NURBS_mesh file if true

!      Outputs:
!         x(xdm,*)  - Generated nurb coordinates
!         nurv(*)   - Lift value for nurb coordinates
!         err       - Error flag, true if error occurs
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cblktr.h'
      include  'cdata.h'
      include  'cnurb.h'
      include  'dstars.h'
      include  'iofile.h'
      include  'trdata.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   prt,prth,prtz,prv,prx,err,errck, pinput, pcomp,nwtfl
      character cd*15, carg*(*), wt*15, lnum(3)*5
      integer   i,j,l,n, ii,il,is, lg,ng,ndm,xdm,mct, nmn,nmx
      real*8    xli, x(xdm,*),nurb(*), xl(50),xs(50),td(16)

      save

      data      wt / 'Weight.' /

      data      lnum/'    1','    2','    3'/
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
        call pprint('   >')
      endif
      if(prv .and. ndm.le.3 .and. nwtfl) then
        read(ior,*) n,ng,(xs(j),j=1,ndm+1)
      else
        il = min(ndm+3,16)
        errck = pinput(td,il)
        if(errck) go to 1001
        n  = nint(td(1))
        ng = nint(td(2))
        do j = 1,min(ndm+1,14)
          xs(j) = td(j+2)
        end do ! j
        if(ndm.gt.14 .and. n.gt.0) then
          do ii = 1,(ndm+3)/16
            is = il+1
            il = min(is+15,ndm+3)
101         errck = pinput(td,il-is+1)
            if(errck) go to 101
            do j = 1,il-is+1
              xs(j+is-3) = td(j)
            end do ! j
          end do ! ii
        endif
      endif
      if(n.gt.0) then
        n = n + starnd
      endif
      if(n.gt.nurnp) then
        write(iow,3001) n,cd
        if(ior.lt.0) then
          write(*,3001) n,cd
        endif
      endif
      if(n.le.0.or.n.gt.nurnp) go to 104

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
          do i = 1,min(3,ndm)
            xl(i) = xr(i)
          end do ! i
          xl(ndm+1) = xs(ndm+1)
        else
          do i = 1,ndm
            xl(i) = 0.0d0
          end do ! i
          xl(ndm+1) = 1.0d0
        endif
        do i = 1,ndm
          do j = 1,ndm
            xl(i) = xl(i) + tr(i,j)*xs(j)
          end do ! j
        end do ! i
        xl(ndm+1) = xs(ndm+1)
      else
        xl(1) = xs(1)
        xl(2) = 1.0d0
      endif
      do i = 1,ndm
        x(i,n) = xl(i)
      end do ! i
      nurb(n) = xl(ndm+1)
      if(nurb(n).le.0.0d0) then
        nurb(n) = 1.0d0
        write(iow,3003) n
        write(  *,3003) n
      endif
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
        if(l.le.0.or.l.gt.nurnp) go to 103
        do i = 1,ndm
          x(i,l) = x(i,l-lg) + xl(i)
        end do ! i
        if(nurb(l).le.0.0d0) then
          nurb(l) = 1.0d0
        endif
        if(prv) mr(np(190)+l-1) = 0
        go to 102
103     write(iow,3000) l,cd
        if(ior.lt.0) write(  *,3000) l,cd
        err = .true.
      endif
      go to 100
104   if(.not.prt) return
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
          if(prt) call prtitl(prth)
          write(iow,2000) cd,(lnum(l),cd,l=1,ndm),'     ',wt
          if(ior.lt.0) then
            write(*,2000) cd,(lnum(l),cd,l=1,ndm),1,wt
          endif
        endif
        if(prx) then
          write(iow,2009) j,(x(l,j),l=1,ndm),nurb(j)
          if(ior.lt.0) then
            write(*,2009) j,(x(l,j),l=1,ndm),nurb(j)
          endif
        endif
108     continue
      end do ! j

!     Formats

2000  format(3x,'Nurb',a//4x,'node',6(a5,a6,'.'))

2009  format(i8,1p,6e12.4:/(8x,1p,6e12.4))

2010  format(' Input: node#, inc., values')

3000  format(' *ERROR* PGENURB: Attempt to generate node',i8,' in ',a)

3001  format(' *ERROR* PGENURB: Attempt to input node',i8,', terminate',
     &       ' input of nodes in ',a)

3002  format(' *ERROR* PGENURB: Attempt to input duplicate node',i8:,
     &       ', terminate input of nodes in ',a)

3003  format(' *WARNING* PGENURB: NURBS weight for node',i7,' not',
     &       ' input. Set to 1.0')

      end

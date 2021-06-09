!$Id:$
      subroutine blktem(ndm,t,prt,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:    Generate block of nodal temperatures in 't' array.

!      Inputs:
!         ndm    - Space dimension of mesh
!         prt    - Print generated data if true
!         prth   - Print headers if true

!      Outputs:
!         t(*)   - Generated temperatures on block
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'iofile.h'

      logical       :: prt,prth, errck, pinput
      integer       :: ndm,l,n,nn,nr,ns,nt,ni,nm,ng,nodinc
      real (kind=8) :: dr,ds,dt
      integer       :: ixl(27)
      real (kind=8) :: t(*),tl(27),td(7)

      save

!     Block mesh generation routine

100   if(ior.lt.0) write(*,1000)
      errck = pinput(td,7)
      if(errck) go to 100
      nn     = nint(td(1))
      nr     = nint(td(2))
      ns     = nint(td(3))
      nt     = nint(td(4))
      ni     = nint(td(5))
      nodinc = nint(td(6))

!     2-d generations

      if(ndm.lt.3) then
        nt     = 1
      endif

!     Reset to default values if necessary

      nodinc = max(nodinc,0)
      nr     = max(nr,1)
      ns     = max(ns,1)
      nt     = max(nt,1)
      ni     = max(ni,1)
      if(prt) then
        call prtitl(prth)
        write(iow,2000) nr,ns,nt,ni,nodinc
        write(iow,2002)
        if(ior.lt.0) then
          write(*,2000) nr,ns,nt,ni,nodinc
          write(*,2002)
        endif
      endif
      do n = 1,27
        tl(n) = 0.0
        ixl(n) = 0
      end do
      nm = 0
      do n = 1,nn
21      if(ior.lt.0) write(*,1001)
        errck = pinput(td,2)
        if(errck) go to 21
        l = nint(td(1))
        if(l.eq.0) l = n
        nm = max0(nm,l)
        ixl(l)  = l
        tl(l) = td(2)
        if(prt.and.ior.gt.0) write(iow,2001) l,tl(l)
        if(prt.and.ior.lt.0) write(*,2001) l,tl(l)
      end do

!     Set generation increments of natural coordinates

      dr = 2.d0/nr
      ds = 2.d0/ns
      dt = 2.d0/nt
      nr = nr + 1
      ns = ns + 1

!     Determine last node number to be generated

      if(ndm.lt.3) then
        if(ndm.eq.1) ns = 1
        ng = nr*ns + ni -1
      else
        nt = nt + 1
        ng = nr*ns*nt + ni -1
      endif
      if(ng.gt.numnp) go to 400

!     Form block of temperatures

      call tblk(nr,ns,nt,tl,t,ixl,dr,ds,dt,ni,ndm,nodinc,nm,prt,prth)
      return

!     Error messages

400   if(ior.gt.0) write(iow,3001) ng,numnp
      if(ior.lt.0) write(  *,3001) ng,numnp

!     Input prompts

1000  format(' Input: nn,nr,ns,nt,ni,nodinc'/3x,'>',$)
1001  format(' Input: node, Temp'/3x,'>',$)

!     Output Formats

2000  format('   T e m p e r a t u r e    G e n e r a t i o n s'//
     &   10x,'Number of r-increments    ',i5/
     &   10x,'Number of s-increments    ',i5/
     &   10x,'Number of t-increments    ',i5/
     &   10x,'First node number         ',i5/
     &   10x,'Node line increment       ',i5/1x)
2001  format(i9,1p,3e12.3)
2002  format(5x,'node',6x,' Temp.')

!     Error format

3001  format(' *ERROR* Insufficient storage for temperatures'/
     1   10x,'final node =',i5,5x,'numnp =',i5)

      end subroutine blktem

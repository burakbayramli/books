!$Id:$
      subroutine blkgen(ndm,nen1,x,ix,prt,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Generate a block of elements and nodes for mesh
!               descriptions.

!      Inputs:
!         ndm        - Dimension of 'x' array
!         nen1       - Dimension for 'ix' array
!         prt        - Print generated data if true
!         prth       - Print headers if true

!      Outputs:
!         x(ndm,*)   - Block of nodal coordinates
!         ix(nen1,*) - Block of elements

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cblktr.h'
      include  'cdata.h'
      include  'crotas.h'
      include  'iofile.h'
      include  'pointer.h'
      include  'region.h'
      include  'comblk.h'

      character (len=15) :: ctype,layer
      character (len=6)  :: xh
      character (len=5)  :: etype, pelabl

      logical       :: prt,prth,pcomp,errck,pinput,tinput,palloc
      logical       :: eltype, pblktyp
      integer       :: i,j,k,l,n,nm,nn,nr,ns,nt,nf,ng,ni,ntyp,nodinc
      integer       :: ndm,nen1,ne,ma,mab, dlayer,nlay
      real (kind=8) :: dr,ds,dt

      integer       :: ix(nen1,*)
      integer       :: ixl(27)
      real (kind=8) :: x(ndm,*),xl(3,27),shp(3,9),tb(7),tc(4),td(15)

      integer       :: nel

      save

      data      xh/' coord'/

!     Block mesh generation routine

100   if(ior.lt.0) then
        if(ndm.le.2) write(*,5000)
        if(ndm.ge.3) write(*,5001)
      endif
      errck = tinput(ctype,1,tb,7)
      if(errck) go to 100

!     Check for form of coordinate system

      if(.not.pcomp(ctype,'pola',4) .and.
     &   .not.pcomp(ctype,'cyli',4) .and.
     &   .not.pcomp(ctype,'sphe',4) .and.
     &   .not.pcomp(ctype,'surf',4)) then
        ctype = 'cartesian'
      endif

!     Set parameters for block

      nr     = nint(tb(1))
      ns     = nint(tb(2))
      nt     = nint(tb(3))
      ntyp   = nint(tb(7))
      do n = 1,27
        ixl(n) = 0
        do j = 1,ndm
          xl(j,n) = 0.0
        end do ! j
      end do ! n
      nn     = 0   ! Number of block nodes
      nm     = 0   ! Number of block nodes
      nlay   = 0   ! Number of material layers in block
      dlayer = 0   ! Direction of layering
      netyp  = 0   ! Default undefined element type

!     Check for layered material properties

110   errck = tinput(layer,1,td,3)
      if(errck) go to 110

      if(pcomp(layer,'laye',4)) then
        dlayer = nint(td(1))
        if(dlayer.eq.1) then
          nlay = nr
        elseif(dlayer.eq.2) then
          nlay = ns
        elseif(dlayer.eq.3) then
          nlay = nt
        endif
        errck = palloc(111,'TEMP1',nlay,1)

        j = 1
        do while(j.le.nlay)
120       errck = tinput(layer,0,td,15)
          if(errck) go to 120
          do i = j,min(j+15,nlay)
            mr(np(111)-1+i) = nint(td(i-j+1))
          end do ! i
          j = j + 16
        end do ! while
        go to 110

!     Data is a block coordinate

      else
        mab    = 0
        eltype = .false.
        do while (.not.eltype)
          eltype = pblktyp(layer,td, ntyp,ns,mab)
          if(.not.eltype) then
            errck = tinput(layer,1,td,5)
          endif
        end do !
        if(eltype) then
          call setval(layer,15,tc(1))
          l  = nint(tc(1))
          if(l.eq.0 ) go to 22
          if(l.gt.27) go to 402
          nm = max(nm,l)
          nn = nn + 1
          ixl(l)  = l
          xl(1,l) = td(1)
          xl(2,l) = td(2)
          xl(3,l) = td(3)
        else
          go to 110  ! Input another record
        endif
      end if ! no layers

!     Input block coordinates

21    if(ior.lt.0) write(*,5002)
        errck = pinput(tc,4)
        if(errck) go to 21
        l = nint(tc(1))
        if(l.eq.0 ) go to 22
        if(l.gt.27) go to 402
        nm = max(nm,l)
        nn = nn + 1
        ixl(l)  = l
        xl(1,l) = tc(2)
        xl(2,l) = tc(3)
        xl(3,l) = tc(4)
      go to 21

!     Set a default value for unspecified block type

22    if(ntyp.eq.0 .and. nt.gt.0 .and. ndm.eq.3 .and. ixl(8).ne.0) then
        ntyp = 10
      endif

      if(pcomp(ctype,'surf',4) .and. ntyp.eq.10) then
        ntyp = 0
      endif

!     2-d generations

      if(ntyp.lt.10) then
        nt     = 1
        ni     = nint(tb(3))
        ne     = nint(tb(4))
        ma     = nint(tb(5))
        nodinc = nint(tb(6))

!     3-d generations

      elseif(ntyp.lt.20) then
        nt     = nint(tb(3))
        ni     = nint(tb(4))
        ne     = nint(tb(5))
        ma     = nint(tb(6))
        nodinc = 0

!     Shell generations

      elseif(ntyp.lt.30) then

        nt     = 1
        ni     = nint(tb(3))
        ne     = nint(tb(4))
        ma     = nint(tb(5))
        nodinc = nint(tb(6))

!     User generations

      else
        if(ndm.le.2) then
          nt     = 1
          ni     = nint(tb(3))
          ne     = nint(tb(4))
          ma     = nint(tb(5))
          nodinc = nint(tb(6))
        else
          nt     = nint(tb(3))
          ni     = nint(tb(4))
          ne     = nint(tb(5))
          ma     = nint(tb(6))
          nodinc = 0
        endif
      endif

!     Set the final material number

      if(mab.gt.0) then
        ma = mab
      endif

!     Reset to default values if necessary

      if(ni.eq.0) ni = nio + 1
      if(ne.eq.0) ne = neo + 1
      if(ma.eq.0) ma = mao

      ma     = max(ma,1)
      nodinc = max(nodinc,0)
      nr     = max(nr,1)
      ns     = max(ns,1)
      nt     = max(nt,1)
      ni     = max(ni,1)

!     Output block data

      if(prt) then
        call prtitl(prth)
        write(iow,2000) nr,ns,nt,ni,ne,ma,nodinc,ntyp
        if(ne.le.0) write(iow,3000)
        if(ior.lt.0) then
          write(*,2000) nr,ns,nt,ni,ne,ma,nodinc,ntyp
          if(ne.le.0) write(*,3000)
        endif
        if(nlay.gt.0) then
          write(iow,2005) (j,j=1,min(5,nlay))
          write(iow,2006) (j,mr(np(111)-1+j),j=1,nlay)
          if(ior.lt.0) then
            write(*,2005) (j,j=1,min(5,nlay))
            write(*,2006) (j,mr(np(111)-1+j),j=1,nlay)
          endif
        end if ! nlay > 0
        write(iow,2002) ctype,(i,xh,i=1,ndm)
        if(ior.lt.0) then
          write(*,2002) ctype,(i,xh,i=1,ndm)
        endif
        do l = 1,27
          if(ixl(l).gt.0) then
            write(iow,2001) l,(xl(i,l),i=1,ndm)
            if(ior.lt.0) then
              write(*,2001) l,(xl(i,l),i=1,ndm)
            endif
          endif
        end do ! l
      endif

!     Set generation increments of natural coordinates

      dr = 2.d0/nr
      ds = 2.d0/ns

!     Determine last element number to be generated

      if(ntyp.lt.10) then
        if(nn.le.3) then
          ng = ni + nr
          if(ns.eq.1) then
            nf = ne + nr - 1
          else
            nf = ne + nr/2 - 1
          endif
          nr = nr + 1
        else
          if (ntyp.eq.0) then
            nf = ne + nr*ns - 1
          elseif (abs(ntyp).eq.7) then
            nf = ne + (nr*ns)/2 - 1
          elseif (ntyp.ge.8) then
            nf = ne + (nr*ns)/4 - 1
          elseif (ntyp.lt.0) then
            nf = ne + 4*nr*ns - 1
          else
            nf = ne + 2*nr*ns - 1
          endif

!         Determine last node number to be generated

          nr = nr + 1
          ns = ns + 1
          if(ndm.eq.1) ns = 1
          ng = nr*ns + nodinc*(ns-1) + ni -1
          if(ntyp.eq. -7) then
            ng = ng + ((nr-1)*(ns-1))/2
          elseif(ntyp .eq. -1) then
            ng = ng + (nr-1)*(ns-1)
          elseif(ntyp .eq.  8) then
            ng = ng - ((nr-1)*(ns-1))/4
          endif
        endif
        if(nf.gt.numel.and.ne.gt.0) go to 401
        if(ng.gt.numnp) go to 400

!       Generate nodes

        call sblkn(nr,ns,xl,ixl,shp,x,dr,ds,ni,n,ndm,nodinc,ntyp,
     &             nm,ctype,prt,prth)

!       Generate elements

        call sblke(nr,ns,x,ix,ni,ne,n,ndm,nen1,nodinc,ntyp,nm,ma,
     &             dlayer,mr(np(111)),ctype)

!     3-d generations

      elseif(ntyp.lt.20) then
        dt = 2.d0/nt
        if(ntyp.eq.10) then
          nf = ne + nr*ns*nt - 1
        elseif(ntyp.eq.11) then
          nf = ne + 6*nr*ns*nt - 1
        else
          write(iow,4003) ntyp
          call plstop(.true.)
        endif
        if(nf.gt.numel.and.ne.gt.0) go to 401
        nr = nr + 1
        ns = ns + 1
        nt = nt + 1
        ng = nr*ns*nt + ni -1
        if(ng.gt.numnp) go to 400
          call vblkn(nr,ns,nt,xl,x,ixl,dr,ds,dt,
     &               ni,ndm,ctype,prt,prth)

          if(ne.gt.0) then
            call vblke(nr,ns,nt,ix,ni,ne,nf,nen1,ma,ntyp,
     &                 dlayer,mr(np(111)))
          endif

!     User generations

      else
        dt   = 2.d0/nt
        nf   = ne
        ng   = ni
        ntyp = ntyp - 30
        call ublk(ntyp,nn,nr,ns,nt,xl,x,ixl,ix,dr,ds,dt,
     &            ng,nf,ndm,nen1,ma,ctype,prt, 2)
        if(nf.gt.numel.and.ne.gt.0) go to 401
        if(ng.gt.numnp) go to 400
      endif

!     Set old numbers

      if(ne.gt.0) neo = nf
      nio = ng
      mao = ma

!     Set node type to active

      do n = ni,ng
        mr(np(190)-1+n) = 0
      end do ! n

!     Set region number

      do n = ne,nf
        ix(nen1-1,n) = nreg
      end do ! n

      if(nlay.gt.0) then
        errck = palloc(111,'TEMP1',0,1)
      endif

!     Print lists if wanted

      if(prt.and.ne.gt.0) then
        do n = ne,nf,50
          call prtitl(prth)
          write(iow,2003) (i,i=1,nen)
          if(ior.lt.0) then
              write(  *,2003) (i,i=1,nen)
          endif
          j = min(nf,n+49)
          do i = n,j
            ma = ix(nen1,i)
            etype = pelabl(ix(nen+7,i))
            nel = 0
            do k = 1,nen
              if(ix(k,i).gt.0) nel = k
            end do ! k
            write(iow,2004) i,ma,nreg,etype,(ix(k,i),k=1,nel)
            if(ior.lt.0) then
              write(*,2004) i,ma,nreg,etype,(ix(k,i),k=1,nel)
            endif
          end do ! i
        end do ! n
      endif

      return

!     Error messages

400   write(iow,4000) ng,numnp
      if(ior.lt.0) write(  *,4000) ng,numnp
      return

401   write(iow,4001) nf,numel
      if(ior.lt.0) write(  *,4001) nf,numel
      return

402   write(iow,4002) l
      if(ior.lt.0) write(*,4002) l

!     Formats

2000  format('   N o d e   G e n e r a t i o n s'//
     &   10x,'Number of xi_1-increments ',i5/
     &   10x,'Number of xi_2-increments ',i5/
     &   10x,'Number of xi_3-increments ',i5/
     &   10x,'First node number         ',i5/
     &   10x,'First element number      ',i5/
     &   10x,'Element material number   ',i5/
     &   10x,'Node line increment       ',i5/
     &   10x,'Block type (0-10)         ',i5/1x)

2001  format(i9,1p,3e12.3)

2002  format(/5x,'Block coordinates specified as: ',a,//,
     &       5x,'node',3(i6,a6))

2003  format('   E l e m e n t   C o n n e c t i o n s'//
     &   '   Elmt Mat Reg  Type',7(i3,'-node'):/(21x,7(i3,'-node')))

2004  format(i7,2i4,1x,a5,7i8:/(21x,7i8))

2005  format(/5x,'Layered Material Properties'/
     &      /(7x,4(i2,'-Layer Matl')))

2006  format(7x,4(i8,i5))

3000  format(' *WARNING* No elements are generated ')

4000  format(' *ERROR* Insufficient storage for nodes'/
     &   10x,'final node =',i5,5x,'numnp =',i5)

4001  format(' *ERROR* Insufficient storage for elements'/
     &   10x,'final element =',i5,5x,'numel =',i5)

4002  format(' *ERROR* Block node has number > 27.  No. =',i8)

4003  format(' *ERROR* Block type incorrect: Ntype =',i8)

5000  format(' Input: type,nr,ns,ni,ne,ma,nodinc,ntyp'/3x,'>',$)

5001  format(' Input: type,nr,ns,nt,ni,ne,ma,ntyp'/3x,'>',$)

5002  format(' Input: node, x-1, x-2, x-3'/3x,'>',$)

      end subroutine blkgen

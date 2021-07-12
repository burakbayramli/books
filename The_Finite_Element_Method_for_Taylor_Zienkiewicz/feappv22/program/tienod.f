c$Id:$
      subroutine tienod(ix,x,ip,ir,ibuck,ib,ndm,nen,nen1,
     &                  numnp,numel,n1,n2,r1,r2,nt,td)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Procedure to connect nodes which have same coordinates.

c      Inputs:
c         ix(nen1,*)  - Element nodal connection list
c         x(ndm,*)    - Nodal coordinates for mesh
c         ip(*)       - Node number list
c         ir(*)       -
c         ibuck(*)    - Array for bucket numbers
c         ib(*)       -
c         ndm         - Spatial dimension of mesh
c         nen         - Number of nodes/element
c         nen1        - Dimension of ix array
c         numnp       - Number of nodes in mesh
c         numel       - Number of elements in mesh
c         n1          - First  node number for search (Node option)
c         n2          - Second node number for search (Node option)
c         r1          - First  region number for tie (Region option)
c         r2          - Second region number for tie (Region option)
c         nt          - Coordinate directrion (Coordinate option)
c         xt          - Coordinate value      (Coordinate option)

c      Outputs:
c         ix(nen1,*)  - Element nodal connection list with tied node
c                       eliminated
c         x(ndm,*)    - Nodal coordinates for mesh with tied node
c                       eliminated
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   fflg
      integer   ndm,nen,nen1,numnp,numel,nr,nt
      integer   i,j,k,l,ii,ji,ki,jf,kf
      integer   i1,j1,k1,k2,ni,nn,nm,n1,n2,r1,r2,list,nbuck,nty

      real*8    xt, snx,sny,snz

      integer   ix(nen1,*),ip(numnp),ir(numnp)
      integer   ibuck(numnp),ib(numnp),nsize(4)
      integer   ibox(0:1000),jbox(1000),lbuck(8)

      real*8    xmd(3), xmx(3), xmn(3), xtol(3)
      real*8    x(ndm,numnp),tol(3),td(*)

      save

c     Set search coordinate and tolerance to tie nodes

      xt = td(1)
      if(td(2).gt.0.0d0) then
        snx = td(2)
      else
        snx   = 1.d-3/sqrt(dble(max(1,numnp)))
      endif
      write(iow,2000) snx

c     Determine maximum number of buckets in any direction

      nsize(4) = dble(numnp*ndm/10)**(1.d0/3.d0)
      nsize(4) = max(1,min(nsize(4),10))

c     Compute bounding box

      fflg = .false.
      nty  =  np(49) - 1
      do j = n1,n2
        if(mr(nty+j).ge.0) then
          if(fflg) then
            do i = 1,ndm
              xmx(i) = max(xmx(i),x(i,j))
              xmn(i) = min(xmn(i),x(i,j))
            end do
          else
            do i = 1,ndm
              xmx(i) = x(i,j)
              xmn(i) = x(i,j)
              fflg = .true.
            end do
          endif
        endif
      end do
      do i = 1,ndm
        tol(i) = (xmx(i) - xmn(i))*snx
        xmn(i) =  xmn(i) - tol(i)
        xmx(i) =  xmx(i) + tol(i)
      end do

c     Determine total number of buckets in each direction

      snx = 0.0d0
      do i = 1,ndm
        snx = max(snx,tol(i))
      end do

      nn = 1
      do i = 1,ndm

c       Limit number of buckets to 1 for thin coordinate directions

        if(tol(i).lt.1.d-4*snx) then
          nsize(i) = 1
          tol(i)   = snx
        else
          nsize(i) = nsize(4)
        end if
        xmd(i) = max(tol(i),(xmx(i)-xmn(i))/max(1.d0,dble(nsize(i))))
        nn     = nn*nsize(i)
      end do

c     Count number of nodes in a bucket

      do j = 0,nn
        ibox(j) = 0
      end do ! j
      nm = 0
      do j = n1,n2
        if(mr(nty+j).ge.0 .and. ib(j).ge.0) then
          nn       = nbuck(x(1,j),xmd,xmn,ndm,nsize)
          nm       = max(nn,nm)
          ibox(nn) = ibox(nn) + 1
        endif
      end do

c     Put nodes in buckets

      do i = 1,nm
        ibox(i) = ibox(i-1) + ibox(i)
        jbox(i) = ibox(i-1)
      end do

      do j = n1,n2
        if(mr(nty+j).ge.0 .and. ib(j).ge.0) then
          nn = nbuck(x(1,j),xmd,xmn,ndm,nsize)
          jbox(nn) = jbox(nn) + 1
          ibuck(jbox(nn)) = j
        endif
      end do

c     Set up original numbers

      do j = 1,numnp
        ir(j) = -1
      end do

c     Set 'ir' to material number or region number

      do j = 1,numel
        do i = 1,nen
          if(ix(i,j).gt.0) then
            if(nt.eq.-2) then
              if(ix(nen1,j).eq.r1 .or. ix(nen1,j).eq.r2) then
                ir(ix(i,j)) = ix(nen1,j)
              endif
            elseif(ix(nen1-1,j).eq.r1 .or. ix(nen1-1,j).eq.r2) then
              ir(ix(i,j)) = ix(nen1-1,j)
            endif
          endif
        end do
      end do

c     Do a bucket search to tie

      if(ndm.ge.2) then
        jf = 2
      else
        jf = 1
      endif
      if(ndm.ge.3) then
        kf = 2
      else
        kf = 1
      endif
      nr = max(nt,1)
      do nn = 1,nm
        do k1 = ibox(nn-1)+1,ibox(nn)
          k = ibuck(k1)
          if(mr(nty+k).lt.0) go to 210
          if(nt.gt.0) then
            if( abs(x(nr,k)-xt).gt.tol(nr)) go to 210
          elseif(nt.lt.0) then
            if(ir(k).ne.r1) go to 210
          endif

c         Set up list to search if there is more than one bucket

          list     = 1
          lbuck(1) = nn
          if(nm.gt.1) then

c           Tolerance search coordinate to catch points near bucket edge

            snz = 1.0d0
            do ki = 1,kf
              if(ndm.ge.3) then
                xtol(3) = x(3,k) + snz*tol(3)
                xtol(3) = max( xmn(3),min( xtol(3),xmx(3) ) )
              endif
              sny = 1.0d0
              do ji = 1,jf
                if(ndm.ge.2) then
                  xtol(2) = x(2,k) + sny*tol(2)
                  xtol(2) = max( xmn(2),min( xtol(2),xmx(2) ) )
                endif
                snx = 1.0d0
                do ii = 1,2
                  xtol(1) = x(1,k) + snx*tol(1)
                  xtol(1) = max( xmn(1),min( xtol(1),xmx(1) ) )

c                 Check if in current list - add if not

                  ni = nbuck(xtol,xmd,xmn,ndm,nsize)
                  if(ni.gt.0 .and. ni.le.nm) then
                    do l = 1,list
                      if(ni.eq.lbuck(l)) go to 100
                    end do
                    list        = list + 1
                    lbuck(list) = ni
                  endif
100               snx = -snx
                end do
                sny = -sny
              end do
              snz = -snz
            end do
          endif

c         Search all active buckets

          do l = 1,list
            ni = lbuck(l)
            if(ni.eq.nn) then
              k2 = k1 + 1
            else
              k2 = ibox(ni-1) + 1
            endif

c           Search nodes in active bucket

            do j1 = k2,ibox(ni)
              j = ibuck(j1)
              if(mr(nty+j).lt.0) go to 200
              if(nt.lt.0) then
                if(ir(j).ne.r2) go to 200
              endif
              do i = 1,ndm
                if(abs(x(i,k)-x(i,j)).gt.tol(i)) go to 200
              end do

c             Connect node-j to node-k in id-list (eliminate node-j)

              do i1 = ibox(ni-1)+1,ibox(ni)
                i = ibuck(i1)
                if(ip(i).eq.ip(j)) then
                  ip(i) = ip(k)
                endif
              end do
200           continue
            end do
          end do
210       continue
        end do
      end do

c     Eliminate node j from element connections for solution

      do j = 1,numel
        do i = 1,nen
          k = abs(ix(i,j))
          if(k.gt.0) ix(i,j) = ip(k)
        end do
      end do

c     Format

2000  format(/6x,'Gap Tolerance =',1p,1e12.4/1x)

      end

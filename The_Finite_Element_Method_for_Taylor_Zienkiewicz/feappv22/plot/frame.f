c$Id:$
      subroutine frame(x,ndm,numnp,issw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Computes scaling values for plot screen area

c      Inputs:
c         x(ndm,*)  - Nodal coordinates of mesh
c         ndm       - Spatial dimension of mesh
c         numnp     - Number of nodes in mesh
c         issw      - Switch: < 0 for no reflections;
c                             > 0 for reflections

c      Outputs:
c         none      - Data stored in common blocks
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata1.h'
      include  'pdata4.h'
      include  'pdatxt.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   errv
      integer   ndm, numnp, issw, isw, ii, ij, i,j ,n
      integer   nty
      real*8    xcen,xw1, xw2
      integer   ip(8)
      real*8    x(ndm,*),xmn(3),xmx(3),xmino(3),xmaxo(3)
      real*8    xp(3,8), xpm(3,2), rp(3,6)

      save

      data rp/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 1.d0,1.d0,0.d0,
     &        1.d0,0.d0,1.d0, 0.d0,1.d0,1.d0, 0.d0,0.d0,1.d0/

c     Determine window coordinates

      isw   = abs(issw)
      dx(2) = 0.d0
      sx(2) = 0.d0
      ii = min(ndm,3)
      ij = min(ndm,2)
      if(isw.eq.1) then
        call pzero(xmin,3)
        call pzero(xmax,3)
        do i = 1,ii
          xmin(i) = x(i,1)
          do n = 1,numnp
            xmin(i) = max(xmin(i),x(i,n))
          end do
          xmax(i) = x(i,1)
        end do
      else
        do i = 1,ii
          xmin(i) = xmino(i)
          xmax(i) = xmaxo(i)
        end do
      endif
      nty = np(49) - 1
      do n = 1,numnp
        if(mr(nty+n) .ge. 0) then
          do i = 1,ii
            xmin(i) = min(xmin(i),x(i,n))
            xmax(i) = max(xmax(i),x(i,n))
            if(isw.eq.1) then
              xmino(i) = xmin(i)
              xmaxo(i) = xmax(i)
            endif
          end do
        endif
      end do

c     Perform reflections for window scaling

      if(issw.gt.0) then

c       Cartesian view

        do i = 1,ii
          xmin(i) = min (xmin(i),
     &                  (xmin(i)-xsyc(i)) + xsyc(i),
     &                  (xmax(i)-xsyc(i)) + xsyc(i) )
          xmax(i) = max (xmax(i),
     &                  (xmin(i)-xsyc(i)) + xsyc(i),
     &                  (xmax(i)-xsyc(i)) + xsyc(i) )
        end do

c     Perspective view

      else
        do i = 1,3
          xp(i,1) = (xmin(i) - xsyc(i)) + xsyc(i)
          xp(i,2) = (xmax(i) - xsyc(i)) + xsyc(i)
        end do
        do j = 1,6
          do i = 1,3
            xp(i,j+2) = xp(i,1) + rp(i,j)*(xp(i,2)-xp(i,1))
          end do
        end do
        do i = 1,8
          ip(i) = 1
        end do
        call perspj(xp,ip,8,errv)
        do i = 1,3
          xpm(i,1) = max(xp(i,1),xp(i,2),xp(i,3),xp(i,4),
     &                   xp(i,5),xp(i,6),xp(i,7),xp(i,8))
          xpm(i,2) = min(xp(i,1),xp(i,2),xp(i,3),xp(i,4),
     &                   xp(i,5),xp(i,6),xp(i,7),xp(i,8))
        end do

        do i = 1,3
          xmax(i) = xpm(i,1)
          xmin(i) = xpm(i,2)
        end do
      endif

c     Plot region determination

      do i = 1,ij
        xw1 = xmin(i)
        xw2 = xmax(i)

c       Modify window if nzm1 or nzm2 are nonzero

        if (nzm1.gt.0 .and. nzm1.le.numnp ) then
          xw1 = x(i,nzm1)
        endif
        if (nzm2.gt.0 .and. nzm2.le.numnp ) then
          xw2 = x(i,nzm2)
        endif
        xmn(i) = min(xw1,xw2)
        xmx(i) = max(xw1,xw2)
        dx(i) = xmx(i) - xmn(i)
        sx(i) = xmx(i) + xmn(i)
      end do

c     Rescale window

      if(dx(1).gt.dx(2)) then
        xmn(2) = (sx(2) - dx(1))*0.5d0
        xmx(2) = (sx(2) + dx(1))*0.5d0
      else
        xmn(1) = (sx(1) - dx(2))*0.5d0
        xmx(1) = (sx(1) + dx(2))*0.5d0
      endif
      do i = 1,ij
        xmin(i) = max(xmin(i),xmn(i))
        xmax(i) = min(xmax(i),xmx(i))
      end do
      scaleg = max(xmax(1)-xmin(1),xmax(2)-xmin(2))
      if(ii.gt.2) scaleg = max(scaleg,xmax(3)-xmin(3))
      if(scaleg.le.0.0d0) scaleg = 1.d0
      do i = 1,ij
        xmin(i) = xmin(i) - scaleg*0.01d0
        xmax(i) = xmax(i) + scaleg*0.01d0
      end do

c     Default values

      scale  = max(xmax(1)-xmin(1),xmax(2)-xmin(2))
      if(scale.le.0.0d0) scale = scaleg*0.1d0

c     Reset values for deformed plotting

c     do i = 1,3
      do i = 1,ii
        xcen = xmax(i)+xmin(i)
        xmax(i) = (xcen + 1.1d0*scale)*0.5d0
        xmin(i) = (xcen - 1.1d0*scale)*0.5d0
      end do
      scaleg = 0.4d0/scale
      scale  = scaleg
      s0(1)  = 0.5d0
      s0(2)  = 0.5d0

      end

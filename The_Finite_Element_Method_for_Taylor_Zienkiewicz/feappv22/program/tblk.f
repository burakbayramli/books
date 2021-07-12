c$Id:$
      subroutine tblk(nr,ns,nt,tl,t,ixl,dr,ds,dt,ni,ndm,nodinc,nm,
     &                prt,prth)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Generate a block of temperatures

c      Inputs:
c         nr        - Number elements in 1-local coordinate dir.
c         ns        - Number elements in 2-local coordinate dir.
c         nt        - Number elements in 3-local coordinate dir.
c         tl(*)     - Block nodal temperature array
c         ixl(*)    - Block nodal connection list
c         dr        - 1-local coordinate increment
c         ds        - 2-local coordinate increment
c         dt        - 3-local coordinate increment
c         ni        - Initial node number for block
c         ndm       - Spatial dimension of mesh
c         nodinc    - Increment array for block
c         nm        - Number master nodes on block
c         prt       - Output generated data if true
c         prth      - Output title/header data if true

c      Outputs:
c         t(*)      - Nodal temperatures for block
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'iofile.h'

      logical   prt,prth,phd
      integer   i,j,k,l,n, ll, mct, nr,ns,nt, ni,ndm,nodinc,nm
      real*8    dr,ds,dt, xsj

      integer   ixl(9)
      real*8    ss(3),xl(2,9),tl(*),t(*),shp2(3,9),shp3(4,8)

      save

      data xl/-1.d0,-1.d0,1.d0,-1.d0,1.d0,1.d0,-1.d0,1.d0,
     &         0.d0,-1.d0,1.d0, 0.d0,0.d0,1.d0,-1.d0,0.d0,0.d0,0.d0/

c     Check that all corners of brick are defined

      if(ndm.eq.3) then
        do k = 1,nm
          if(ixl(k).ne.k) go to 900
        end do
      endif
      n = ni
      mct = 0
      ss(3) = -1.0
      do k = 1,nt
        ss(2) = -1.0
        do j = 1,ns
          ss(1) = -1.0
          do i = 1,nr

c           Compute shape functions and coordinates for each point

            t(n) = 0.0
            if(ndm.lt.3) then
              call shp2d(ss,xl,shp2,xsj,2,nm,ixl,.true.)
              do l = 1,nm
                ll   = ixl(l)
                t(n) = t(n) + shp2(3,ll)*tl(ll)
              end do
            elseif(ndm.eq.3) then
              call shp3d(ss,xsj,shp3,tl,1)
              do l = 1,8
                t(n) = t(n) + shp3(4,l)*tl(l)
              end do
            endif

c           Output point

            if(prt) then
               mct = mct + 1
               phd = mod(mct,50).eq.1
               call prtitl(prth.and.phd)
               if(phd) write(iow,2000) l
               write(iow,2001) n,t(n)
               if(ior.lt.0) then
                 if(phd) write(*,2000) l
                 write(*,2001) n,t(n)
               endif
            endif
            n = n + 1
            ss(1) = ss(1) + dr
          end do
          n = n + nodinc
          ss(2) = ss(2) + ds
        end do
        ss(3) = ss(3) + dt
      end do
      return

c     Error

900   write(iow,3000) k
      if(ior.lt.0) then
        write(*,3000) k
        return
      endif
      call plstop()

c     Formats

2000  format(/'  N o d a l   T e m p e r a t u r e s'//
     &    6x,'Node',i7,' Temp.')

2001  format(i10,1p,1e13.4)

3000  format(' *ERROR* Block node',i3,' is undefined')

      end

!$Id:$
      subroutine prj2dl(gap0,tolg,tol,x0,ip,xin,x,xl,ndm,numnp,polfl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Project points onto line segment.  Line segment may
!               be represented in polar or cartesian coordinates.

!      Inputs:
!         gap0    - Specified gap tolerance
!         tolg    - Specified gap tolerance
!         tol     - Specified search tolerance
!         x0(*)   - Center of polar frame
!         x(*)    - Nodal coordinates
!         xl(*)   - Patch polar coordinates
!         ndm     - Space dimension of mesh
!         numnp   - Number of nodes in mesh
!         polfl   - Polar flag

!      Scratch:
!         ip(*)   - Nodal integer list storage
!         xin(*)  - Nodal real list storage

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      logical       :: polfl
      integer       :: i,n,nits
      integer       :: ndm,numnp

      real (kind=8) :: dx,dy, ff,df,xi,dxi
      real (kind=8) :: tolg,tol,tolxi,d,gap,gap0,angmin,th

      integer       :: ip(numnp)

      real (kind=8) :: xin(*),x(ndm,*)
      real (kind=8) :: xx(2),xp(2),x0(2),xl(2,3),dxl(2),dxq(2)
      real (kind=8) :: xmin(2),xmax(2)

      save

      data      tolxi / 1.0d-8 /
      data      nits /50/

!     Find maximum and minimum of search region

      do i = 1,2
        xmin(i) = min(xl(i,1),xl(i,2),xl(i,3))
        xmax(i) = max(xl(i,1),xl(i,2),xl(i,3))
      end do ! i

!     Set gap condition

      if(gap0.gt.0.0d0) then
        gap = gap0
      else
        gap = 0.5d0*tolg*max(xmax(1)-xmin(1),xmax(2)-xmin(2))
      endif
      if(polfl) then
        th      = xmax(1)*gap
        xmin(1) = xmin(1) - 1.5d0*th
        xmax(1) = xmax(1) + 1.5d0*th
        angmin  = xmin(2) - gap
        xmax(2) = xmax(2) + gap
        xmin(2) = angmin
      else
        th = max(xmax(1)-xmin(1),xmax(2)-xmin(2))
        do i = 1,2
!         xmin(i) = xmin(i) - 1.5d0*gap
!         xmax(i) = xmax(i) + 1.5d0*gap
          xmin(i) = xmin(i) - 1.1d0*th
          xmax(i) = xmax(i) + 1.1d0*th
        end do ! i
      endif

!     Set minimum for search area

      angmin = xmin(2)
      th     = 45.d0/atan(1.d0)

!     Tag nodes within surface loading area

      do n = 1,numnp
        ip(n)  = 0
        xin(n) = 0.0d0
      end do ! n

!     Find nodes on surface (within gap)

      do i = 1,2
        dxl(i) = 0.5d0*(xl(i,2) - xl(i,1))
        dxq(i) = xl(i,1) + xl(i,2) - 2.d0*xl(i,3)
      end do ! i

      do n = 1,numnp
        if(polfl) then
          xx(1) = sqrt((x(1,n) - x0(1))**2 + (x(2,n) - x0(2))**2)
          xx(2) = atan2((x(2,n) - x0(2)),(x(1,n) - x0(1)))*th
          if(xx(2).lt.angmin) then
            xx(2) = xx(2) + 360.d0
          endif
        else
          xx(1) = x(1,n)
          xx(2) = x(2,n)
        endif
        if((xx(1).ge.xmin(1) .and. xx(1).le.xmax(1)) .and.
     &     (xx(2).ge.xmin(2) .and. xx(2).le.xmax(2)) ) then

!         Compute location of closest point on surface

          xi = -1.0d0
          ff = (xl(1,1) - xx(1))**2 + (xl(2,1) - xx(2))**2
          do i = 2,3
            df = (xl(1,i) - xx(1))**2 + (xl(2,i) - xx(2))**2
            if(df.lt.ff) then
              ff = df
              xi = dble(3-i)
            endif
          end do ! i
          do i = 1,nits
            xp(1) = xl(1,3) + xi*(dxl(1) + 0.5d0*xi*dxq(1))
            xp(2) = xl(2,3) + xi*(dxl(2) + 0.5d0*xi*dxq(2))
            dx    = dxl(1)  + xi*dxq(1)
            dy    = dxl(2)  + xi*dxq(2)
            ff    = dx*(xp(1) - xx(1)) + dy*(xp(2) - xx(2))
            df    = dx*dx + dy*dy
            if(i.gt.3) then
              df = df + dxq(1)*(xp(1) - xx(1)) + dxq(2)*(xp(2) - xx(2))
            endif
            dxi   = -ff/df
            xi    = xi + dxi
            if(abs(dxi).lt.tolxi) go to 200
          end do ! i

!         Non linear interations did not converge

          write(  *,*) ' No convergence in PRJ2DL. Node =',n,dxi
          write(iow,*) ' No convergence in PRJ2DL. Node =',n,dxi

!         Compute gap and if too large eliminate this node

200       d = sqrt((xx(1) - xp(1))**2 + (xx(2) - xp(2))**2)
!         if(d .lt. gap .and. abs(xi).lt.1.0d0+tol) then
          if(d .lt. gap .and. abs(xi).lt.3.0d0+tol) then
            xin(n) = xi
            ip(n)  = 1
          else
            ip(n) = 0
          endif
        endif
      end do ! n

      end subroutine prj2dl

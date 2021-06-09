!$Id:$
      subroutine ptinvert(xp,xl,wt,knot,ord,len,ndm, u)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose:  Point inversion routine

!     Use:
!        Given a point 'xp' find the location on the knot vector 'u'

!     Inputs:
!        xp(ndm)       - Cartesian coordinates of point
!        xl(ndm,*)     - Control point coordinates
!        wt(*)         - Weight of control points
!        knot(*)       - Knot vector for control points
!        ord           - Order of knot vector
!        len           - Length of knot vector
!        ndm           - Space dimension of line

!     Outputs:
!        u             - Location on knot vector for point 'xp'
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'qudshp.h'

      integer    ndm,ord,len
      real*8     xp(*),xl(ndm,*),wt(*),knot(*)
      real*8     c,c2,cp,cpp
      real*8     fleft,fright,fpt

      logical    newton,notconv
      integer    p,n,nn,np, i,j,l, is,ir, its
      real*8     u,ur,du,umin,cmin, num,den

      real*8     tol
      data       tol / 1.d-10 /

      integer    nits
      data       nits / 10 /

!     Evaluate 1D shape functions and derivatives each direction

      np = ord + 1
      p  = len - np

!     Compute tolerance

      fpt = 0.0d0
      do j = 1,ndm
        fpt = fpt + xp(j)*xp(j)
      end do ! j
      fpt = max(tol,tol*fpt)

!     Locate first knot span

      u = knot(1)
      i = 2
      do while (u.eq.knot(i))
        i = i + 1
      end do ! while
      i = i - 1
      call pt_shp(u,i,shp2,wt,knot,ord)

      is    = i - ord - 1
      c2    = 0.0d0
      fleft = 0.0d0
      do j = 1,ndm
        c  = -xp(j)
        cp =  0.0d0
        do n = 1,np
          c  = c  + shp2(3,n,1)*xl(j,n+is)
          cp = cp + shp2(1,n,1)*xl(j,n+is)
        end do ! n
        c2    = c2    + c *c
        fleft = fleft + cp*c
      end do ! j
      if(c2.gt.fpt) then
        newton = .true.
        do l = i+1,len-ord
          if(u.eq.knot(l)) then
            ir = i
            ur = u
          else
            newton = .false.
            ir = l
            ur = knot(ir)
            du = (ur - u)*1.d-6
            ur = u - du
            call pt_shp(ur,i,shp2,wt,knot,ord)
            ur = knot(ir)
            is     = i - ord - 1
            c2     = 0.0d0
            fright = 0.0d0
            do j = 1,ndm
              c  = 0.0d0
              cp = 0.0d0
              do n = 1,np
                c  = c  + shp2(3,n,1)*xl(j,n+is)
                cp = cp + shp2(1,n,1)*xl(j,n+is)
              end do ! n
              fright = fright + cp*c
              c2     = c2     + c *c
            end do ! j
            if(fright*fleft .lt.0.0d0 .or. c2.lt.fpt) then
              newton = .true.
              go to 100
            endif
            fleft = fright
          endif
          i = ir
          u = ur
        end do ! l

100     continue
        if(newton) then
          nn = 10
          du = (ur - u)/dble(nn)
          cmin = c2
          umin = u
          do l = 1,nn
            u = u + du
            call pt_shp(u,i,shp2,wt,knot,ord)
            is    = i - ord - 1
            c2    = 0.0d0
            do j = 1,ndm
              c  = -xp(j)
              do n = 1,np
                c  = c  + shp2(3,n,1)*xl(j,n+is)
              end do ! n
              c2    = c2    + c*c
            end do ! j
            if(c2.lt.cmin) then
              umin = u
              cmin = c2
            endif
          end do ! l
          u       = umin
          notconv = .true.
          its     = 0
          is      = i - ord - 1
          do while (notconv)
            call pt_shp(u,i,shp2,wt,knot,ord)
            num     = 0.0d0
            den     = 0.0d0
            do j = 1,ndm
              c   = -xp(j)
              cp  =  0.0d0
              cpp =  0.0d0
              do n = 1,np
                c   = c   + shp2(3,n,1)*xl(j,n+is)
                cp  = cp  + shp2(1,n,1)*xl(j,n+is)
                cpp = cpp + shp2(2,n,1)*xl(j,n+is)
              end do ! n
              num = num + cp*c
              den = den + cpp*c + cp*cp
            end do ! j
            du = num/den
            u  = u - du
            if(abs(du).lt.tol*abs(u)) then
              notconv = .false.
            endif
            its = its + 1
            if(its.gt.nits) then
              notconv = .false.
              write(*,*) ' NO CONVERGENCE IN NEWTON',u,c
            endif
          end do ! while

        endif
      endif

      end

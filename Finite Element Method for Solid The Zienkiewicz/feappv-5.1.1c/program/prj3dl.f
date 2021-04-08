!$Id:$
      subroutine prj3dl(pl,ix,x,x0,id,ie,f,ibn,ang,gap0,nen,nen1,ndm,
     &                  ndf,numnp,numel,prt,prth,fnorm,polfl,ddof,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute loads on 3-D surfaces

!      Inputs:
!         pl(ndf)        - Patterns
!         ix(nen1,*)     - Element nodal connection lists
!         ie(*)          - Element information array
!         x(ndm,*)       - Nodal coordinates
!         x0(ndm)        - Origin of polar coordinates
!         gap0           - User specified gap
!         nen            - Maximum number of nodes on any element
!         nen1           - Dimension of ix array
!         ndm            - Spatial dimension of mesh
!         ndf            - Degree of freedoms/node
!         numnp          - Number of nodes
!         numel          - Number of elements/faces
!         prt            - Output values when true
!         prth           - Output header when true
!         fnorm          - Force/Displ type: 1 = Normal,
!                                            2 = Tangential,
!                                            3 = Displacement
!         polfl          - Polar/cylindrical coordinate flag
!         ddof           - Direction for displacement/tangent
!         isw            - Switch:           1 = Forces;
!                                            2 = Boundary codes

!      Temporary:
!         ibn(numnp)     - Marks nodes on patch

!      Outputs:
!         f(ndf,numnp,*) - Nodal loads          : isw = 1
!         id(ndf,*)      - Nodal boundary codes : isw = 2
!         ang(*)         - Angle boundary cond  : isw = 3
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdat1.h'
      include  'pdata5.h'
      include  'pdata6.h'
      include  'iofile.h'

      integer       :: nef,nen,nen1,ndm,ndf,numnp,numel
      integer       :: fnorm,ddof,isw, projpt
      integer       :: ibn(numnp),ix(nen1,numel),id(ndf,numnp),ie(nie,*)
      real (kind=8) :: pl(*),x(ndm,numnp),x0(ndm),f(ndf,numnp,*),ang(*)

      logical       :: polfl, errc, pinput, prt, prth
      integer       :: i,j,k,l,n, i1,i2, iel,iiel,ifac,lint
      integer       :: ic(9), iq(4,7), it(3,4), il(4,7)
      real (kind=8) :: gap0,gap,tol,tolgap,load,xsj(3),dir,den,th
      real (kind=8) :: td(5),xmin(3),xmax(3)
      real (kind=8) :: xp(3,9),v1(3),v2(3),xi(3),xx(3),xl(3,4)
      real (kind=8) :: nn(3),normal(3),pr(4),pp(9),shp(9),shps(4)
      real (kind=8) :: sg(3,4),tg(4,4),fl(3,4),xc(3),vv(2), angl,sn,cn

      save

!     8-node brick faces

      data iq  /3,2,1,4, 1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8, 5,6,7,8,
     &          1,2,3,4/

!     4-node tetraheron faces

      data it  /1,2,4, 2,3,4, 3,1,4, 1,3,2/

!     Solution tolerances

      data tol /1.0d-3/

!     Input points

      do n = 1,9
        ic(n) = 0
        pp(n) = 0.d0
      end do

!     Force pressure inputs

      if(isw.eq.1) then
        if(fnorm.eq.1) then
          write(iow,2000) (n,n=1,3)
        elseif(fnorm.eq.3) then
          write(iow,2010) (n,n=1,3)
        endif
        i = 1
        do while(i.ne.0)
          errc = pinput(td,5)
          if(errc .or. td(1).eq.0.0d0) then
            backspace(ior)
            i       = 0
          else
            i       = nint(td(1))
            xp(1,i) = td(2)
            xp(2,i) = td(3)
            xp(3,i) = td(4)
            pp(i)   = td(5)
            ic(i)   = i
            write(iow,2001) i,(xp(n,i),n=1,3),pp(i)
          endif
        end do

!     Boundary Code inputs

      elseif(isw.eq.2) then

        write(iow,2004) (n,n=1,3),(n,n=1,ndf)
        write(iow,2005) (int(pl(n)),n=1,ndf)
        i = 1
        do while(i.ne.0)
          errc = pinput(td,5)
          if(errc .or. td(1).eq.0.0d0) then
            backspace(ior)
            i       = 0
          else
            i       = nint(td(1))
            xp(1,i) = td(2)
            xp(2,i) = td(3)
            xp(3,i) = td(4)

            ic(i)   = i
            write(iow,2001) i,(xp(n,i),n=1,3)
          endif
        end do

!     Boundary angle inputs

      elseif(isw.eq.3) then

        write(iow,2011) (n,n=1,3),(n,n=1,ndf)
        i = 1
        do while(i.ne.0)
          errc = pinput(td,5)
          if(errc .or. td(1).eq.0.0d0) then
            backspace(ior)
            i       = 0
          else
            i       = nint(td(1))
            xp(1,i) = td(2)
            xp(2,i) = td(3)
            xp(3,i) = td(4)
            pp(i)   = td(5)
            ic(i)   = i
            write(iow,2001) i,(xp(n,i),n=1,3),pp(i)
          endif
        end do
      end if

!     Set missing points

      if(min(ic(1),ic(2),ic(3),ic(4)).eq.0) then
        write(iow,3000) ' Missing corner coordinate in PROJ3D'
        call plstop(.true.)
      endif

!     Fill missing mid-side nodes

      do i = 1,4
        if(ic(i+4).eq.0) then
          j         = mod(i,4) + 1
          xp(1,i+4) = 0.5d0*(xp(1,i) + xp(1,j))
          xp(2,i+4) = 0.5d0*(xp(2,i) + xp(2,j))
          xp(3,i+4) = 0.5d0*(xp(3,i) + xp(3,j))
          pp(i+4)   = 0.5d0*(  pp(i) +   pp(j))
          j = i+4
          if(isw.eq.1 .or. isw.eq.3) then
            write(iow,2001) j,(xp(n,j),n=1,3),pp(j)
          elseif(isw.eq.2) then
            write(iow,2001) j,(xp(n,j),n=1,3)
          endif
        endif
      end do ! i

!     Fill central node

      if(ic(9).eq.0) then
        xp(1,9) = 0.50d0*(xp(1,5) + xp(1,6) + xp(1,7) + xp(1,8))
     &          - 0.25d0*(xp(1,1) + xp(1,2) + xp(1,3) + xp(1,4))
        xp(2,9) = 0.50d0*(xp(2,5) + xp(2,6) + xp(2,7) + xp(2,8))
     &          - 0.25d0*(xp(2,1) + xp(2,2) + xp(2,3) + xp(2,4))
        xp(3,9) = 0.50d0*(xp(3,5) + xp(3,6) + xp(3,7) + xp(3,8))
     &          - 0.25d0*(xp(3,1) + xp(3,2) + xp(3,3) + xp(3,4))
        pp(9)   = 0.50d0*(pp(5)   + pp(6)   + pp(7)   + pp(8)  )
     &          - 0.25d0*(pp(1)   + pp(2)   + pp(3)   + pp(4)  )
        i = 9
        if(isw.eq.1 .or. isw.eq.3) then
          write(iow,2001) i,(xp(n,i),n=1,3),pp(i)
        elseif(isw.eq.2) then
          write(iow,2001) i,(xp(n,i),n=1,3)
        endif
      endif

!     Initialize surface node indicators to zero

      do n = 1,numnp
        ibn(n) = 0
      end do ! n

!     Set min/max for patch coordinates

      do i = 1,3
        xmin(i) = xp(i,1)
        xmax(i) = xp(i,1)
        do n = 2,9
          xmin(i) = min(xmin(i),xp(i,n))
          xmax(i) = max(xmax(i),xp(i,n))
        end do ! n
      end do ! i

!     Polar minimum angle

      th     = 45.d0/atan(1.0d0)

!     Set gap value

      gap = 0.0d0
      do i = 1,3
        gap = gap + xmax(i) - xmin(i)
      end do ! i
      gap = 1.d-4*gap

      if(gap0 .gt. 0.0d0) then
        gap    = gap0
      else
        tolgap = tol/dble(numnp)**(1.d0/3.d0)
        do i = 1,3
          gap = max(gap,tolgap*(xmax(i)-xmin(i)))
        end do ! i
      endif

      do i = 1,3
        xmin(i) = xmin(i) - 1.5d0*gap
        xmax(i) = xmax(i) + 1.5d0*gap
      end do ! i

!     Determine potential nodes for loads, etc.

      if(polfl) then
        angl  = 0.5d0*(xmin(2) + xmax(2))
        vv(1) = cos(angl/th)
        vv(2) = sin(angl/th)
      endif
      do n = 1,numnp
        if(polfl) then
          xc(1) =  sqrt((x(1,n) - x0(1))**2 + (x(2,n) - x0(2))**2)
          cn    =  vv(1)*(x(1,n) - x0(1)) + vv(2)*(x(2,n) - x0(2))
          sn    = -vv(2)*(x(1,n) - x0(1)) + vv(1)*(x(2,n) - x0(2))
          xc(2) =  atan2(sn,cn)*th + angl
          xc(3) =  x(3,n) - x0(3)
        else
          xc(1) =  x(1,n)
          xc(2) =  x(2,n)
          xc(3) =  x(3,n)
        endif

!       Eliminate nodes outside patch block

        if((xc(1).lt.xmin(1) .or. xc(1).gt.xmax(1)) .or.
     &     (xc(2).lt.xmin(2) .or. xc(2).gt.xmax(2)) .or.
     &     (xc(3).lt.xmin(3) .or. xc(3).gt.xmax(3)) ) then
          ibn(n) = -1
        endif

!       Search remaining nodes for those near patch

        if(ibn(n).ge.0) then
          ibn(n) = projpt(xc,xp,xi,gap,normal,shp)
        endif
      end do ! n

!     Force computations for normal pressures

      if(isw.eq.1) then

!       Determine elements with faces on patch

        if(prt) then
          call prtitl(prth)
          if(fnorm.eq.1) then
            write(iow,2002) (j,j=1,3)
          elseif(fnorm.eq.3) then
            write(iow,2008)
          endif
        endif

        do n = 1,numel

!         Determine element type

          iel = ie(nie-1,ix(nen1,n))
          if(iel.ge.0) then
            iiel = inord(iel)
          else
            iiel = exord(-iel)
          endif
          do i = nen,1,-1
            if(ix(i,n).ne.0) go to 100
          end do

!         No face if iiel < 0

100       if(iiel.lt.0) then

!         Tetrahedral element faces

          elseif(iiel .eq. 9 ) then

            nef = 3
            i1  = 4
            i2  = 1
            do i = 1,4
              do j = 1,3
                il(j,i) = it(j,i)
              end do ! j
            end do ! i

!         Brick and shell element faces

          else

            nef = 4

            if(i.eq.8) then
              i1 = 6
              i2 = 1
            elseif(i.eq.4) then
              i1 = 7
              i2 = 6
            else
              i1 = 0
              i2 = 1
            endif
            do i = 1,7
              do j = 1,4
                il(j,i) = iq(j,i)
              end do ! j
            end do ! i

          endif

          do i = 1,i1,i2
            ifac = 0
            do j = 1,nef
              ic(j) = ix(il(j,i),n)
              if(ic(j).gt.0) then
                ifac = ifac + ibn(ic(j))
              endif
            end do

!           Compute pressures for matching faces

            if(ifac.eq.nef) then
              do j = 1,3
                v1(j) =  x(j,ic(2)) - x(j,ic(nef))
                v2(j) =  x(j,ic(3)) - x(j,ic(1))
                xx(j) =  0.0d0
                do k = 1,nef
                  xx(j)   = xx(j) + x(j,ic(k))
                  xl(j,k) = x(j,ic(k))
                end do ! k
                xx(j) = xx(j)/dble(nef)
              end do ! j
              nn(1) = v1(2)*v2(3) - v1(3)*v2(2)
              nn(2) = v1(3)*v2(1) - v1(1)*v2(3)
              nn(3) = v1(1)*v2(2) - v1(2)*v2(1)
              if(polfl) then
                xc(1) = sqrt((xx(1) - x0(1))**2 + (xx(2) - x0(2))**2)
                cn    =  vv(1)*(xx(1) - x0(1)) + vv(2)*(xx(2) - x0(2))
                sn    = -vv(2)*(xx(1) - x0(1)) + vv(1)*(xx(2) - x0(2))
                xx(2) =  atan2(sn,cn)*th + angl
                xx(1) = xc(1)
                call pdegree(xx(2), sn,cn)
                xx(3) = xx(3) - x0(3)
                xc(1) =  nn(1)*cn + nn(2)*sn
                nn(2) = -nn(1)*sn + nn(2)*cn
                nn(1) =  xc(1)
              endif
              ifac  = projpt(xx,xp,xi,gap,normal,shp)
              dir   = nn(1)*normal(1)+nn(2)*normal(2)+nn(3)*normal(3)
              den   = sqrt(nn(1)*nn(1)+nn(2)*nn(2)+nn(3)*nn(3))
     &              * sqrt(normal(1)*normal(1)+normal(2)*normal(2)
     &                    +normal(3)*normal(3))
              if(dir.gt.0.8d0*den) then

!               Compute load at element nodes

                do j = 1,nef
                  if(polfl) then
                    xc(1) =  sqrt((xl(1,j) - x0(1))**2
     &                          + (xl(2,j) - x0(2))**2)
                    cn    =  vv(1)*(x(1,j) - x0(1))
     &                     + vv(2)*(x(2,j) - x0(2))
                    sn    = -vv(2)*(x(1,j) - x0(1))
     &                     + vv(1)*(x(2,j) - x0(2))
                    xc(2) =  atan2(sn,cn)*th + angl
                    xc(3) =  xl(3,j) - x0(3)
                  else
                    xc(1) =  xl(1,j)
                    xc(2) =  xl(2,j)
                    xc(3) =  xl(3,j)
                  endif
                  ifac  = projpt(xc,xp,xi,gap,normal,shp)
                  pr(j) = 0.0d0
                  do k = 1,9
                    pr(j) = pr(j) + shp(k)*pp(k)
                  end do
                  fl(1,j) = 0.0d0
                  fl(2,j) = 0.0d0
                  fl(3,j) = 0.0d0
                end do ! j

!               Normal Force:

                if(fnorm.eq.1) then

!                 Compute loads on nodes

                  if(nef.eq.4) then
                    call int2d(2,lint,sg)
                  else
                    call tint2d(3,lint,tg)
                    do l = 1,lint
                      sg(1,l) = tg(1,l)
                      sg(2,l) = tg(2,l)
                      sg(3,l) = tg(4,l)*0.5d0
                    end do ! l
                  endif
                  do l = 1,lint

                    call shp3p(sg(1,l),xl,shps,xsj,nef)
                    load  = 0.0d0
                    do k = 1,nef
                      load  = load  + shps(k)*pr(k)
                    end do ! k

                    load = load*sg(3,l)
                    do k = 1,nef
                      fl(1,k) = fl(1,k) + load*xsj(1)*shps(k)
                      fl(2,k) = fl(2,k) + load*xsj(2)*shps(k)
                      fl(3,k) = fl(3,k) + load*xsj(3)*shps(k)
                    end do ! k
                  end do ! l

!                 Check for sloping boundaries

                  do k = 1,nef
                    if(ang(ic(k)).ne.0.0d0) then
                      call pdegree(ang(ic(k)), sn,cn)
                      td(3)   =  cn*fl(1,k) + sn*fl(2,k)
                      fl(2,k) = -sn*fl(1,k) + cn*fl(2,k)
                      fl(1,k) =  td(3)
                    endif
                  end do ! k

                  if(prt) then
                    do k = 1,nef
                      write(iow,2003) k,ic(k),(fl(j,k),j=1,3),pr(k)
                    end do ! k
                  endif

                  do k = 1,nef
                    f(1,ic(k),1) = f(1,ic(k),1) + fl(1,k)
                    f(2,ic(k),1) = f(2,ic(k),1) + fl(2,k)
                    f(3,ic(k),1) = f(3,ic(k),1) + fl(3,k)
                  end do ! k

!               Displacement: Component ddof

                elseif(fnorm.eq.3) then

                  do k = 1,nef
                    f(ddof,ic(k),2) = pr(k)
                    if(prt) then
                      write(iow,2009) k,ic(k),ddof,f(ddof,ic(k),2)
                      if(ior.lt.0) then
                        write(*,2009) k,ic(k),ddof,f(ddof,ic(k),2)
                      endif
                    endif
                  end do ! k

                end if ! fnorm

              endif ! dir
            endif ! ifac

          end do ! i
        end do ! n

!     Set the boundary codes

      elseif(isw.eq.2) then

        if(prt) then
          call prtitl(prth)
          write(iow,2006) (j,j=1,ndf)
        endif
        do n = 1,numnp
          if(ibn(n).eq.1) then
            do j = 1,ndf
              id(j,n) = abs(id(j,n)) + int(pl(j))
            end do ! j
            if(prt) then
              write(iow,2007) n,(id(j,n),j=1,ndf)
            endif
          endif
        end do ! n

!     Set the boundary angles

      elseif(isw.eq.3) then
        if(prt) then
          write(iow,2012)
        endif
        do n = 1,numnp
          if(ibn(n).eq.1) then

!           Interpolate angle on patch to node position

            ifac  = projpt(xc,xp,xi,gap,normal,shp)
            pr(1) = 0.0d0
            do k = 1,9
              pr(1) = pr(1) + shp(k)*pp(k)
            end do
            if(prt) then
              write(iow,2013) n,pr(1)
            endif

!           Store global angle

            ang(n) = pr(1)

          endif
        end do ! n

      endif

!     Formats

2000  format(/6x,'Node',3(i5,' Coord'),3x,'Pressure')
2001  format(i10,1p,6e11.3)
2002  format(/7x,'N o d a l    F o r c e s'//3x,'Local',5x,'Global'/
     &       4x,'Node',6x,'Node',3(i5,' Force'),3x,'Pressure')
2003  format(i8,i10,1p,5e11.3)

2004  format(/6x,'Node',3(i5,' Coord'),9(i2,' BC':))
2005  format(43x,9i5)

2006  format(4x,'Node',9(i3,' BC':))
2007  format(i10,9i6)

2008  format(/7x,'N o d a l    D i s p l a c e m e n t s'
     &      //3x,'Local',3x,'Global'/
     &        4x,'Node',4x,'Node     DOF Displacement')

2009  format(3i8,1p,e13.4)
2010  format(/6x,'Node',3(i5,' Coord'),5x,'Displ.')

2011  format(/6x,'Node',3(i5,' Coord'),6x,'Angle')
2012  format(/7x,'N o d a l    A n g l e s'//
     &       10x,'Global Node    Angle'/)
2013  format(i20,1p,5e11.3)

3000  format(/'  *ERROR* ',a)

      end subroutine prj3dl

      function projpt(x,xp,xi,gap,normal,shp)

      implicit  none

      include  'iofile.h'

      integer   projpt

      logical   noconv
      integer   i,n,iters
      real (kind=8) :: gap,detr, tol, a11,a12,a22
      real (kind=8) :: r(2), t1(3),t2(3), dt11(3),dt12(3),dt22(3)
      real (kind=8) :: dx(3),xi(2),dxi(2),shp(9),d1shp(2,9),d2shp(3,9)
      real (kind=8) :: normal(3),x(3),xp(3,9)

      data      tol /1.d-6/

!     Projections to nine node patch

      noconv = .true.
      iters  = 0
      xi(1)  = 0.0d0
      xi(2)  = 0.0d0
      do while(noconv)
        iters = iters + 1
        call pshp9(xi,shp,d1shp,d2shp)
        do i = 1,3
          dx(i)   = -x(i)
          t1(i)   = 0.0d0
          t2(i)   = 0.0d0
          dt11(i) = 0.0d0
          dt12(i) = 0.0d0
          dt22(i) = 0.0d0
          do n = 1,9
            dx(i)   = dx(i)   + shp(n)*xp(i,n)
            t1(i)   = t1(i)   + d1shp(1,n)*xp(i,n)
            t2(i)   = t2(i)   + d1shp(2,n)*xp(i,n)
            dt11(i) = dt11(i) + d2shp(1,n)*xp(i,n)
            dt12(i) = dt12(i) + d2shp(2,n)*xp(i,n)
            dt22(i) = dt22(i) + d2shp(3,n)*xp(i,n)
          end do ! n
        end do ! i

        r(1) = dx(1)*t1(1)   + dx(2)*t1(2)   + dx(3)*t1(3)
        r(2) = dx(1)*t2(1)   + dx(2)*t2(2)   + dx(3)*t2(3)
        a11  = t1(1)*t1(1)   + t1(2)*t1(2)   + t1(3)*t1(3)
        a12  = t1(1)*t2(1)   + t1(2)*t2(2)   + t1(3)*t2(3)
        a22  = t2(1)*t2(1)   + t2(2)*t2(2)   + t2(3)*t2(3)

        if(iters.gt.3) then
          a11 = a11
     &        + dx(1)*dt11(1) + dx(2)*dt11(2) + dx(3) + dt11(3)
          a12 = a12
     &        + dx(1)*dt12(1) + dx(2)*dt12(2) + dx(3) + dt12(3)
          a22 = a22
     &        + dx(1)*dt22(1) + dx(2)*dt22(2) + dx(3) + dt22(3)
        endif

        detr   = 1.d0/(a11*a22 - a12*a12)
        dxi(1) = (-a22*r(1) + a12*r(2))*detr
        dxi(2) = ( a12*r(1) - a11*r(2))*detr

        xi(1)  = xi(1) + dxi(1)
        xi(2)  = xi(2) + dxi(2)

        if((max(abs(dxi(1)),abs(dxi(2))).lt.tol)
     &      .or. (iters .gt. 100)              ) noconv = .false.

      end do ! while

!     Stop on non-convergence

      if(iters.gt.100) then
        write(iow,3000) ' No convergence in PROJPT'
        call plstop(.true.)
      endif

!     Check position of projection: projpt = 1 "on" surface; = 1 "not"

      if((dx(1)**2+dx(2)**2+dx(3)**2 .lt. gap*gap) .and.
     &   (max(abs(xi(1)),abs(xi(2))) .lt. 1.d0+1.d2*tol) ) then
        projpt    = 1
        normal(1) = t1(2)*t2(3) - t1(3)*t2(2)
        normal(2) = t1(3)*t2(1) - t1(1)*t2(3)
        normal(3) = t1(1)*t2(2) - t1(2)*t2(1)
      else
        projpt    = 0
      endif

!     Format

3000  format(/'  *ERROR* ',a)

      end function projpt

      subroutine pshp9(xi,shp,d1shp,d2shp)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute 9-node shape functions.

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      real (kind=8) :: xi(2), shp(9),d1shp(2,9),d2shp(3,9)

      real (kind=8) :: sh1m,sh2m,sh1p,sh2p,sh1c,sh2c
      real (kind=8) :: dn1m,dn2m,dn1p,dn2p,dn1c,dn2c, ddn1,ddn2,ddnc

      save

!     Constant parameters for 9-node shape functions

      sh1m = 0.5d0*(xi(1)*xi(1) - xi(1))
      sh2m = 0.5d0*(xi(2)*xi(2) - xi(2))
      sh1p = 0.5d0*(xi(1)*xi(1) + xi(1))
      sh2p = 0.5d0*(xi(2)*xi(2) + xi(2))
      sh1c = 1.0d0 - xi(1)*xi(1)
      sh2c = 1.0d0 - xi(2)*xi(2)

      dn1m = xi(1) - 0.5d0
      dn2m = xi(2) - 0.5d0
      dn1p = xi(1) + 0.5d0
      dn2p = xi(2) + 0.5d0
      dn1c = -2.0d0*xi(1)
      dn2c = -2.0d0*xi(2)

      ddn1 =  1.0d0
      ddn2 =  1.0d0
      ddnc = -2.0d0

!     Form shape functions

      shp(1)     = sh1m*sh2m
      shp(2)     = sh1p*sh2m
      shp(3)     = sh1p*sh2p
      shp(4)     = sh1m*sh2p
      shp(5)     = sh1c*sh2m
      shp(6)     = sh1p*sh2c
      shp(7)     = sh1c*sh2p
      shp(8)     = sh1m*sh2c
      shp(9)     = sh1c*sh2c

!     Form first derivatives of shape functions

      d1shp(1,1) = dn1m*sh2m
      d1shp(1,2) = dn1p*sh2m
      d1shp(1,3) = dn1p*sh2p
      d1shp(1,4) = dn1m*sh2p
      d1shp(1,5) = dn1c*sh2m
      d1shp(1,6) = dn1p*sh2c
      d1shp(1,7) = dn1c*sh2p
      d1shp(1,8) = dn1m*sh2c
      d1shp(1,9) = dn1c*sh2c

      d1shp(2,1) = sh1m*dn2m
      d1shp(2,2) = sh1p*dn2m
      d1shp(2,3) = sh1p*dn2p
      d1shp(2,4) = sh1m*dn2p
      d1shp(2,5) = sh1c*dn2m
      d1shp(2,6) = sh1p*dn2c
      d1shp(2,7) = sh1c*dn2p
      d1shp(2,8) = sh1m*dn2c
      d1shp(2,9) = sh1c*dn2c

!     Form second derivatives of shape functions

      d2shp(1,1) = ddn1*sh2m
      d2shp(1,2) = ddn1*sh2m
      d2shp(1,3) = ddn1*sh2p
      d2shp(1,4) = ddn1*sh2p
      d2shp(1,5) = ddnc*sh2m
      d2shp(1,6) = ddn1*sh2c
      d2shp(1,7) = ddnc*sh2p
      d2shp(1,8) = ddn1*sh2c
      d2shp(1,9) = ddnc*sh2c

      d2shp(2,1) = dn1m*dn2m
      d2shp(2,2) = dn1p*dn2m
      d2shp(2,3) = dn1p*dn2p
      d2shp(2,4) = dn1m*dn2p
      d2shp(2,5) = dn1c*dn2m
      d2shp(2,6) = dn1p*dn2c
      d2shp(2,7) = dn1c*dn2p
      d2shp(2,8) = dn1m*dn2c
      d2shp(2,9) = dn1c*dn2c

      d2shp(3,1) = sh1m*ddn2
      d2shp(3,2) = sh1p*ddn2
      d2shp(3,3) = sh1p*ddn2
      d2shp(3,4) = sh1m*ddn2
      d2shp(3,5) = sh1c*ddn2
      d2shp(3,6) = sh1p*ddnc
      d2shp(3,7) = sh1c*ddn2
      d2shp(3,8) = sh1m*ddnc
      d2shp(3,9) = sh1c*ddnc

      end subroutine pshp9

      subroutine shp3p(sg,xl,shps,xsj,nef)

      implicit  none

      integer   j,nef
      real (kind=8) :: sg(3),xl(3,4),shps(4),xsj(3)
      real (kind=8) :: x1(3),x2(3),xm(3)

      save

      if(nef.eq.3) then
        do j = 1,3
          x1(j)   = xl(j,1) - xl(j,3)
          x2(j)   = xl(j,2) - xl(j,3)
        end do ! j
        shps(1) = sg(1)
        shps(2) = sg(2)
        shps(3) = 1.d0 - sg(1) - sg(2)
      else
        do j = 1,3
          xm(j) =   xl(j,1) - xl(j,2) + xl(j,3) - xl(j,4)
          x1(j) = (-xl(j,1) + xl(j,2) + xl(j,3) - xl(j,4)
     &          +   xm(j)*sg(2))*0.25d0
          x2(j) = (-xl(j,1) - xl(j,2) + xl(j,3) + xl(j,4)
     &          +   xm(j)*sg(1))*0.25d0
        end do ! j

        shps(1) = (0.5d0 - 0.5d0*sg(1))*(0.5d0 - 0.5d0*sg(2))
        shps(2) = (0.5d0 + 0.5d0*sg(1))*(0.5d0 - 0.5d0*sg(2))
        shps(3) = (0.5d0 + 0.5d0*sg(1))*(0.5d0 + 0.5d0*sg(2))
        shps(4) = (0.5d0 - 0.5d0*sg(1))*(0.5d0 + 0.5d0*sg(2))

      endif

      xsj(1)  = x1(2)*x2(3) - x1(3)*x2(2)
      xsj(2)  = x1(3)*x2(1) - x1(1)*x2(3)
      xsj(3)  = x1(1)*x2(2) - x1(2)*x2(1)

      end subroutine shp3p

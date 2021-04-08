!$Id:$
      subroutine pltfor(x,f,angl,id,ip,ndm,ndf,numnp,n1, isgn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Draw vectors for forces on mesh

!      Inputs:
!         x(ndm,*)  - Nodal coordinates for mesh
!         f(ndf,*)  - Nodal forces
!         angl(*)   - Value of angle for sloping boundary
!         id(ndf,*) - Boundary condition indicator array
!         ip(*)     - Active node indicator
!         ndm       - Dimension of x array
!         ndf       - Dimension of f and id arrays
!         numnp     - Number of nodes in mesh
!         n1        - Flag, place tip at node if > 0
!         isgn      - Flag, plot displacements if > 1

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'pdata1.h'
      include  'pdata4.h'
      include  'pdatxt.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: vfl,zoom, fdis

      integer       :: ndm,ndf,numnp,n1,isgn, i,j,k,n
      real (kind=8) :: fm,x1,x2,x3,dx1,dx2,dx3,d,tm, cs,sn,ang

      integer       :: id(ndf,*),ip(*)
      real (kind=8) :: dd(3),xx(3,4),x(ndm,*),f(ndf,*),angl(*)

      save

!     Compute longest vector

      call pzero(dd, 3)
      call pzero(xx,12)

      fdis   = isgn.gt.1
      fm   = 0.d0
      do n = 1,numnp
        if(ip(n).gt.0 .and. mr(np(190)+n-1).ge.0
     &                .and. zoom(x(1,n),ndm)) then
          d = 0.d0
          do i = 1,min(ndm,ndf)
            j = pdf(i)
            if(j.gt.0 .and. j.le.ndf) then
              if(id(j,n).gt.0 .or. isgn.lt.0) then
                d = d + f(j,n)**2
              end if
            end if
          end do
          fm = max(fm,d)
        endif
      end do

!     Zero length vectors

      if(fm.le.0.0d0) then
        if(iow.lt.0) write(*,2000)

!     Compute vector at each node

      else

        fm = isgn*sqrt(fm)*scalef*40.d0
        x3 = 0.0d0
        do n = 1,numnp
          if(ip(n).gt.0 .and. zoom(x(1,n),ndm)) then
            x1 = x(1,n)
            x2 = x(2,n)
            vfl = .false.
            do i = 1,3
              dd(i) = 0.0d0
            end do
            do i = 1,min(ndm,ndf)
              j = pdf(i)
              if(j.gt.0 .and. j.le.ndf) then
                if( ((id(j,n).gt.0) .or. isgn.lt.0)
     &              .and. (f(j,n).ne.0.0d0) .or. fdis ) then
                  dd(i) = f(j,n)
                  vfl = .true.
                endif
              endif
            end do
            if(vfl) then
              dd(1) = dd(1)/fm
              dd(2) = dd(2)/fm
              if(angl(n).ne.0.0d0) then
                ang = angl(n)*0.017453292d0
                cs = cos(ang)
                sn = sin(ang)
                tm    =  dd(1)*cs - dd(2)*sn
                dd(2) =  dd(1)*sn + dd(2)*cs
                dd(1) =  tm
              endif
              if(ndm.ge.3) then
                dd(3) = dd(3)/fm
                x3 = x(3,n)
                xx(3,1) = x3
                xx(3,2) = xx(3,1) + dd(3)
                xx(3,3) = xx(3,2) -.6d0*dd(3) + .2d0*(dd(1)+dd(2))
                xx(3,4) = xx(3,2) -.6d0*dd(3) - .2d0*(dd(1)+dd(2))
              endif
              xx(1,1) = x1
              xx(2,1) = x2
              xx(1,2) = xx(1,1) + dd(1)
              xx(2,2) = xx(2,1) + dd(2)
              xx(1,3) = xx(1,2) -.6d0*dd(1) - .2d0*(dd(2)+dd(3))
              xx(2,3) = xx(2,2) -.6d0*dd(2) + .2d0*(dd(1)+dd(3))
              xx(1,4) = xx(1,2) -.6d0*dd(1) + .2d0*(dd(2)+dd(3))
              xx(2,4) = xx(2,2) -.6d0*dd(2) - .2d0*(dd(1)+dd(3))

              do k = 1, 4
                do j = 1,3
                  xx(j,k) = (xx(j,k) - xsyc(j)) + xsyc(j)
                end do
              end do
              if(n1.eq.0) then
                dx1 = 0.d0
                dx2 = 0.d0
                dx3 = 0.d0
              else
                dx1 = xx(1,1) - xx(1,2)
                dx2 = xx(2,1) - xx(2,2)
                dx3 = xx(3,1) - xx(3,2)
              endif
              call plotl(xx(1,1)+dx1,xx(2,1)+dx2,xx(3,1)+dx3,3)
              call plotl(xx(1,2)+dx1,xx(2,2)+dx2,xx(3,2)+dx3,2)
              call plotl(xx(1,3)+dx1,xx(2,3)+dx2,xx(3,3)+dx3,2)
              call plotl(xx(1,4)+dx1,xx(2,4)+dx2,xx(3,4)+dx3,2)
              call plotl(xx(1,2)+dx1,xx(2,2)+dx2,xx(3,2)+dx3,2)
            endif
          endif
        end do
      endif

2000  format('  Zero values acting on mesh ')

      end subroutine pltfor

!$Id:$
      subroutine vblkn(nr,ns,nt,xl,x,ixl,dr,ds,dt,
     &                 ni,ndm,ctype,prt,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Generate a block of 3-d 8-node brick elements

!      Inputs:
!         nr        - Number elements in 1-local coordinate dir.
!         ns        - Number elements in 2-local coordinate dir.
!         nt        - Number elements in 3-local coordinate dir.
!         xl(ndm,*) - Block nodal coordinate array
!         ixl(*)    - Block nodal connection list
!         dr        - 1-local coordinate increment
!         ds        - 2-local coordinate increment
!         dt        - 3-local coordinate increment
!         ni        - Initial node number for block
!         ndm       - Spatial dimension of mesh
!         ctype     - Type of block coordinates
!         prt       - Output generated data if true
!         prth      - Output title/header data if true

!      Outputs:
!         x(ndm,*)  - Nodal coordinates for block
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'cdat2.h'
      include  'iofile.h'
      include  'trdata.h'

      character (len=15) :: ctype
      character (len=6)  :: xh
      logical       :: prt,prth,phd, pcomp
      integer       :: ni,ndm,nr,ns,nt,i,j,k,l,m,n,mct, ixl(*)
      real (kind=8) :: dr,ds,dt, rr, sn2,cn2,sn3,cn3
      real (kind=8) :: ss(3),xl(3,*),x(ndm,*),xx(3)

      save

      data      xh/' coord'/

!     Check that all corners of brick are defined

      do k = 1,3
        xx(k) = 0.0d0
      end do ! k

      do k = 1,8
        if(ixl(k).ne.k) go to 900
      end do
      call bcor3d(ixl,xl)
      n = ni
      mct = 0
      ss(3) = -1.0d0
      do k = 1,nt
        ss(2) = -1.0d0
        do j = 1,ns
          ss(1) = -1.0d0
          do i = 1,nr

!           Compute coordinates of node

            call xbcor3d(ss,xl, xx)

!           Convert coordinates if necessary

            if(pcomp(ctype,'pola',4)) then
              call pdegree(xx(2), sn2,cn2)
              rr    = xx(1)
              xx(1) = x0(1) + rr*cn2
              xx(2) = x0(2) + rr*sn2
              xx(3) = x0(3) + xx(3)
            elseif(pcomp(ctype,'sphe',4)) then
              call pdegree(xx(2), sn2,cn2)
              call pdegree(xx(3), sn3,cn3)
              rr   = xx(1)
              xx(1) = x0(1) + rr*cn2*sn3
              xx(2) = x0(2) + rr*sn2*sn3
              xx(3) = x0(3) + rr*cn3
            endif

!           Transform to global coordinates

            do m = 1,ndm
              x(m,n) = xr(m)+tr(m,1)*xx(1)+tr(m,2)*xx(2)+tr(m,3)*xx(3)
            end do

!           Output point

            if(prt) then
               mct = mct + 1
               phd = mod(mct,50).eq.1
               call prtitl(prth.and.phd)
               if(phd) write(iow,2000) (l,xh,l=1,ndm)
               write(iow,2001) n,(x(l,n),l=1,ndm)
               if(ior.lt.0) then
                 if(phd) write(*,2000) (l,xh,l=1,ndm)
                 write(*,2001) n,(x(l,n),l=1,ndm)
               endif
            endif
            n = n + 1
            ss(1) = ss(1) + dr
          end do
          ss(2) = ss(2) + ds
        end do
        ss(3) = ss(3) + dt
      end do

      return

!     Error

900   write(iow,3000) k
      if(ior.lt.0) then
        write(*,3000) k
        return
      endif
      call plstop(.true.)

!     Formats

2000  format(/'  N o d a l   C o o r d i n a t e s'//6x,'Node',5(i7,a6))

2001  format(i10,1p,5e13.4)

3000  format(' *ERROR* Block node',i3,' is undefined')

      end subroutine vblkn

      subroutine bcor3d(ixl,xl)

      implicit   none

      integer       :: ixl(27), imid(12),amid(12),bmid(12)
      real (kind=8) :: xl(3,27)

      integer       :: i,j

      data       imid/9,10,11,12, 13,14,15,16, 18,19,20,21/
      data       amid/1, 2, 3, 4,  1, 2, 3, 4,  5, 6, 7, 8/
      data       bmid/5, 6, 7, 8,  2, 3, 4, 1,  6, 7, 8, 5/

!     Mid edge coordinates

      do i = 1,12
        if(ixl(imid(i)).eq.0) then
          do j = 1,3
            xl(j,imid(i)) = 0.5d0*(xl(j,amid(i)) + xl(j,bmid(i)))
          end do ! j
          ixl(i) = i
        endif
      end do ! i

!     Bottom and top

      if(ixl(17).eq.0) then
        do j = 1,3
          xl(j,17) = 0.50d0*(xl(j,13) + xl(j,14) + xl(j,15) + xl(j,16))
     &             - 0.25d0*(xl(j, 1) + xl(j, 2) + xl(j, 3) + xl(j, 4))
        end do ! j
        ixl(17) = 17
      endif

      if(ixl(22).eq.0) then
        do j = 1,3
          xl(j,22) = 0.50d0*(xl(j,18) + xl(j,19) + xl(j,20) + xl(j,21))
     &             - 0.25d0*(xl(j, 5) + xl(j, 6) + xl(j, 7) + xl(j, 8))
        end do ! j
        ixl(22) = 22
      endif

!     Mid-face

      if(ixl(23).eq.0) then
        do j = 1,3
          xl(j,23) = 0.50d0*(xl(j,13) + xl(j, 9) + xl(j,10) + xl(j,18))
     &             - 0.25d0*(xl(j, 1) + xl(j, 2) + xl(j, 5) + xl(j, 6))
        end do ! j
        ixl(23) = 23
      endif

      if(ixl(24).eq.0) then
        do j = 1,3
          xl(j,24) = 0.50d0*(xl(j,14) + xl(j,10) + xl(j,11) + xl(j,19))
     &             - 0.25d0*(xl(j, 2) + xl(j, 3) + xl(j, 6) + xl(j, 7))
        end do ! j
        ixl(24) = 24
      endif

      if(ixl(25).eq.0) then
        do j = 1,3
          xl(j,25) = 0.50d0*(xl(j,15) + xl(j,11) + xl(j,12) + xl(j,20))
     &             - 0.25d0*(xl(j, 3) + xl(j, 4) + xl(j, 7) + xl(j, 8))
        end do ! j
        ixl(25) = 25
      endif

      if(ixl(26).eq.0) then
        do j = 1,3
          xl(j,26) = 0.50d0*(xl(j,16) + xl(j,12) + xl(j, 9) + xl(j,21))
     &             - 0.25d0*(xl(j, 1) + xl(j, 4) + xl(j, 8) + xl(j, 5))
        end do ! j
        ixl(26) = 26
      endif

!     Center node

      if(ixl(27).eq.0) then
        do j = 1,3
          xl(j,27) = 0.25d0*(xl(j,13) + xl(j,14) + xl(j,15) + xl(j,16)
     &                     + xl(j,18) + xl(j,19) + xl(j,20) + xl(j,21)
     &                     + xl(j,23) + xl(j,24) + xl(j,25) + xl(j,26)
     &                     - xl(j, 1) - xl(j, 2) - xl(j, 3) - xl(j, 4)
     &                     - xl(j, 5) - xl(j, 6) - xl(j, 7) - xl(j, 8))

        end do ! j
        ixl(27) = 27
      endif

      end subroutine bcor3d

      subroutine xbcor3d(ss,xl, x)

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute shape functions and coordinates for each point

!      Inputs:
!         ss(3)   - Natural coordinates for point
!         xl(3,*) - Nodal coordinates for brick

!      Outputs:
!         x(3)    - Cartesian coordinates for point
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: j,l, ix(27),iy(27),iz(27)
      real (kind=8) :: ss(3),xl(3,27),x(3)
      real (kind=8) :: lshp(3,3),shp

      data      ix/1,3,3,1, 1,3,3,1, 1,3,3,1, 2,3,2,1,2, 2,3,2,1,2,
     &             2,3,2,1,2/
      data      iy/1,1,3,3, 1,1,3,3, 1,1,3,3, 1,2,3,2,2, 1,2,3,2,2,
     &             1,2,3,2,2/
      data      iz/1,1,1,1, 3,3,3,3, 2,2,2,2, 1,1,1,1,1, 3,3,3,3,3,
     &             2,2,2,2,2/


      save

      do j = 1,3
        lshp(1,j) = 0.5d0*ss(j)*(ss(j) - 1.d0)
        lshp(2,j) = (1.d0 - ss(j)*ss(j))
        lshp(3,j) = 0.5d0*ss(j)*(ss(j) + 1.d0)
      end do ! j

      do j = 1,3
        x(j) = 0.0d0
      end do

      do l = 1,27
        shp = lshp(ix(l),1)*lshp(iy(l),2)*lshp(iz(l),3)
        do j = 1,3
          x(j) = x(j) + shp*xl(j,l)
        end do
      end do

      end subroutine xbcor3d

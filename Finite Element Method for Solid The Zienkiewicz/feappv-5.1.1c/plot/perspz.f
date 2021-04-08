!$Id:$
      subroutine perspz(x,ix, ip, nen1,nen,ndm,numnp,numel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Sort perspective projection of coordinates by z-values

!      Inputs:
!         x (ndm,*) - Nodal coordinates
!         ix(nen1,*)- Element connection list
!         ndm       - 1st dimension of x
!         nen1      - 1st dimension of ix
!         nen       - Number nodes connected to elements
!         numel     - Total number of elements
!         numnp     - Total number of nodal points

!      Outputs:
!         ip(*)     - Element order for z-coordinates
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'ppers.h'
      include  'pdata4.h'
      include  'pointer.h'
      include  'comblk.h'

      integer       :: i,ii, n,nen1,nen,ndm,numnp,numel,nne
      integer       :: ix(nen1,numel), ip(numel), ic(numel)
      real (kind=8) :: t(3),x(ndm,numnp)
      real (kind=4) :: zmax,zn(numnp),ze(numel)

      save

!     Loop over data points and find projection

      do n=1,numnp
        if(mr(npty-1+n).ge.0) then
          do i=1,3
            t(i) = x(i,n) - e(i)
          end do
          zn(n) = -real(xlbda(3,1)*t(1)
     &                + xlbda(3,2)*t(2)
     &                + xlbda(3,3)*t(3))
        else
          zn(n) = 0.0e0
        endif
      end do

!     Search visible faces for depth sort

      do n = 1,nfac
        nne = ip(n)
        if(nne.gt.0 .and. nne.le.numel) then
          if(ix(nen1,nne).gt.0) then
            zmax  = 0.0e0
            do i = 1,nen
              ii = abs(ix(i,nne))
              if( ii.gt.0 .and. ii.le.numnp) then
                zmax = max(zmax,zn(ii))
              endif
            end do
            ze(nne) = zmax
          endif
        endif
      end do ! n

!     Sort element plot order array 'ip' to produce hidden surface

      call merges ( -1, ze, 1, ip, nfac, ic )

      end subroutine perspz

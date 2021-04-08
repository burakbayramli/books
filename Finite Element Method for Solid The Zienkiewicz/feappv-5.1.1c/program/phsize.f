!$Id:$
      subroutine phsize(x,xl,ix)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute maximum and minimum element size

!      Inputs:
!         x(ndm,*)    - Nodal coordinates
!         xl(ndm,*)   - Element nodal coordinates
!         ix(nen1,*)  - Element nodal coordinates

!      Outputs:
!         hsize(2)    - Element min/max size
!                       1 = minimum; 2 = maximum
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'  ! numnp,numel,nen
      include   'sdata.h'  ! ndm,nen1
      include   'iofile.h' ! iow
      include   'qudshp.h' ! hsize(:)

      integer       :: ix(nen1,numel)
      real (kind=8) :: x(ndm,*), xl(ndm,*)

!     Local variables

      integer       :: i,n, nel

      save

!     Initialize

      hsize(:)  = 0.0d0
      hksize(:) = 0.0d0

!     Loop over elements

      do n = 1,numel

!       Loop over element nodes set local coordinates

        nel = 0
        do i = 1,nen
          if(ix(i,n).gt.0) then
            nel = nel + 1
            xl(:,nel) = x(:,ix(i,n))
          endif
        end do ! i

!       Compute hsize(:) for element

        call hsizend(xl, ndm,nel)

      end do ! n

!     Output results

      if(max(hsize(1),hsize(2)).gt.0.0d0) then
        write(iow,2001) hsize
      endif
      if(max(hksize(1),hksize(2)).gt.0.0d0) then
        write(iow,2002) hksize
      endif

!     Formats

2001  format(/5x,'E l e m e n t   S i z e   V a l u e s'/
     &       10x,'h-minimum =',1p,1e12.4/
     &       10x,'h-maximum =',1p,1e12.4)

2002  format(/5x,'E l e m e n t   K n o t   S i z e   V a l u e s'/
     &       10x,'hk-minimum =',1p,1e12.4/
     &       10x,'hk-maximum =',1p,1e12.4)

      end ! subroutine phsize

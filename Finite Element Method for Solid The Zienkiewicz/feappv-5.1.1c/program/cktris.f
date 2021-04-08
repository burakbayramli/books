!$Id:$
      subroutine cktris(ix,xl,shp,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Check isoparametric triangles elements for valid data

!      Inputs:
!         ix(*)     - Nodes connected to element
!         xl(ndm,*) - Nodal coordinates for element
!         ndm       - Spatial dimension of problem

!      Outputs:
!         None

!      Scratch:
!         shp(*)    - Storage for shape functions
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'eldata.h'
      include  'iofile.h'
      include  'pointer.h'
      include  'comblk.h'

      integer       :: ndm, ineg, i,l
      real (kind=8) :: xsj

      integer       :: ic(2,7)
      integer       :: ix(*)
      real (kind=8) :: shp(*),xl(ndm,*),el(3,7)

      save

      data el/0.0d0,0.0d0,1.0d0, 1.0d0,0.0d0,0.0d0, 0.0d0,1.0d0,0.0d0,
     &        0.5d0,0.0d0,0.5d0, 0.5d0,0.5d0,0.0d0, 0.0d0,0.5d0,0.5d0,
     &        0.3333333333333d0, 0.3333333333333d0, 0.3333333333333d0/

!     Check element for input errors

      ineg = 0
      do l = 1,nel
        if(ix(l).gt.0) then
          if(mr(np(190)-1+ix(l)).lt.0) then
            ineg       = ineg + 1
            ic(1,ineg) = l
            ic(2,ineg) = abs(ix(l))
          endif
        endif
      end do

      if(ineg.gt.0) then
        write(iow,2000) n_el,(ic(1,i),ic(2,i),i=1,ineg)
        if(ior.lt.0) then
          write(*,2000) n_el,(ic(1,i),ic(2,i),i=1,ineg)
        endif
      else
        do l = 1,nel
          call  shptri(el(1,l),xl,ndm,nel,xsj,shp, .true.)
          if(xsj.le.0.0d0) then
            ineg       = ineg + 1
            ic(1,ineg) = l
            ic(2,ineg) = abs(ix(l))
          endif
        end do

        if(ineg.gt.0) then
          write(iow,2001) n_el,(ic(1,i),ic(2,i),i=1,ineg)
          if(ior.lt.0) then
            write(*,2001) n_el,(ic(1,i),ic(2,i),i=1,ineg)
          endif
        endif

!       Try to fix element by reversing order of nodes on 'ix'

        if(ineg.eq.nel) then
          if(ior.lt.0) write(*,2002) n_el
          l     = ix(2)
          ix(2) = ix(3)
          ix(3) = l
          if(nel.eq.6 .or. nel.eq.7) then
            l     = ix(6)
            ix(6) = ix(4)
            ix(4) = l
          endif
        endif

      endif

!     Formats

2000  format(' >Element',i8,' coordinates not input for nodes:'/
     &      ('                    Local =',i3,' Global =',i4))

2001  format(' >Element',i8,' has negative jacobian at nodes:'/
     &      ('                    Local =',i3,' Global =',i4))

2002  format(' >Element',i8,' Reverse numbers to fix negative jacobian')

      end subroutine cktris

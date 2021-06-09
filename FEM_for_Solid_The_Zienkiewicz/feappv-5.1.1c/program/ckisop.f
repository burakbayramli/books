!$Id:$
      subroutine ckisop(ix,xl,shp,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Check isoparametric elements for data input errors

!      Inputs:
!         ix(*)     - List of nodes connected to element
!         xl(ndm,*) - Nodal coordinates for element
!         ndm       - Spatial dimension of mesh

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

      integer       :: xn(9),yn(9),ic(18)
      integer       :: ix(*)
      real (kind=8) :: ss(2),shp(*),xl(ndm,*)

      save

      data      xn/-1, 1,1,-1, 0,1,0,-1,0/
      data      yn/-1,-1,1, 1,-1,0,1, 0,0/

!     Check element for input errors

      ineg = 0
      do l = 1,nel
        if(ix(l).gt.0) then
          if(mr(np(190)-1+ix(l)).lt.0) then
            ic(ineg+1) = l
            ic(ineg+2) = abs(ix(l))
            ineg = ineg + 2
          endif
        endif
      end do
      if(ineg.gt.0) then
        write(iow,2000) n_el,(ic(i),i=1,ineg)
        if(ior.lt.0) write(*,2000) n_el,(ic(i),i=1,ineg)
      else
        do l = 1,nel
          ss(1) = xn(l)
          ss(2) = yn(l)
          call  shp2d (ss,xl,shp,xsj,ndm,nel,ix,.false.)
          if(xsj.le.0.0d0) then
            ic(ineg+1) = l
            ic(ineg+2) = abs(ix(l))
            ineg = ineg + 2
          endif
        end do
        if(ineg.gt.0) then
          write(iow,2001) n_el,(ic(i),i=1,ineg)
          if(ior.lt.0) write(*,2001) n_el,(ic(i),i=1,ineg)
        endif

!       Try to fix element

        if(ineg.eq.2*nel) then
          if(nel.eq.3) then
            if(ior.lt.0) write(*,2002) n_el
            l     = ix(2)
            ix(2) = ix(3)
            ix(3) = l
          elseif(nel.eq.4) then
            if(ior.lt.0) write(*,2002) n_el
            l     = ix(1)
            ix(1) = ix(4)
            ix(4) = l
            l     = ix(2)
            ix(2) = ix(3)
            ix(3) = l
          endif
        endif
      endif

2000  format(' >Element',i8,' coordinates not input for nodes:'/
     &      ('                    Local =',i3,' Global =',i4))

2001  format(' >Element',i8,' has negative jacobian at nodes:'/
     &      ('                    Local =',i3,' Global =',i4))

2002  format(' >Element',i8,' Reverse numbers to fix negative jacobian')

      end subroutine ckisop

!$Id:$
      subroutine solid3d(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!     Three Dimensional Solid Element Driver
!_____________________________________________________________________c

      implicit  none

      include  'cdata.h'
      include  'eldata.h'
      include  'evdata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'qudshp.h'
      include  'strnum.h'
      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw
      integer       :: i,tdof

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,*),xl(ndm,*),tl(*),s(nst,nst),p(nst)
      real (kind=8) :: th(125)

      save

!     Extract type data

      stype = nint(d(16))
      etype = nint(d(17))
      dtype = nint(d(18))
      hflag = d(30).eq.1.d0

!     Set nodal temperatures: Can be specified or computed

      if(isw.gt.1) then
        tdof = nint(d(19))
        if(tdof.le.0) then
          do i = 1,nel ! {
            th(i) = tl(i)
          end do ! i     }
        else
          do i = 1,nel ! {
            th(i) = ul(tdof,i)
          end do ! i     }
        endif
      endif

!     Go to correct process

      go to(1,2,3,3,5,3,3,3,3,3,3,3), isw

!     Output element type

      if(isw.eq.0 .and. ior.lt.0) then
        write(*,*) '   SOLID3D: 3-d Solid Linear/Finite Defm. Element.'
      endif

      return

!     Input material properties

1     write(iow,2001)
      if(ior.lt.0) write(*,2001)
      call inmate(d,tdof,  0 ,1)
      nh1 = nh1 + 2
      if(etype.eq.3) then
        nh1 = nh1 + 12
      endif

!     Set tdof to zero if 1, 2, 3, or larger than ndf

      if(tdof.gt.ndf) then
        write(iow,3003)
        if(ior.lt.0) write(*,3003)
        tdof = 0
      elseif(tdof.ge.1 .and. tdof.le.3) then
        write(iow,3004)
        if(ior.lt.0) write(*,3004)
        tdof = 0
      endif

!     Deactivate dof in element for dof > 3

      do i = 4,ndf
        ix(i) = 0
      end do

!     If temperature dof is specified activate dof

      if(tdof.gt.0) then
        ix(tdof) = 1
      endif

!     Set plot sequence for 4-nod tet or 8-node brick

      pstyp = 3

!     Set number of stress projections

      istv = max(istv,6)

      return

!     Check element for errors

2     call ckbrk8 ( n_el, ix, xl, ndm, nel, shp3 )

      return

!     Compute stress-divergence vector (p) and stiffness matrix (s)

3     if(etype.eq.1) then

!       Displacement Model

        if(dtype.gt.0) then
          call sld3d1(d,ul,xl,th,s,p,ndf,ndm,nst,isw)
        else
          call fld3d1(d,ul,xl,s,p,ndf,ndm,nst,isw)
        endif

      endif

      return

!     Compute mass or geometric striffness matrix

5     if(imtyp.eq.1) then
        call mass3d(d,xl,s,p,ndf,ndm,nst)
      else
!       Geometric stiffness here
      endif
      return

!     Formats for input-output

2001  format(
     & /5x,'T h r e e   D i m e n s i o n a l   S o l i d',
     &     '   E l e m e n t'/)
3003  format(' *WARNING* Thermal d.o.f. > active d.o.f.s : Set to 0')
3004  format(' *WARNING* Thermal d.o.f. can not be 1 to 3: Set to 0')

      end subroutine solid3d

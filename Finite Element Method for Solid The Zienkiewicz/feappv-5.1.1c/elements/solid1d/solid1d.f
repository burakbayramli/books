!$Id:$
      subroutine solid1d(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plane and axisymmetric linear elastic element driver

!      Inputs:
!         d(*)      - Element parameters
!         ul(ndf,*) - Current nodal solution parameters
!         xl(ndm,*) - Nodal coordinates
!         ix(*)     - Global nodal connections
!         tl(*)     - Nodal temp vector
!         ndf       - Degree of freedoms/node
!         ndm       - Mesh coordinate dimension
!         nst       - Element array dimension
!         isw       - Solution option switch

!      Outputs:
!         s(nst,*)  - Element array
!         p(*)      - Element vector
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'eqsym.h'
      include  'evdata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'setups.h'
      include  'strnum.h'
      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw, i,nhv, ix(*)
      real (kind=8) :: d(*),ul(ndf,4),xl(ndm,*),tl(*),s(nst,nst),p(nst)

      save

!     Extract type data

      stype = nint(d(16))
      etype = nint(d(17))
      dtype = nint(d(18))
      hflag = d(30).eq.1.0d0

!     Input material properties

      if(isw.eq.1) then

        write(iow,2001)
        if(ior.lt.0) write(*,2001)
        nhv = 0
        call inmate(d,nhv,0,1)
        if(etype.eq.2) then
          nh1 = nh1 + 2
        endif

!       Deactivate dof in element for dof > 2

        do i = 2,ndf
          ix(i) = 0
        end do ! i

!       Set plot sequence for lines

        pstyp = 1

!       Set number of projected stresses

        istv = max(istv,6)

!     Check element for errors in input data

      elseif(isw.eq.2) then

!     Compute mass or geometric stiffness matrix

      elseif(isw.eq.5) then

        if(imtyp.eq.1) then
          call mass1d(d,xl,s,p,ndf,ndm,nst)
        else
!         put call to geometric stiffness routine here
        endif

      endif

!     Compute stress-divergence vector (p) and stiffness matrix (s)

!     Compute residuals and tangents for parts

      if(isw.eq.1 .or. isw.eq.3 .or. isw.eq.4  .or.
     &   isw.eq.6 .or. isw.eq.8 .or. isw.eq.10) then

!       Displacement Model

        if(etype.eq.1) then

          if(dtype.gt.0) then
            call sld1d1(d,ul,xl,tl,s,p,ndf,ndm,nst,isw)
          else
            write(*,*) ' No finite deformation in FEAPpv'
          endif

!       Mixed Model (B-Bar)

        elseif(etype.eq.2) then

          if(dtype.gt.0) then
            call sld1d2(d,ul,xl,tl,s,p,ndf,ndm,nst,isw)
          else
            write(*,*) ' No finite deformation in FEAPpv'
          endif

!       Enhanced Strain Model (B-Bar)

        elseif(etype.eq.3) then

          if(dtype.gt.0) then
            call sld1d3(d,ul,xl,tl,s,p,ndf,ndm,nst,isw)
          else
!           No routine supplied
          endif

        endif
      endif

!     Formats for input-output

2001  format(
     & /5x,'O n e   D i m e n s i o n a l   S o l i d   E l e m e n t'/)

      end subroutine solid1d

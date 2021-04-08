!$Id:$
      subroutine solid2d(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Plane and axisymmetric linear thermo-elastic element routine
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'eldata.h'
      include  'evdata.h'
      include  'hdata.h'
      include  'iofile.h'
      include  'pmod2d.h'
      include  'qudshp.h'
      include  'strnum.h'
      include  'vem_data.h'
      include  'comblk.h'

      integer       :: ndf,ndm,nst,isw
      integer       :: i, nhv, ne

      integer       :: ix(*)
      real (kind=8) :: d(*),ul(ndf,16),xl(ndm,*),tl(*)
      real (kind=8) :: s(nst,nst,2),p(nst,2)
      real (kind=8) :: shp(3,9), trS, fact

      save

!     Extract type data

      stype = nint(d(16))
      etype = nint(d(17))
      dtype = nint(d(18))
      hflag = d(30).eq.1.0d0

      if(nint(d(189)).eq.8) then  ! VEM

!       Set VEM order from element data

        if(ix(nen+8).gt.0) then
          k_order = ix(nen+8)
        else
          k_order = 1
        endif

      endif

!     Input material properties

      if(isw.eq.1) then
        write(iow,2001)
        if(ior.lt.0) write(*,2001)
        nhv = 0
        call inmate(d,i,nhv,1)
        if(etype.eq.2) then
          nh1 = nh1 + 2
        endif

!       Deactivate dof in element for dof > 2

        do i = 3,ndf
          ix(i) = 0
        end do ! i

!       Set plot sequence for 2-d

        pstyp = 2

!       Set number of projected stresses

        istv = max(9,istv)

!     Check element for errors in input data

      elseif(isw.eq.2) then
        if(nel.eq.3. .or. nel.eq.6 .or. nel.eq.7) then
          call cktris(ix,xl,shp,ndm)
        else
          call ckisop(ix,xl,shp,ndm)
        endif

!     Compute mass or geometric stiffness matrix

      elseif(isw.eq.5) then

        if(imtyp.eq.1) then
          call mass2d(d,xl,ix,s,p,ndf,ndm,nst)
        else
!         put call to geometric stiffness routine here
        endif

      endif

!     Compute residuals and tangents for parts

      if(isw.eq.1 .or. isw.eq.3 .or. isw.eq.4  .or.
     &   isw.eq.6 .or. isw.eq.8 .or. isw.eq.10) then

!       Displacement Model

        if(etype.eq.1) then

          if(dtype.gt.0) then
            call sld2d1(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)
          else
            call fld2d1(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
          endif

!       Mixed Model (B-Bar)

        elseif(etype.eq.2) then

          if(dtype.gt.0) then
            call sld2d2(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)
          else
            call fld2d2(d,ul,xl,ix,s,p,ndf,ndm,nst,isw)
          endif

!       Enhanced Strain Model

        elseif(etype.eq.3) then

          if(dtype.gt.0) then
            call sld2d3(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)
          else
!           No routine supplied
          endif

        endif

!       VEM stabilization

        if(vemfl .and. (isw.eq.3 .or. isw.eq.6)) then

!         Compute stabilizing parameter

          if(isw.eq.3) then

            if(k_order.eq.1) then
              ne = nel
            else ! k_order = 2
              ne = nel/2
            endif

            trS = 0.0d0
            do i = 1,ndf*nel
              trS = trS + s(i,i,1)
            end do ! i

!           Scaling for near incompressiblity (nu > 0.4)

            if(d(2).gt.0.4) then
              fact = min(1.0d0,(7.d0 - 1.4d1*d(2))/(1.d0 + d(2)))
              trS  = trS*fact
            endif

            trS = trS/dble(ne)

            s(:,:,2) = 0.0d0
            p(:,2)   = 0.0d0
            call vem_stab2d(trS, ul, s(1,1,2),p(1,2), ndf,nel,nst,2,isw)
            s(:,:,1) = s(:,:,1) + s(:,:,2)
            p(:,1)   = p(:,1) + p(:,2)
            s(:,:,2) = 0.0d0
            p(:,2)   = 0.0d0

          endif
        endif

      endif

!     Formats for input-output

2001  format(
     & /5x,'T w o   D i m e n s i o n a l   S o l i d   E l e m e n t'/)

      end subroutine solid2d

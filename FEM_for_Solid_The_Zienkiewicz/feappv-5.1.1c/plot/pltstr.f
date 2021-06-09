!$Id:$
      subroutine pltstr(dt,sp,st,numnp,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute nodal stress values from integrals of
!               element values.

!      Inputs:
!         dt(*)      - Integral of mesh volume (diagonal weights)
!         st(numnp,*)- Integral of element values x volume
!         numnp      - Number of nodes in mesh
!         ndm        - Spatial dimension of mesh

!      Outputs:
!         sp(numnp,*)- Principal values of stresses.
!                      N.B. Assumes st(*,1) = sig_11
!                      N.B. Assumes st(*,2) = sig_22
!                      N.B. Assumes st(*,3) = sig_33
!                      N.B. Assumes st(*,4) = sig_12
!                      N.B. Assumes st(*,5) = sig_23
!                      N.B. Assumes st(*,6) = sig_31
!         st(numnp,*)- Nodal values of plot quantities described
!                      by elements
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata3.h'
      include  'strnum.h'
      include  'prstrs.h'
      include  'comblk.h'

      integer       :: numnp,ndm, i,ii
      real (kind=8) :: dh,press, third
      real (kind=8) :: dt(numnp),sp(numnp,*),st(numnp,*),sig(9)

      save

      data      third / 0.3333333333333333d0 /

      do ii = 1,numnp

        dh = dt(ii)
        if(dh.ne.0.0d0) then

!         Error estimator

          hr(ner+ii-1) = hr(ner+ii-1)/dh

!         Stress projections

          do i = 1,npstr-1
            st(ii,i) = st(ii,i)/dh
          end do

          if(istv.gt.0) then

!           Two-dimensional

            if(ndm.eq.2) then
              sig(1)   = st(ii,1)
              sig(2)   = st(ii,2)
              sig(3)   = st(ii,3)
              sig(4)   = st(ii,4)
              call pstr2d(sig,sig(7))
              sp(ii,1) = sig(7)
              sp(ii,2) = sig(8)
              sp(ii,3) = sig(3)
              sp(ii,4) = sig(9)

!           Three-dimensional

            elseif(ndm.eq.3) then
              sig(1)   = st(ii,1)
              sig(2)   = st(ii,2)
              sig(3)   = st(ii,3)
              sig(4)   = st(ii,4)
              sig(5)   = st(ii,5)
              sig(6)   = st(ii,6)
              call pstr3d(sig,sig(7))
              sp(ii,1) = sig(7)
              sp(ii,2) = sig(8)
              sp(ii,3) = sig(9)
            endif

!           Compute mean stress and mises stress
            press    = (sp(ii,1) + sp(ii,2) + sp(ii,3))*third
            sp(ii,5) = press
            sp(ii,6) = sqrt(1.5d0*((sp(ii,1) - press)**2
     &                           + (sp(ii,2) - press)**2
     &                           + (sp(ii,3) - press)**2))
            sp(ii,7) = (third*(sp(ii,1)-press)**3
     &                       *(sp(ii,2)-press)**3
     &                       *(sp(ii,3)-press)**3)
            if(sp(ii,7).lt.0.0d0) then
              sp(ii,7) = -(abs(sp(ii,7))**third)
            else
              sp(ii,7) =  (abs(sp(ii,7))**third)
            endif
          endif
        endif

      end do

      end subroutine pltstr

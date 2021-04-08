!$Id:$
      subroutine pbezlin()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Construct Bezier mesh and Linear meshes for plots

!      Inputs:

!      Outputs:
!         none   - Users are responsible for generating outputs
!                  through common blocks, etc.  See programmer
!                  manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'qudshp.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    setvar,palloc
      integer    nd_pre, ne_pre

!     Set linear mesh for plot use

      ne_lin = numel*npl_int**ndm
      ne_pre = ne_lin
      nd_pre = max(2*ndm*ne_lin,4*(ndm-1)*ne_lin)
      setvar = palloc(276, 'I_LIN',19*ne_lin  , 1)     ! IX_LIN
      setvar = palloc(277, 'X_LIN', ndm*nd_pre, 2)     ! X_LIN
      call pzeroi(mr(np(276)),19*ne_lin)               ! Zero IX_LIN
      call pzero (hr(np(277)),ndm*nd_pre)              ! Zero X_LIN
      call pmsh_lin(mr(np(32)) , mr(np(33)) , hr(np(43)),
     &              hr(np(44)) , hr(np(263)), hr(np(264)),
     &              mr(np(276)), hr(np(277)),nd_pre,ne_pre )
      if(ne_lin.gt.ne_pre .or. nd_lin.gt.nd_pre) then
        write(*,*) ' Element Storage =',ne_lin,ne_pre
        write(*,*) ' Coord.  Storage =',nd_lin,nd_pre
      endif
      setvar = palloc(276, 'I_LIN',  19*ne_lin, 1)     ! IX_LIN
      setvar = palloc(277, 'X_LIN', ndm*nd_lin, 2)     ! X_LIN

      end

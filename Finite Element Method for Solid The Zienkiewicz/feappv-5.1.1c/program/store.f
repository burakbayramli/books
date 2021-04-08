!$Id:$
      subroutine store(v,w,nv,itrn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Store/retrieve BFGS vectores in blank common arrays

!      Inputs:
!         v(*),w(*) - BFGS Vectors to save
!         nv        - Number of vector pair to save
!         itrn      - Switch: = 1 to save; = 2 to retrieve

!      Outputs:
!         none      - Saves in hr(*):  (itrn = 1)
!         v(*),w(*) - BFGS Vector   :  (itrn = 2)
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'p_point.h'
      include  'pointer.h'
      include  'comblk.h'

      integer   nv, itrn, ns

      real (kind=8) :: v(*),w(*)

      save

      ns = (neq+neq)*(nv-1)

      if(itrn.eq.1) then
        point = np(71) + ns
        call pmove(v,hr(point),neq)
        point = point + neq
        call pmove(w,hr(point),neq)
      elseif(itrn.eq.2) then
        point = np(71) + ns
        call pmove(hr(point),v,neq)
        point = point + neq
        call pmove(hr(point),w,neq)
      endif

      end

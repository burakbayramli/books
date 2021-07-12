c$Id:$
      subroutine store(v,w,nv,itrn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Store/retrieve BFGS vectores in blank common arrays

c      Inputs:
c         v(*),w(*) - BFGS Vectors to save
c         nv        - Number of vector pair to save
c         itrn      - Switch: = 1 to save; = 2 to retrieve

c      Outputs:
c         none      - Saves in hr(*):  (itrn = 1)
c         v(*),w(*) - BFGS Vector   :  (itrn = 2)
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'pointer.h'
      include  'comblk.h'

      integer   nv, itrn, ns, mtp

      real*8    v(*),w(*)

      save

      ns = (neq+neq)*(nv-1)

      if(itrn.eq.1) then
        mtp = np(71) + ns
        call pmove(v,hr(mtp),neq)
        mtp = mtp + neq
        call pmove(w,hr(mtp),neq)
      elseif(itrn.eq.2) then
        mtp = np(71) + ns
        call pmove(hr(mtp),v,neq)
        mtp = mtp + neq
        call pmove(hr(mtp),w,neq)
      endif

      end

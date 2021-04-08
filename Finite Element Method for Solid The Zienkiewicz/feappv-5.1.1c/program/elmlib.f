!$Id:$
      subroutine elmlib(d,u,x,ix,t,s,p,i,j,k,jel,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Element library driver routine
!               N.B. Must set library flags in Subroutine PELNUM
!                    for new program modules

!      Inputs:
!         d(*)    - Material parameters
!         u(*)    - Element solution parameters
!         x(*)    - Element nodal coordinates
!         ix(*)   - Element nodal numbers
!         t(*)    - Element temperatures
!         i       - Number dof/node           (ndf)
!         j       - Spatial dimension of mesh (ndm)
!         k       - Size of element arrays    (nst)
!         jel     - Element type number

!      Outputs:
!         d(*)    - Material parameters (isw = 1 only)
!         s(*,*)  - Element array
!         p(*)    - Element vector
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'eldata.h'
      include  'iofile.h'

      integer       :: i,j,k,jel,isw, ix(*)
      real (kind=8) :: p(*),s(*),d(*),u(*),x(*),t(*)

      save

      if(isw.ge.3 .and. k.gt.0) then
        call pzero(s,k*k)
        call pzero(p,k  )
      endif

!     User element routines
      if(jel.gt.0) then

        if(    jel.eq. 1) then
          call elmt01(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq. 2) then
          call elmt02(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq. 3) then
          call elmt03(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq. 4) then
          call elmt04(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq. 5) then
          call elmt05(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq. 6) then
          call elmt06(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq. 7) then
          call elmt07(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq. 8) then
          call elmt08(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq. 9) then
          call elmt09(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq.10) then
          call elmt10(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq.11) then
          call elmt11(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq.12) then
          call elmt12(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq.13) then
          call elmt13(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq.14) then
          call elmt14(d,u,x,ix,t,s,p,i,j,k,isw)
        elseif(jel.eq.15) then
          call elmt15(d,u,x,ix,t,s,p,i,j,k,isw)
        else
          go to 400
        endif

!     Program element library routines
      elseif(isw.gt.0) then

!       1-D Element library
        if(j.eq.1) then

          if(    jel.eq. -1) then
            call solid1d(d,u,x,ix,t,s,p,i,j,k,isw)
          elseif(jel.eq. -2) then
            call trussnd(d,u,x,ix,t,s,p,i,j,k,isw)
          elseif(jel.eq. -3) then
            write(iow,*) ' No 1-d frame element available. n_el =',n_el
            write(  *,*) ' No 1-d frame element available. n_el =',n_el
            call plstop(.true.)
          elseif(jel.eq. -4) then
            write(iow,*) ' No 1-d plate element available. n_el =',n_el
            write(  *,*) ' No 1-d plate element available. n_el =',n_el
            call plstop(.true.)
          elseif(jel.eq. -5) then
            write(iow,*) ' No 1-d shell element available. n_el =',n_el
            write(  *,*) ' No 1-d shell element available. n_el =',n_el
            call plstop(.true.)
          elseif(jel.eq. -6) then
            write(iow,*) ' No 1-d membrane element available.',
     &                   '  n_el =',n_el
            write(  *,*) ' No 1-d membrane element available.',
     &                   '  n_el =',n_el
            call plstop(.true.)
          elseif(jel.eq. -7) then
            write(iow,*) ' No 1-d thermal element available.',
     &                   '  n_el =',n_el
            write(  *,*) ' No 1-d thermal element available.',
     &                   '  n_el =',n_el
            call plstop(.true.)
          elseif(jel.eq. -8) then
            write(iow,*) ' No 1-d thermal convectionelement available.',
     &                   '  n_el =',n_el
            write(  *,*) ' No 1-d thermal convectionelement available.',
     &                   '  n_el =',n_el
            call plstop(.true.)
          elseif(jel.eq. -9) then
            call pointnd(d,u,s,p,i,k,isw)
          else
            go to 400
          endif

!       2-D element library
        elseif(j.eq.2) then

          if(    jel.eq. -1) then
            call solid2d(d,u,x,ix,t,s,p,i,j,k,isw)
          elseif(jel.eq. -2) then
            call trussnd(d,u,x,ix,t,s,p,i,j,k,isw)
          elseif(jel.eq. -3) then
            call frame2d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -4) then
            call plate2d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -5) then
            call shell2d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -6) then
            write(iow,*) ' No 2-d membrane element available.',
     &                   '  n_el =',n_el
            write(  *,*) ' No 2-d membrane element available.',
     &                   '  n_el =',n_el
            call plstop(.true.)
          elseif(jel.eq. -7) then
            call therm2d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -8) then
            call convec2d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -9) then
            call pointnd(d,u,s,p,i,k,isw)
          else
            go to 400
          endif

!       3-D Element library
        elseif(j.eq.3) then

          if(    jel.eq. -1) then
            call solid3d(d,u,x,ix,t,s,p,i,j,k,isw)
          elseif(jel.eq. -2) then
            call trussnd(d,u,x,ix,t,s,p,i,j,k,isw)
          elseif(jel.eq. -3) then
            call frame3d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -4) then
            write(iow,*) ' No 3-d plate element available:',
     &                   '  Use SHELL.',n_el
            write(  *,*) ' No 3-d plate element available:',
     &                   '  Use SHELL.',n_el
            call plstop(.true.)
          elseif(jel.eq. -5) then
            call shell3d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -6) then
            call membr3d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -7) then
            call therm3d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -8) then
            call convec3d(d,u,x,ix,s,p,i,j,k,isw)
          elseif(jel.eq. -9) then
            call pointnd(d,u,s,p,i,k,isw)
          else
            go to 400
          endif

        endif

      endif

      return

!     Error
400   write(iow,4000) n_el,jel,isw
      write(  *,4000) n_el,jel,isw
      call plstop(.true.)

!     Format
4000  format('  *ERROR* ELMLIB: Element:',i6,', type number',i3,
     &       ' input, isw =', i3)

      end subroutine elmlib

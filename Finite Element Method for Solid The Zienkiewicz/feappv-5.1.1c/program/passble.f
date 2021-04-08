!$Id:$
      subroutine passble(s,p,ld,ix, jp,a,al,b,
     &                   afl,bfl, nsp,nov,jsw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    21/04/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Assemble global arrays from element arrays

!      Inputs:
!        s(*)   - Element array
!        p(*)   - Element vector
!        ld(*)  - Assembly numbers
!        ix(*)  - Global nodes for elements
!        jp(*)  - Pointer array
!        afl    - Matrix assembly if .true.
!        bfl    - Vector assembly if .true.
!        nsp    - Size of 's' and 'p'
!        nov    - Number of elements connected to 'ix'
!        jsw    - Switch parameter

!      Outputs:
!        a(*)   - Diagonal and upper global array
!        al(*)  - Lower global array
!        b(*)   - Global vector
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'compac.h'
      include   'compas.h'
      include   'eldata.h'
      include   'eqsym.h'
      include   'hdatam.h'
      include   'prstrs.h'
      include   'sdata.h'
      include   'strnum.h'
      include   'pointer.h'
      include   'comblk.h'

      include   'iofile.h'

      include   'setups.h'

      logical       :: afl,bfl
      integer       :: jsw,nsp,nov,  ld(*), ix(*),jp(*)
      real (kind=8) :: p(*),s(nsp,*), a(*),al(*),b(*)

      save

!     Add element projections to total array

      if(jsw.eq.8) then

!       Check that 'iste' was set in user element projection routine

        if(iel.gt.0 .and. iste.eq.0 .and. pstyp.gt.0) then
          write(  *,4000) iel
          write(iow,4000) iel
          call plstop(.true.)
        elseif(pstyp.gt.0) then

!         Prevent accumulation of weights for multiple elements

          if(nov.gt.0) then
            p(1:nen) = 0.0d0
          endif
          nov = nov + 1
          call seproj(p,s,p(nen+1),hr(nph),hr(nph+numnp),hr(ner),ix,
     &                nel,nen,numnp)
        endif

!     Stiffness and residual assembly

      elseif(afl.or.bfl) then

!       Assemble for real arithmetic

        call dasble(s,p,ld,jp,nsp,neqs,afl,bfl,b,al,a(neq+1),a)

      endif

!     Formats

4000  format(' --> ERROR in User Element elmt',i2/5x,
     &       'Variable ISTE not set in projection module.'/1x)

      end subroutine passble

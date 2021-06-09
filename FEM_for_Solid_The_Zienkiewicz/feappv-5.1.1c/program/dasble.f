!$Id:$
      subroutine dasble(s,p,ld,jp,ns,neqs,afl,bfl,  b,al,au,ad)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Assemble symmetric/unsymmetric arrays for 'DATRI'

!      Inputs:
!         s(ns,ns) - Element array to assemble
!         p(ns)    - Element vector to assemble
!         ld(ns)   - Local to Globasl equation numbers for assemble
!         jp(*)    - Pointer array for upper/lower parts of A array.
!         ns       - Size of element arrays
!         neqs     - Number of equations in A which are symmetric
!         afl      - If true, assemble A array
!         bfl      - If true, assemble B vector

!      Outputs:
!         b(*)     - Assembled right hand side B vector
!         al(*)    - Assembled lower part of A array
!         au(*)    - Assembled upper part of A array
!         ad(*)    - Assembled diagonal part of A array
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'compac.h'
      include  'compas.h'
      include  'setups.h'

      include  'pointer.h'
      include  'comblk.h'

      logical       :: afl, alfl, bfl
      integer       :: i, ii, j, jj, je, ns, neqs

      integer       :: ld(ns),jp(*)
      real (kind=8) :: al(*),au(*),ad(*),b(*),s(ns,ns),p(ns)

      save

!     Assemble matrix

      if(solver.and.afl) then

!       Check for compressed assembly

        if(compfl) then

          if(neqs.gt.1) then
            alfl = .false.
          else
            alfl = .true.
          endif

!         Compressed stiffness assembly

          if(castif) then
            call cassem(ad,au,al,s,mr(np(94)),mr(np(93)),
     &                  ld,ns,alfl,kbycol,kdiag,kall)

!         Compressed damping assembly

          elseif(cadamp) then
            call cassem(ad,au,al,s,mr(np(204)),mr(np(203)),
     &                  ld,ns,alfl,cbycol,cdiag,call)

!         Compressed mass assembly

          elseif(camass) then
            call cassem(ad,au,al,s,mr(np(91)),mr(np(90)),
     &                  ld,ns,alfl,mbycol,mdiag,mall)

!         Compressed user assembly

          elseif(causer) then
            call cassem(ad,au,al,s,mr(np(152)),mr(np(151)),
     &                  ld,ns,alfl,ubycol,udiag,uall)
          endif

        else

!       Loop through rows to perform assembly

          je = jp(neqs)
          do i = 1,ns
            if(ld(i).gt.0) then
              ii = ld(i) + 1

!             Loop through columns to perform assembly

              do j = 1,ns
                if(ld(j).eq.ld(i)) then
                  ad(ld(i)) = ad(ld(i)) + s(i,j)
                elseif(ld(j).gt.ld(i)) then
                  jj = ii + jp(ld(j)) - ld(j)
                  au(jj) = au(jj) + s(i,j)
                  if(ld(j).gt.neqs) then
                    al(jj-je) = al(jj-je) + s(j,i)
                  endif
                endif
              end do
            endif
          end do
        endif
      endif

!     Assemble a vector

      if(solver.and.bfl) then
        do i = 1,ns
          if(ld(i).gt.0) b(ld(i))  = b(ld(i))  + p(i)
        end do
      endif

!     User supplied assembler

      if(.not.solver) then
        call uasble(s,p,ld,ns,afl,bfl,b)
      endif

      end subroutine dasble

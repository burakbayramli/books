c$Id:$
      subroutine dasble(s,p,ld,jp,ns,neqs,afl,bfl,  b,al,au,ad)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Assemble symmetric/unsymmetric arrays for 'DATRI'

c      Inputs:
c         s(ns,ns) - Element array to assemble
c         p(ns)    - Element vector to assemble
c         ld(ns)   - Local to Globasl equation numbers for assemble
c         jp(*)    - Pointer array for upper/lower parts of A array.
c         ns       - Size of element arrays
c         neqs     - Number of equations in A which are symmetric
c         afl      - If true, assemble A array
c         bfl      - If true, assemble B vector

c      Outputs:
c         b(*)     - Assembled right hand side B vector
c         al(*)    - Assembled lower part of A array
c         au(*)    - Assembled upper part of A array
c         ad(*)    - Assembled diagonal part of A array
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'compac.h'
      include  'compas.h'
      include  'setups.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   afl, alfl, bfl
      integer   i, ii, j, jj, je, ns, neqs

      integer   ld(ns),jp(*)
      real*8    al(*),au(*),ad(*),b(*),s(ns,ns),p(ns)

      save

c     Assemble matrix

      if(solver.and.afl) then

c       Check for compressed assembly

        if(compfl) then

          if(neqs.gt.1) then
            alfl = .false.
          else
            alfl = .true.
          endif

c         Compressed stiffness assembly

          if(castif) then
            call cassem(ad,au,al,s,mr(np(3)),mr(np(2)),
     &                  ld,ns,alfl,kbycol,kdiag,kall)

c         Compressed damping assembly

          elseif(cadamp) then
            call cassem(ad,au,al,s,mr(np(19)),mr(np(18)),
     &                  ld,ns,alfl,cbycol,cdiag,call)

c         Compressed mass assembly

          elseif(camass) then
            call cassem(ad,au,al,s,mr(np(11)),mr(np(10)),
     &                  ld,ns,alfl,mbycol,mdiag,mall)

c         Compressed user assembly

          elseif(causer) then
            call cassem(ad,au,al,s,mr(np(152)),mr(np(151)),
     &                  ld,ns,alfl,ubycol,udiag,uall)
          endif

        else

c       Loop through rows to perform assembly

          je = jp(neqs)
          do i = 1,ns
            if(ld(i).gt.0) then
              ii = ld(i) + 1

c             Loop through columns to perform assembly

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

c     Assemble a vector

      if(solver.and.bfl) then
        do i = 1,ns
          if(ld(i).gt.0) b(ld(i))  = b(ld(i))  + p(i)
        end do
      endif

c     User supplied assembler

      if(.not.solver) then
        call uasble(s,p,ld,ns,afl,bfl,b)
      endif

      end

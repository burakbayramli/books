c$Id:$
      subroutine modify(b,ld,s,dul,nst)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Modify element residual for effects of specified
c               boundary values.

c               b(i) = b(i) - s(i,j)*dul(j)

c      Inputs:
c         ld(*)  - Array with negative entries where boundary
c                  solution to be imposed
c         s(*,*) - Element tangent array
c         dul(*) - Value of specified solution increments
c         nst    - Dimension of element arrays

c      Outputs:
c         b(*)   - Residual modified for effect of increments
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   nst,i,j, ii
      integer   ld(nst)
      real*8    b(nst),s(nst,nst),dul(nst)

c     Loop over columns and search for boundary terms

      do j = 1,nst
        if(ld(j).lt.0) then

c         Loop over rows to modify active equations

          do i = 1,nst
            ii = ld(i)
            if(ii.gt.0) then
              b(ii) = b(ii) - s(i,j)*dul(j)
            endif
          end do

        endif
      end do

      end

!$Id:$
      subroutine formhh(hh,g,neqg,neq)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Forms finite element G and H arrays

!      Inputs:
!         neqg   - Number of constraint equations
!         neq    - Number of active equations

!      Outputs:
!         hh(neqg,*) - Export matrix  (TEMP5: hr(np(115)))
!         g(neq,*,2) - G-vector       (TEMP6: hr(np(116)))
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'compas.h'
      include  'ndata.h'
      include  'pointer.h'
      include  'print.h'
      include  'p_int.h'
      include  'comblk.h'

      integer       :: neqg,neq,mm,nn, i
      real (kind=8) :: hh(neqg,neqg),g(neq,neqg,2)

      save

!     Loop through equations to form Gu' = A_inv * Gu

      do mm = 1,neqg

!       Solve linear equations with Gu as right-hand side

        fp(1) = na
        fp(2) = nau
        fp(3) = nal
        fp(4) = np(21)
        call psolve(g(1,mm,1),fp,.false.,.true.,.true.,prnt)

        do nn = 1,neqg
          do i = 1,neq
            hh(nn,mm) = hh(nn,mm) - g(i,nn,2)*g(i,mm,1)
          end do ! i
        end do ! nn
      end do ! mm

      end subroutine formhh

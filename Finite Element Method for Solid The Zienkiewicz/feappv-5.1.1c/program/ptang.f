!$Id:$
      subroutine ptang(g,ht, neq,nss)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Multiply shear terms by 0.5

!      Inputs:
!        g(:,:)     - G1 - array unreduced
!        ht(:,:)    - H  - array unreduced
!        neq        - Size of G1 array
!        nss        - No. Columns in G1 and H

!      Outputs:
!        g(:,:)     - G1 - array reduced
!        ht(:,:)    - H  - array reduced
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'elpers.h'

      integer       :: neq,nss, a,b,ia
      real (kind=8) :: g(neq,nss), ht(nss,nss)

      save

!     Modify shear values

      do a = 4,nss
        ptau(a) = ptau(a)*0.5d0
        do ia = 1,neq
          g(ia,a) = g(ia,a)*0.5d0
        end do ! ia
        do b = 1,nss
          ht(a,b) = ht(a,b)*0.5d0
        end do ! b
        do b = 1,nss
          ht(b,a) = ht(b,a)*0.5d0
        end do ! b
      end do ! a

      end subroutine ptang

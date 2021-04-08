!$Id:$
      subroutine tau2sig(tau,ctau,volmr,detf, sig,dd, ntau)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Convert Kirchhoff quatitites to Cauchy ones
!              Also subtracts stress term from moduli.

!     Inputs:
!       tau(6)    - Kirchhoff stress
!       ctau(6,6) - Kirchhoff moduli (with geometric stress added)
!       volmr     - Volume of RVE cell (reciprocal)
!       detf      - Deformation gradient determinant
!       ntau      - Size of tangent matrix

!     Outputs:
!       sig(6)    - Cauchy stress
!       dd(6,6)   - Spatial moduli
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer       :: ntau, a,b, i,j,k,ik
      integer       :: ismap(3,3)
      real (kind=8) :: volmr,detf,jinv
      real (kind=8) :: tau(6),ctau(ntau,ntau),sig(6),dd(ntau,ntau)

      save

      data        ismap / 1,4,6,  4,2,5,  6,5,3 /

!     Compute geometric stress tangent

      dd(:,:)   = 0.0d0
      do k = 1,3
        do j = 1,3
          b  = ismap(j,k)
          do i = 1,3
            ik      = ismap(i,k)
            a       = ismap(i,j)
            dd(a,b) = dd(a,b) - tau(ik)
          end do ! k
        end do ! i
      end do ! j

!     Divide shear term

      do a = 4,6
        do b = 1,6
          dd(a,b) = dd(a,b)*0.5d0
          dd(b,a) = dd(b,a)*0.5d0
        end do ! b
      end do ! a

!     Subtract geometric stress tangent from moduli

      do b = 1,6
        do a = 1,6
          dd(a,b) = ctau(a,b) + dd(a,b)
        end do ! a
      end do ! b

!     Append mixed terms (??)

      if(ntau.gt.6) then
        do a = 1,6
          dd(a,7) = ctau(a,7)
          dd(7,a) = ctau(7,a)
        end do ! a
        dd(7,7) = ctau(7,7)
      endif

!     Scale stress and moduli by RVE volume and Jacobian

      jinv    = volmr/detf
      sig(:)  = tau(:)*jinv
      dd(:,:) = dd(:,:)*jinv

      end subroutine tau2sig

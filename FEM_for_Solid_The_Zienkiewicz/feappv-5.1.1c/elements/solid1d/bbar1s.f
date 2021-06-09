!$Id:$
      subroutine bbar1s(phi,shp,dvol,lint,nel,npm,hh,irad,jrad,theta,
     &                  bbar)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute mixed formulation for the volumetric response.
!              4-node and 9-node case for small deformation problem.

!     Inputs:
!        phi(3,*)      - Stress functions
!        shp(2,8,*)    - Shape functions and derivatives
!        vol(*)        - Volume elements
!        lint          - Number of quadrature points
!        nel           - Number of nodes on element (should be 4 or 9)
!        npm           - Number of pressure modes
!        irad(*)       - Inverse radius (or zero) at quadrature points
!        jrad(*)       - Inverse radius (or zero) at quadrature points
!        theta(3,*)    - Volumetric strain from displacements

!     Outputs:
!        hh(3,3)       - Volume/pressure shape integrals (inverse)
!        theta(3,*)    - Mixed volumetric strain
!        bbar(8,*)     - Mixed volumetric derivative of shape function
!                        (N.B. Includes axisymmetric part using irad(*))
!                        (N.B. Includes spherical    part using jrad(*))
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: lint,   nel,  npm,  i,  j,  k, l
      real (kind=8) :: shp(2,20,*),  dvol(*)  ,  ht(3,2)   ,  h1
      real (kind=8) :: gg(3,10)   ,  hh(3,3)  ,  hv(3,2)   ,  hg(3,10)
      real (kind=8) :: irad(*)    ,  jrad(*)  ,  phi(3,10),  theta(3,*)
      real (kind=8) :: bbar(8,*)

      save

!     2-Node Element

      if(nel.eq.2) then

        do j = 1,nel
          bbar(j,1) = 0.0d0
        end do ! j
        hh(1,1) = 0.d0
        ht(1,1) = 0.0d0
        ht(1,2) = 0.0d0

        do l = 1,lint

!         H-array and G-array

          hh(1,1) = hh(1,1) + dvol(l)
          ht(1,1) = ht(1,1) + theta(1,l)*dvol(l)
          ht(1,2) = ht(1,2) + theta(2,l)*dvol(l)

!         G-array

          do j = 1,nel
            bbar(j,1) = bbar(j,1) + (shp(2,j,l) *(irad(l)+jrad(l))
     &                            +  shp(1,j,l))* dvol(l)
          end do ! j
        end do ! l

!       Average Jacobian

        hh(1,1)    = 1.d0 / hh(1,1)

!       Small deformation case

        theta(1,1) = hh(1,1)*ht(1,1)
        theta(2,1) = hh(1,1)*ht(1,2)
        theta(3,1) = theta(1,1) - theta(2,1)

!       Modify bbar for B-bar type computations

        do j = 1,nel
          bbar(j,1) = bbar(j,1)*hh(1,1)
        end do ! j

!       Copy for other quadrature points

        do l = 2,lint
          theta(1,l) = theta(1,1)
          theta(2,l) = theta(2,1)
          theta(3,l) = theta(3,1)
          do j = 1,nel
            bbar(j,l) = bbar(j,1)
          end do ! j
        end do ! l

!     Higher order elements: npm = 2 (3 nodes); npm = 3 (4 nodes)

      else

        do i = 1,npm
          do j = 1,nel
            gg(i,j) = 0.0d0
          end do ! j
          do j = 1,npm
            hh(i,j) = 0.0d0
          end do ! j
          ht(i,1) = 0.0d0
          ht(i,2) = 0.0d0
        end do ! i

!       Quadrature loop

        do l = 1,lint
          do j = 1,npm

!           H-array

            h1      = phi(j,l) * dvol(l)
            ht(j,1) = ht(j,1)  + theta(1,l)*h1
            ht(j,2) = ht(j,2)  + theta(2,l)*h1
            do i = 1,npm
              hh(i,j)   = hh(i,j)   + phi(i,l)*h1
            end do ! i

!           G-array

            do i = 1,nel
              gg(j,i) = gg(j,i) + (shp(1,i,l)
     &                          +  shp(2,i,l)*(irad(l)+jrad(l)))*h1
            end do ! i
          end do ! j

        end do ! l

!       Invert H-array

        call invert(hh,npm,3)

        do j = 1,nel
          do i = 1,npm
            hg(i,j) = 0.0d0
            do k = 1,npm
              hg(i,j) = hg(i,j) + hh(i,k)*gg(k,j)
            end do ! k
          end do ! i
        end do ! j

        do j = 1,2
          do i = 1,npm
            hv(i,j) = 0.0d0
            do k = 1,npm
              hv(i,j) = hv(i,j) + hh(i,k)*ht(k,j)
            end do ! k
          end do ! i
        end do ! j

        do l = 1,lint
          theta(1,l) = hv(1,1)
          theta(2,l) = hv(1,2)
          do k = 2,npm
            theta(1,l) = theta(1,l) + phi(k,l)*hv(k,1)
            theta(2,l) = theta(2,l) + phi(k,l)*hv(k,2)
          end do ! k
        end do ! l

        do l = 1,lint
          theta(3,l) = theta(1,l) - theta(2,l)
          do j = 1,nel
            bbar(j,l) = hg(1,j)
            do k = 2,npm
              bbar(j,l) = bbar(j,l) + phi(k,l)*hg(k,j)
            end do ! k
          end do ! j
        end do ! l

      endif

      end subroutine bbar1s

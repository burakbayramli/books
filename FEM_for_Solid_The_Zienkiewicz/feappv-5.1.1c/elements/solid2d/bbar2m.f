!$Id:$
      subroutine bbar2m(sg,shp,jac,detf,lint,nel,hh,theta,shpbar)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute mixed formulation for the volumetric response

!     Inputs:
!        sg(3,*)       - Quadrature points and weights at gauss points
!        shp(3,9,*)    - Shape functions and derivatives at gauss points
!        jac(*)        - Volume elements at gauss points at t_n+1
!        detf(2,*)     - Jacobian determinant at gauss points
!        lint          - Number of quadrature points
!        nel           - Number of nodes on element (should be 8)

!     Outputs:
!        hh(3,3)       - Reference configuration shape integrals (inverse)
!        theta(2,*)    - Mixed jacobian determinant for element
!        shpbar(2,9,*) - Mixed derivatives of shape functions.
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: lint,  nel,  i,  j,  l
      real (kind=8) :: dvol, h0, h1, h2

      real (kind=8) :: sg(3,*),    shp(3,64,*), jac(*), detf(2,*)
      real (kind=8) :: theta(2,*), shpbar(2,9,*)
      real (kind=8) :: gg(3,2,9),  hh(3,3),ji(3,2),hj(3,2),hg(3,2,9)
      real (kind=8) :: phi(3)

      save

!     4-Node Element

      if(nel.eq.4) then

        do j = 1,nel
          shpbar(1,j,1) = 0.0d0
          shpbar(2,j,1) = 0.0d0
        end do ! j
        hh(1,1) = 0.d0
        h1      = 0.d0
        h2      = 0.d0

        do l = 1,lint

!         H-array and D-array

          dvol    = jac(l) * detf(1,l)
          hh(1,1) = hh(1,1) + jac(l)
          h1      = h1      + jac(l) * detf(1,l)
          h2      = h2      + jac(l) * detf(2,l)

!         G-array

          do j = 1,nel
            do i = 1,2
              shpbar(i,j,1) = shpbar(i,j,1) + shp(i,j,l) * dvol
            end do
          end do
        end do

!       Modify shpbar for B-bar type computations

        h0 = 1.d0/h1

        do j = 1,nel
          do i = 1,2
            shpbar(i,j,1) = shpbar(i,j,1)*h0
          end do
        end do

!       Average Jacobian

        hh(1,1)    = 1.d0 / hh(1,1)
        theta(1,1) = h1   * hh(1,1)
        theta(2,1) = h2   * hh(1,1)

        do l = 2,lint
          theta(1,l) = theta(1,1)
          theta(2,l) = theta(2,1)
          do j = 1,nel
            shpbar(1,j,l) = shpbar(1,j,1)
            shpbar(2,j,l) = shpbar(2,j,1)
          end do ! j
        end do ! l

!     9-Node element

      elseif(nel.eq.9) then

        do i = 1,3
          do j = 1,nel
            gg(i,1:2,j) = 0.0d0
          end do ! j
          hh(i,1:3) = 0.0d0
          ji(i,1:2) = 0.0d0
        end do ! i

!       Quadrature loop

        phi(1) = 1.d0
        do l = 1,lint
          phi(2) = sg(1,l)
          phi(3) = sg(2,l)
          do j = 1,3

            h0 = phi(j)*jac(l)
            h1 = h0*detf(1,l)
            h2 = h0*detf(2,l)

!           Ji-array

            ji(j,1) = ji(j,1) + h1
            ji(j,2) = ji(j,2) + h2

!           H-array

            do i = 1,3
              hh(i,j)    = hh(i,j)    + phi(i)*h0
            end do ! i

!           G-array

            do i = 1,nel
              gg(j,1:2,i) = gg(j,1:2,i) + shp(1:2,i,l)*h1
            end do ! i
          end do ! j

        end do ! l

!       Invert H-array

        call invert(hh,3,3)

        do j = 1,2
          do i = 1,3
            hj(i,j) = hh(i,1)*ji(1,j)
     &              + hh(i,2)*ji(2,j)
     &              + hh(i,3)*ji(3,j)
          end do ! i
        end do ! j

        do j = 1,nel
          do i = 1,3
            hg(i,1,j) = hh(i,1)*gg(1,1,j)
     &                + hh(i,2)*gg(2,1,j)
     &                + hh(i,3)*gg(3,1,j)
            hg(i,2,j) = hh(i,1)*gg(1,2,j)
     &                + hh(i,2)*gg(2,2,j)
     &                + hh(i,3)*gg(3,2,j)
          end do ! i
        end do ! j

        do l = 1,lint
          theta(1,l) = hj(1,1) + sg(1,l)*hj(2,1) + sg(2,l)*hj(3,1)
          theta(2,l) = hj(1,2) + sg(1,l)*hj(2,2) + sg(2,l)*hj(3,2)
          h0         = 1.d0/theta(1,l)
          do j = 1,nel
            shpbar(1,j,l) = h0*(    hg(1,1,j)
     &                    + sg(1,l)*hg(2,1,j)
     &                    + sg(2,l)*hg(3,1,j))
            shpbar(2,j,l) = h0*(    hg(1,2,j)
     &                    + sg(1,l)*hg(2,2,j)
     &                    + sg(2,l)*hg(3,2,j))
          end do ! j
        end do ! l

      endif

      end subroutine bbar2m

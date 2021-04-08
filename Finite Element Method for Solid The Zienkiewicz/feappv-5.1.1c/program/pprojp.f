!$Id:$
      subroutine pprojp(ixl,xl,xs,p, ndm,ndf,nel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set averaged TAU stress

!      Inputs:
!        ixl(ndf,*)   - DOF indicators: -1 = no equation
!                                       >0 = Eq. number of dof
!                                        0 = boundary dof
!        xl(ndm,nel)  - Element nodal coordinates
!        p(ndf,nel)   - Element residual
!        ndm          - Spatial dimension of mesh
!        nel          - Number of maximum node on element

!      Working:
!        xs(ndm,nel)  - Element nodal coordinates

!      Outputs:
!        ptau(6)      - Stress (through common)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'elpers.h'

      integer       :: ndm,ndf,nel
      integer       :: i,ib,ir,a, it

      integer       :: ixl(ndf,*), isIb(3,3)
      real (kind=8) :: xl(ndm,nel),xs(ndm,nel),p(ndf,nel)

      save

      data       isIb / 1, 4, 6,
     &                  4, 2, 5,
     &                  6, 5, 3/

!     Thermal problem

      if(prtype.eq.1) then

        it = 1
        do ir = 1,nel
          if(ixl(it,ir).eq.0) then  ! Assemble P1
            do ib = 1,ndm
              pflux(ib) = pflux(ib) - p(it,ir)*xl(ib,ir) ! flux
            end do ! ib
          endif
        end do ! ir

      endif

!     Mechanical problem

      if(prtype.eq.2) then

!       Form current coordinates

        do ir = 1,nel
          do i = 1,ndm
            xs(i,ir) = xl(i,ir)
            if(finflg) then
              do a = 1,ndm
                xs(i,ir) = xs(i,ir) + gradu(i,a)*xl(a,ir)
              end do ! a
            endif
          end do ! i
        end do ! ir

!       Hill-Mandel update of stress

        do ir = 1,nel
          do i = 1,ndm
            if(ixl(i,ir).eq.0) then
              do ib = 1,ndm
                a      = isIb(i,ib)
                ptau(a) = ptau(a) - p(i,ir)*xs(ib,ir)  ! Stress
              end do ! ib
            endif
          end do ! i
        end do ! ir

      endif ! prtype

      end subroutine pprojp

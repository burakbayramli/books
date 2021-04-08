!$Id:$
      subroutine pperdis(id,x,u)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set boundary displacement of periodic boundary problem

!      Inputs:
!         gradt(3)        - Temperature  gradient (in common 'elpers')
!         gradu(3,3)      - Displacement gradient (in common 'elpers')
!         id(ndf,numnp,2) - Equation/boundary codes
!         x(ndm,numnp)    - Reference coordinates

!     Outputs:
!         u(ndf,numnp)  - Force/Displacements at nodes
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'                 ! numnp,numel
      include   'elpers.h'                ! gradu(3,3)
      include   'sdata.h'                 ! ndm,ndf

      integer       :: id(ndf,numnp,2)
      real (kind=8) :: x(ndm,numnp), u(ndf,numnp)
      real (kind=8) :: ubase0(4)

      integer       :: i,j,n

      save

!     Temperature: Loop over nodes and directions

      if(prtype.eq.1) then
        ubase0(:) = 0.0d0
        ubase0(1) = temp0
        do n = 1,numnp
          if(id(1,n,2).ne.0) then        ! Restrained dof
            u(1,n) = ubase0(1)
            do i = 1,ndm
              u(1,n) = u(1,n) + gradt(i)*x(i,n)
            end do ! i
          end if
        end do ! n
      endif

!     Displacements: Loop over nodes and directions

      if(prtype.eq.2) then
        do n = 1,numnp
          do j = 1,ndm
            if(id(j,n,2).ne.0) then        ! Restrained dof
              u(j,n) = 0.0d0
              do i = 1,ndm
                u(j,n) = u(j,n) + gradu(j,i)*x(i,n)
              end do ! i
            end if

          end do ! j
        end do ! n
      endif

      end subroutine pperdis

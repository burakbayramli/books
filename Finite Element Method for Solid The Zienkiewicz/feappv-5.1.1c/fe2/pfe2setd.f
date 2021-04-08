!$Id:$
      subroutine pfe2setd(id,x, f)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Set boundary displacement values using received values

!     Inputs:
!         id(ndf,numnp,2) - Equation/boundary codes
!         x(ndm,numnp)    - Reference coordinates

!     Outputs:
!         f(ndf,numnp,2)  - Force/Displacements at nodes
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'                 ! numnp,numel
      include   'elpers.h'                ! xc(3)
      include   'idptr.h'                 ! id31
      include   'sdata.h'                 ! ndm,ndf

      integer    id(ndf,numnp,2)
      real*8     x(ndm,numnp), f(ndf,numnp,2)

      integer    i,j,n

      save

!     Thermal model

      if(prtype.eq.1) then
        do n = 1,numnp
          if(id(1,n,2).ne.0) then        ! Restrained dof
            f(1,n,2) = ttemp
            do i = 1,ndm
              f(1,n,2) = f(1,n,2) + gradt(i)*x(i,n)
            end do ! i
          else
            f(1,n,2) = 0.0d0
          end if
        end do ! n

!     Mechanical model

      elseif(prtype.eq.2) then
        do n = 1,numnp
          do j = 1,ndm
            f(j,n,2) = 0.0d0
            if(id(j,n,2).ne.0) then        ! Restrained dof
              do i = 1,ndm
                f(j,n,2) = f(j,n,2) + gradu(j,i)*x(i,n)
              end do ! i
            end if
          end do ! j
        end do ! n

      endif

      end subroutine pfe2setd

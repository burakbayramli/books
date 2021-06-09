!$Id:$
      subroutine nodel(ix,nnid,nd,ln,ne,numnp,numel,nsum,nen,nen1)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Formation of "nd" and "ln" arrays for profile
!               optimization.

!      Inputs:
!         ix(nen1,*)     - Element connection array
!         nnid(*)        - Number active dof at nodes
!         numnp          - Number of nodes in mesh
!         numel          - Number of elements in mesh
!         nen            - Maximum number nodes/element
!         nen1           - Dimension of ix  array

!      Outputs:
!         nd(*)          - Nodal-element array
!         ln(*)          - Location array
!         ne(*)          - Element order to minimize front
!         nsum           - Maximum value in location array
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: l, m, n,nn,nen,nen1,numnp,numel,nsum
      integer       :: nd(numnp),ix(nen1,*),ln(numnp),ne(*)
      integer       :: nnid(numnp)

      save

!     Formation of "nd" and "ln" arrays

      nd(1:numnp) = 0

!     Count elements or constraints attached to nodes

      do m = 1,numel

        if(ix(nen1-1,m).ge.0) then
          do l = 1,nen
            n = abs(ix(l,m))
            if(n.gt.0) then
              if(nnid(n).gt.0) then
                nd(n) = nd(n) + 1
              endif
            endif
          end do ! l
        end if

      end do ! m

!     Form location array

      nsum = 0
      do n = 1,numnp
        nsum  = nsum + nd(n)
        ln(n) = nsum
      end do ! n

!     Form nodel-element array

      do m = 1,numel

        if(ix(nen1-1,m).ge.0) then
          do l = 1,nen
            n  = abs(ix(l,m))
            if(n.gt.0) then
              if(nnid(n).gt.0) then
                nd(n)  = nd(n) - 1
                nn     = ln(n) - nd(n)
                ne(nn) = m
              endif
            endif
          end do ! l
        endif

      end do ! m

      end subroutine nodel

!$Id:$
      subroutine comelm(eq,ix, ir, ndf,nen, kpo,kp,neq,
     &                  bycol,wdiag,rc_all)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Compute equation numbers from elements

!      Inputs:
!         eq(ndf,*)  -  Active unknowns at each node.
!         ix(nen1,*) - List of nodes connected to each element
!         ndf        -  Number of unknowns at each node.
!         nen        -  Maximum number of nodes on any element
!         kpo        -  Initial row entry
!         bycol      -  Storage by columns if true
!         wdiag      -  Include diagonal if true
!         rc_all     -  All terms in row/col if true

!      Outputs:
!         ir(*)      -  Row number of each nonzero in stiffness matrix.
!         kp         -  Last entry in ir array
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      logical      :: addeq, bycol, wdiag, rc_all
      integer      :: ndf,nen,kpo,kp,neq, i,l,m, kk,neqj
      integer      :: eq(ndf,*),ix(*),ir(*)

      save

      do l = 1,nen
        kk = ix(l)
        if(kk.gt.0) then
          do m = 1, ndf
              neqj = eq(m,kk)

!             Check if equation to be added

              if(rc_all) then                        ! all terms
                addeq   = neqj.gt.0
              elseif(bycol) then                     ! by columns
                if(wdiag) then
                  addeq = neqj.le.neq.and.neqj.gt.0  ! diagonal in
                else
                  addeq = neqj.lt.neq.and.neqj.gt.0  ! diagonal out
                endif
              else                                   ! by rows
                if(wdiag) then
                  addeq = neqj.ge.neq                ! diagonal in
                else
                  addeq = neqj.gt.neq                ! diagonal out
                endif
              endif

!             Add equation to list

              if(addeq) then

!               Check if equation already in list.

                do i = kpo, kp
                  if(ir(i).eq.neqj) go to 200
                end do ! i

!               New equation, add to list

                kp     = kp + 1
                ir(kp) = neqj
200             continue
              endif
          end do ! m
        endif
      end do ! l

      end subroutine comelm

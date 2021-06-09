!$Id:$
      subroutine comproa(numnp, nen, nen1, ndf, ix, eq,
     &                   ic, ielc, ir, kp, bycol, wdiag, rc_all)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Compute number of locations needed for equation
!                connection list.

!      Inputs:
!         numnp      -  Number of nodes in mesh
!         nen        -  Maximum number of nodes on any element
!         nen1       -  Dimension for 'ix' array
!         ndf        -  Number of unknowns at each node.
!         ix(nen1,*) -  List of nodes connected to each element
!         eq         -  Active unknowns at each node.
!         ic         -  Pointer for ielc list
!         ielc(*)    -  Holds set of elements connected to each node.
!         bycol      -  Storage by columns if true
!         wdiag      -  Include diagonal if true
!         rc_all     -  All terms in row/col if true

!      Working vector:
!         ir(*)      -  Row number of each nonzero in stiffness matrix.

!      Outputs:
!         kp         -  Dimension of IR array.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'compac.h'
      include  'pointer.h'
      include  'comblk.h'

      logical      :: bycol, wdiag, rc_all
      integer      :: i, j, ne, nep, neq, nn
      integer      :: numnp, nen, nen1, ndf, kp, kpo, kpf
      integer      :: ix(nen1,*), eq(ndf,*), ic(*), ir(*), ielc(*)

      save

!     Set up compressed profile pointers.
      neq = 0
      do i = 1, numnp
        do j = 1,ndf
          neq = max(neq,eq(j,i))
        end do ! j
      end do ! i

!     Zero temporary array to start
      do j = 1,neq
        ir(j) = 0
      end do ! j

!     Do all equations
      kp  = 0
      nep = 1
      do i = 1, neq
        ne  = ic(i)
        kpo = 1
        kpf = 0
        do j = nep, ne
          nn = ielc(j)

!         Check element type(>0: FE, <0: contact)
          if(nn.gt.0) then
            call comelm(eq,ix(1,nn), ir, ndf,nen,  kpo,kpf,i,
     &                  bycol,wdiag,rc_all)
            if(np(210).ne.0) then
              call comelmeq(mr(np(32)),ix(1,nn),mr(np(210)),ir,kpo,kpf,
     &                      nn,i,bycol,wdiag,rc_all)
            endif
!         No contact in personal version
          else
            write(*,*) ' **ERROR** Incorrect COMPROa Type'
          endif

!         End element tests

        end do ! j

!       Zero entry for next row/column check
        do j = 1,kpf
          ir(j) = 0
        end do ! j

!       Accumulate required storage pointer
        kp  = kp + kpf ! returns total storage for sparse matrix
        nep = ne + 1
      end do ! i

      end subroutine comproa

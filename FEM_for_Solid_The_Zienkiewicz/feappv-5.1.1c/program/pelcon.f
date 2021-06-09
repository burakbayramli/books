!$Id:$
      subroutine pelcon(numel, nen, neix, ix, eq, ic, ielc, icneq, sgn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Compute number of elements connected to each node
!                matrix.

!      Inputs:
!         numel      -  Number of elements in mesh
!         nen        -  Maximum number of nodes on any element
!         neix       -  Dimension for 'ix' array
!         ix(nen1,*) -  List of nodes connected to each element
!         eq(ndf,*)  -  Nodal equation numbers
!         icneq      -  Dimension of IELC (= ic(neq))
!         ic(*)      -  Pointer array

!      Outputs:
!         ielc(*)    -  Holds the set of elements connected to each node.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comblk.h'
      include  'pointer.h'
      include  'sdata.h'

      integer       :: i, j,k,kk,kp, n
      integer       :: icneq, numel, nen, neix, neql, sgn
      integer       :: ix(neix,*), eq(ndf,*), ic(*), ielc(*)

      save

!     Find elements connected to each node
      if(sgn.gt.0) call pzeroi(ielc, icneq)

      do i = 1, numel
        do j = 1, nen
          n = ix(j,i)
          if(n.gt.0) then
            do k = 1,ndf
              kk = eq(k,n)
              neql = max(neql,kk)
              if(kk.gt.0) then
                kp = ic(kk)
                do while( ielc(kp).ne.0 )
                  kp = kp - 1
                end do ! while
                ielc(kp) =  i   ! Number of finite element
              endif ! kk > -0
            end do ! k
          endif ! n > 0
        end do ! j
      end do ! i

!     Element equation (Lagrange multiplier treatment)
      if(np(210).ne.0) then
        call pelconl(i,mr(np(32)),ix(1,i),mr(np(210)),
     &               ic,ielc,neql)
      endif

      end subroutine pelcon

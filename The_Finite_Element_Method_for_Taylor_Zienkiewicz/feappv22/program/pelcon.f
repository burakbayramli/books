c$Id:$
      subroutine pelcon(numel, nen, neix, ix, ic, ielc, icneq, sgn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Compute number of elements connected to each node
c                matrix.

c      Inputs:
c         numel      -  Number of elements in mesh
c         nen        -  Maximum number of nodes on any element
c         neix       -  Dimension for 'ix' array
c         ix(nen1,*) -  List of nodes connected to each element
c         icneq      -  Dimension of IELC (= ic(neq))
c         ic(*)      -  Pointer array

c      Outputs:
c         ielc(*)    -  Holds the set of elements connected to each node.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comblk.h'
      include  'pointer.h'
      include  'sdata.h'

      integer   i, j,k, n
      integer   icneq, numel, nen, neix, kp, sgn
      integer   ix(neix,*), ic(*), ielc(*), nty

      save

c     Find elements connected to each node

      if(sgn.gt.0) call pzeroi(ielc, icneq)

      do i = 1, numel
        do j = 1, nen
          n = ix(j,i)
          if(n.gt.0) then
            do k = 1,ndf
              nty = mr(np(31) + n*ndf - ndf + k - 1)
              if(nty.gt.0) then
                kp = ic(nty)
100             if(ielc(kp).eq.0) go to 110
                kp = kp - 1
                go to 100
110             if(sgn.gt.0) then
                  ielc(kp) =  i
                else
                  ielc(kp) = -i
                endif
              endif ! nty > 0
            end do ! k
          endif ! n > 0
        end do ! j
      end do ! i

      end

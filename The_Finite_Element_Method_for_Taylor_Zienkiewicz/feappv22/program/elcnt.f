c$Id:$
      subroutine elcnt(numnp, numel, nen, neix, ix, ic,  flag)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute array for performing assembly of stiffness
c               matrices into a compacted data structure;

c      Inputs:
c          numnp  -  Number of nodes in the mesh.
c          numel  -  Number of elements in the mesh.
c          nen    -  Max. number of nodes per element.
c          neix   -  Dimension of ix array.
c          ix     -  Element conectivity array.

c      Outputs:
c          ic     -  Array of length numnp.  It first holds the
c                    element degree of each node, then becomes a
c                    pointer for an array that contains the set
c                    of elements connected to each node.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comblk.h'
      include  'pointer.h'
      include  'sdata.h'

      logical   flag
      integer   i,j,k,n
      integer   numnp, numel, nen, neix
      integer   ix(neix, *), ic(*), nty

      save

c     Count number of elements attached to each node.

      if(flag) call pzeroi(ic, numnp*ndf)
      do i = 1, numel
        do j =  1, nen
          n = ix(j,i)
          if(n.gt.0) then
            do k = 1,ndf
              nty = mr(np(31) + ndf*n - ndf + k -1)
              if(nty.gt.0) ic(nty) = ic(nty) + 1
            end do ! k
          end if ! n > 0
        end do
      end do

      end

      subroutine sumcnt(ic,nneq,kp)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Sum counts to get storage of connections

c      Inputs:
c          ic(*)  - Array of 'heights'
c          nneq   - Number of entries

c      Outputs:
c          ic(*)  - Pointer array
c          kc     - Length of pointer array
c-----[--.----+----.----+----.-----------------------------------------]
      implicit   none
      integer    i,kp,nneq,ic(*)

c     Set up pointers.

      do i = 2, nneq
         ic(i) = ic(i) + ic(i-1)
      end do
      kp = ic(nneq)

      end

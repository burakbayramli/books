c$Id:$
      subroutine cashift(ap,ac,s,jp,jc,ir,neq)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Add a multiple of a compressed matrix to a profile matrix

c      Inputs:
c         ap(*)  - Entries for profile matrix
c         ac(*)  - Entries for compressed matrix
c         s      - Scalar multiplier
c         jp(*)  - Pointer array for profile row/columns
c         jc(*)  - Pointer array to locate entries in rows/columns
c         ir(*)  - Location of non-zero entries in AC
c         neq    - Number of equations

c      Outputs:
c         ap(*)  - Modified ntries for profile matrix

c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   neq,ni,nj,jp0
      real*8    s

      integer   jp(*),jc(*),ir(*)
      real*8    ap(*),ac(*)

c     Loop over number of equations

      do ni = 2,neq

        jp0 = jp(ni) - ni + 1

        do nj = jc(ni-1)+1,jc(ni)
          ap(ir(nj)+jp0) = ap(ir(nj)+jp0) - s*ac(nj)
        end do

      end do

      end

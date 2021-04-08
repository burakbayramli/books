!$Id:$
      subroutine cashift(ap,ac,s,jp,jc,ir,neq)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Add a multiple of a compressed matrix to a profile matrix

!      Inputs:
!         ap(*)  - Entries for profile matrix
!         ac(*)  - Entries for compressed matrix
!         s      - Scalar multiplier
!         jp(*)  - Pointer array for profile row/columns
!         jc(*)  - Pointer array to locate entries in rows/columns
!         ir(*)  - Location of non-zero entries in AC
!         neq    - Number of equations

!      Outputs:
!         ap(*)  - Modified ntries for profile matrix

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: neq,ni,nj,jp0
      real (kind=8) :: s

      integer       :: jp(*),jc(*),ir(*)
      real (kind=8) :: ap(*),ac(*)

!     Loop over number of equations

      do ni = 2,neq

        jp0 = jp(ni) - ni + 1

        do nj = jc(ni-1)+1,jc(ni)
          ap(ir(nj)+jp0) = ap(ir(nj)+jp0) - s*ac(nj)
        end do

      end do

      end subroutine cashift

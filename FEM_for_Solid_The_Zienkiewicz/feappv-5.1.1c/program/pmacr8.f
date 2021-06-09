!$Id:$
      subroutine pmacr8 (lct,ct,j)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Command language instruction subprogram: Part 8 for NURBS

!     Inputs:
!        lct      - Command option for current command
!        ct(3)    - Command parameters for current command
!        j        - Number of command to execute

!     Outputs:
!        Depends on value of command j: None for serial version
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      character (len=15) :: lct

      integer       :: j
      real (kind=8) :: ct(3)

      save

!     Solution command instruction subprogram - part 8.

      if(nint(ct(1)).ge.0) then

!       [elevate block n_blk, k_dir inc_order
!       [elevate patch n_blk, k_dir inc_order

        if(j.eq.1) then

          write(*,2000) ' ELEVate ',lct,ct(1:3)

!       [insert block n_blk, k_dir u_knot n_times
!       [insert patch n_blk, k_dir u_knot n_times

        elseif(j.eq.2) then

          write(*,2000) ' INSErt ',lct,ct(1:3)

        endif
      endif

!     Formats

2000  format('  *ERROR*',a,'command only available in NURBS version'/
     &       '           Input record: ',a,1p,3e12.4)

      end subroutine pmacr8

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

      include   'pointer.h'
      include   'comblk.h'

      integer    j
      character  lct*15
      real*8     ct(3)

      save

!     Solution command instruction subprogram - part 8.

      if(nint(ct(1)).ge.0) then

!       [extract block n_blk, k_dir inc_order
!       [extract patch n_blk, k_dir inc_order

        if(j.eq.1) then

          call pn_elev(lct,ct, mr(np(310)))

!       [insert block n_blk, k_dir u_knot n_times
!       [insert patch n_blk, k_dir u_knot n_times

        elseif(j.eq.2) then

          call pn_insert(lct,ct, mr(np(310)))

        endif
      endif

!     Formats

      end

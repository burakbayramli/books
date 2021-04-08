!$Id:$
      logical function ualloc(num,vname,length,precis)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Define, delete, or resize a user dictionary entry.
!               Pointer defined for integer (single) and real
!               (double precision) arrays.

!               N.B. Currently limited to 200 names by dimension of
!               common blocks 'allotd','allotn','pointer'

!      Inputs:

!         num        - Entry number for array (see below)
!         vname      - Name of array          (see below)
!         length     - Length of array defined: =0 for delete
!         precis     - Precision of array: 1 = integers; 2 = reals

!      Output:

!         up(num)    - Pointer to first word of array in blank common
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'allotd.h'

      character     :: vname*(*)
      logical       :: usetmem
      integer       :: i, num, precis
      integer       :: length

!     Storage definitions for UALLOC variables
      integer    list
      parameter (list = 1)

      character (len=5) :: names(list)

      save

!     Define and store names

      data   (names(i),i=1,list)/

     &         'DUMMY'/

!     Short description of variables

!              'DUMMY',     !     1: Start here with user defined names

!     Do memory management operations

      ualloc = usetmem(list,names,num,vname,length,precis)

      end function ualloc

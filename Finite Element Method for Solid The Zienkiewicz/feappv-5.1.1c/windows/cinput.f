!$Id:$
      function cinput()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    10/23/2017
!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose:  Handle standard input reads

!      Inputs:

!      Outputs:
!         record - common block variable for the read
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit    none

      logical  :: cinput

      include  'comfil.h'

      record = ' '  ! clear record
      read(*,'(a)',err=100,end=100) record

      cinput = .true.
      return

100   cinput = .false.

      end function cinput

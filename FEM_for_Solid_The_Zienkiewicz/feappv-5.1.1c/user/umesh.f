!$Id:$
      logical function umesh(cc,tx,prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: User mesh command interface

!      Inputs:
!         cc     - User command option
!         tx(*)  - Command line input data
!         prt    - Output if true

!      Outputs:
!         umesh  - Flag to indicate if successful in matching command
!         N.B. Users must provide other output via common blocks, etc.

!     IOR is logical unit number for data inputs (if negative input from *)
!     IOW is logical unit number for output data
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      character (len=15) :: tx(*)
      character (len=4)  :: cc
      logical       :: prt,pcomp

!     Match on 'USER':  Can add as many checks as desired with 'user'
!                       replaced by 4-character word for each command.

      if(pcomp(cc,'user',4)) then

        if(prt) write(  *,*) ' '
        if(prt) write(  *,*) '    USER COMMAND = ',cc
        if(prt) write(  *,*) ' '

!       Return TRUE to indicate command was successful

        umesh = .true.

      elseif(pcomp(cc,'use2',4)) then

!       Second user command location, etc. for others.

        umesh = .true.

      else

!       Return FALSE for no match on user mesh command

        umesh = .false.

      endif

      end function umesh

!$Id:$
      subroutine umacr1(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!       1. Remove 'prt' from argument list                  09/07/2009
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Reverse sign of eigenvector to allow comparisons

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'            ! neq
      include  'iofile.h'
      include  'umac1.h'

      include  'pointer.h'          ! np(77)
      include  'comblk.h'           ! hr(*)

      logical       :: pcomp
      character     :: lct*15
      integer       :: nn
      real (kind=8) :: ctl(3)

      save

!     Set command word

      if(pcomp(uct,'mac1',4)) then      ! Usual    form
        uct = 'rvec'                    ! Specify 'name'
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

        nn = nint(ctl(1))   ! Number of vector to reverse

        write(*,'(a,i4)') ' --> Reversing sign of vector',nn
        call ueigrev(hr(np(77)), neq,nn)

      endif

      end subroutine umacr1

      subroutine ueigrev(evec, neq,nn)

      implicit   none

      integer       :: neq,nn
      real (kind=8) :: evec(neq,*)

      evec(:,nn) = -evec(:,nn)

      end subroutine ueigrev

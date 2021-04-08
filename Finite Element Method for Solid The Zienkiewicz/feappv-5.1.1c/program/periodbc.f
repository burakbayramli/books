!$Id:$
      subroutine periodbc(prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set RVE problem and formulation type

!      Inputs:
!         prt        - Flag, print input data if true

!      Outputs:
!         Values saved in include files
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'chdata.h'
      include   'elpers.h'
      include   'iofile.h'
      include   'setups.h'

      character (len=15) :: tx(2)

      logical       :: prt, pcomp, tinput, pinput, errck
      integer       :: i
      real (kind=8) :: td(4),tc(1)

      perflg = .false.
      fluxfl = .false.
      stflag = .false.

      tx(1) = 'star'
      do while (.not.pcomp(tx(1),'    ',4))
        errck = tinput(tx,2,tc,1)

        if(pcomp(tx(1),'ther',4)) then ! Thermal    activate
          prtype = 1
          fluxfl = .true.
          errck = pinput(td,4)
          gradt0(:) = td(1:3)
          temp0     = td(4)
          if(pcomp(tx(2),'prop',4)) then
            prpropt = nint(tc(1))
          else
            prpropt = 0
          endif
          if(prt) then
            write(iow,2000) 'Thermal',(i,i=1,3),1,gradt0(:)
            write(iow,2002) temp0
            write(iow,2003) prpropt
            if(ior.lt.0) then
              write(*,2000) (i,i=1,3),1,gradt0(:)
              write(*,2002) temp0
              write(*,2003) prpropt
            endif
          endif
        elseif(pcomp(tx(1),'mech',4)) then ! Mechanical activate
          prtype = 2
          stflag = .true.
          gradu0(:,:) = 0.0d0
          do i = 1,3
            errck       = pinput(td,3)
            gradu0(i,:) = td(1:3)
          end do ! i
          if(pcomp(tx(2),'prop',4)) then
            prpropu = nint(tc(1))
          else
            prpropu = 0
          endif
          if(prt) then
            write(iow,2001) 'Mechanical',(i,i=1,3),
     &                       (i,gradu0(i,:),i=1,3)
            write(iow,2003) prpropu
            if(ior.lt.0) then
              write(*,2001) (i,i=1,3),(i,gradu0(i,:),i=1,3)
              write(*,2003) prpropu
            endif

          endif
        endif ! prtype
      end do ! while

!     Activate periodic case (not needed for incremental form)

      perflg = .true.

!     Formats

2000  format(/5x,a,' Periodic Problem'
     &      /10x,'Periodic Grad_T Values'
     &      /9x,3(10x,'x-',i1)/(10x,'T-',i1,1p,3e13.5))

2001  format(/5x,a,' Periodic Problem'
     &      /10x,'Periodic Grad_u Values'
     &      /9x,3(10x,'x-',i1)/(10x,'u-',i1,1p,3e13.5))

2002  format(10x,'Ref. Temperature =',1p,1e13.5)

2003  format(/10x,'Proportional Load Number =',i4)

      end subroutine periodbc

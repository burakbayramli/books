!$Id:$
      subroutine pinitl(lct,err)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Establish initial conditions for solution vectors
!               [init,disp] - set initial displacements
!               [init,rate] - set initial rates

!      Inputs:
!        lct        - Parameter: disp,rate,spin

!      Outputs:
!        hr(np(40)) - Initial displacements
!        hr(np(42)) - Initial rates
!        err        - Set to .true. if error occurs
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'ddata.h'
      include   'iofile.h'
      include   'print.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      character (len=15) :: lct

      logical       :: err, pcomp, palloc,setvar

      err = .false.

!     Rate initial conditions

      if(pcomp(lct,'rate',4)) then

!       Check that memory allocated for rates

        setvar = palloc( 42,'VEL  ',nneq*nrt ,2)

        call genvec(ndf,ndf,hr(np(42)),' rate ',prt,prth,err,.false.)
        call genclr(ndf,hr(np(42)), mr(np(190)), numnp)

!     Displacement initial conditions

      else
        call genvec(ndf,ndf,hr(np(40)),' displ. ',prt,prth,err,.false.)
      endif

!     Check for errors

      if(err .and. ior.gt.0 ) then
        write(iow,3000)
        call plstop(.true.)
      endif

!     Format

3000  format(/'  *ERROR* Bad initial conditions')

      end subroutine pinitl

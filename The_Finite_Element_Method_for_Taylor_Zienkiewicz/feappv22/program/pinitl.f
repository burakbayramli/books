c$Id:$
      subroutine pinitl(lct,err)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Establish initial conditions for solution vectors
c               [init,disp] - set initial displacements
c               [init,rate] - set initial rates

c      Inputs:
c        lct        - Parameter: disp,rate,spin

c      Outputs:
c        hr(np(40)) - Initial displacements
c        hr(np(42)) - Initial rates
c        err        - Set to .true. if error occurs
c-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'iofile.h'
      include   'print.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    err, pcomp, palloc,setvar
      character  lct*15

      err = .false.

c     Rate initial conditions

      if(pcomp(lct,'rate',4)) then

c       Check that memory allocated for rates

        setvar = palloc( 42,'VEL  ',nneq*5 ,2)

        call genvec(ndf,ndf,hr(np(42)),' rate ',prt,prth,err,.false.)
        call genclr(ndf,hr(np(42)), mr(np(49)), numnp)

c     Displacement initial conditions

      else
        call genvec(ndf,ndf,hr(np(40)),' displ. ',prt,prth,err,.false.)
      endif

c     Check for errors

      if(err .and. ior.gt.0 ) then
        write(iow,3000)
        call plstop()
      endif

c     Format

3000  format(/'  *ERROR* Bad initial conditions')

      end

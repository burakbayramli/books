!$Id:$
      subroutine dicont(id,numnp,ndf,lflag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Provides information for displacement control
!               in arc length.

!      Inputs:
!         id(ndf,*) - Equation numbers for each dof
!         numnp     - number of nodal points in mesh
!         ndf       - Number dof/node
!         lflag     - If true, changes arc length

!      Outputs:
!         Equation number of assigned displacement
!         Factors to scale arc length control
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'arclel.h'
      include  'arclei.h'
      include  'arcler.h'
      include  'comfil.h'
      include  'ioincl.h'
      include  'iofile.h'

      character (len=1) :: ch

      logical       :: errck, pinput, cinput
      integer       :: numnp,ndf,lflag

      integer       :: id(ndf,*)
      real (kind=8) :: td(3)

      save

      if (lflag .ne. 0) go to 100

!     Read in if numerical damping desired or not

      if(ior.lt.0) write(*,3002)
      errck = pinput(td,1)
      ndamp = nint(td(1))
      write (iow,2003) ndamp

!     Restart flag

      if (refl) go to 100
50    if (kflag.eq.4.or.kflag.eq.5) then
        if(ior.lt.0) write(*,3001)
        errck = pinput(td,3)
        nodis = nint(td(1))
        nddis = nint(td(2))
        alfa0 = td(3)
        if(ior.lt.0) then
          if(nodis.le.0 .or. nodis.gt.numnp) go to 50
          if(nddis.le.0 .or. nddis.gt.ndf  ) go to 50
        endif

!       Calculation of eq.nr for prescribed displacement

        ndis = id(nddis,nodis)
        write (iow,3000) nodis,nddis,ndis,alfa0
        if(ndis.le.0) then
          if(ior.lt.0) then
            write(*,2001)
            go to 50
          else
            write(iow,2001)
            call plstop(.true.)
          endif
        endif
      endif
      return

!     For restart only
!     Any method (displacement control stiff.param. just for chance)

 100  write(iow,2005) rlnew,c0,cs01,cs02

!     Arc length method (any)

      if (kflag.lt.4.or.kflag.eq.6) then
        if(ior.lt.0) then
          write(*,2006) ds0,r
          write(*,2007)
!         read (*,1000) ch
          if(.not.cinput()) then
            write(*,*) 'CINPUT error in DICONT'
          end if
          ch = record(1:1)
        else
          read (ior,1000,end=900) record
          irecrd(isf) = irecrd(isf) + 1
          ch          = record(1:1)
        endif
        if(ch.eq.'n' .or. ch.eq.'N') then
          if(ior.lt.0) write(*,3003)
          errck = pinput(td,2)
          ds0 = td(1)
          r   = td(2)
          write(iow,2006) ds0,r
        endif
      endif

!     Displacement control

      if (kflag.eq.4.or.kflag.eq.5) then
        if(ior.lt.0) then
          write(*,2009) nodis,nddis,alfa0
          write(*,2008)
!         read (*,1000) ch
          if(.not.cinput()) then
            write(*,*) 'CINPUT error in DICONT'
          end if
          ch = record(1:1)
        else
          read (ior,1000,end=900) record
          irecrd(isf) = irecrd(isf) + 1
          ch          = record(1:1)
        endif
        if(ch.eq.'n' .or. ch.eq.'N') then
          go to 50
        endif
      endif
      return

!     Eof encountered

900   call  endclr ('DICONT',ch)

!     Formats

1000  format(a1)

2001  format('   Displacement control specified on restrained node')

2003  format('   Numerical damping = ',i3,3x,'(1 = no damping)')

2005  format('   v a l u e s  for  r e s t a r t:',/,
     & '     Current load level      = ',g12.4,/,
     & '     S t i f f n e s s  parameter values ',/,
     & '     Stiff.param first step  = ',g12.4,/,
     & '     Stiff.param 1.prev.step = ',g12.4,/,
     & '     Stiff.param 2.prev.step = ',g12.4,/)

2006  format('   Given arc length         = ',g12.4/
     *       '   Load direction           = ',f12.4)

2007  format('   Keep arc-length and load-direction (y or n): ',$)

2008  format('   Keep displacement control parameters (y or n): ',$)

2009  format('   Node number      = ',i3,/,
     &       '   Ndof number      = ',i3,/,
     &       '   Prescribed disp. = ',f10.3,/)

3000  format('   S i n g l e   D i s p l a c e m e n t   C o n t r o l '
     & ,/,  '   node nr.  ndof.nr.  equat.nr.  prescribed displ. ',/,
     &      4x,i4,7x,i2,8x,i4,6x,g12.5)

3001  format('   Input: node nr., dof.nr.,  prescribed displ.-> ',$)

3002  format('   Input: numerical damping (1 = no damping)->',$)

3003  format('   Input: new arc-length, new load direction->',$)

      end subroutine dicont

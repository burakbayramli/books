!$Id:$
      subroutine global()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set global solution parameter

!      Inputs:
!         none

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'modcon.h'
      include  'pglob1.h'
      include  'sdata.h'
      include  'refng.h'

      character (len=15) :: vtype(2)

      logical       :: pcomp, tinput, errck
      integer       :: i
      real (kind=8) :: td(14)

      save

!     Input a record

      write(iow,2000)
      if(ior.lt.0) then
        write(*,2000)
      end if
100   if(ior.lt.0) write(*,3000)
      errck = tinput(vtype,2,td,14)

!     Two Dimensional Options

      if(pcomp(vtype(1),'plan',4) .or. pcomp(vtype(1),'axis',4)) then

!       Plane stress/strain option

        if(ndm.le.2) then

!         Plane stress option

          if(    pcomp(vtype(2),'stre',4)) then

            g2type = 1
            write(iow,2001)
              if(ior.lt.0) then
              write(*,2001)
            end if

!         Plane strain option (default for 'plane')

          elseif(pcomp(vtype(2),'stra',4)) then

            g2type = 2
            write(iow,2002)
            if(ior.lt.0) then
              write(*,2002)
            end if

!         Axisymmetric option

          elseif(pcomp(vtype(2),'axis',4)) then

            g2type = 3
            write(iow,2003)
            if(ior.lt.0) then
              write(*,2003)
            end if

          endif

!       Error

        else

          write(iow,4000)
          if(ior.lt.0) then
            write(*,4000)
          end if

        endif

!     Kinematics: Small Deformation

      elseif(pcomp(vtype(1),'smal',4)) then

        gdtype = 1
        write(iow,2004)
        if(ior.lt.0) then
          write(*,2004)
        end if

!     Kinematics: Finite Deformation

      elseif(pcomp(vtype(1),'fini',4)) then

        gdtype = -1
        write(iow,2005)
        if(ior.lt.0) then
          write(*,2005)
        end if

!     Thermal-Mechanical Coupling: Temperature DOF

      elseif(pcomp(vtype(1),'temp',4)) then
        if(pcomp(vtype(2) ,'dof',3)) then

          gtdof = nint(td(1))
          if(gtdof.gt.0 .and. gtdof.le.ndf) then
            write(iow,2006) gtdof
            if(ior.lt.0) then
              write(*,2006) gtdof
            end if
          else
            if(ior.lt.0) then
              write(*,4001) gtdof
              return
            end if
            write(iow,4001) gtdof
            call plstop(.true.)
          end if
        end if

!     Define reference node/vector for element use

      elseif(pcomp(vtype(1),'refe',4)) then

        if(pcomp(vtype(2),'node',4)) then

          gref = 1
          do i = 1,ndm
            grefx(i) = td(i)
          end do

          write(iow,2007) (grefx(i),i=1,ndm)
          if(ior.lt.0) then
            write(*,2007) (grefx(i),i=1,ndm)
          end if

        elseif(pcomp(vtype(2),'vect',4)) then

          gref = 2
          do i = 1,ndm
            gtref(i) = td(i)
          end do

          write(iow,2008) (gtref(i),i=1,ndm)
          if(ior.lt.0) then
            write(*,2008) (gtref(i),i=1,ndm)
          end if

        else

          gref = 0
          write(iow,2009)
          if(ior.lt.0) then
            write(*,2009)
          end if

        endif

!     Global dof factors

      elseif(pcomp(vtype(1),'grou',4)) then

        do i = 1,ndf
          gfac(i) = td(i)
        end do

        write(iow,2010) (i,gfac(i),i=1,ndf)
        if(ior.lt.0) then
          write(*,2010) (i,gfac(i),i=1,ndf)
        end if

!     Rayleigh Damping factors

      elseif(pcomp(vtype(1),'rayl',4)) then

        gray(1) = td(1)
        gray(2) = td(2)
        write(iow,2011) gray(1),gray(2)
        if(ior.lt.0) then
          write(*,2011) gray(1),gray(2)
        endif
        rayla0 = gray(1)
        rayla1 = gray(2)

!     User: Global parameters

      elseif(.not.pcomp(vtype(1),' ',1)) then

        call uglobl(vtype(1),td)

      elseif(pcomp(vtype(1),' ',1)) then

        return

      end if

      go to 100

!     Formats

2000  format(/5x,'G l o b a l   P a r a m e t e r s'/1x)

2001  format(10x,'Plane Stress Analysis'/1x)

2002  format(10x,'Plane Strain Analysis'/1x)

2003  format(10x,'Axisymmetric Analysis'/1x)

2004  format(10x,'Kinematics: Small Deformation'/1x)

2005  format(10x,'Kinematics: Finite Deformation'/1x)

2006  format(10x,'Thermo-mechanical Coupling: Temperature DOF =',i3/1x)

2007  format(10x,'Reference node coordinates'/15x,3(1p,e14.5:))

2008  format(10x,'Reference vector components'/15x,3(1p,e14.5:))

2009  format(10x,'No reference state set')

2010  format(10x,'Ground acceleration components'/(15x,i10,1p,e14.5:))

2011  format(10x,'Rayleigh Damping Ratios'/
     &       15x,'Mass  value: a0',1p,1e14.5/
     &       15x,'Stiff value: a1',1p,1e14.5)

3000  format(/5x,'Input Global Parameter'/10x,'>',$)

4000  format(10x,'*WARNING* Can not set plane/axisymmetric option in ',
     &           'this mode.')

4001  format(10x,'*ERROR* Temperature degree-of-freedom input as ',
     &            i3/1x)

      end subroutine global

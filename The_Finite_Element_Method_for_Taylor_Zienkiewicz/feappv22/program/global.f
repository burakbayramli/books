c$Id:$
      subroutine global()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set global solution parameter

c      Inputs:
c         none

c      Outputs:
c         none
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'modcon.h'
      include  'pglob1.h'
      include  'sdata.h'
      include  'refng.h'

      logical   pcomp, tinput, errck
      character type(2)*15
      integer   i
      real*8    td(14)

      save

c     Input a record

      write(iow,2000)
      if(ior.lt.0) then
        write(*,2000)
      end if
100   if(ior.lt.0) write(*,3000)
      errck = tinput(type,2,td,14)

c     Two Dimensional Options

      if(pcomp(type(1),'plan',4) .or. pcomp(type(1),'axis',4)) then

c       Plane stress/strain option

        if(ndm.le.2) then

c         Plane stress option

          if(    pcomp(type(2),'stre',4)) then

            g2type = 1
            write(iow,2001)
              if(ior.lt.0) then
              write(*,2001)
            end if

c         Plane strain option (default for 'plane')

          elseif(pcomp(type(2),'stra',4)) then

            g2type = 2
            write(iow,2002)
            if(ior.lt.0) then
              write(*,2002)
            end if

c         Axisymmetric option

          elseif(pcomp(type,'axis',4)) then

            g2type = 3
            write(iow,2003)
            if(ior.lt.0) then
              write(*,2003)
            end if

          endif

c       Error

        else

          write(iow,4000)
          if(ior.lt.0) then
            write(*,4000)
          end if

        endif

c     Kinematics: Small Deformation

      elseif(pcomp(type,'smal',4)) then

        gdtype = 1
        write(iow,2004)
        if(ior.lt.0) then
          write(*,2004)
        end if

c     Kinematics: Finite Deformation

      elseif(pcomp(type,'fini',4)) then

        gdtype = -1
        write(iow,2005)
        if(ior.lt.0) then
          write(*,2005)
        end if

c     Thermal-Mechanical Coupling: Temperature DOF

      elseif(pcomp(type,'temp',4)) then
        if(pcomp(type(2) ,'dof',3)) then

          gtdof = td(1)
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
            call plstop()
          end if
        end if

c     Define reference node/vector for element use

      elseif(pcomp(type,'refe',4)) then

        if(pcomp(type(2),'node',4)) then

          gref = 1
          do i = 1,ndm
            grefx(i) = td(i)
          end do

          write(iow,2007) (grefx(i),i=1,ndm)
          if(ior.lt.0) then
            write(*,2007) (grefx(i),i=1,ndm)
          end if

        elseif(pcomp(type(2),'vect',4)) then

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

c     Global dof factors

      elseif(pcomp(type,'grou',4)) then

        do i = 1,ndf
          gfac(i) = td(i)
        end do

        write(iow,2010) (i,gfac(i),i=1,ndf)
        if(ior.lt.0) then
          write(*,2010) (i,gfac(i),i=1,ndf)
        end if

c     Rayleigh Damping factors

      elseif(pcomp(type,'rayl',4)) then

        gray(1) = td(1)
        gray(2) = td(2)
        write(iow,2011) gray(1),gray(2)
        if(ior.lt.0) then
          write(*,2011) gray(1),gray(2)
        endif
        rayla0 = gray(1)
        rayla1 = gray(2)

c     User: Global parameters

      elseif(.not.pcomp(type,' ',1)) then

        call uglobl(type,td)

      elseif(pcomp(type,' ',1)) then

        return

      end if

      go to 100

c     Formats

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

      end

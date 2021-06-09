!$Id:$
      subroutine umacr4(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Output 4-node element mesh from NURBS or T-Spline mesh

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cnurb.h'
      include  'iofile.h'
      include  'sdata.h'
      include  'umac1.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   pcomp
      character lct*15
      real*8    ctl(3)

      save

!     Set command word

      if(pcomp(uct,'mac4',4)) then      ! Usual    form
        uct = 'nout'                    ! Specify 'name'
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

!       Check if linear mesh exists

        if(np(276).ne.0) then
          call uout4nd(lct,mr(np(276)),hr(np(277)), ndm, ne_lin,nd_lin)
        else
          write(*,*) ' MESH NOT YET DEFINED'
        endif

      endif

      end
      subroutine uout4nd(lct,i_lin,x_lin, ndm, ne_lin,nd_lin)

      implicit   none

      include   'iofile.h'

      logical    pcomp
      character  lct*15, name*15
      integer    ndm,nd_lin,ne_lin,nm_lin, i,n
      integer    i_lin(17,*)
      real*8     x_lin(ndm,*)

!     Determine maximum number of materials

      nm_lin = 0
      do n = 1,ne_lin
        nm_lin = max(i_lin(17,n),nm_lin)
      end do ! n

!     Set output filename

      if(pcomp(lct,'    ',4)) then
        name = 'Z4node'
      else
        name  = lct
      endif

      if(ior.lt.0) then
        write(*,3000) ' Output 4-node element file: ',name
      endif
      write(iow,3000) ' Output 4-node element file: ',name

!     Open file and write control information

      open(unit=3,file = name,status = 'unknown')
      write(3,2000) 'feap * * File=',name,nd_lin,ne_lin,nm_lin,ndm,ndm,4

!     Output coordinates

      write(3,2001) 'COORdinates all'
      do n = 1,nd_lin
        write(3,2002) n,0,(x_lin(i,n),i=1,ndm)
      end do ! n

!     Output elements

      write(3,2001) 'ELEMents all'
      do n = 1,ne_lin
        write(3,2003) n,0,i_lin(17,n),(i_lin(i,n),i=1,4)
      end do ! n

!     Write end mesh

      write(3,2001) 'END MESH'
      write(3,2001) 'INTER'
      write(3,2001) 'STOP'

!     Formats

2000  format(a,a/6i8)
2001  format(/a)
2002  format(2i8,1p,3e16.7)
2003  format(7i8)

3000  format(a,a/1x)

      end

!$Id:$
      subroutine umacr1(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!       1. Remove 'prt' from argument list                  09/07/2009
!       2. Add 'uhelpfl' comment option                     19/10/2017
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: NURBS mesh creation for subproblems

!      Use:     NURBs - Command line first instruction

!                       Creates mesh input file: Unurbs
!                       which can be called to generate ELEV & INSE
!                       commands

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'bdata.h'
      include  'chdata.h'
      include  'comfil.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'sdata.h'
      include  'umac1.h'

      logical             :: pcomp
      character (len= 15) :: lct
      real      (kind=8)  :: ctl(3)

!     Local variables

      logical             :: exst, isopen, pflg
      character (len=128) :: nurfil
      character (len=  3) :: adde
      integer   (kind= 4) :: i

      save

!     Set command word

      if(pcomp(uct,'mac1',4)) then      ! Usual    form
        uct = 'nurb'                    ! Specify 'name'

      elseif(uhelpfl) then              ! Write help information

        write(*,*) 'COMMAND: NURB - Generate mesh'

      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

!       nurfil = finp
!       adde   = 'nur'
!       call addext(nurfil,adde,128,3)
!       open(unit=ios,file=nurfil,access = 'sequential')
        open(unit=ios,file='Unurbs',access = 'sequential')

!       Write header and title information
        write(ios,2000) head,ndm,ndf

!       Add parameter and material data
        inquire(file=fmtl,exist=exst,opened=isopen)
        if(exst) then
          if(.not.isopen) then
            open(unit=iwd,file=fmtl,status='old')
          endif
          pflg = .true.
          write(ios,'(a)') ' '
          do while(pflg)
            read(iwd,'(a)',end=200) xxx(1:80)
            do i = 80,1,-1
              if(xxx(i:i).ne.' ') go to 100
            end do ! i
            i = 1
100         if(xxx(i:i).eq.char(13)) xxx(i:i) = ' '
            write(ios,'(a)') xxx(1:i)
          end do ! while
200       close(iwd,status='keep')
        else
          write(*,*) 'ERROR:NURB - Material file does not exist'
        endif

!       Add include and close information
        write(ios,2001)
        close(unit=ios,status='keep')

      endif

!     Formats

2000  format(20a4/'  0  0  0',i5,i5,'  0'/)
2001  format('include NURBS_mesh'//'END MESH'//'STOP')

      end subroutine umacr1

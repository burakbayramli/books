!$Id:$
      subroutine plboun()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    23/11/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input element boundary conditions for a DOF

!      Inputs:
!        nlbou        - Number of LBOU files

!      Use:             LBOUndary
!                         ne  dof
!                         ne  dof
                            ! Terminate with blank record
!      Outputs:
!        lagbc(ndl,*) - Element boundary conditions
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
!     include   'eldata.h'
      include   'edgdat.h'
      include   'sdata.h'
      include   'pointer.h'
      include   'comblk.h'

      character (len=8)  :: fext
      character (len=4)  :: etype

      logical       :: setval, palloc
      integer       :: n

!     Allocate array
      if(np(210).eq.0) then
        call setndl(mr(np(33)),mr(np(32)))
        setval = palloc(210,'LAGBC',numel*ndl*2,1)
      endif

      call pzeroi(mr(np(210)),numel*ndl)
!     Loop over sets of records
      do n = 0,nlbou-1
        call setext('lbou', n, fext, .false.)
        call pinpfl('PLBOUN',fext, etype, 1)          ! Open file
        call plbound(mr(np(210)+ndl*numel), ndl,numel)
        call pinpfl('PlBOUN',fext, etype, 2)          ! Delete file
      end do ! n

      end subroutine plboun

      subroutine plbound(lagbc, ndl,numel)

      implicit   none

      include   'bdata.h'
      include   'iofile.h'
      include   'print.h'

      real (kind=8) :: td(2)
      logical       :: setval, pinput, sethed
      integer       :: ndl,numel
      integer       :: lagbc(ndl,numel)
      integer       :: me, dof

!     Loop over input records in set
      sethed = .true.
      me     = 1
      do while(me.ne.0)
        setval = pinput(td,2)
        me  = nint(td(1))
        dof = nint(td(2))
!       Check for error
        if(me .lt.0 .or. me. gt.numel) then
          write(iow,3000) me,numel,dof,ndl
          call plstop(.true.)
        elseif(me.gt.0) then
!         Check for error
          if(dof.le.0 .or. dof.gt.ndl) then
            write(iow,3000) me,numel,dof,ndl
            call plstop(.true.)
!         Set B.C.
          else
            if(sethed) then
              write(iow,2000) head
              sethed = .false.
            endif
            write(iow,'(2i8)') me,dof
            lagbc(dof,me) = 1
          endif ! dof check
        endif ! me
      end do ! while

!     Formats

2000  format(/1x,19a4,a3//5x,'E l e m e n t   B o u n d a r y   ',
     &      'C o n d i t i o n s'//6x,'Elmt    DOF')


3000  format('--> ERROR in LBOU: elmt =',i8,' maximum =',i5/
     &       '                   dof  =',i8,' maximum =',i5)

      end subroutine plbound

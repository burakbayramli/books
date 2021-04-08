!$Id:$
      logical function tinput(tx,mt,d,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input routine for real data.: returns true on error
!               Data input device: Returns true on error

!      Inputs:
!         mt        - Number of text data items to extract from input
!         nn        - Number of real data items to extract from input
!                     N.B. Input performed by this function

!      Outputs:
!         tx(*)     - Values of text data input
!         d(*)      - Values of real data input
!         tinput    - Flag, returns true if error occurs during input
!-----[--.----+----.----+----.-----------------------------------------]
      implicit    none

      include    'chdata.h'
      include    'comfil.h'
      include    'iodata.h'
      include    'ioincl.h'
      include    'iofile.h'
      include    'iosave.h'
      include    'setups.h'

      include    'mpif.h'

      logical            :: cinput, vinput, pcomp, cksep, first

      integer            :: i,j,k,mc,mm,mt,nn,lrec, iskip
      integer            :: ierr, msg_stat(MPI_STATUS_SIZE)
      character (len=15) :: txl,tx(*),tl(16)
      real      (kind=8) :: d(*)
      integer            ::  usr_msg
      parameter             (usr_msg=11)

!     Parameters
      integer            :: cw    ! Field width for parsing
      integer            :: fw    ! Field width for parsing
      integer            :: rec   ! Record length

      save

      data        lrec /256/
      data        cw   / 15/
      data        fw   / 15/
      data        rec  / 80/


!     Check on number of items

      if(mt+nn.gt.16) then
        tinput = .true.
        if(ior.lt.0) then
          write(*,2000) mt+nn
          return
        else
          write(iow,2000) mt+nn
          call plstop(.true.)
        endif
      else
        tinput = .false.
      endif

!     Initialize arrays

10    tx(1) = ' '
      do j = 1,nn
        d(j) = 0.0d0
      end do ! j
      record = ' '

11    if(ior.gt.0) then
        read (ior,1000,err=901,end=902) record
        irecrd(isf) = irecrd(isf) + 1
        iskip       = 1
      else

        if(rank.eq.0) then
!         read (  *,1000,err=901,end=902) record
          if(.not.cinput()) then
            go to 902
          endif
          if(mpiflg) then
            do j = 1,ntasks-1
              call MPI_Send(record, 256, MPI_CHARACTER, j, usr_msg,
     &                      MPI_COMM_WORLD, ierr)
            end do ! j
          endif
        else
          if(mpiflg) then
            call MPI_Recv(record, 256, MPI_CHARACTER, 0, usr_msg,
     &                    MPI_COMM_WORLD, msg_stat, ierr)
          else
            tinput = .false.
            return
          endif
        endif

        iskip = 3
      endif

!     Strip control characters, leading blanks and comments

      yyy = record
      call pstrip(xxx,yyy,iskip)

12    do k = lrec,1,-1
        if(xxx(k:k).ne.' ') go to 13
      end do ! k
      k = 1
13    if(lsave) write(lfile,1000) xxx(1:k)

!     Load character variables

      if(pcomp(xxx,'incl',4)) then
        mm = max(2,mt)
      else
        mm = mt
      endif

      tl(1) = '    '
      if(mm.gt.0) then
        mc = 1
        txl = ' '
        j   = 0
        first = .false.

!       String text between double quotes (")

        do i = 1,lrec
          if(xxx(i:i).eq.'"' .or. first) then
            if(first) then
              if(xxx(i:i).eq.'"') then
                first    = .false.
              else
                j        = j + 1
                if(j.le.cw) then
                  txl(j:j) = xxx(i:i)
                endif
              endif
            else
              first      = .true.
            endif

!         Non-separator string data

          elseif(.not.cksep(xxx(i:i))) then
            j          = j + 1
            if(j.le.cw) then
              txl(j:j)   = xxx(i:i)
            endif

!         Separator encountered: save character string data

          else
            tl(mc)     = txl
            txl        = ' '
            j          = 0
            mc         = mc + 1
            if(mc.gt.mm) go to 14
          endif
        end do ! i
      else
        i = 0
      end if

14    do j = 1,mt
        tx(j) = tl(j)
      end do ! j

!     Change to an include file

      if(pcomp(tl(1),'incl',4)) then
        if(  pcomp(tl(2),'end',3) ) then
          if(ior.eq.icf) then
            call pincld('end')
          endif
          return
        else
          call pincld(tl(2))
          go to 10
        endif
      endif

!     Finish inputs for parameters

      call acheck(xxx,yyy,fw,rec,rec)
      zzz = xxx(i+1:rec)
      if(nn.gt.0 .and. .not.pcomp(tl(1),'proc',4)) then
        tinput = vinput(xxx(i+1:lrec),lrec-i,d,nn)
      else
        tinput = .false.
      end if

      return

!     Read error encoutered

901   call  errclr ('TINPUT ')
      goto  11

!     EOF encountered

902   if(eofile) then
        tinput = .true.
      elseif(ior.eq.ios) then  ! End of file on an include file
        tinput = .false.       ! Can trap using a test on 'end'
        tx(1)  = 'end  '
      elseif(ior.eq.icf) then
        call pincld('end')
        tinput = .false.
        tx(1)  = ' '
      else
        call  endclr ('TINPUT',yyy)
        goto  12
      endif

!     Formats

1000  format(a)

2000  format(' *ERROR* TINPUT: Too many items requested, limit = 16:',
     &       ' Requested',i8)

      end function tinput

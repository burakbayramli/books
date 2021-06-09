!$Id:$
      subroutine pfe2rve(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    28/01/2016
!       1. Correct distribution of RVE's onto ums(*,*)      23/01/2018
!       2. Add 'nsbuf',print of MPI_SSend infor             24/01/2018
!       3. Add 'rveflg' to limit use of RVE to one time     27/01/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: MPI - Manipulation command to select mesh for micro
!                     RVE problems.  Sets problem type.

!      Use  : Solution command name: 'rve point n_pnt'
!             point - 'point': Activate output to files
!             n_pnt -  RVE number for output

!      N.B. Currently restricted to 64 processors by dimensions in
!           include file 'oumatl.h'

!      Inputs:
!         lct(*) - 'poin't - location to output data for plotting

!      Outputs:
!         files  - 90 = deformation gradients
!                  91 = stresses
!                  92 = tangent moduli
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'
      include   'oumatl.h'
      include   'chdata.h'
      include   'debugs.h'
      include   'setups.h'

      include   'pointer.h'
      include   'comblk.h'

      include   'mpif.h'

      character  (len=15) :: lct
      character (len=132) :: sbuf
      integer             :: i,ii,n, nsbuf, nsmax, ierr,usr_msg
      real       (kind=8) :: ctl(3)

      save

      data        nsbuf / 132 /

      if(rank.eq.0 .and. rveflg) then

        n_pnt = 0

        call uprocset(mr(np(269)), nsmax)
        usr_msg = 14
        ii = 0
        do n = 1,nsmax
          if(unproc(n).gt.0) then
            do i = 1,unproc(n)
              ii = ii + 1

!             Set problem file, type and deformation state

              sbuf(1:128) = matfile(n)
              write(sbuf(129:130),'(i2)') prtyp(1,n)
              write(sbuf(131:132),'(i2)') prtyp(2,n)

!             Send a file name

              if(sbuf(1:1).ne.' ') then
                if(debug) then
                 write(*,*) 'PFE2RVE:MPI_SSend:NSBUF,MSG:',nsbuf,usr_msg
                endif
                call MPI_SSend(sbuf, nsbuf, MPI_CHARACTER, ii, usr_msg,
     &                      MPI_COMM_WORLD, ierr)
              endif
            end do ! i
          endif
        end do ! n

!       Prevent multiple calls to RVE command
        rveflg = .false.

      else

        write(*,*) ' --> WARNING: RVE command only allowed once'

      endif

      end subroutine pfe2rve

      subroutine uprocset(rvema, nsmax)

      implicit   none

      include   'debugs.h'
      include   'iofile.h'
      include   'setups.h'
      include   'oumatl.h'
      include   'pointer.h'
      include   'comblk.h'

      logical    err, setval, palloc
      integer    n,nsmax, maxn,maxp, minn,minp, totp, totsend
      integer    rvema(*)

      save

      if(debug) call iprint(rvema,1,nsend,1,'RVEMA')
      umproc(1:64) = 0

      err = .false.
      nsmax = 0
      do n = 1,nsend
        if(rvema(n).gt.64) then
          err = .true.
          exit
        elseif(rvema(n).gt.0) then
          umproc(rvema(n)) = umproc(rvema(n)) + 1
          nsmax            = max(nsmax,rvema(n))
        endif
      end do ! n

!     Error on number of processors

      if(err) then
        write(iow,3000)
        call plstop(.true.)
      endif

!     Check for correct number of sends

      totsend = 0
      do n = 1,nsmax
        totsend = totsend + umproc(n)
      end do ! n

      if(totsend.ne.nsend) then
        write(iow,3001) totsend,nsend
        call plstop(.true.)
      endif

!     Allocate number of processors for each material

      minp = 64
      minn = 0
      maxp = 0
      maxn = 0
      totp = 0
      do n = 1,nsmax
        if(umproc(n).gt.0) then
          unproc(n) = nint((ntasks-1)*dble(umproc(n))/dble(totsend))
          unproc(n) = max(1,unproc(n))
          totp      = totp + unproc(n)
          if(unproc(n).gt.maxp) then
            maxp = max(maxp,unproc(n))
            maxn = n
          elseif(unproc(n).lt.minp) then
            minp = max(minp,unproc(n))
            minn = n
          endif
        endif
      end do ! n

!     Check that number of processors is correct

      do while (totp.ne.ntasks - 1)
        if(totp .gt. ntasks - 1) then
          unproc(maxn) = unproc(maxn) - 1
          totp         = totp - 1
        elseif(totp.lt.ntasks - 1) then
          unproc(minn) = unproc(minn) + 1
          totp         = totp + 1
        endif
        minp = 64
        minn = 0
        maxp = 0
        maxn = 0
        do n = 1,nsmax
          if(unproc(n).gt.maxp) then
            maxp = max(maxp,unproc(n))
            maxn = n
          elseif(unproc(n).lt.minp) then
            minp = max(minp,unproc(n))
            minn = n
          endif
        end do ! n
      end do ! while

      if(debug) then
        call iprint(umproc,1,nsmax,1,'UMPROC')
        call iprint(unproc,1,nsmax,1,'UNPROC')
      endif

!     Set up order to send data to each processor

      nrow = 0
      do n = 1,nsmax
        if(unproc(n).gt.0) then
          nrow = max(nrow,(umproc(n) + unproc(n) - 1)/unproc(n))
        endif
      end do ! n
      ncol = ntasks - 1

!     Allocate array to store send list

      setval = palloc( 270,'RVESD',nrow*ncol, 1)

      call psetrvsd(mr(np(269)),mr(np(270)),nsmax)

!     Formats

3000  format(' *ERROR* More user materials than processors.'/
     &       '         Increase number of processors.')

3001  format(' *ERROR* Incorrect number of sends.'/
     &       '         Total sends =',i8,' Should be =',i8)

      end subroutine uprocset

      subroutine psetrvsd(rvema,ums,nsmax)

      implicit   none

      include   'debugs.h'
      include   'oumatl.h'
      include   'setups.h'

      integer    nsmax, nc,nr, i,j,jj, n
      integer    rvema(*),ums(ncol,nrow)

      save

      j  = 0
      nc = 0
      do n = 1,nsmax
        if(unproc(n).gt.0) then
          nr = 0
          jj = 0
          do i = 1,nsend
            if(rvema(i).eq.n) then
              jj = jj + 1
              j  = mod(jj-1,unproc(n)) + 1
              if(j.eq.1) then
                nr = nr + 1
              endif
              ums(nc+j,nr) = i
            endif
          end do ! i
          nc = nc + unproc(n)
        endif
      end do ! n

!     Tag last send to each processor

      do n = 1,ncol
        do nr = nrow,1,-1
          if(ums(n,nr).gt.0) then
            ums(n,nr) = -ums(n,nr)
            exit
          endif
        end do ! nr
      end do ! n

      if(debug) then
        call iprint(ums,ncol,nrow,nc,'UMS')
      endif

      end subroutine psetrvsd

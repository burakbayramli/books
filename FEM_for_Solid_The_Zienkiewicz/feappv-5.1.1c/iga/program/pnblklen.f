!$Id:$
      subroutine pnblklen(nblok,nblkd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set nurb blocks length

!      Data: is = block number (automatically counted)
!        block nblksd(1,is) nside

!      Inputs:
!         prt   - Print flag

!      Outputs:
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cnurb.h'
      include   'iofile.h'
      include   'iodata.h'

      logical    pcomp,errck,tinput, startfl, sidefl
      character  tx*15, typ*15
      integer    i,j,nn, nblok, nblkd
      real*8     td(4),tc(16)

      tx    = 'start'
      startfl = .true.
      do while (.not.pcomp(tx,'    ',4))
        errck = tinput(tx,1,td,4)
        if(pcomp(tx,'surf',4) .or. pcomp(tx,'bloc',4)) then
          nblok = nblok + 1
          if(startfl) then
            write(ios,'(a)') 'NBLOck'
            startfl = .false.
          endif
          write(ios,'(a,4i6)') '  block',(nint(td(i)),i=1,4)
          if(nint(td(1)).eq.2) then

!           Input eside data for bending strips

            errck = tinput(typ,1,tc,4)
            if(pcomp(typ,'esid',4)) then
              write(ios,'(a,4i8)') '  eside',(nint(tc(i)),i=1,4)
            else
              if(ior.gt.0) backspace (ior)
            endif
          elseif(nint(td(1)).eq.3) then

            nn     = 0
            sidefl = .true.
            do while(sidefl)
              errck = tinput(typ,0,tc,16)
              if(nint(tc(1)).gt.0) then
                do i = 1,16
                  j = i
                  if(nint(tc(i)).eq.0) then
                    j      = j - 1
                    sidefl = .false.
                    exit
                  endif
                end do ! i
                write(ios,'(16i8)') (nint(tc(i)),i=1,j)
                nn = nn + j
              else
!               if(ior.gt.0) backspace (ior)
                sidefl = .false.
              endif
            end do ! while
            nblkd = max(nblkd,nn)
          endif

        endif
      end do ! while
      write(ios,'(a)') ' '

      end

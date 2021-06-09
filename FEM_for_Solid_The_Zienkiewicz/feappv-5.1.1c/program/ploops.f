!$Id:$
      subroutine ploops(lp_in,txt,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Control program for mesh LOOP-NEXT commands

!      Inputs:
!         lp_in      - Logical for starting loop
!         txt        - Text storing loop range value
!         isw        - Switch: isw = 1 for LOOP
!                              isw = 2 for NEXT

!      Outputs:
!         Depends on commands specified
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iodata.h'
      include   'iofile.h'

      character (len=15) :: txt, txl, lp_file, lp_fil0

      logical       :: lp_in, lopen
      integer       :: isw,lp_lun,lp_num,lp_ior(0:9),lp_cur(9),lp_max(9)
      real (kind=8) :: loopval

      save

!     [loop,#] - Loop start

      if(isw.eq.1) then

        if(lp_in) then

          lp_in     = .false.

!         Check rank number

          lp_fil0 = 'feaploop000.0'

!         Set loopvalue

          call setval(txt,15,loopval)
          lp_max(1) = max(1,nint(loopval))

!         Construct file(s) for loop commands

          call ploopin(lp_max(1))
          lp_ior(0) =  ior

          lopen     = .true.
          lp_lun    =  icl - 1
          do while(lopen)
            lp_lun  =  lp_lun + 1
            inquire(unit = lp_lun, opened = lopen)
          end do ! while

          ior       = lp_lun
          lp_file   = lp_fil0
          open(unit = ior, file = lp_file,status = 'old')
          rewind(ior)

          lp_num    = 0

        else

          call setval(txt,15,loopval)
          lp_num         = lp_num + 1
          lp_max(lp_num) = max(1,nint(loopval))
          lp_ior(lp_num) = ior
          lp_cur(lp_num) = 1

        endif

!     [next] - Loop end

      elseif(isw.eq.2) then

        if(.not.lp_in) then

          lp_cur(lp_num) = lp_cur(lp_num) + 1

          if(lp_cur(lp_num).gt.lp_max(lp_num)) then

            close(ior)
            lp_num = lp_num - 1
            ior    = lp_ior(lp_num)
            lp_lun = lp_lun - 1
            if(lp_num.eq.0) then
              lp_in = .true.
            endif

          else

            rewind(ior)
            read(ior,'(a)') txl ! prevent a re-read of the loop

          endif

!       Error

        else

          write(iow,2000)
          call plstop(.true.)

        endif
      endif

!     Output format

2000  format(/5x,'LOOP-NEXT error in PMESH: NEXT before LOOP.'/)

      end subroutine ploops

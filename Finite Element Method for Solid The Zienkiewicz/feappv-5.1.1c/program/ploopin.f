!$Id:$
      subroutine ploopin(lp_max)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Data input routine for mesh description

!      Inputs:
!        lp_max  - Number of times to loop in root loop
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iodata.h'
      include   'iofile.h'
      include   'setups.h'

      character (len=256) :: xxx,yyy
      character (len=15)  :: lp_file
      character (len=1)   :: lp_ext

      logical       :: pcomp, lopen
      integer       :: lp_lun, lp_max, lp_num, lenx, loop_l(9)

      save

!     Set parameters

      lp_file = 'feaploop000.0'
      lp_lun  =  icl - 1
      lopen   = .true.
      do while(lopen)
        lp_lun = lp_lun + 1
        inquire(unit = lp_lun,opened = lopen)
      end do ! while

!     Start root file

      lp_num         = 1
      loop_l(lp_num) = lp_lun
      open(unit = lp_lun, file = lp_file, status = 'unknown')
      rewind(lp_lun)
      write(lp_lun,1000) 'loop',lp_max

!     Fill remaining file entries

      xxx   = 'start'
      do while(.not.pcomp(xxx,'next',4) .and. lp_num.gt.0)

        read(   ior,1000) yyy
        call pstrip(xxx,yyy,1)

!       New loop encountered: Reset file structures

        if(pcomp(xxx,'loop',4)) then

!         Establish new filename and save to current file

          write(lp_ext,'(i1)') lp_num
          lp_file(13:13) = lp_ext
          yyy(1:5)       = 'file='
          yyy(6:18)      = lp_file(1:13)
          write(lp_lun,1000) yyy(1:18)

!         Establish new file structure

          lopen = .true.
          do while(lopen)
            lp_lun       = lp_lun + 1
            inquire(unit=lp_lun,opened=lopen)
          end do ! while

          open(unit = lp_lun, file = lp_file, status = 'unknown')
          rewind(lp_lun)

          lp_num         = lp_num + 1
          if(lp_num.gt.9) then
            write(iow,3000)
            call plstop(.true.)
          endif
          loop_l(lp_num) = lp_lun
        endif

!       Compress amount to be written

        lenx = 80
        do while(xxx(lenx:lenx).eq.' ' .and. lenx.gt.1)
          lenx = lenx - 1
        end do ! while
        write(lp_lun,1000) xxx(1:lenx)

!       End of loop structure encountered: Set back to previous

        if(pcomp(xxx,'next',4)) then

          if(lp_num.gt.1) then
            xxx = 'restart'
          endif

          close(lp_lun)
          lp_num = lp_num - 1
          lp_lun = loop_l(lp_num)
        endif
      end do ! while

!     Format structure

1000  format(a,i5)

3000  format(' *ERROR* PLOOPIN: Mesh loop levels nested deeper than 9')

      end subroutine ploopin

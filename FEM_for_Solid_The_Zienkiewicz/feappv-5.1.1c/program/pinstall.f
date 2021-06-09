!$Id:$
      subroutine pinstall()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      call pinset(1)
      call pinset(2)

      end subroutine pinstall

      subroutine pinset(isw)

      implicit  none

      include  'chdata.h'
      include  'comfil.h'
      include  'codat.h'
      include  'hlpdat.h'
      include  'iofile.h'
      include  'iosave.h'
      include  'prmptd.h'
      include  'pathn.h'

      character (len=15) :: text(3)

      logical       :: pcomp,cksep,exst, flag, tinput
      integer       :: i,j, isw
      real (kind=8) :: td(1)

      save

!-----[--.----+----.----+----.-----------------------------------------]

      if(isw.eq.1) then
        inquire(file='feap.ins',exist=exst)
        if(exst) then
          open( unit = ior, file = 'feap.ins', status = 'old',
     &          access = 'sequential')
        endif
      else
        inquire(file='./.feap.ins',exist=exst)
        if(exst) then
          open( unit = ior, file = './.feap.ins', status = 'old',
     &          access = 'sequential')
        endif
      endif
      if(exst) then
        eofile = .true.
        flag   = .false.
        do while(.not.flag)

          flag = tinput(text,3,td,0)

          if(pcomp(text(1),'manfile',7)) then

!           Find the second separator to extract the file name

            call pstrip(xxx,record,1)
            do j = 1,80
              if(cksep(xxx(j:j)) ) then
                do i = j+1,80
                  if(cksep(xxx(i:i)) ) go to 100
                end do ! i
              endif
            end do ! i
            write(iow,3000)
            call plstop(.true.)

!           Extract name

100         if    (pcomp(text(2),'mesh',4)) then
              file(1) = xxx(i+1:i+45)
            elseif(pcomp(text(2),'macr',4)) then
              file(2) = xxx(i+1:i+45)
            elseif(pcomp(text(2),'plot',4)) then
              file(3) = xxx(i+1:i+45)
            elseif(pcomp(text(2),'elem',4)) then
              file(4) = xxx(i+1:i+45)
            endif

          elseif(pcomp(text(1),'noparse',7)) then

            coflg = .false. ! Numerical input mode, noparsing

          elseif(pcomp(text(1),'parse  ',7)) then

            coflg = .true.  ! Parse all input as expressions

!         Set graphics default options

          elseif(pcomp(text(1),'graphic',7)) then
            if    (pcomp(text(2),'prompt ',7)) then
              if(pcomp(text(3),'off',3)) then
                prompt = .false.
              else
                prompt = .true.
              endif
            elseif(pcomp(text(2),'default',7)) then
              if(pcomp(text(3),'off',3)) then
                defalt = .false.
              else
                defalt = .true.
              endif
            endif

!         Set PostScript default mode

          elseif(pcomp(text(1),'postscr',7)) then
            if    (pcomp(text(2),'color  ',7)) then
              pscolr = .true. ! Color PostScript
              if(pcomp(text(3),'reverse',7)) then
                psrevs = .true. ! Color order is reversed
              else
                psrevs = .false.! Color order is normal
              endif
            else
              pscolr = .false.! Grayscale PostScript
            endif

!         Set help display level:

          elseif(pcomp(text(1),'helplev',7)) then
            if    (pcomp(text(2),'basic  ',7)) then
              hlplev = 0      ! Basic
            elseif(pcomp(text(2),'interme',7)) then
              hlplev = 1      ! Intermediate
            elseif(pcomp(text(2),'advance',7)) then
              hlplev = 2      ! Advanced
            else
              hlplev = 3      ! Expert
            endif
          endif

        end do ! while
        close(ior)
      endif

!-----[--.----+----.----+----.-----------------------------------------]

3000  format(/' *ERROR* on FILE record in feap.ins')

      end subroutine pinset

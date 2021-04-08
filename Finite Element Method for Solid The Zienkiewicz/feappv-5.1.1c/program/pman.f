!$Id:$
      subroutine pman(mname,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Output manual page to screen.  Manual page location
!               set in main program.  Must have extender '.t'

!      Inputs:
!         mname     - Name of command to display
!         nn        - Type of command: 1=mesh, 2=macro, 3=plot

!      Outputs:
!         To screen
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'iodata.h'
      include  'pathn.h'

      character (len=80) :: line
      character (len=44) :: filel
      character (len=4)  :: mname
      character (len=1)  :: y

      logical       :: lexst,cinput
      integer       :: nn, iposl, ipos, i,j,mod

      save

!     Path name to where manual files is stored in 'file'
!     set ipos to last entry in path

      filel = file(nn)
      iposl = ipos(filel,44)

!     Find first blank, add 'name' and extender '.t'

      if(mname(1:1).lt.'a' .or. mname(1:1).gt.'z') then
        write(*,2006) filel
        return
      endif

      do i = 1,4
        if(mname(i:i).eq.' ') go to 101
        filel(iposl+i:iposl+i) = mname(i:i)
      end do
      i = 5
101   filel(iposl+i:iposl+i+1)   = '.t'
      if(i.lt.5) then
         do j = 39+i,44
           filel(j:) = ' '
         end do
      endif

      write(*,2001) mname
      write(*,2002) filel

!     Open file for manual page

      inquire(file=filel,exist=lexst)
      if(lexst) then
        open(ios,file=filel,status='unknown')
        rewind ios
        do j = 1,1000
          read(ios,1000,end=200) line
          if(line(1:1).ne.'.') then
            do i = 80,1,-1
              if(line(i:i).ne.' ') go to 60
            end do
            i = 1
 60         write(*,2003) line(1:i)
          endif
          if(mod(j,20).eq.0) then
            write(*,2004)
!61         read(*,1000,err=62,end=63) y
 61         if(.not.cinput()) then
              goto 63
            end if
            y = record(1:1)
            goto  64
!62         call  errclr ('PMAN  ')
            call  errclr ('PMAN  ')
            goto  61
 63         call  endclr ('PMAN  ',y)
 64         if(y.ne.'y') go to 200
          endif
        end do
        close (ios)
200     write(*,2005)
      else
        write(*,2006) filel
      endif

!     Formats

1000  format(a)

2001  format(/3x,'Help requested for: ',a/)

2002  format(/3x,'File name: ',a/)

2003  format(1x,a)

2004  format(/3x,'More wanted? (y or n) >',$)

2005  format(/3x,'End of help request.'/)

2006  format(/3x,'The requested file does not exist; file:'/
     &       8x,a44)

      end subroutine pman

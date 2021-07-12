c$Id:$
      subroutine pman(name,nn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Output manual page to screen.  Manual page location
c               set in main program.  Must have extender '.t'

c      Inputs:
c         name      - Name of command to display
c         nn        - Type of command: 1=mesh, 2=macro, 3=plot

c      Outputs:
c         To screen
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iodata.h'
      include  'pathn.h'

      logical   lexst
      character name*4,filel*44,line*80,y*1
      integer   nn, iposl, ipos, i,j,mod

      save

c     Path name to where manual files is stored in 'file'
c     set ipos to last entry in path

      filel = file(nn)
      iposl = ipos(filel,44)

c     Find first blank, add 'name' and extender '.t'

      if(name(1:1).lt.'a' .or. name(1:1).gt.'z') then
        write(*,2006) filel
        return
      endif

      do i = 1,4
        if(name(i:i).eq.' ') go to 101
        filel(iposl+i:iposl+i) = name(i:i)
      end do
      i = 5
101   filel(iposl+i:iposl+i+1)   = '.t'
      if(i.lt.5) then
         do j = 39+i,44
           filel(j:) = ' '
         end do
      endif

      write(*,2001) name
      write(*,2002) filel

c     Open file for manual page

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
 61         read(*,1000,err=62,end=63) y
            goto  64
 62         call  errclr ('PMAN  ')
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

c     Formats

1000  format(a)

2001  format(/3x,'Help requested for: ',a/)

2002  format(/3x,'File name: ',a/)

2003  format(1x,a)

2004  format(/3x,'More wanted? (y or n) >',$)

2005  format(/3x,'End of help request.'/)

2006  format(/3x,'The requested file does not exist; file:'/
     &       8x,a44)

      end

!$Id:$
      subroutine fppsop(scal)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Open a new PostScript file to receive plot data
!               Maximum files: 676 (FeapAA.eps to FeapZZ.eps)
!
!      Inputs:
!         scal      - Scale factor for plot data to be written

!      Outputs:
!         none      - Outputs are written to PostScript file
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'bdata.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'plpost.h'
      include  'psdat2.h'
      include  'psdat4.h'
      include  'psdat5.h'
      include  'psdat6.h'

      character (len=68) :: title
      character (len=24) :: cdate
      character (len=21) :: string
      character (len=8)  :: uname

      logical       :: fexist
      integer       :: i, ii, iln(2)
      real (kind=8) :: scal

      save

      nxtchr = 0

!     Get current date and user's name for banner page

      call fdate(cdate)
      call getlog(uname)

!     Find a file name that does not already exist

      fname = 'FeapAA.eps'
      inquire(file = fname,exist = fexist)
      do while(fexist)
        call postname(fname)
        inquire(file = fname,exist = fexist)
      end do ! while

!     Set initial BoundingBox coordinates

      xll = 5800
      yll = 4500
      xur = 0
      yur = 0

!     Add problem title to file

      ii = 0
      do i = 1,67,4
        ii           = ii + 1
        title(i:i+3) = head(ii)
      end do ! i

!     Show initialization worked, i.e. we opened file.

      if(ior.lt.0) write(*,2000) fname(1:10)

      open(unit=lun,file='temp.eps',status='unknown')

!     Write header information to file

      call fppsin('%!PS-Adobe-3.0 EPSF-3.0')
      call fppsdu()
      call fppsin('%%BoundingBox: (atend) ')
      call fppsdu()
      call fppsin('%%Title: '//title)
      call fppsdu()
      call fppsin('%%Creator: '//uname)
      call fppsdu()
      call fppsin('%%Creation Date: '//cdate//' ')
      call fppsdu()
      call fppsin('%%EndComments ')
      call fppsdu()

!     Set procedure definitions

      call fppsin('/m {moveto} bind def /l {lineto} bind def ')
      call fppsdu()
      call fppsin('/s {stroke} bind def /f {fill} bind def ')
      call fppsdu()
      call fppsin('/n {newpath} bind def /c {closepath} bind def ')
      call fppsdu()
      call fppsin('/g {setgray} bind def /h {setrgbcolor} bind def ')
      call fppsdu()
      call fppsin('/d {setdash} bind def /lw {setlinewidth} bind def ')
      call fppsdu()

!     Set gray scale shades

      call fppsin('/g0 { 0.0 g} bind def /g1 { 1.0 g} bind def ')
      call fppsdu()
      call fppsin('/g2 {0.95 g} bind def /g3 {0.81 g} bind def ')
      call fppsdu()
      call fppsin('/g4 {0.67 g} bind def /g5 {0.53 g} bind def ')
      call fppsdu()
      call fppsin('/g6 {0.39 g} bind def /g7 {0.25 g} bind def ')
      call fppsdu()
      call fppsin('/g8 {0.11 g} bind def ')
      call fppsdu()

!     Set color scale hues

      call fppsin('/h0 { 0.0 0.0 0.0 h} bind def')
      call fppsdu()
      call fppsin('/h1 { 0.0 0.0 1.0 h} bind def ')
      call fppsdu()
      call fppsin('/h2 { 0.4 0.6 0.9 h} bind def')
      call fppsdu()
      call fppsin('/h3 { 0.0 0.9 0.9 h} bind def ')
      call fppsdu()
      call fppsin('/h4 { 0.0 0.8 0.0 h} bind def')
      call fppsdu()
      call fppsin('/h5 { 0.9 0.9 0.0 h} bind def ')
      call fppsdu()
      call fppsin('/h6 { 0.9 0.6 0.4 h} bind def')
      call fppsdu()
      call fppsin('/h7 { 1.0 0.0 0.0 h} bind def ')
      call fppsdu()
      call fppsin('/h8 { 1.0 1.0 1.0 h} bind def ')
      call fppsdu()

!     Set Line types

      call fppsin('/l1 { [] 0 d } bind def ')
      call fppsdu()
      call fppsin('/l2 { [5 30] 0 d } bind def ')
      call fppsdu()
      call fppsin('/l3 { [40 20 5 20] 0 d } bind def ')
      call fppsdu()
      call fppsin('/l4 { [40] 0 d } bind def ')
      call fppsdu()
      call fppsin('/l5 { [60] 0 d } bind def ')
      call fppsdu()
      call fppsin('/l6 { [5 20 5 40 40 40] 0 d } bind def ')
      call fppsdu()
      call fppsin('/l7 { [40 60 80 60] 0 d } bind def ')
      call fppsdu()
      call fppsin('/l8 { [80] 0 d } bind def ')
      call fppsdu()

!     Set for 12 point type in landscape mode (10 in portrait)

      if(psfram) then
        call fppsin
     &   ('/H {/Helvetica findfont 91 scalefont setfont} bind def')
      else
        call fppsin
     &   ('/H {/Helvetica findfont 100 scalefont setfont} bind def')
      endif
      call fppsdu()
      call fppsin('/w {stringwidth pop 2 div neg 0 rmoveto} bind def')
      call fppsdu()

!     Set clipping definitions

      call fppsin('/gr {grestore} bind def  /gs {gsave} bind def')
      call fppsdu()
      call fppsin('/cl {gr gs 802 802 3333 3333 rectclip} bind def')
      call fppsdu()
      call fppsin('/fl {gr gs   0   0 5800 4800 rectclip} bind def')
      call fppsdu()

!     End of prolog

      call fppsin('%%EndProlog ')
      call fppsdu()

!     Start landscape mode plot

      if(psfram) then
        call fppsin('%Landscape mode ')
        call fppsdu()
        if(blk) then
          call fppsin(' 0 0 0 h ')
          call fppsin('n 0 0 m 612 0 l 612 792 l 0 792 l c f')
          call fppsdu()
        end if
        call fppsin('90 rotate -10 -625 translate ')
        write(string,'(f7.4,f7.4,a7)') scal*0.1333,scal*0.1333,' scale '
        call fppsin(string)
        pscal = scal*0.1333

!     Start portrait mode plot

      else
        call fppsin('%Portrait mode ')
        call fppsdu()
        write(string,'(f7.4,f7.4,a7)') scal*0.1,scal*0.1,' scale '
        pscal = scal*0.1
        call fppsin(string)
        if(blk) then
          call fppsdu()
          call fppsin('0 0 0 h n ')
          call fppsin('318 318 m 5800 318 l 5800 4495 l 318 4495 l c f')
        end if
      endif
      call fppsdu()
      call fppsin('1 setlinecap 1 setlinejoin ')

!     Initialize plot state for lines, fills, and colors to false

      lstrk = .false.
      lfill = .false.
      xold  = -9980
      yold  = -9980
      dold  = -1
      lwold = -1
      clin  = 'g0'
      oclin = '  '
      cvar  = ' g0'
      ocvar = ' '
      colv  = 'z '
      ocolv = '  '

      iln(1) = 0
      iln(2) = 1
      call plline( iln )
      call fppsin(' gs n')
      call fppsdu()

!     Format

2000  format(' --> Opening FEAP PostScript file: ',a )

      end subroutine fppsop

      subroutine postname( name )

      implicit   none

      character  name*(*)

      integer       :: n, nc
      logical       :: add

!     Initialize

      n   =  6
      add = .true.

!     Check names

      do while( add .and. n.gt.1 )

!       Get a character from 'name'

        nc = ichar(name(n:n))

!       Check that it is less than a 'Z'

        if(nc.lt.90) then
          name(n:n) =  char(nc+1)
          add       = .false.

!       It is a 'Z' (or something erroneous!).  Do next column

        else
          name(n:n) = 'A'
        endif

        n = n - 1

      end do ! while

!     Too many files exist!

      if(n.le.1) then
        write(*,*) ' *ERROR* - Too many file names'
      endif

      end subroutine postname

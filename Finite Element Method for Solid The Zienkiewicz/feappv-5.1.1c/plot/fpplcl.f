!$Id:$
      subroutine fpplcl()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Close a PostScript file.

!      Inputs:
!         none

!      Outputs:
!         none      - Outputs written to postscript file: feappost.-
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'pdatps.h'
      include  'plflag.h'
      include  'psdat4.h'
      include  'psdat6.h'

      character llx*9,lly*9,urx*9,ury*9

      save

!     Close line with stroke if necessary

      if(lstrk) then
        call fppsin('s')
      endif

!     Add closing information to file

      call fppsdu()
      call fppsin('gr showpage')
      call fppsdu()
      call fppsin('%%Trailer')
      call fppsdu()
      call fppsin('%%EOF')
      call fppsdu()

!     Convert bounding box coordinates to character array

      if(psfram) then
        write(llx,'(i9)') -nint(yur*pscal) - 3 + 625
        write(lly,'(i9)')  nint(xll*pscal) - 3 -  10
        write(urx,'(i9)') -nint(yll*pscal) + 3 + 625
        write(ury,'(i9)')  nint(xur*pscal) + 3 -  10
      els e
        write(llx,'(i9)')  nint(xll*pscal) - 3
        write(lly,'(i9)')  nint(yll*pscal) - 3
        write(urx,'(i9)')  nint(xur*pscal) + 3
        write(ury,'(i9)')  nint(yur*pscal) + 3
      endif

!     Create 'Feap#.eps file

      call feapbb(fname,llx,lly,urx,ury)

      if(ior.lt.0) write(*,2000) fname(1:10)

2000  format(' --> Closing FEAP PostScript file: ',a )

      end subroutine fpplcl

      subroutine feapbb(filer,llx,lly,urx,ury)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Puts bounding box at beginning of file

!      Inputs:

!      Outputs:
!-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'iodata.h'

      character (len=80) :: line
      character (len=50) :: boundbox
      character (len=12) :: filer
      character (len=9)  :: llx,lly,urx,ury

      logical      :: eofile
      integer      :: ii

      save

!     Set up bounding box record

      boundbox( 1:14) ='%%BoundingBox:'
      boundbox(15:23) = llx
      boundbox(24:32) = lly
      boundbox(33:41) = urx
      boundbox(42:50) = ury

!     Open and rewind write file

      open(unit=ios,file=filer,status='unknown')
      rewind(lun)
      rewind(ios)

!     Read records from 'temp.eps' copy to 'Feap#.eps'

      eofile = .true.
      do while (eofile)
        read(lun,'(a)',end=200) line

!       Non bounding box records

        if(line(3:7).ne.'Bound') then
          do ii = 80,1,-1
            if(line(ii:ii).ne.' ') go to 100
          end do
100       write(ios,'(a)') line(1:ii)

!       BoundingBox record

        else
          write(ios,'(a50)') boundbox
        endif
      end do

200   close(lun,status='delete')
      close(ios)

      end subroutine feapbb

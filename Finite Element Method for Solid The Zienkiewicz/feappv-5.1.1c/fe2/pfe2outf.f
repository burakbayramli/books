!$Id:$
      subroutine pfe2outf(tfinp,nfile)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Output FE^2 RVE driver files: Iname.01, etc.

!      Inputs:
!         tfinp     - Name of file "Iname"
!         nfile     - Number of files to output

!      Outputs:
!         Files: Iname.001, etc. to Iname.nfile
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'comfil.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'umac1.h'

      character ufinp*128, tfinp*128, fext*3
      integer   n,nfile

      save

!     Specify number of files to output

      n = index(tfinp,' ') - 1
      write(*,2000) nfile,tfinp(1:n)

      do n = 1,nfile
        ufinp = tfinp
        fext  = '000'
        if(n.lt.10) then
          write(fext(3:3),'(i1)') n
        elseif(n.lt.100) then
          write(fext(2:3),'(i2)') n
        else
          write(fext(1:3),'(i3)') n
        endif
        call addext(ufinp,fext,128,3)
        open(unit=ios,file = ufinp)
        write(ios,2001)
        close(unit=ios,status='keep')
      end do ! n

!     Formats

2000  format(' --> Creating',i3,' RVE files named ',a,'.001 etc.')
2001  format('nocount'/'fe2feap * * RVE for FE-squared micro-scale'//
     &       'include solve_mpi'//'stop')

      end subroutine pfe2outf

!$Id:$
      subroutine pinpfl(filnm,fext, itype, isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose:  Open file for input of saved input data

!     Inputs:
!        filnam   - Character name of calling routine
!        fext     - Extender name of file

!     Outputs:
!        itype     - Type of action: 'set' or 'add'
!        isw      - Switch: 1 = open; 2 = close
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdat2.h'
      include   'comfil.h'
      include   'conval.h'
      include   'dstars.h'
      include   'iodata.h'
      include   'iofile.h'
      include   'ioincl.h'
      include   'print.h'
      include   'setups.h'
      include   'trdata.h'
      include   'pointer.h'
      include   'comblk.h'

      character (len=132) :: fnam
      character (len=8)   :: fext
      character (len=4)   :: itype
      character           :: filnm*(*)

      logical        :: lsave
      integer        :: isw, iosave

      save

!     Open unit for data saves
      if(isw.eq.1) then

        fnam = fsav
        call addext(fnam,fext,128,8)
        call opnfil(fext,fnam,-2,ios,lsave)

        if(lsave) then
          iosave = ior
          ior    = ios

          vvsave(:,:) = vvv(:,:)

          read(ios,1000) itype,fincld(isf),irecrd(isf),prt,prth
          read(ios,1001) vvv

        endif

!     Close file and restore parameters

      else

        close(ior,status = 'delete')
        ior = iosave

        vvv(:,:) = vvsave(:,:)

      endif

!     Formats

1000  format(a4,2x,a12,i8,2l5)
1001  format(1p,4e20.12)

      end subroutine pinpfl

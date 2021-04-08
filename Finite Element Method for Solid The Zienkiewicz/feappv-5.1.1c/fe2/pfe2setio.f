!$Id:$
      subroutine pfe2setio(ni,u,h,lenu,lenh,flgh,iounit,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2008
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: I/O routines for saving displacement & history data.

!      Inputs:
!         ni        - Point number
!         u(lenu)   - Displacments (if isw = 1)
!         h(lenh)   - History data (if isw = 1)
!         lenu      - Number entries in 'u' array
!         lenh      - Number entries in 'h' array
!         flgh      - History data exists if true
!         iounit    - I/O logical unit number
!         isw       - Switch parameter: = 1 for read, = 2 for write

!      Outputs:
!         u(lenu)   - Displacments (if isw = 2)
!         h(lenh)   - History data (if isw = 2)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'debugs.h'
      include   'elpers.h'
      include   'iofile.h'
      include   'rdata.h'
      include   'rdat1.h'
      include   'setups.h'

      logical    flgh
      integer    ni, lenu,lenh, iounit, isw, ri
      real*8     u(lenu),h(lenh)

      save

      if(debug) then
        call udebug(' usetio',isw)
      endif

!     Input displacement and history data

      if(isw.eq.1) then
        read (iounit,end=999) rnmax
        read (iounit,end=999) ri,u
        if(flgh) read (iounit,end=999) h
        if(ri.eq.0) then
          write(iow,*) ' WARNING: READ:',rank,' NI =',ni,' RI =',ri
          write(  *,*) ' WARNING: READ:',rank,' NI =',ni,' RI =',ri
        else
          ni = ri
        endif
        if(debug) then
          call mprint(u,1,lenu/4,1,'U_histin')
          if(flgh) call mprint(h,1,lenh,1,'H_histin')
        endif

!     Output displacement and history data

      elseif(isw.eq.2) then
        write(iounit) rnmax
        write(iounit) ni,u
        if(flgh) write(iounit) h
        call pflush(iounit)
        if(debug) then
          call mprint(u,1,lenu/4,1,'U_histout')
          if(flgh) call mprint(h,1,lenh,1,'H_histout')
        endif
      endif

      return

!     Error message

999   write(iow,*) ' ERROR: READ =',rank,' NI =',ni,' IOUNIT =',iounit,
     &             ' LENU =',lenu
      write(  *,*) ' ERROR: READ =',rank,' NI =',ni,' IOUNIT =',iounit,
     &             ' LENU =',lenu

      end subroutine pfe2setio

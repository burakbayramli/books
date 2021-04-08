!$Id:$
      subroutine dcheck(x,vd,nt,error)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Internal input of values from string data

!      Inputs:
!         x(*)  - Character array from inputs
!         nt    - Length of character string

!      Outputs:
!         vd(*) - Numerical values from string inputs
!         error - True of error occurs during inputs
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'conval.h'
      include  'iofile.h'

      character (len=256) :: y
      character           :: x*(*)

      logical       :: error
      integer       :: nt,n0,nn, i, ivd(2,16)
      real (kind=8) :: vd(*)

      save

!     Expression substitution using predefined constants

      n0 = 15
      nn = 1
      do i = 1,nt
        if(x(i:i).eq.',') nn = nn + 1
      end do ! i
      call acheck(x,y,n0,nt,256)
      call pcharr(y,ivd,n0,nn*n0)
      error = .false.
      read(y,'(17f15.0)',err=200) (vd(i),i=1,nn)
      do i = 1,nn
        if(ivd(1,i).gt.0) then
          vd(i) = vvv(ivd(1,i),ivd(2,i))
        elseif(ivd(1,i).lt.0) then
          vd(i) = www(-ivd(1,i))
        endif
      end do ! i
      return

!     Error

 200  write(iow,3000) nn,y(1:nn*15)
      if(ior.lt.0) then
        write(  *,3000) nn,y(1:nn*15)
      endif
      call errclr('INPUTS')
      error = .true.

3000  format(/' *ERROR* DCHECK: Attempt to input',i3,' value(s).',
     &        '  Input is:'/a)

      end subroutine dcheck

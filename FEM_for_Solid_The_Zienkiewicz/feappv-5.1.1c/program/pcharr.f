!$Id:$
      subroutine pcharr(y,ivd,n0,nt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Identify 'character' variables in the string
!               N.B. Lower case variables have been input,
!                    upper case computed

!      Inputs:
!         y(*)     - String to search
!         n0       - Field width
!         nt       - Number of characters in y-array

!      Outputs:
!         ivd(2,*) - Number of characters found
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: n0,nt,n1,k,i,j,n,ivd(2,*)
      character     :: y*(*)

      save

      n1 = n0 - 1
      k  = 0
      do i = 1,nt,n0
        k = k + 1
        ivd(1,k) = 0
        ivd(2,k) = 0
        n = ichar( y(i:i) ) - 64
        if(n.gt.0) then
          if(n.gt.58) go to 200
          if(n.gt.26) then
            ivd(1,k) = n - 32
            j = ichar(y(i+1:i+1))
            if(j.eq.32) then
              ivd(2,k) = 0
            elseif(j.ge.ichar('a') .and. j.le.ichar('z')) then
              ivd(2,k) = j - ichar('a') + 1
            elseif(j.ge.ichar('0') .and. j.le.ichar('9')) then
              ivd(2,k) = j - ichar('0') + 27
            endif
          else
            ivd(1,k) = - n
          endif
          y(i   :i+n1) = ' '
          y(i+n1:i+n1) = '0'
        endif
      end do
      return

!     Error

 200  call errclr('PCHARR')

      end subroutine pcharr

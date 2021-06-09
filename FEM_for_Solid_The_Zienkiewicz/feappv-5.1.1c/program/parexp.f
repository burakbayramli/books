!$Id:$
      subroutine parexp(x,xs,v,nex,error)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Identify parenthetical expressions and evaluate

!      Inputs:
!         x*(*)    - String containing expression to evaluate

!      Scratch:
!         xs*(*)   - Array used to temporarily store expression
!         v(*)     - Array to hold values

!      Outputs:
!         x*(*)    - Expression replaced by upper case letter
!         nex      - Number of upper case letters used
!         error    - Flag, true if error occurs

!      Common returns:
!         www(*)   - Upper case letters with values assigned
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'conval.h'

      logical       :: error
      character     :: x*(*),xs*(*)
      integer       :: i,j,k,l, i1,i2,nex
      real (kind=8) :: val, v(*)

      save

!     Find parenthetical expressions and remove

      do i = 1,75
        if(x(i:i).eq.'(') then
          i1 = i + 1
          do j = i1,75
            if(x(j:j).eq.'(') then
              call errclr('PAREXP')
              call plstop(.true.)
            elseif(x(j:j).eq.')') then
              do l = 1,j-i+1
                xs(l:l) = ' '
              end do ! l
              i2 = j - 1
              if(i2.lt.i1) then
                call errclr('PAREXP')
                call plstop(.true.)
              else
                k = 0
                do l = i1,i2
                  k = k + 1
                  xs(k:k) = x(l:l)
                  x(l:l)  = ' '
                end do ! l
                x(i2+1:i2+1)  = ' '

!               Evaluate expression in parenthesis

                call evalex(xs,v,val,k,error)
                if(error) return
                nex = nex + 1
                www(nex) = val

!               Put upper case letter in expression and close up remainder

                x(i:i) = char(nex +64)
                i2 = i2 -i1 + 2
                do l = i1,75
                  x(l:l) = ' '
                  if(l+i2.le.75) then
                    x(l:l) = x(l+i2:l+i2)
                  endif
                end do ! l
              endif
              go to 100
            endif
          end do ! j
100       continue
        endif
      end do ! i

      end subroutine parexp

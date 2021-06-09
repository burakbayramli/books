!$Id:$
      subroutine evalex(xs,v,val,ns,error)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Identify expression in character string and evaluate

!      Inputs:
!         xs(*) - Character string of input data
!         v(*)  - Array of real values
!         ns    - Length of character string

!      Outputs:
!         val   - Value of expression
!         error - Error indicator if true
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'corfil.h'
      include  'iofile.h'

      character (len=256) :: x
      character (len=1)   :: y,op(25)
      character           :: xs*(*)

      logical       :: error
      integer       :: i,j,k,ns,num
      real (kind=8) :: val, v(*)

      save

!     Pack expression by removing any ' ' characters

      num = 0
      do i = 1,ns
        if(xs(i:i).ne.' ') then
          num        = num + 1
          x(num:num) = xs(i:i)
        endif
      end do ! i

!     Evaluate an expression: (+) add, (-) subtract, (*) multiply,
!                             (/) divide, (^) power.

      k  = 0
      do i = 1,num
        if(k.eq.0 .and. x(i:i).eq.' ') go to 100
        if((x(i:i).eq.'+') .or. (x(i:i).eq.'-')) then
          if(i.gt.2) then
            y = x(i-1:i-1)
            if(y.eq.'e'.or.y.eq.'d'.or.y.eq.'E'.or.y.eq.'D') then
              if((x(i-2:i-2).ge.'0'.and.x(i-2:i-2).le.'9').or.
     &            x(i-2:i-2).eq.'.') go to 100
            endif
          endif
          k      = k + 1
          op(k)  = x(i:i)
          x(i:i) = ','
        endif
        if((x(i:i).eq.'*').or.(x(i:i).eq.'/').or.(x(i:i).eq.'^')) then
          k      = k + 1
          op(k)  = x(i:i)
          x(i:i) = ','
        endif
100     continue
      end do ! i

      call dcheck(x,v,num,error)

      if(error) return

!     Compute value of expression

      val = v(1)
      if(k.ge.1) then

!      1. Evaluate all exponentiations

        i = 1
110     if(op(i).eq.'^') then
          v(i) = v(i) ** v(i+1)
          k = k - 1
          do j = i,k
            v(j+1) = v(j+2)
            op(j) = op(j+1)
          end do ! j
        endif
        i = i + 1
        if(i.le.k) go to 110

!      2. Evaluate all multiplications and divisions

        i = 1
120     if(op(i).eq.'*' .or. op(i).eq.'/') then
          if(op(i).eq.'*') then
            v(i) = v(i) * v(i+1)
          else
            if(v(i+1).eq.0.0d0) then
              write(iow,3000) let
              if(v(i).ne.0.0d0) then
                v(i) = sign(1.d20,v(i))
              else
                v(i) = 0.0d0
              endif
            else
              v(i) = v(i) / v(i+1)
            endif
          endif
          k = k - 1
          do j = i,k
            v(j+1) = v(j+2)
            op(j) = op(j+1)
          end do ! j
        else
          i = i + 1
        endif
        if(i.le.k) go to 120

!      3. Evaluate all additions and subractions

        val = v(1)
        if(k.gt.0) then
          do i = 1,k
            if(op(i).eq.'+') val = val + v(i+1)
            if(op(i).eq.'-') val = val - v(i+1)
          end do ! i
        endif
      endif

!     Format

3000  format(' *ERROR* Dividing by zero in expression for: ',a)

      end subroutine evalex

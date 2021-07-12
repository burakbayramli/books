c$Id:$
      subroutine evalex(xs,v,val,ns,error)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Identify expression in character string and evaluate

c      Inputs:
c         xs(*) - Character string of input data
c         v(*)  - Array of real values
c         ns    - Length of character string

c      Outputs:
c         val   - Value of expression
c         error - Error indicator if true
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'corfil.h'
      include  'iofile.h'

      logical   error
      character xs*(*),x*255,y*1,op(25)*1
      integer   i,j,k,ns,num
      real*8    val, v(*)

      save

c     Pack expression by removing any ' ' characters

      num = 0
      do i = 1,ns
        if(xs(i:i).ne.' ') then
          num        = num + 1
          x(num:num) = xs(i:i)
        endif
      end do ! i

c     Evaluate an expression: (+) add, (-) subtract, (*) multiply,
c                             (/) divide, (^) power.

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

c     Compute value of expression

      val = v(1)
      if(k.ge.1) then

c      1. Evaluate all exponentiations

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

c      2. Evaluate all multiplications and divisions

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

c      3. Evaluate all additions and subractions

        val = v(1)
        if(k.gt.0) then
          do i = 1,k
            if(op(i).eq.'+') val = val + v(i+1)
            if(op(i).eq.'-') val = val - v(i+1)
          end do ! i
        endif
      endif

c     Format

3000  format(' *ERROR* Dividing by zero in expression for: ',a)

      end

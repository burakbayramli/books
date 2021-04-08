!$Id:$
      subroutine pfuncs(x,v,val,nex,error)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Evaluate expressions with functions in input records for
!               inc,   dec,
!               exp,   sin,   cos,   tan,   abs,   int,   log,   atan,
!               asin,  acos,  sqrt,  cosh,  sinh,  tanh,  cosd,  sind,
!               tand,  atand, asind, acosd

!      Inputs:
!         x(*)   - String of input
!         v(*)

!      Outputs:
!         nex    - Number of www(*) used to hold function value
!         error  - Flag, true if error occurs
!         val    - Expression value
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'conval.h'

      character (len=75) :: yy
      character (len=1)  :: x(*)
      logical       :: pcomp,error
      integer       :: i,ii,j,jj,k,kk,nn,nex
      real (kind=8) :: padd,psub,val,v(*)
      real (kind=8) :: sind, cosd, tand, acosd, asind, atand

      external      :: sind, cosd, tand, acosd, asind, atand

      save

!     Evaluate functions in expressions

      yy(1:75) = ' '
      k = 0
      i = 1
140   continue
        k = k + 1
        if(    pcomp(x(i),'atand',5))then
          kk = 1
        elseif(pcomp(x(i),'asind',5))then
          kk = 2
        elseif(pcomp(x(i),'acosd',5))then
          kk = 3
        elseif(pcomp(x(i),'atan',4))then
          kk = 4
        elseif(pcomp(x(i),'asin',4))then
          kk = 5
        elseif(pcomp(x(i),'acos',4))then
          kk = 6
        elseif(pcomp(x(i),'cosh',4))then
          kk = 7
        elseif(pcomp(x(i),'sinh',4))then
          kk = 8
        elseif(pcomp(x(i),'tanh',4))then
          kk = 9
        elseif(pcomp(x(i),'cosd',4))then
          kk = 10
        elseif(pcomp(x(i),'sind',4))then
          kk = 11
        elseif(pcomp(x(i),'tand',4))then
          kk = 12
        elseif(pcomp(x(i),'sqrt',4))then
          kk = 13
        elseif(pcomp(x(i),'exp',3)) then
          kk = 14
        elseif(pcomp(x(i),'sin',3)) then
          kk = 15
        elseif(pcomp(x(i),'cos',3)) then
          kk = 16
        elseif(pcomp(x(i),'tan',3)) then
          kk = 17
        elseif(pcomp(x(i),'abs',3)) then
          kk = 18
        elseif(pcomp(x(i),'int',3)) then
          kk = 19
        elseif(pcomp(x(i),'log',3)) then
          kk = 20
        elseif(pcomp(x(i),'inc',3)) then
          kk = 21
        elseif(pcomp(x(i),'dec',3)) then
          kk = 22
        else
          kk = 0
        endif

!       Evaluate functions

        if(kk.ne.0 ) then

!         Functions 1 to 3

          if(kk.le.3) then
            j = 5

!         Functions 4 to 13

          elseif(kk.le.13) then
            j = 4

!         Functions 14 to 22

          else
            j = 3

          endif

          nn = ichar(x(i+j)) - 64
          if(nn.gt.26) then
            jj = nn + ichar(x(i+j+1))
            if    (jj.ge.ichar('a') .and. jj.le.ichar('z')) then
              ii = jj - ichar('a') + 1
            elseif(jj.ge.ichar('0') .and. jj.le.ichar('9')) then
              ii = jj - ichar('0') + 27
            else
              ii = 0
            endif

            val = vvv(nn-32,ii)
          else
            val = www(nn)
          endif

          nex = nex + 1
          if(    kk.eq.1) then
            www(nex) = atand(val)
          elseif(kk.eq.2) then
            www(nex) = asind(val)
          elseif(kk.eq.3) then
            www(nex) = acosd(val)
          elseif(kk.eq.4) then
            www(nex) = atan(val)
          elseif(kk.eq.5) then
            www(nex) = asin(val)
          elseif(kk.eq.6) then
            www(nex) = acos(val)
          elseif(kk.eq.7) then
            www(nex) = cosh(val)
          elseif(kk.eq.8) then
            www(nex) = sinh(val)
          elseif(kk.eq.9) then
            www(nex) = tanh(val)
          elseif(kk.eq.10) then
            www(nex) = cosd(val)
          elseif(kk.eq.11) then
            www(nex) = sind(val)
          elseif(kk.eq.12) then
            www(nex) = tand(val)
          elseif(kk.eq.13) then
            www(nex) = sqrt(val)
          elseif(kk.eq.14) then
            www(nex) = exp(val)
          elseif(kk.eq.15) then
            www(nex) = sin(val)
          elseif(kk.eq.16) then
            www(nex) = cos(val)
          elseif(kk.eq.17) then
            www(nex) = tan(val)
          elseif(kk.eq.18) then
            www(nex) = abs(val)
          elseif(kk.eq.19) then
            www(nex) = int(val)
          elseif(kk.eq.20) then
            www(nex) = log(val)
          elseif(kk.eq.21) then
            www(nex) = padd(val)
          elseif(kk.eq.22) then
            www(nex) = psub(val)
          endif
          yy(k:k) = char(nex+64)
          i = i + j
        else
          yy(k:k) = x(i)
        endif
        i = i + 1
      if(i.lt.75) go to 140

!     Final evaluation of expression

      call evalex(yy,v,val,k,error)

      end subroutine pfuncs

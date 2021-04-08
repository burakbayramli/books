!$Id:$
      subroutine pnsides(lside,nsides,prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set nurb sides

!      Data:
!        side i, lside(i), kside(i), nsides(j,i):j=1,lside(i)

!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'bdata.h'
      include   'cnurb.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'nblend.h'

      integer    lside(2,*), nsides(dsideig,*)
      logical    pcomp,errck,tinput,pinput, prt
      character  tx*15
      integer    j, ii,jj,jx, is, isp
      real*8     td(16)

      jx  = 0
      is  = nursd
      isp = is + 1
      tx    = 'start'
      do while (.not.pcomp(tx,'    ',4))
        errck = tinput(tx,1,td,15)
        if(pcomp(tx,'side',4) .or. pcomp(tx,'bnet',4)) then
          is        = nint(td(1))
          nursd     = max(nursd,is)
          lside(1,is) = nint(td(2))
          lside(2,is) = nint(td(3))
          do ii = 1,min(12,lside(1,is))
            nsides(ii,is) = nint(td(ii+3))
          end do ! i
          do jj = 12,lside(1,is)-1,16
            errck = pinput(td,16)
            do ii = 1,min(16,lside(1,is)-jj)
              nsides(ii+jj,is) = nint(td(ii))
            end do ! ii
          end do ! jj
        endif
        jx = max(jx,lside(1,is))

      end do ! while

!     Output nurb side results

      if(prt) then
        write(iow,2000) head,(j,j=1,jx)
        do ii = isp,nursd
          write(iow,2001) ii,lside(2,ii),(nsides(j,ii),j=1,lside(1,ii))
        end do ! ii

      endif

!     Formats

2000  format(/1x,19a4,a3//5x,'S I D E   N e t   N o d e s'//
     &  '     Side  Knot',8(i3,'-Node')/(15x,8(i3,'-Node')))

2001  format(i9,i6,8i8/(15x,8i8))

      end

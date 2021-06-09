!$Id:$
      subroutine  setnelm(knots,lek,ord, nelm)

      implicit    none

      integer     lek,ord,nelm, n
      real*8      knots(*)

      nelm = 0
      do n = ord+1,lek-ord-1
        if(knots(n+1).gt.knots(n)) then
          nelm = nelm + 1
        endif
      end do ! n

      end

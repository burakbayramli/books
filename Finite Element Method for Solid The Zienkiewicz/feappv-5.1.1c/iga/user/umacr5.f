!$Id:$
      subroutine umacr5(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Bezier extraction from NURBS curve.
!                Final form is power matrix form.

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'sdata.h'
      include  'iofile.h'
      include  'umac1.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   pcomp
      character lct*15
      real*8    ctl(3)

      save

!     Set command word

      if(pcomp(uct,'mac5',4)) then      ! Usual    form
        uct = 'bezi'                    ! Specify 'Bezier'
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

        call localbez(ctl,hr(np(43)),hr(np(263)), ndm,hr(np(298)),
     &                mr(np(299)),mr(np(308)),mr(np(309)))

      endif

      end
      subroutine localbez(ctl,x,wt,ndm,knots,nsides,lknot,lside)

      implicit   none

      include   'cnurb.h'
      include  'igdata.h'
      include  'iofile.h'

      integer    ndm, nb
      real*8     ctl(3),x(ndm,*),wt(*),knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*)

      integer    side, knot, ord, lek, len, i,j, nod
      real*8     Pw(4,40), Qw(4,6,40), M(6,6)

      side = max(1,nint(ctl(1)))
      knot = lside(2,side)
      lek  = lknot(1,knot)
      ord  = lknot(2,knot)
      len  = lek - ord - 1

      write(  *,*) ' SIDE =',side,' KNOT =',knot,' ORDER =',ord
      write(  *,*) ' LENK =',lek,' LENCP =',len
      write(iow,*) ' SIDE =',side,' KNOT =',knot,' ORDER =',ord
      write(iow,*) ' LENK =',lek,' LENCP =',len

!     Form Pw array

      do i = 1,len
        nod = nsides(i,side)
        do j = 1,ndm
          Pw(j,i) = x(j,nod)
        end do ! j
        Pw(ndm+1,i) = wt(nod)
      end do ! i

      call DecomposeCurve(len, ord, knots(1,knot), Pw, nb, Qw, ndm)

!     Compute the power matrix form

      call BezierToPowerMatrix(ord, M)

      end

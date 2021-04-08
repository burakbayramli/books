!$Id:$
      subroutine serchl(gtol,id,prsd,pu,d,stol,t,neq, step)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Line search driver program

!      Inputs:
!         gtol      - Line search reference value
!         id(*)     - Equation numbers for each dof
!         prsd      - Pointer to current residual
!         pu        - Pointer to current solution
!         stol      - Line search convergence tolerance
!         t(*)      - Working solution storage
!         neq       - Number of equations

!      Outputs:
!         d(*)      - Line search vector * step
!         step      - Step size for line search
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'endata.h'
      include  'iofile.h'
      include  'hdatam.h'

      include  'p_iterat.h'

      integer       :: j, neq,linmax
      real (kind=8) :: g, gtol,stol,step, sa,sb, ga,gb

      integer   id(*)
      real (kind=8) :: d(*),t(*)

      real (kind=8) :: gamma1

      save

      data      linmax /10/

!     Set history update flag false for line searches only

      hflgu  = .false.
      h3flgu = .false.

!     Line search in direction 'd' and return step size in 'step'

      sa = 1.0d0
      ga = gamma1(id,pu,prsd,d,t,sa)
      sb = 0.0d0
      gb = aengy

!     Find bracket on zero

      if(ga*gb.gt.0.0d0) then
         write(iow,3000)
         if(ior.lt.0) write(*,3000)

!     Perform 'linmax' steps of line-search

      else
        j = 0
10      j = j + 1
        if (j.le.linmax) then

          step = sa - ga*(sa-sb)/(ga-gb)
          g    = gamma1(id,pu,prsd,d,t,step)

!         Output line-search parameters

          write(iow,3001) j,step,g,sa,sb
          if(ior.lt.0) write(*,3001) j,step,g,sa,sb

!         Update postions for next iteration

          gb = 0.5d0*gb
          if (g*ga.lt.0.0d0) then
            sb = sa
            gb = ga
          endif
          sa = step
          ga = g

!         Check convergence

          if(abs(g).gt.stol*abs(gtol)) go to 10
        endif

!       Multiply solution increment by step size computed

        do j = 1,neq
          d(j) = step*d(j)
        end do

!       Update histories

        hflgu  = .true.
        h3flgu = .false.
        g      = gamma1(id,pu,prsd,d,t,1.d0)

      endif

!     Formats

3000  format(' -> No line search - Energy positive at full step.')

3001  format(' -> Iteration',i3,' Step Size =',1p,e12.5,' Energy =',
     &       1p,e12.5,' sa=',f6.3,' sb=',f6.3)

      end subroutine serchl

!$Id:$
      subroutine iterat(ad,jp,pu,prsd,oldrsd,d,t,
     &                  accrcy,v,w,prt,id,nbfgs,stol, etol)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: BFGS algorithm  for solution to equation

!          R(u) = F - P(u) = 0
!          Variable number of vectors
!          Max. no. of vectors = nbfgs ( < = 15 ) (input data)

!     CALL sequence
!      pmacr3.f -> bfgs.f (iterat) -> formfe.f (isw = 6)
!      -> pform.f -> elmlib.f ->  elmtxx.f

!    Description
!      routine will return solution and residual to equation:
!          K(u)*du - b = 0
!      Routine evaluates RHS of equation through routine operat.

!      Inputs:
!       ad(*)       - Factored coefficient array (au follows)
!       jp()*)      - Pointer to end of columns in au.
!       pu          - Pointer for solution vectors
!       prsd        - Pointer for current residual
!       oldrsd(*)   - Old residual
!       prt         - Print if true
!       id(*)       - Equation numbers for each dof
!       stol        - Solution tolerance for line search
!       etol        - Solution tolerance for energy norm

!      Outputs:
!       d(*)        - Incremental solution vector for step
!       v(*)        - BFGS vectors
!       w(*)        - BFGS vectors
!       nbfgs       - Number BFGS steps

!      Scratch:
!       t(*)        - Temporary storage vectors
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'counts.h'
      include  'ddata.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'hdatam.h'
      include  'ndata.h'
      include  'pointer.h'
      include  'rdata.h'
      include  'sdata.h'
      include  'comblk.h'

      include  'p_iterat.h'

      logical       :: accrcy,prt
      integer       :: i,nupd,nbfgs
      integer       :: id(*), jp(*)
      real (kind=8) :: g,g0,s, rnorm,onorm,dnorm,energy,stol,etol

      real (kind=8) :: ad(*),oldrsd(*),d(*),v(*),w(*),t(*)

      real (kind=8) :: dot, gamma1

      save

!     Set Starting values

      iform = 0
      nupd  = 0
      s     = 1.0d0
      g0    = 0.0d0

!     Initialization of residual R(u+s*d) [set to rsd]

      call pzero(oldrsd,neq)
      call pzero(hr(prsd),neq)
      call pzero(     d,neq)

!     Compute residual for du = 0

      g = gamma1(id,pu,prsd,d,t,s)

!     Loop for momentum balance iteration

      do i = 1,nbfgs

        if (prt) then
          rnorm = sqrt ( dot (hr(prsd),hr(prsd),  neq))
          onorm = sqrt ( dot (oldrsd,oldrsd,  neq))
          dnorm = sqrt ( dot (     d,     d,  neq))
          write(iow,2000) i, rnorm, onorm, dnorm
          if (ior.lt.0) write(iow,2000) i, rnorm, onorm, dnorm
        endif

!       Compute search direction (d) by factorized form (u,w)
!                          d : not including time step

        call dfind(ad,jp,d,hr(prsd),oldrsd,nupd,g0,g,s,neq,v,w,nbfgs)

!       Do line search if necessary [new residual in rsd]

        s  = 1.d0
        g  = gamma1(id,pu,prsd,d,t,s)
        g0 = dot(d,oldrsd,neq)
        if(ior.lt.0) then
          write(*,2003) i,g0,g
        endif
        write(iow,2003) i,g0,g

!       Compute step size using scalar line search and update solution

        if(abs(g).gt.stol*abs(g0)) then
          call serchl(g0,id,prsd,pu,d,stol,t,neq, s)

!       Call to update histories

        else
          hflgu  = .true.
          h3flgu = .false.
          g = gamma1(id,pu,prsd,d,t,s)
        endif

        call pupdate(id,hr(np(30)),hr(pu),hr(np(42)),d,fl(9),2)

!       Convergence checks: Norm of residual vector and energy incr.

        rnorm  = sqrt(dot(hr(prsd),hr(prsd),neq))
        energy = abs(g0*s)
        if (rnmax.eq.0.0d0) then
          rnmax  = energy
        endif

        write(iow,2001) rnorm,energy,rnmax
        if(ior.lt.0) write(*,2001) rnorm,energy,rnmax

        accrcy = energy.le.(etol*rnmax)

        if (accrcy) go to 100

      end do

      i = nbfgs

100   write(iow,2002) i,iform
      if(ior.lt.0) write(*,2002) i,iform

!     Formats

2000  format(4x,'Start of iteration no. ',i5/
     &       6x,'|Residual|  |Old Residual|   |Search Vect|'/
     &       3d16.5)

2001  format('   | Res | =',g13.5,' | Energy |=',g13.5,
     &         ' | Max Engy |=',g13.5)

2002  format(4x,i3,' BFGS iterations with',i3,' RHS forms')

2003  format(4x,'Iteration',i3,': G0 =',1p,1e15.7,', G =',1p,1e15.7)

      end subroutine iterat

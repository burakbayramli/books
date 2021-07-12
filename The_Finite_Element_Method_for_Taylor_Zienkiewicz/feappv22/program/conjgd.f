c$Id:$
      subroutine conjgd(ad,ac,adr,x,jc,ir,r,z,p,b,
     &                  neq,jmax,tol,rn,rn0)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Preconditioned conjugate gradient with preconditioning

c         See:  R.M. Ferencz,
c                   "EBE Preconditioning Techniques for Large
c                    Scale, Vectorized Finite Element Analysis
c                    in Nonlinear Solid and Structural Mechanics,"
c                    Ph.D. Dissertation, Stanford, March 1989.
c      Inputs:
c         ad(*)    - Diagonal entries of coefficient matrix
c         ac(*)    - Compressed part of symmetric coefficient matrix
c         x(*)     - Residual
c         jc(*)    - Pointer array to locate column/rows in ir array
c         ir(*)    - Location of non-zero terms in symmetric array.
c         neq      - Number of equations
c         jmax     - Maximum number of iterations
c         tol      - Solution tolerance
c         rn0      - Initial residual

c      Outputs:
c         x(*)     - Solution
c         rn       - Final residual

c      Scratch:
c         adr(*)   - Storage for reciprocal diagonals
c         r(*)     - Scratch arrays
c         z(*)     - Scratch arrays
c         p(*)     - Scratch arrays
c         b(*)     - Scratch arrays
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'sdata.h'

      logical   noconv
      integer   neq,ni,nj,jmax
      real*4    tt, etime, tary(2)
      real*8    alp,bet,gam,rn,rn0,tol,tolr0,dot

      integer   jc(*),ir(*)
      real*8    ad(neq),ac(*),adr(neq),x(*)
      real*8    r(*),z(*),p(*),b(*)

      save

c     Initialize

      do ni = 1,neq
        r(ni) = x(ni)
        b(ni) = x(ni)
        x(ni) = 0.0d0
      end do

      if(rn0.eq.0.0d0) rn0 = dot(r,r,neq)
      tolr0 = rn0*(1.d5*tol)**2

c     Set-up initial solution vectors 'z' and 'p'

c     Diagonal preconditioning


      do ni = 1,neq
        if(ad(ni).ne.0.0d0) then
          adr(ni) = 1.d0/ad(ni)
        else
          adr(ni) = 0.0d0
        end if
      end do

      do ni = 1,neq
        z(ni) = adr(ni)*r(ni)
        p(ni) = z(ni)
      end do

      gam    = dot(r,z,neq)
      noconv = .true.
      nj     = 1

100   if(noconv) then

c       Form A*p product for current tangent

        call caprod(ad,ac,p,z,jc,ir,neq)

c       Compute update vector

        alp = dot(p,z,neq)
        alp = gam/alp

        do ni = 1,neq
          x(ni) = x(ni) + alp*p(ni)
        end do

        if(mod(nj,50).eq.0) then
          call caprod(ad,ac,x,z,jc,ir,neq)
          do ni = 1,neq
            r(ni) = b(ni) - z(ni)
          end do
        else
          do ni = 1,neq
            r(ni) = r(ni) - alp*z(ni)
          end do
        endif

c       Check convergence

        rn     = dot(r,r,neq)
        if(mod(nj,20).eq.0) then
          tt = etime(tary)
          if(ior.lt.0) then
            write(*,2000) nj,sqrt(rn),tary
          endif
          write(iow,2000) nj,sqrt(rn),tary
        endif
        noconv = (rn.gt.tolr0) .and. ( nj.lt.jmax)
        nj     = nj + 1

c       Diagonal preconditioning

        do ni = 1,neq
          z(ni) = adr(ni)*r(ni)
        end do

c       Compute final update vector for step

        bet = gam
        gam = dot(r,z,neq)
        bet = gam/bet
        do ni = 1,neq
          p(ni) = z(ni) + bet*p(ni)
        end do

        go to 100
      endif

c     Output solution data

      if(mod(nj-1,20).ne.0) then
        tt = etime(tary)
        if(ior.lt.0) write(  *,2000) nj-1,sqrt(rn),tary
      endif
      write(iow,2000) nj-1,sqrt(rn),tary

c     Format

 2000 format('   CG: NJ =',i5,' Resid. =',1p,e10.3,
     &         ' Cpu. = ',1p,e10.3,' Sys. = ',1p,e10.3)
      end

c$Id:$
      subroutine dfind(ad,jp,d,ri,r0,nupd,g0,g,s,neq,v,w,nbfgs)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute BFGS update vectors and search direction

c      Inputs:
c         ad(*)     - Diagonals and upper part of factored A_j
c         jp(*)     - Pointer array to locate ends of columns of A_j
c         ri(*)     - Current residual
c         r0(*)     - Old residual
c         g0        - Line-search initial value
c         g         - Line-search final value
c         s         - Line-search step size
c         neq       - Number of equations

c      Outputs:
c         d(*)      - Solution increment vector
c         v         - BFGS update vectors
c         w         - BFGS update vectors
c         nbfgs     - Number BFGS steps

c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      logical   up
      integer   i,j,ii,nupd,neq,nbfgs
      real*8    s,g0,g,condmx,delgam,dlkdl,coef,fact,stcond, engy

      integer   jp(*)
      real*8    ad(*),d(*),ri(*),r0(*),v(*),w(*)

      real*8    dot

      save

      data      condmx /1.0d5/

c     Find a new search direction using bfsg method in factored form

c     delgam = delta-(i) : gamma-(i)
c     dlkdl  = delta-(i) : K-(i-1) : delta-(i)

      delgam = s*(g0-g)
      dlkdl  = s*s*g0
      up     = delgam.gt.0.0d0 .and. dlkdl.gt.0.0d0

c     If G(0) > G(s) & G(0) > 0

      if (up) then

        stcond=sqrt(delgam/dlkdl)

        if(ior.lt.0) write(*,2000) stcond

c       Compute updating vectors v, w and put residual into d

        fact = s/delgam
        coef = 1.d0 + s*stcond
        do i = 1,neq
          v(i)  = ri(i) - coef*r0(i)
          w(i)  = d(i)*fact
          d(i)  = ri(i)
          r0(i) = ri(i)
        end do

c       Check estimate on condition number

        up = up.and.stcond.lt.condmx
        if ( up ) then

c         Save updating factors

          call store(v,w,nupd+1,1)

c         Compute search direction: d

          coef=fact*g
          do i=1,neq
            d(i) = d(i) + coef*v(i)
          end do
        endif
      else
        do i = 1,neq
          d(i)  = ri(i)
          r0(i) = ri(i)
        end do
      endif

c     Right half of update

      do i=1,nupd
        ii = nupd - i + 1
        call store(v,w,ii,2)
        coef = dot(w,d,neq)
        do j = 1,neq
          d(j)=d(j)+coef*v(j)
        end do
      end do

c     Forward + backward substitution

      call dasol(ad(neq+1),ad(neq+1),ad,d,jp,neq,neq,engy)

      if(up) nupd=nupd+1

c     Left half of updating

      do i = 1,nupd
        if(i.gt.1) call store(v,w,i,2)
        coef = dot(v,d,neq)
        do j = 1,neq
          d(j) = d(j) + coef*w(j)
        end do
      end do

      nupd = mod(nupd,nbfgs)

c     Format

2000  format(' ---> Stiffness condition no. ',e12.5)

      end

!$Id:$
      subroutine dfind(ad,jp,d,ri,r0,nupd,g0,g,s,neq,v,w,nbfgs)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute BFGS update vectors and search direction

!      Inputs:
!         ad(*)     - Diagonals and upper part of factored A_j
!         jp(*)     - Pointer array to locate ends of columns of A_j
!         ri(*)     - Current residual
!         r0(*)     - Old residual
!         g0        - Line-search initial value
!         g         - Line-search final value
!         s         - Line-search step size
!         neq       - Number of equations

!      Outputs:
!         d(*)      - Solution increment vector
!         v         - BFGS update vectors
!         w         - BFGS update vectors
!         nbfgs     - Number BFGS steps

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      logical       :: up
      integer       :: i,j,ii,nupd,neq,nbfgs
      real (kind=8) :: s,g0,g,condmx,delgam,dlkdl,coef,fact,stcond, engy

      integer       :: jp(*)
      real (kind=8) :: ad(*),d(*),ri(*),r0(*),v(*),w(*)

      real (kind=8) :: dot

      save

      data      condmx /1.0d5/

!     Find a new search direction using bfsg method in factored form

!     delgam = delta-(i) : gamma-(i)
!     dlkdl  = delta-(i) : K-(i-1) : delta-(i)

      delgam = s*(g0-g)
      dlkdl  = s*s*g0
      up     = delgam.gt.0.0d0 .and. dlkdl.gt.0.0d0

!     If G(0) > G(s) & G(0) > 0

      if (up) then

        stcond=sqrt(delgam/dlkdl)

        if(ior.lt.0) write(*,2000) stcond

!       Compute updating vectors v, w and put residual into d

        fact = s/delgam
        coef = 1.d0 + s*stcond
        do i = 1,neq
          v(i)  = ri(i) - coef*r0(i)
          w(i)  = d(i)*fact
          d(i)  = ri(i)
          r0(i) = ri(i)
        end do

!       Check estimate on condition number

        up = up.and.stcond.lt.condmx
        if ( up ) then

!         Save updating factors

          call store(v,w,nupd+1,1)

!         Compute search direction: d

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

!     Right half of update

      do i=1,nupd
        ii = nupd - i + 1
        call store(v,w,ii,2)
        coef = dot(w,d,neq)
        do j = 1,neq
          d(j)=d(j)+coef*v(j)
        end do
      end do

!     Forward + backward substitution

      call dasol(ad(neq+1),ad(neq+1),ad,d,jp,neq,neq,engy)

      if(up) nupd=nupd+1

!     Left half of updating

      do i = 1,nupd
        if(i.gt.1) call store(v,w,i,2)
        coef = dot(v,d,neq)
        do j = 1,neq
          d(j) = d(j) + coef*w(j)
        end do
      end do

      nupd = mod(nupd,nbfgs)

!     Format

2000  format(' ---> Stiffness condition no. ',e12.5)

      end subroutine dfind

!$Id:$
      subroutine psolve(b,fp,factor,solve,cfr, prnt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Driver for equation solvers

!     Inputs:
!        b(*)   - Right hand side
!        fp(*)  - Pointers for array locations
!        factor - Factor matrix if true
!        solve  - Solve equations if true
!        cfr    - Equation system unsymmetric if true
!        prnt   - Print diagnostics if true

!     Outputs:
!        Users responsible for placing solutions in correct location
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'compas.h'
      include   'endata.h'
      include   'eqsym.h'
      include   'iofile.h'
      include   'ndata.h'
      include   'p_int.h'
      include   'pathn.h'
      include   'rdata.h'
      include   'rdat0.h'
      include   'rdat1.h'
      include   'setups.h'

      include   'pointer.h'
      include   'comblk.h'

      logical       :: factor,solve,cfr,prnt, setvar,palloc,flags(5)
      integer       :: naur,njpr, i
      real (kind=4) :: etime, tt,tary(2)
      real (kind=8) :: rn, rsdi, b(*)

      if(solver) then

!       Factor tangent matrix

        if(factor) then
          if(ittyp.le.-1) then
            tt    = etime(tary)
            tdiff = tary(1)

            call datri(hr(fp(3)),hr(fp(2)),hr(fp(1)),mr(fp(4)),neqs,neq)

            tt    = etime(tary)
            tdiff = tary(1) - tdiff  ! save for timing solutions
            if(prnt) then
              write(iow,2001) tary
              if(ior.lt.0) then
                write(*,2001) tary
              endif
            endif
          endif
        endif

!       Solve equations

        if(solve) then

!         Diagonal solution

          if(ittyp.eq.0) then

            aengy = 0.0d0
            do i = 0,neq-1
              if(hr(fp(1)+i) .ne. 0.0d0) then
                rsdi   = b(i+1)
                b(i+1) = b(i+1)/hr(fp(1)+i)
                aengy  = aengy + rsdi*b(i+1)
              endif
            end do ! i

!         Direct solution

          elseif(ittyp.le.-1) then

            call dasol (hr(fp(3)),hr(fp(2)),hr(fp(1)),b,
     &                  mr(fp(4)),neqs,neq,aengy)

!         Conjugate gradient solver

          else
            if(rnmax.eq.0.0d0) then
              rn0 = rnorm*rnorm
            endif
            njpr = 1
            naur = 1

            setvar = palloc(111,'TEMP1',5*neq,2) ! allocate temp storage
            fp(6) = np(111)
            do i = 7,10
              fp(i) = fp(i-1) + neq
            end do

            if(prnt .and. ior.lt.0) write(*,*) 'START CG: SOLVER'

            call conjgd(hr(fp(1)),hr(fp(2)),hr(fp(6)),b,
     &                  mr(np(93)),mr(np(94)),hr(fp(7)),hr(fp(8)),
     &                  hr(fp(9)),hr(fp(10)),neq,neq,tol,rn,rn0)

            setvar = palloc(111,'TEMP1', 0, 2) ! destroy temp storage

            aengy = sqrt(rn)
            rnmax = max(rnmax,sqrt(rn0))
            if(prnt) then
              tt    = etime(tary)
              write(iow,2002) tary
              if(ior.lt.0) then
                write(*,2002) tary
              endif
            endif

          endif
        endif

!     User supplied solver routine

      else
        flags(1) = .false.
        flags(2) =  factor
        flags(3) =  cfr
        flags(4) =  solve
        flags(5) = .false.
        call usolve(flags, hr(fp(4)))
      endif
!     Formats

2001  format('   End Triangular Decomposition',28x,'t=',0p,2f9.2)
2002  format('   End Conjugate Gradient Solution',25x,'t=',0p,2f9.2)

      end subroutine psolve

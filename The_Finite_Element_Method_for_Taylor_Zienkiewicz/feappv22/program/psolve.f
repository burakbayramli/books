c$Id:$
      subroutine psolve(fp,factor,solve,cfr, prnt)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c     Purpose: Driver for equation solvers

c     Inputs:
c        fp(*)  - Pointers for array locations
c        factor - Factor matrix if true
c        solve  - Solve equations if true
c        cfr    - Equation system unsymmetric if true
c        prnt   - Print diagnostics if true

c     Outputs:
c        Users responsible for placing solutions in correct location
c-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'compas.h'
      include   'endata.h'
      include   'eqsym.h'
      include   'iofile.h'
      include   'ndata.h'
      include   'pathn.h'
      include   'rdata.h'
      include   'rdat0.h'
      include   'rdat1.h'
      include   'setups.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    factor,solve,cfr,prnt, setvar,palloc,flags(5)
      integer    naur,njpr, i
      integer    fp(5),tp(5)
      real*4     etime, tt,tary(2)
      real*8     rn

      if(solver) then

c       Factor tangent matrix

        if(factor) then
          if(ittyp.le.-1) then
            tt    = etime(tary)
            tdiff = tary(1)

            call datri(hr(fp(3)),hr(fp(2)),hr(fp(1)),mr(fp(5)),neqs,neq)

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

c       Solve equations

        if(solve) then

c         Direct solution

          if(ittyp.le.-1) then

            call dasol (hr(fp(3)),hr(fp(2)),hr(fp(1)),hr(fp(4)),
     &                  mr(fp(5)),neqs,neq,aengy)

c         Conjugate gradient solver

          else
            if(rnmax.eq.0.0d0) then
              rn0 = rnorm*rnorm
            endif
            njpr = 1
            naur = 1

            setvar = palloc(81,'TEMP1',5*neq,2) ! allocate temp storage
            tp(1) = np(81)
            do i = 2,5
              tp(i) = tp(i-1) + neq
            end do

            if(prnt .and. ior.lt.0) write(*,*) 'START CG: SOLVER'

            call conjgd(hr(fp(1)),hr(fp(2)),hr(tp(1)),hr(fp(4)),
     &                  mr(np(2)),mr(np(3)),hr(tp(2)),hr(tp(3)),
     &                  hr(tp(4)),hr(tp(5)),neq,neq,tol,rn,rn0)

            setvar = palloc(81,'TEMP1', 0, 2) ! destroy temp storage

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

c     User supplied solver routine

      else
        flags(1) = .false.
        flags(2) =  factor
        flags(3) =  cfr
        flags(4) =  solve
        flags(5) = .false.
        call usolve(flags, hr(fp(4)))
      endif
c     Formats

2001  format('   End Triangular Decomposition',28x,'t=',0p,2f9.2)
2002  format('   End Conjugate Gradient Solution',25x,'t=',0p,2f9.2)

      end

!$Id:$
      subroutine seteq(eq,prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set equation numbers for solution

!      Inputs:
!        eq(ndf,*) - Boundary condition indicators
!        prt       - Flag, output results if true

!      Outputs:
!        eq(ndf,*) - Equation numbers for each dof.  Active dof
!                    have positive numbers for equation, fixed
!                    dof have negative numbers
!        neq       - Number of active equations in problem
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'mxsiz.h'
      include  'cdata.h'
      include  'lmdata.h'
      include  'sdata.h'
      include  'p_point.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: prt, setlagf, setvar, palloc
      integer       :: nad,n,nn, i, j
      integer       :: eq(ndf,numnp)

      save

!     Set equation numbers
      neq  = 0
      nad  = 0
      do n = 0,numnp-1
        nn = mr(np(89)+n)  ! Renumber list
        do i = 1,ndf
          j = eq(i,nn)
          if(j.eq.0) then
            neq     = neq + 1
            eq(i,nn) = neq
          else
            nad     = nad - 1
            eq(i,nn) = nad
          endif
        end do
      end do

!     Link nodes from data
      if(lkflg) then
        call plink(eq,hr(np(43)),ndm,ndf,numnp,neq,prt)
      endif

!     Add nodal Lagrange multiplier equations
      if(lagrfl) then
        setvar = palloc(120,'TEMP0',numnp*3,1)
        point  = np(31) + ndf*numnp               ! B.C.
        call nodlnum(mr(np(120)),eq,mr(point),mr(np(33)),
     &               mr(np(89)),ndfl(1))
        setvar = palloc(120,'TEMP0',    0,1)
      endif

!     Add element Lagrange multiplier equations
      if(setlagf(mr(np(33)),mr(np(32)))) then

!       Allocate storage for multipliers
        if(ndl.eq.0) call setndl(mr(np(33)),mr(np(32)))
        if(np(210).eq.0) then
          setvar = palloc(210,'LAGBC',ndl*numel*2,1)
        endif
        if(np(212).eq.0) then
          setvar = palloc(212,'LAGRN',numnp,1)
        else
          call pzeroi(mr(np(212)),numnp)
        endif
        if(np(213).eq.0) then
          setvar = palloc(213,'ULAGR',3*ndl*numel,2)
        endif

!       Determine number of multipliers for each element
        call setlagm(mr(np(210)),mr(np(33)), mr(np(32)))

!       Adjust equation numbers
        setvar = palloc(120,'TEMP0',numel,1)
        call newlnum(mr(np(210)),mr(np(212)),mr(np(120)),eq,
     &               mr(np( 33)),mr(np( 32)),mr(np(89)))
        setvar = palloc(120,'TEMP0',    0,1)

      endif

      end subroutine seteq

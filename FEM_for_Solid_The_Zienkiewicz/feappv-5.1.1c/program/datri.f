!$Id:$
      subroutine datri(al,au,ad,jp, neqs, neqt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Triangular decomposition of matrix stored in profile or
!               sparse form.
!               N.B. Sparse form for symmetric only.

!         Equations '   1  ' to 'neqs' are symmetric.
!         Equations 'neqs+1' to 'neqt' are unsymmetric.

!         Use:
!          a.) All equations are unsymmetric       : neqs = 1 (or 0)
!              N.B.  The top 1 x 1 submatrix is always symmetric.
!                    Both 'al' and 'au' must be provided.

!          b.) All equations are symmetric         : neqs = neqt
!              N.B.  In this case array 'al' is not used.

!          c.) First 'neqs' equations are symmetric: 1 < neqs < neqt
!              N.B.  Storage of 'al' for unsymmetric equations only.

!      Inputs:
!         al(*)  - Unfactored lower triangular terms
!         au(*)  - Unfactored upper triangular terms
!         ad(*)  - Unfactored diagonal terms
!         jp(*)  - Pointer to row/column ends in 'al' and 'au'.
!         neqs   - Number of symmetric equations
!         neqt   - Number of equations in A.

!      Outputs:
!         al(*)  - Factored lower triangular terms
!         au(*)  - Factored upper triangular terms
!         ad(*)  - Factored diagonal terms
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'counts.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'print.h'

      integer       :: neqs, neqt, neq , n0, n1, n2, n3
      integer       :: i,id,ie,ih,is,idh,ifig, j,jd,je,jh,jr,jrh
      real (kind=8) :: zero, one, tol, dd, daval, dfig, dimx, dimn

      integer       :: jp(*)
      real (kind=4) :: tary(2)
      real (kind=8) :: al(*),au(*),ad(*)

      real (kind=4) :: etime, tt
      real (kind=8) :: dot, dsred, dured

      save

!     N.B.  tol should be set to approximate half-word precision.

      data      zero,one/0.0d0,1.0d0/
      data      tol/0.5d-07/

!     Set error counters and initial values for conditioning check

      n0   = 0
      n1   = 0
      n2   = 0
      n3   = 0
      dfig = one
      dimx = zero
      dimn = zero
      do j = 1,neqt
        dimn = max(dimn,abs(ad(j)))
      end do

!     Loop through columns to perform triangular decomposition

      neq = max(1,neqs)

!     Reduce symmetric equations

      jd  = 1
      do j = 1,neq
        jr = jd + 1
        jd = jp(j)
        jh = jd - jr
        if(jh.gt.0) then
          is = j - jh
          ie = j - 1

!         if diagonal is zero compute a norm for singularity test

          if(ad(j).eq.zero) call datest(au(jr),jh,daval)

          do i = is,min(ie,neq)
            jr = jr + 1
            id = jp(i)
            ih = min(id-jp(i-1),i-is+1)
            if(ih.gt.0) then
              jrh = jr - ih
              idh = id - ih + 1
              au(jr) = au(jr) - dot( au(jrh), au(idh), ih )
            endif
          end do
        endif

!       Reduce diagonal

        dd = ad(j)
        if(jh.ge.0) then
          jr    = jd - jh
          jrh   = j - jh - 1
          ad(j) = ad(j) - dsred(au(jr),ad(jrh),jh+1)

!         Count errors

          if(abs(ad(j)).lt.tol*abs(dd))    n0 = n0 + 1
          if(dd.lt.zero.and.ad(j).gt.zero) n1 = n1 + 1
          if(dd.gt.zero.and.ad(j).lt.zero) n1 = n1 + 1
          if(ad(j) .eq.  zero)             n2 = n2 + 1

!         Complete rank test for a zero diagonal case

          if(dd.eq.zero.and.jh.gt.0) then
            if(abs(ad(j)).lt.tol*daval)    n3 = n3 + 1
          endif
        endif

!       Store reciprocal of diagonal

        if(ad(j).ne.zero) then
          dimx  = max(dimx,abs(ad(j)))
          dimn  = min(dimn,abs(ad(j)))
          dfig  = max(dfig,abs(dd/ad(j)))
          ad(j) = one/ad(j)
        endif

!       Output interactive equation processing

        if(ior.lt.0 .and. mod(j,1000).eq.0 .and. prnt) then
          tt = etime(tary)
          write(*,3000) j,tary
        endif
      end do

!     Reduce unsymmetric equations

      je  = jp(neq)
      jd  = je
      do j = neq+1,neqt
        jr = jd + 1
        jd = jp(j)
        jh = jd - jr
        if(jh.gt.0) then
          is = j - jh
          ie = j - 1

!         If diagonal is zero compute a norm for singularity test

          if(ad(j).eq.zero) call datest(au(jr),jh,daval)

          do i = is,min(ie,neq)
            jr = jr + 1
            id = jp(i)
            ih = min(id-jp(i-1),i-is+1)
            if(ih.gt.0) then
              jrh = jr - ih
              idh = id - ih + 1
              au(jr)    = au(jr   ) - dot( au(jrh   ), au(idh), ih )
              al(jr-je) = al(jr-je) - dot( al(jrh-je), au(idh), ih )
            endif
          end do

          do i = max(neq+1,is),ie
            jr = jr + 1
            id = jp(i)
            ih = min(id-jp(i-1),i-is+1)
            if(ih.gt.0) then
              jrh = jr - ih
              idh = id - ih + 1
              au(jr)    = au(jr)    - dot( au(jrh), al(idh-je), ih )
              al(jr-je) = al(jr-je) - dot( al(jrh-je), au(idh), ih )
            endif
          end do
        endif

!       Reduce diagonal

        dd = ad(j)
        if(jh.ge.0) then
          jr    = jd - jh
          jrh   = j - jh - 1
          ad(j) = ad(j) - dured(al(jr-je),au(jr),ad(jrh),jh+1)

!         Count errors

          if(abs(ad(j)).lt.tol*abs(dd))    n0 = n0 + 1
          if(dd.lt.zero.and.ad(j).gt.zero) n1 = n1 + 1
          if(dd.gt.zero.and.ad(j).lt.zero) n1 = n1 + 1
          if(ad(j) .eq.  zero)             n2 = n2 + 1

!         Complete rank test for a zero diagonal case

          if(dd.eq.zero.and.jh.gt.0) then
            if(abs(ad(j)).lt.tol*daval)    n3 = n3 + 1
          endif
        endif

!       Store reciprocal of diagonal

        if(ad(j).ne.zero) then
          dimx  = max(dimx,abs(ad(j)))
          dimn  = min(dimn,abs(ad(j)))
          dfig  = max(dfig,abs(dd/ad(j)))
          ad(j) = one/ad(j)
        endif

!       Output interactive equation processing

        if(ior.lt.0 .and. mod(j,1000).eq.0 .and. prnt) then
          tt = etime(tary)
          write(*,3000) j,tary
        endif
      end do

!     Print error summary and conditioning information

      dd = zero
      if(dimn.ne.zero) dd = dimx/dimn
      ifig = nint(dlog10(dfig) + 0.6d0)
      if(prnt) then
        if(n0.gt.0) write(iow,2000) n0,nstep,niter
        if(n1.gt.0) write(iow,2001) n1,nstep,niter
        if(n2.gt.0) write(iow,2002) n2,nstep,niter
        if(n3.gt.0) write(iow,2003) n3,nstep,niter
        if(pfr) write(iow,2004) dimx,dimn,dd,ifig
        if(ior.lt.0) then
          if(n0.gt.0) write(*,2000) n0,nstep,niter
          if(n1.gt.0) write(*,2001) n1,nstep,niter
          if(n2.gt.0) write(*,2002) n2,nstep,niter
          if(n3.gt.0) write(*,2003) n3,nstep,niter
          if(pfr) write(*,2004) dimx,dimn,dd,ifig
        endif
      endif

!     Formats

2000  format(' *WARNING* Lost at least 7 digits in',
     &       ' reducing diagonals of',i5,' equations.'/
     &       ' Step:',i7,' Iteration:',i6)

2001  format(' *WARNING* sign of diagonal changed when reducing',
     &    i5,' equations.'/' Step:',i7,' Iteration:',i6)

2002  format(' *WARNING* reduced diagonal is zero in',
     &    i5,' equations.'/' Step:',i7,' Iteration:',i6)

2003  format(' *WARNING* rank failure for zero unreduced diagonal in',
     &    i5,' equations.'/' Step:',i7,' Iteration:',i6)

2004  format(' Condition check: D-max',e11.4,'; D-min',e11.4,
     & '; Ratio',e11.4/' Maximum no. diagonal digits lost:',i3)

3000  format(' ---> Equation = ',i5,' , Time: CPU = ',f12.2,
     &       ' , System = ',f12.2)

      end subroutine datri

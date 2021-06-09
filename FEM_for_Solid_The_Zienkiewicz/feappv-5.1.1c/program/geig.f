!$Id:$
      subroutine geig(g,h,d,p,t,nv,nvs,prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Solve general eigenproblem 'g*p = h*p*d'

!      Inputs:
!         g(*)   - Left hand projected array
!         h(*)   - Right hand projected array
!         nv     - Size of problem
!         nvs    - Eigenvectors h orthogonal if positive
!         prt    - Output computations if true

!      Outputs:
!         d(*)   - Eigenvalues of problem
!         p(*,*) - Eigenvectors of problem
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      logical       :: prt
      integer       :: nv,nvs,ir, i, j

      real (kind=8) :: g(*),h(*),d(*),p(nv,*),t(*)

      save

!     Output projected matrices

      if(prt) then
        call wprojm(g,nv,'Projected G')
        call wprojm(h,nv,'Projected H')
      endif

!     Compute standard eigenvalue problem matrix 'c'

      call chlfwd(h,g,p,nv)

!     Perform eignfunction decomposition of 'c'

      call eisql(g,d,t,p,nv,ir)

!     Compute vectors of original problem

      call chlbac(h,p,nv)

      if(prt) call mprint(p,nv,nv,nv,'vectors  p')

!     Divide eigenvectors by eigenvalue to prevent overflows

      ir = 0
      do j = 1,nv
        ir = ir + j
        if(nvs.gt.0 .and. d(j).ne.0.0d0) then
          t(1) = 1.0d0/d(j)
        elseif(nvs.le.0 .and. h(ir).ne.0.0d0) then
          if(d(j).ne.0.0d0) then
            t(1) = abs(d(j))/sqrt(abs(h(ir)))
          else
            t(1) = 1.0d0/sqrt(abs(h(ir)))
          endif
        else
          t(1) = 1.0d0
        endif
        if(p(j,j).lt.-0.00001d0) t(1) = -t(1)
        do i = 1,nv
          p(i,j) = p(i,j)*t(1)
        end do
      end do

      end subroutine geig

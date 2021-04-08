!$Id:$
      subroutine subsp(a,b,v,t,g,h,d,dp,dtol,p,nf,nv,neq,imas
     &                 ,shift,tol,prt,its)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Subspace iteration to extract lowest nf eigenpairs
!               Solves:  (A - shift*B)*V = B*V*d for V and d

!      Inputs:
!         a(*)      - Coefficient matrix (tangent stiffness)
!         b(*)      - Coefficient matrix (mass or geometric stiffness)
!         nf        - Number of pairs to converge
!         nv        - Size of subspace problem > or = nf
!         neq       - Size of A,B
!         imas      - Switch: =1 for consistent B; =2 for diagonal B
!         shift     - Value of shift
!         tol       - Tolerance to converge pairs
!         prt       - Flag, output iteration arrays if true
!         its       - Maximum number of subspace iterations

!      Scratch:
!         t(neq)    - Working vector
!         g(*)      - Projection of A - shift*B matrix
!         h(*)      - Projection of B           matrix
!         dp(*)     - Previous iteration values
!         dtol(*)   - Tolerance of eigenvalue iterations
!         p(nv,*)   - Eigenvectors of G*P = H*P*d

!      Outputs:
!         v(neq,*)  - Eigenvectors
!         d(*)      - Eigenvalues
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'compas.h'
      include  'evdata.h'
      include  'gltran.h'
      include  'iofile.h'

      logical       :: conv,prt,soltyp
      integer       :: i,j,k,n
      integer       :: nf,nv,neq,imas,its, itlim, it,itt, num,nmas
      real (kind=4) :: etime, tary(2)
      real (kind=8) :: shift,tol, dm,dr

      integer       :: pdf(1)
      real (kind=8) :: a(*),b(*),v(neq,*),t(neq)
      real (kind=8) :: g(*),h(*),d(*),dp(*),dtol(*),p(nv,*),dpp(4)

      save

      data      pdf / 1 /

!     Compute initial iteration vectors

      call pzero(v,nv*neq)
      num    = 0
      nmas   = 0
      soltyp = ittyp.eq.-1
      do n = 1,neq

!       Count number of values less than current shift

        if(soltyp .and. a(n).lt.0.0d0) num = num + 1

!       Count number of nonzero masses

        if(b(n).ne.0.0d0) nmas = nmas + 1

      end do

      if(soltyp) then
        write(iow,2002) num,shift
        if(ior.lt.0) write(*,2002) num,shift
      endif
      nmas = nmas/nv
      i = 0
      j = 1
      do n = 1,neq
        dm = b(n)
        if(dm.ne.0.0d0) then
          v(n,j) = dm
          i      = i + 1
          if(mod(i,nmas).eq.0) j = min(j+1,nv)
        endif
      end do

      do i = 1,nv
        dp(i)   = 0.0
        dtol(i) = 1.0d0
        call scalev(v(1,i),pdf,1,1,neq)
      end do

!     Compute new vectors and project 'a' onto 'g'

      conv = .false.
      itlim = its
      if(nv.eq.nf) itlim = 1
      do it = 1,itlim
        itt = it

!       Project 'b' matrix to form 'h' and compute 'z' vectors

        call sprojb(b,v,t,h,neq,nv,imas)

!       Project 'a' matrix to form 'g'

        call sproja(v,t,g,neq,nv)

!       Solve reduced eigenproblem

        if(imtyp.eq.1) then

!         Eigenproblem: 'g*p = h*p*d'; e.g., vibration modes

          call geig(g,h,d,p,t,nv,1,prt)

        else

!         Eigenproblem: 'h*p = g*p*1/d'; e.g., structural buckling

          call geig(h,g,d,p,t,nv,0,prt)

          do n = 1,nv
            if(d(n).ne.0.0d0) d(n) = 1.d0/d(n)
          end do

!         Resort eigenvalues and vectors to ascending order

          num = nv + 1
          do n = 1,nv/2
            dm       = d(n)
            d(n)     = d(num-n)
            d(num-n) = dm
            do i = 1,nv
              dm         = p(i,n)
              p(i,n)     = p(i,num-n)
              p(i,num-n) = dm
            end do
          end do
        endif

!       Compute new iteration vector 'u' from 'z'

        do i = 1,neq

!         Move row of 'v' into temporary vector 't'

          do j = 1,nv
            t(j) = v(i,j)
          end do

!         Compute new iiteration vector entries

          do j = 1,nv
            v(i,j) = 0.0d0
            do k = 1,nv
              v(i,j) = v(i,j) + t(k)*p(k,j)
            end do
          end do
        end do

!       Check for convergence

        do n = 1,nv
          if(d(n).ne.0.0d0) dtol(n) = abs((d(n)-dp(n))/d(n))
          dp(n) = d(n)
        end do

!       Compute maximum error for iteration

        conv = .true.
        dm   =  0.0d0
        do n = 1,nf
          if(dtol(n).gt.tol) conv = .false.
          dm = max(dm,dtol(n))
        end do

        if(prt) then
          if(ior.gt.0) write(iow,2006) it,(d(n),n=1,nv)
          if(ior.lt.0) write(  *,2006) it,(d(n),n=1,nv)
          if(itlim.gt.1) then
            if(ior.gt.0) write(iow,2001) it,(dtol(n),n=1,nv)
            if(ior.lt.0) write(  *,2001) it,(dtol(n),n=1,nv)
          endif
        endif

        if(conv) go to 200
        if(ior.lt.0) write(*,2005) it, etime(tary), dm
      end do

!     Scaled vectors mass orthonormal

200   dm = 1.d0/gtan(1)
      do n = 1,nv
        d(n) = (1.0/d(n) + shift)*dm
        dp(n)= sqrt(abs(d(n)))
      end do

!     Output solution values

      write(iow,2000) itt,(d(n),n=1,nv)
      if(itt.gt.1) write(iow,2001) itt,(dtol(n),n=1,nv)
      if(imtyp.eq.1) then
        write(iow,2003) (dp(n),n=1,nv)
        dm = 0.5d0/acos(-1.0d0)
        write(iow,2004) (dp(n)*dm,n=1,nv)
        write(iow,2007)
        dr = 1.d0/dm
        do i = 0,nv-1,4
          do j = 1,min(4,nv-i)
            if(abs(dp(i+j)).gt.tol*10.d0) then
              dpp(j) = dr/dp(i+j)
            else
              dpp(j) = 0.0d0
            endif
          end do
          write(iow,2008) (dpp(j),j=1,min(4,nv-i))
        end do
      endif
      if(ior.lt.0) then
        write(*,2000) itt,(d(n),n=1,nv)
        if(itt.gt.1) write(*,2001) itt,(dtol(n),n=1,nv)
        if(imtyp.eq.1) then
          write(*,2003) (dp(n),n=1,nv)
          write(*,2004) (dp(n)*dm,n=1,nv)
          write(*,2007)
          do i = 0,nv-1,4
            do j = 1,min(4,nv-i)
              if(abs(dp(i+j)).gt.tol*10.d0) then
                dpp(j) = dr/dp(i+j)
              else
                dpp(j) = 0.0d0
              endif
            end do
            write(*,2008) (dpp(j),j=1,min(4,nv-i))
          end do
        endif
      endif

!     Formats

2000  format(/'  SUBSPACE: Current eigenvalues, iteration',i4/
     +        (5x,1p,4d17.8))

2001  format( '  SUBSPACE: Current residuals,   iteration',i4/
     +        (5x,1p,4d17.8))

2002  format(5x,'There are',i4,' eigenvalues less than shift',1p,e12.4)

2003  format(/'  SUBSPACE: Square root of eigenvalues (rad/t)'/
     +        (5x,1p,4d17.8))

2004  format(/'  SUBSPACE: Square root of eigenvalues (Hz.)'/
     +        (5x,1p,4d17.8))

2005  format('  Completed subspace iteration',i4,' Time =',f9.2,
     +       '  Max. tol =',1p,e12.5)

2006  format(/'  SUBSPACE: Inverse eigenvalues, iteration',i4/
     +        (5x,1p,4d17.8))

2007  format(/'  SUBSPACE: Period in units of time      (T)')

2008  format(5x,1p,4d17.8)

      end subroutine subsp

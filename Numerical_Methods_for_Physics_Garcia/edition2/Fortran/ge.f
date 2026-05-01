      real*8 function ge( AA, bb, N, Nm, x )
      integer*4 N, Nm
      real*8 AA(Nm,Nm), bb(Nm), x(Nm)
! ge - Function to perform Gaussian elimination to solve A*x = b
!      using scaled column pivoting
! Inputs
!    AA   -    Matrix A (N by N)
!    bb   -    Vector b (N by 1)
!    N    -    Dimension of A, b, and x (used)
!    Nm   -    Dimension of A, b, and x (allocated memory)
! Outputs
!    x    -    Vector x (N by 1)
!  determ -    Determinant of matrix A  (return value)

      integer*4 MAXN
      parameter( MAXN = 500 )

      integer*4 i, j, k, index(MAXN), signDet, jPivot, indexJ
      real*8 scale(MAXN), scaleMax, ratiomax, ratio, coeff
      real*8 determ, sum, A(MAXN,MAXN), b(MAXN)

      if( Nm .gt. MAXN ) then
        write(*,*) 'ERROR - Matrix is too large for ge routine'
        stop
      endif

      ! Copy matrix A and vector b so as not to modify original
      do i=1,N
        b(i) = bb(i)
        do j=1,N
          A(i,j) = AA(i,j)
        enddo
      enddo

      !* Set scale factor, scale(i) = max( |A(i,j)| ), for each row
      do i=1,N
        index(i) = i          ! Initialize row index list
        scaleMax = 0.0
        do j=1,N
          if( abs(A(i,j)) .gt. scaleMax ) then
            scaleMax = abs(A(i,j))
          endif
        enddo
        scale(i) = scaleMax
      enddo

      !* Loop over rows k = 1, ..., (N-1)
      signDet = 1
      do k=1,N-1
        !* Select pivot row from max( |A(j,k)/s(j)| )
        ratiomax = 0.0
        jPivot = k
        do i=k,N
          ratio = abs(A(index(i),k))/scale(index(i))
          if( ratio .gt. ratiomax ) then
            jPivot = i
            ratiomax = ratio
          endif
        enddo
        !* Perform pivoting using row index list
        indexJ = index(k)
        if( jPivot .ne. k ) then     ! Pivot
          indexJ = index(jPivot)
          index(jPivot) = index(k)   ! Swap index jPivot and k
          index(k) = indexJ
          signDet = -1*signDet       ! Flip sign of determinant
        endif
        !* Perform forward elimination
        do i=k+1,N
          coeff = A(index(i),k)/A(indexJ,k)
          do j=k+1,N
            A(index(i),j) = A(index(i),j) - coeff*A(indexJ,j)
          enddo
          A(index(i),k) = coeff
          b(index(i)) = b(index(i)) - A(index(i),k)*b(indexJ)
        enddo
      enddo
      !* Compute determinant as product of diagonal elements
      determ = signDet     ! Sign of determinant
      do i=1,N
        determ = determ * A(index(i),i)
      enddo

      !* Perform backsubstitution
      x(N) = b(index(N))/A(index(N),N)
      do i=N-1,1,-1
        sum = b(index(i))
        do j=i+1,N
          sum = sum - A(index(i),j)*x(j)
        enddo
        x(i) = sum/A(index(i),i)
      enddo

      ge = determ
      return
      end

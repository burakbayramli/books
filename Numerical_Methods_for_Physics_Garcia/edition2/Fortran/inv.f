      real function inv(AA, N, Nm, Ainv)
      integer*4 N, Nm
      real*8 AA(Nm,Nm), Ainv(Nm,Nm)
! Compute inverse of matrix
! Input
!    AA   -    Matrix A (N by N)
!    N    -    Dimension of matrix A (used)
!    Nm   -    Dimension of matrix A (allocated memory)
! Outputs
!   Ainv  -    Inverse of matrix A (N by N)
!  determ -    Determinant of matrix A (return value)

      integer*4 MAXN
      parameter( MAXN = 500 )
      integer*4 i, j, k, index(MAXN), signDet, jPivot, indexJ
      real*8 scale(MAXN), b(MAXN,MAXN)   ! Scale factor and work array
      real*8 A(MAXN,MAXN)                ! Working copy of input matrix
      real*8 scalemax, ratio, ratiomax, coeff, determ, sum

      if( Nm .gt. MAXN ) then
        write(*,*) 'ERROR - Matrix is too large for inv routine'
        stop
      endif

      ! Copy matrix A so as not to modify original
      do i=1,N
        do j=1,N
          A(i,j) = AA(i,j)
        enddo
      enddo

      !* Matrix b is initialized to the identity matrix
      do i=1,N
       do j=1,N
         if( i .eq. j ) then
           b(i,j) = 1.0
         else
           b(i,j) = 0.0
         endif
       enddo
      enddo

      !* Set scale factor, scale(i) = max( |a(i,j)| ), for each row
      do i=1,N
        index(i) = i              ! Initialize row index list
        scalemax = 0.0
        do j=1,N
          if( abs(A(i,j)) .gt. scalemax ) then
            scalemax = abs(A(i,j))
          endif
        enddo
        scale(i) = scalemax
      enddo

      !* Loop over rows k = 1, ..., (N-1)
      signDet = 1
      do k=1,N-1
        !* Select pivot row from max( |a(j,k)/s(j)| )
        ratiomax = 0.0
        jPivot = k
        do i=k,N
          ratio = abs(A(index(i),k))/scale(index(i))
          if( ratio .gt. ratiomax ) then
            jPivot=i
            ratiomax = ratio
          endif
        enddo
        !* Perform pivoting using row index list
        indexJ = index(k)
        if( jPivot .ne. k ) then           ! Pivot
          indexJ = index(jPivot)
          index(jPivot) = index(k)    ! Swap index jPivot and k
          index(k) = indexJ
          signDet = -1*signDet        ! Flip sign of determinant
        endif
        !* Perform forward elimination
        do i=k+1,N
          coeff = A(index(i),k)/A(indexJ,k)
          do j=k+1,N
            A(index(i),j) = A(index(i),j) - coeff*A(indexJ,j)
          enddo
          A(index(i),k) = coeff
          do j=1,N
            b(index(i),j) = b(index(i),j) - A(index(i),k)*b(indexJ,j)
          enddo
        enddo
      enddo

      !* Compute determinant as product of diagonal elements
      determ = signDet        ! Sign of determinant
      do i=1,N
        determ = determ*A(index(i),i)
      enddo

      !* Perform backsubstitution
      do k=1,N
        Ainv(N,k) = b(index(N),k)/A(index(N),N)
        do i=N-1,1,-1
          sum = b(index(i),k)
          do j=i+1,N
            sum = sum - A(index(i),j)*Ainv(j,k)
          enddo
          Ainv(i,k) = sum/A(index(i),i)
        enddo
      enddo

      inv = determ   ! Return the determinant
      return
      end

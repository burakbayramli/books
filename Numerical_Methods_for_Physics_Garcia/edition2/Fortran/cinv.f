! Compute inverse of complex matrix

      subroutine cinv( A, N, MAXN, Ainv )
      integer*4 N, MAXN
      complex*16 A(MAXN,MAXN), Ainv(MAXN,MAXN)
! Inputs
!   A       Matrix A to be inverted
!   N       Elements used in matrix A (N by N)
!  MAXN     Matrix dimenstions as A(MAXN,MAXN)
! Outputs
!  Ainv     Inverse of matrix A

      integer*4 MAXMAXN
      parameter( MAXMAXN = 200 )
      integer*4 i, j, k, index(MAXMAXN), jPivot, indexJ
      real*8 scale(MAXMAXN), scaleMax, ratio, ratioMax
      complex*16 AA(MAXMAXN,MAXMAXN), B(MAXMAXN,MAXMAXN), coeff, sum

      if( MAXN .gt. MAXMAXN ) then
        write(*,*) 'ERROR in cinv: Matrix too large'
        stop
      endif

      !* Matrix B is initialized to the identity matrix
      do i=1,N
       do j=1,N
         AA(i,j) = A(i,j)  ! Copy matrix so as not to overwrite
         B(i,j) = 0.0
       enddo
       B(i,i) = 1.0
      enddo

      !* Set scale factor, scale(i) = max( |a(i,j)| ), for each row
      do i=1,N
        index(i) = i     ! Initialize row index list
        scaleMax = 0.0
        do j=1,N
          if( abs(AA(i,j)) .gt. scaleMax ) then
            scaleMax = abs(AA(i,j))
          endif
        enddo
        scale(i) = scaleMax
      enddo

      !* Loop over rows k = 1, ..., (N-1)
      do k=1,(N-1)
        !* Select pivot row from max( |a(j,k)/s(j)| )
        ratiomax = 0.0
        jPivot = k
        do i=k,N
          ratio = abs(AA(index(i),k))/scale(index(i))
          if( ratio .gt. ratiomax ) then
            jPivot=i
            ratiomax = ratio
          endif
        enddo
        !* Perform pivoting using row index list
        indexJ = index(k)
        if( jPivot .ne. k ) then     ! Pivot
          indexJ = index(jPivot)
          index(jPivot) = index(k)   ! Swap index jPivot and k
          index(k) = indexJ
        endif
        !* Perform forward elimination
        do i=k+1,N
          coeff = AA(index(i),k)/AA(indexJ,k)
          do j=k+1,N
            AA(index(i),j) = AA(index(i),j) - coeff*AA(indexJ,j)
          enddo
          AA(index(i),k) = coeff
          do j=1,N
            B(index(i),j) = B(index(i),j) - AA(index(i),k)*B(indexJ,j)
          enddo
        enddo
      enddo

      !* Perform backsubstitution
      do k=1,N
        Ainv(N,k) = B(index(N),k)/AA(index(N),N)
        do i=N-1,1,-1
          sum = B(index(i),k)
          do j=i+1,N
            sum = sum - AA(index(i),j)*Ainv(j,k)
          enddo
          Ainv(i,k) = sum/AA(index(i),i)
        enddo
      enddo

      return
      end

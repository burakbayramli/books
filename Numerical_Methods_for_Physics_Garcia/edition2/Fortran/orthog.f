! orthog - Program to test if a pair of vectors
! is orthogonal.  Assumes vectors are in 3D space

      program orthog

      !* Initialize the vectors a and b
      integer*4 i
      real*8 a(3), b(3), a_dot_b
      write(*,*) 'Enter the first vector'
      do i=1,3
        write(*,*) '  a(', i, ') = '
        read(*,*) a(i)
      enddo
      write(*,*) 'Enter the second vector'
      do i=1,3
        write(*,*) '  b(', i, ') = '
        read(*,*) b(i)
      enddo

      !* Evaluate the dot product as sum over products of elements
      a_dot_b = 0.0
      do i=1,3
        a_dot_b = a_dot_b + a(i)*b(i)
      enddo

      !* Print dot product and state whether vectors are orthogonal
      if( a_dot_b .eq. 0.0 ) then
        write(*,*) 'Vectors are orthogonal'
      else
        write(*,*) 'Vectors are NOT orthogonal'
        write(*,*) 'Dot product = ', a_dot_b
      endif
      stop
      end

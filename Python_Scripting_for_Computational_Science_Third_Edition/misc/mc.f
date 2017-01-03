
      subroutine gaussv(E, n, mean, stdev)
C     This function darws n indipendent Gaussian random numbers, 
C     with mean 'mean' and standard deviation 'stdev' and puts
C     the numbers in the E array
      integer n
      real*8 E(n), mean, stdev, gauss
C     write (*,*) 'n = ', n, ' m = ', m
C     write (*,*) 'stdev = ', stdev, ' mean = ', mean

      do i = 1, n
         E(i) = gauss(mean, stdev)
C        write(*,*) 'E=',E(i)
      enddo
      return 
      end

      subroutine beamv(u, n, F, L, E, I)
      integer n, j
      real*8 E(n), u(n), F, L, I
c     End deflection of a cantilever beam
      do j = 1, n
         u(j) = (F*L**3)/(3*E(j)*I)
      enddo
      end

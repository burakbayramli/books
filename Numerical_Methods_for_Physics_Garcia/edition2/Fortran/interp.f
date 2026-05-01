! interp - Program to interpolate data using Lagrange
! polynomial to fit quadratic to three data points

      program interp
      integer*4 MAXnplot
      parameter(MAXnplot = 1000)
      !* Initialize the data points to be fit by quadratic
      integer*4 i, nplot
      real*8 x(3), y(3), x_min, x_max, xi(MAXnplot), yi(MAXnplot)
      real*8 intrpf
      write(*,*) 'Enter data points:'
      do i=1,3
        write(*,*) 'x(', i, ') = '
        read(*,*) x(i)
        write(*,*) 'y(', i, ') = '
        read(*,*) y(i)
      enddo

      !* Establish the range of interpolation (from x_min to x_max)
      write(*,*) 'Enter minimum value of x: '
      read(*,*) x_min
      write(*,*) 'Enter maximum value of x: '
      read(*,*) x_max

      !* Find yi for the desired interpolation values xi using
      !  the function intrpf
      nplot = 100     ! Number of points for interpolation curve
      do i=1,nplot
        xi(i) = x_min + (x_max-x_min)*(i-1)/(nplot-1)
        yi(i) = intrpf(xi(i),x,y)  ! Use intrpf function to interpolate
      enddo

      !* Print out the plotting variables: x, y, xi, yi
      open(11,file='x.txt',status='unknown')
      open(12,file='y.txt',status='unknown')
      open(13,file='xi.txt',status='unknown')
      open(14,file='yi.txt',status='unknown')
      do i=1,3
        write(11,*) x(i)
        write(12,*) y(i)
      enddo
      do i=1,nplot
        write(13,*) xi(i)
        write(14,*) yi(i)
      enddo
      stop
      end
!***** To plot in MATLAB; use the script below ********************
!load x.txt; load y.txt; load xi.txt; load yi.txt;
!plot(x,y,'*',xi,yi,'-');
!xlabel('x');  ylabel('y');
!title('Three point interpolation');
!legend('Data points','Interpolation');
!******************************************************************

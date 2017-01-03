      subroutine loop(up, u, um, f, nx, ny, Cx2, Cy2, dt2)
      integer nx, ny
      real*8 up(0:nx, 0:ny), u(0:nx, 0:ny), um(0:nx, 0:ny)
      real*8 Cx2, Cy2, dt2
Cf2py intent(in, out) up
Cf2py intent(in) u
Cf2py intent(in) um
C     special treatment of f: keep its C (row major) storage:
      real*8 f(0:ny, 0:nx)
Cf2py intent(in, c) f

      do j = 1, ny-1
         do i = 1, nx-1
            up(i,j) = - um(i,j) + 2*u(i,j) + 
     &          Cx2*(u(i-1,j) - 2*u(i,j) + u(i+1,j)) +
     &          Cy2*(u(i,j-1) - 2*u(i,j) + u(i,j+1)) +
     &          dt2*f(j,i)
         end do
      end do
      return
      end

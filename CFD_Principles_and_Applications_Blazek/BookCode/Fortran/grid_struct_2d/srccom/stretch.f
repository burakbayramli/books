      SUBROUTINE Stretch( dx1,sumdx,n,r )

C *****************************************************************************
C
C  Computes the coordinate streching ratio according to
C  geometric progression:
C
C  dx1   = length of the first interval
C  sumdx = sum of all intervals (length of the region)
C  n     = number of intervals over sumdx
C  r     = constant ratio of successive intervals dx_i+1/dx_i
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: F, Fp
C
C *****************************************************************************
C
C  This program is free software; you can redistribute it and/or
C  modify it under the terms of the GNU General Public License
C  as published by the Free Software Foundation; either version 2
C  of the License, or (at your option) any later version.
C
C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
C  GNU General Public License for more details.
C
C  You should have received a copy of the GNU General Public License
C  along with this program; if not, write to the Free Software
C  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameter list
      INTEGER n
      REAL*8 dx1, sumdx, r

C ... loop variables
      INTEGER l

C ... local variables
      REAL*8 riter

C ... functions
      REAL*8 F, Fp

C *****************************************************************************

      F(r)  = (r-1.D0)*sumdx - dx1*(r**n-1.D0)
      Fp(r) = sumdx - n*dx1*r**(n-1)

      r = 2.D0
      DO l=1,200                  ! Newton iteration for r
        riter = r - F(r)/Fp(r)
        IF (ABS(riter-r) .LT. 5.D-5)  GOTO 1
        r = riter
      ENDDO

      WRITE(*,1000) n,r,riter,sumdx,dx1
1000  FORMAT(/,'WARNING (Stretch): no convergence.',/,
     &       ' n,r,riter,sumdx,dx1 = ',I5,4E12.4,/)
      RETURN

1     r = riter

      RETURN
      END

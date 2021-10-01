      SUBROUTINE Bezier_control( d,m,b )

C *****************************************************************************
C
C  Computes the control points of a 2-D Bezier curve.
C
C  Input parameters:
C  -----------------
C  M      = no. of segments along curve
C  D(J,K) = coordinates of the weights; J=1(1)2, K=0(1)M
C
C  Output parameters:
C  ------------------
C  B(J,K) = coordinates of the control points; J=1(1)2, K=0(1)3*M
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
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
      INTEGER m
      REAL*8 b(2,0:3*m), d(2,0:m)

C ... loop variables
      INTEGER j, k

C *****************************************************************************
C --- loop over x-, y-coordinates

      DO 10 j=1,2
        DO 20 k=1,m-1
          b(j,3*k-2) = (2.D0*d(j,k-1)+d(j,k))/3.D0
          b(j,3*k)   = (d(j,k-1)+4.D0*d(j,k)+d(j,k+1))/6.D0
          b(j,3*k+2) = (d(j,k)+2.D0*d(j,k+1))/3.D0
20      CONTINUE
        b(j,2)     = (d(j,0)+2.D0*d(j,1))/3.D0
        b(j,3*m-2) = (2.D0*d(j,m-1)+d(j,m))/3.D0

C ----- boundary points B(J,0) and B(J,3*M) are defined such that
C       we get a natural cubic Bezier spline

        b(j,0)   = d(j,0)
        b(j,3*m) = d(j,m)
10    CONTINUE

      RETURN
      END

C #############################################################################

      SUBROUTINE Bezier_interpol( d,m,b )

C *****************************************************************************
C
C  Computes the control points of a 2-D Bezier spline from given
C  interpolation points. The procedure is as follows:
C
C  (1) use interpolation points as locations of the weights
C  (2) compute corresp. control points
C  (3) move control points (in one step!) such that the curve
C      matches the interpolation points
C
C  Input parameters:
C  -----------------
C  M      = no. of segments along curve
C  D(J,K) = coordinates of the interpolation points; J=1(1)2, K=0(1)M
C
C  Output parameters:
C  ------------------
C  B(J,K) = coordinates of the control points; J=1(1)2, K=0(1)3*M
C
C *****************************************************************************
C
C  Subroutines called: Bezier_control
C
C  Functions called: none
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
      INTEGER m
      REAL*8 b(2,0:3*m), d(2,0:m)

C ... loop variables
      INTEGER j, k, kk, mm

C ... local variables
      REAL*8 di

C *****************************************************************************
C --- first approximation of the control points (D=interp. points)

      CALL Bezier_control( d,m,b )

C --- translation of the control points using the prescribed interp. points

      mm = 3*m - 3
      DO 30 j=1,2
        k = 3
40      CONTINUE
          kk = k/3

C ------- three interp. points are identical => do not move control point

          IF (kk .LE. (m-2)) THEN
            IF ((ABS(d(1,kk)-d(1,kk+1)).LT.1.D-8).AND.
     &          (ABS(d(2,kk)-d(2,kk+1)).LT.1.D-8).AND.
     &          (ABS(d(1,kk)-d(1,kk+2)).LT.1.D-8).AND.
     &          (ABS(d(2,kk)-d(2,kk+2)).LT.1.D-8)) THEN
              k = k + 9
              IF (k .LE. mm) GOTO 40
            ENDIF
          ENDIF

C ------- move control point

          di       = d(j,kk) - b(j,k)
          b(j,k-2) = b(j,k-2) + di/6.D0
          b(j,k-1) = b(j,k-1) + di/2.D0
          b(j,k)   = b(j,k)   + di
          b(j,k+1) = b(j,k+1) + di/2.D0
          b(j,k+2) = b(j,k+2) + di/6.D0
          k        = k + 3
          IF (k .LE. mm) GOTO 40
30    CONTINUE

      RETURN
      END

C #############################################################################

      SUBROUTINE Bezier( b,m,vp,x,y )

C *****************************************************************************
C
C  Computes coordinates (X,Y) of a point of the Bezier curve
C  according to the curve parameter VP
C
C  Input parameters:
C  -----------------
C  M      = no. of segments along curve
C  B(J,K) = coordinates of the control points; J=1(1)2, K=0(1)3*M
C  VP     = curve parameter (must be in the range [0,1])
C
C  Output parameters:
C  ------------------
C  X, Y = x-, y-coordinates of the point
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
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
      INTEGER m
      REAL*8 b(2,0:3*m), x, y, vp

C ... local variables
      INTEGER k
      REAL*8 vv, v,f , f0, f1, f2, f3

C *****************************************************************************

      vv = vp*3.D0*m
      k  = INT(vv/3)*3
      k  = MIN(k,3*m-3)
      v  = (vv-k)/3.D0
      f  = 1.D0 - v
      f0 = f*f*f
      f1 = 3.D0*f*f*v
      f2 = 3.D0*f*v*v
      f3 = v*v*v
      x  = b(1,k)*f0 + b(1,k+1)*f1 + b(1,k+2)*f2 + b(1,K+3)*F3
      y  = b(2,k)*f0 + b(2,k+1)*f1 + b(2,k+2)*f2 + b(2,k+3)*f3

      RETURN
      END

C #############################################################################

      SUBROUTINE Bezier_x( b,m,vpa,vpe,eps,x,y )

C *****************************************************************************
C
C  Given the x-coordinate (X) of a point of the Bezier curve the function
C  computes its y-coordinate (Y). Function employs the "Pegasus" scheme
C  to find the root of the distance function.
C
C  Input parameters:
C  -----------------
C  M       = no. of segments along curve
C  B(J,K)  = coordinates of the control points; J=1(1)2, K=0(1)3*M
C  VPA,VPE = intervall of the curve parameter (VPA <= VP <= VPE)
C  EPS     = max. tolerance
C  X       = x-coordinate of the point
C
C  Output parameters:
C  ------------------
C  Y = y-coordinate of the point
C
C *****************************************************************************
C
C  Subroutines called: Bezier
C
C  Functions called: none
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
      INTEGER m
      REAL*8 b(2,0:3*m), vpa, vpe, eps, x, y

C ... loop variables
      INTEGER iter

C ... local variables
      REAL*8 vp1, vp2, vp3, xp1, xp2, xp3, yp1, yp2, yp3, s12, vpsi

C *****************************************************************************

      vp1 = vpa
      vp2 = vpe
      CALL Bezier( b,m,vp1,xp1,yp1 )
      CALL Bezier( b,m,vp2,xp2,yp2 )
      IF (ABS(xp1-x) .LE. eps) THEN
        x = xp1
        y = yp1
        RETURN
      ENDIF
      IF (ABS(xp2-x) .LE. eps) THEN
        x = xp2
        y = yp2
        RETURN
      ENDIF
      IF ((xp1-x)*(xp2-x) .GT. 0.D0) THEN
        WRITE(*,'(/,A)') 'ERROR (Bezier_x): x-coordinate outside'//
     &                   ' the specified curve interval'
        STOP
      ENDIF

C --- Pegasus scheme

      iter = 0
10    CONTINUE
        iter = iter + 1
        IF (ABS(xp2-x) .LE. eps) THEN
          vpsi = vp2
          GOTO 777
        ELSE IF (ABS(vp2-vp1) .LE. eps) THEN
          vpsi = vp2
          IF (ABS(xp1-x) .LT. ABS(xp2-x)) vpsi = vp1
          GOTO 777
        ELSE
          s12 = (xp2-xp1)/(vp2-vp1)
          vp3 = vp2 - (xp2-x)/s12
          vp3 = MAX(0.D0,vp3)
          vp3 = MIN(1.D0,vp3)
          CALL Bezier( b,m,vp3,xp3,yp3 )
          IF ((xp2-x)*(xp3-x) .LE. 0.D0) THEN
            vp1 = vp2
            xp1 = xp2
          ENDIF
          vp2 = vp3
          xp2 = xp3
        ENDIF
      IF (iter .LT. 1000) GOTO 10

      WRITE(*,'(/,A,/)') 'WARNING (Bezier_x): not converged'

777   CONTINUE
      CALL Bezier( b,m,vpsi,x,y )

      RETURN
      END

      SUBROUTINE Ellgrid( im,jm,ip,jp,x,y,
     &                    icd,jcd,ica,jca,
     &                    itlapla,itsmoo,itgauss,
     &                    damps1,damps2,pspace1,pspace2,
     &                    dampa1,dampa2,pangle1,pangle2,
     &                    omega,p,q,distes,angles )

C *****************************************************************************
C
C  Elliptic grid generation (Laplace & Poisson eq.):
C
C  icd, jcd: distance control (damps1, damps2, pspace1, pspace2)
C     icd = 01 -> along i=1 line
C     icd = 10 -> along i=ip line
C     icd = 11 -> along i=1 & i=ip line
C     jcd = 01 -> along j=1 line
C     jcd = 10 -> along j=jp line
C     jcd = 11 -> along j=1 & j=jp line
C
C  ica, jca: angle control (dampa1, dampa2, pangle1, pangle2)
C     ica = 01 -> along i=1 line
C     ica = 10 -> along i=ip line
C     ica = 11 -> along i=1 & i=ip line
C     jca = 01 -> along j=1 line
C     jca = 10 -> along j=jp line
C     jca = 11 -> along j=1 & j=jp line
C
C  Rem.: angle control is switched off locally if angles(i,j)<0;
C        distance control is switched off locally if distes(i,j)<0.
C
C *****************************************************************************
C
C  Subroutines called: Angle, Gauss, Space, Interp
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
      INTEGER im, jm, ip, jp, icd, jcd, ica, jca, itlapla,
     &        itsmoo, itgauss
      REAL*8 x(im,jm), y(im,jm)
      REAL*8 p(im,jm), q(im,jm)
      REAL*8 distes(im,jm), angles(im,jm)
      REAL*8 damps1, damps2, dampa1, pspace1, pspace2,
     &       dampa2, pangle1, pangle2, omega

C ... loop variables
      INTEGER i, j, it, itgs

C ... local variables
      REAL*8 rx, ry, dsrmax, darmax

C *****************************************************************************

      WRITE(*,1000) icd,jcd,ica,jca,itlapla,itsmoo,itgauss,
     &              damps1,damps2,dampa1,dampa2,pspace1,pspace2,
     &              pangle1,pangle2,omega

      IF (icd.GT.0 .AND. jca.GT.0) THEN
        WRITE(*,*) ' ERROR - control (icd,jca) is impossible'
        STOP
      ENDIF
      IF (ica.GT.0 .AND. jcd.GT.0) THEN
        WRITE(*,*) ' ERROR - control (ica,jcd) is impossible'
        STOP
      ENDIF

C --- initialize source terms

      DO j=1,jm
        DO i=1,im
          p(i,j) = 0.D0
          q(i,j) = 0.D0
        ENDDO
      ENDDO

C --- Laplace's smoothing

      IF (itlapla .NE. 0) THEN
        WRITE(*,'(A)') '   smoothing using Laplace equations:'
        CALL Gauss( im,jm,ip,jp,Iabs(itlapla),x,y,
     &              p,q,omega,rx,ry,itgs )
        WRITE(*,1010) itgs,ABS(rx),ABS(ry)
      ENDIF

      IF (itlapla .LT. 0) RETURN

C --- elliptic smoothing

      WRITE(*,'(A)') '   smoothing using Poisson equations:'

      DO it=1,itsmoo

        CALL Angle( im,jm,ip,jp,x,y,p,q,angles,
     &              ica,jca,dampa1,dampa2,darmax )

        CALL Space( im,jm,ip,jp,x,y,p,q,distes,
     &              icd,jcd,damps1,damps2,dsrmax )

        CALL Interp( im,jm,ip,jp,p,q,icd,jcd,ica,jca,
     &               pangle1,pangle2,pspace1,pspace2 )

        CALL Gauss( im,jm,ip,jp,itgauss,x,y,p,q,omega,rx,ry,itgs )

        IF (it.EQ.1 .OR. MOD(it,5).EQ.0 .OR. it.EQ.itsmoo) THEN
          WRITE(*,1005) it,dsrmax,darmax
          WRITE(*,1010) itgs,ABS(rx),ABS(ry)
        ENDIF

        IF (dsrmax.LT.1.D-3 .AND. darmax.LT.1.D-3) GOTO 9999

      ENDDO

9999  CONTINUE

1000  FORMAT(/,' Elliptic Grid Generation:',/,
     &         '   distance control (i/j) ',I2.2,'  ',I2.2,/,
     &         '   angle    control (i/j) ',I2.2,'  ',I2.2,/,
     &         '   itlapla,itsmoo,itgauss ',I5,2I8,/,
     &         '   spacing damping fact.  ',E12.5,' ',E12.5,/,
     &         '   angle damping fact.    ',E12.5,' ',E12.5,/,
     &         '   spacing decay          ',E12.5,' ',E12.5,/,
     &         '   angle decay            ',E12.5,' ',E12.5,/,
     &         '   relaxation parameter   ',E12.5,/)
1005  FORMAT('   smooth. it.= ',I3,' dsmax= ',E12.5,' damax= ',E12.5)
1010  FORMAT('   GS-iterat. = ',I3,'  resx= ',E12.5,'  resy= ',E12.5,/)
      RETURN
      END

C #############################################################################

      SUBROUTINE Space( im,jm,ip,jp,x,y,p,q,distes,
     &                  icd,jcd,damps1,damps2,dsrmax )

C *****************************************************************************
C
C  Iterative source term control - spacing
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameter list
      INTEGER im, jm, ip, jp, icd, jcd
      REAL*8 x(im,jm), y(im,jm), p(im,jm), q(im,jm), distes(im,jm)
      REAL*8 damps1, damps2, dsrmax

C ... loop variables
      INTEGER i, ip1, j, jp1

C ... local variables
      REAL*8 dqmax, dpmax, dx, dy, dit, dp, dq

C *****************************************************************************

      dqmax = 0.D0
      dpmax = 0.D0

C --- spacing control at i=const.

      IF (icd.EQ.01 .OR. icd.EQ.11) THEN
        i   = 1
        ip1 = 2
        DO j=2,jp-1
          IF (distes(i,j) .GT. 0.D0) THEN
            dx     = x(ip1,j) - x(i,j)
            dy     = y(ip1,j) - y(i,j)
            dit    = SQRT(dx*dx+dy*dy)
            dp     = damps1 * ATAN((distes(i,j)-dit)/distes(i,j))
            dpmax  = MAX(dpmax, ABS(dp))
            p(i,j) = p(i,j) + dp
          ENDIF
        ENDDO
      ENDIF
      IF (icd.EQ.10 .OR. icd.EQ.11) THEN
        i   = ip
        ip1 = ip - 1
        DO j=2,jp-1
          IF (distes(i,j) .GT. 0.D0) THEN
            dx     = x(ip1,j) - x(i,j)
            dy     = y(ip1,j) - y(i,j)
            dit    = SQRT(dx*dx+dy*dy)
            dp     = damps2 * ATAN((distes(i,j)-dit)/distes(i,j))
            dpmax  = MAX(dpmax, ABS(dp))
            p(i,j) = p(i,j) - dp
          ENDIF
        ENDDO
      ENDIF

C --- spacing control at j=const.

      IF (jcd.EQ.01 .OR. jcd.EQ.11) THEN
        j   = 1
        jp1 = 2
        DO i=2,ip-1
          IF (distes(i,j) .GT. 0.D0) THEN
            dx     = x(i,jp1) - x(i,j)
            dy     = y(i,jp1) - y(i,j)
            dit    = SQRT(dx*dx+dy*dy)
            dq     = damps1 * ATAN((distes(i,j)-dit)/distes(i,j))
            dqmax  = MAX(dqmax, ABS(dq))
            q(i,j) = q(i,j) + dq
          ENDIF
        ENDDO
      ENDIF
      IF (jcd.EQ.10 .OR. jcd.EQ.11) THEN
        j   = jp
        jp1 = jp - 1
        DO i=2,ip-1
          IF (distes(i,j) .GT. 0.D0) THEN
            dx     = x(i,jp1) - x(i,j)
            dy     = y(i,jp1) - y(i,j)
            dit    = SQRT(dx*dx+dy*dy)
            dq     = damps2 * ATAN((distes(i,j)-dit)/distes(i,j))
            dqmax  = MAX(dqmax, ABS(dq))
            q(i,j) = q(i,j) - dq
          ENDIF
        ENDDO
      ENDIF

      dsrmax = MAX(dpmax, dqmax)

      RETURN
      END

C #############################################################################

      SUBROUTINE Angle( im,jm,ip,jp,x,y,p,q,angles,
     &                  ica,jca,dampa1,dampa2,darmax )

C *****************************************************************************
C
C  Iterative source term control - angles
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameter list
      INTEGER im, jm, ip, jp, ica, jca
      REAL*8 x(im,jm), y(im,jm), p(im,jm), q(im,jm), angles(im,jm)
      REAL*8 dampa1, dampa2, darmax

C ... loop variables
      INTEGER i, ip1, j, jp1

C ... local variables
      REAL*8 dqmax, dpmax, xi, yi, xj, yj, spro, aval, alph, dp, dq

C *****************************************************************************

      dpmax = 0.D0
      dqmax = 0.D0

C --- angle control along i=const.

      IF (ica.EQ.01 .OR. ica.EQ.11) THEN
        i   = 1
        ip1 = 2
        DO j=2,jp-1
          IF (angles(i,j) .GT. 0.D0) THEN
            xi     = x(ip1,j) - x(i,j)
            yi     = y(ip1,j) - y(i,j)
            xj     = 0.5D0*(x(i,j+1)-x(i,j-1))
            yj     = 0.5D0*(y(i,j+1)-y(i,j-1))
            spro   = xi*xj + yi*yj
            aval   = SQRT(xi*xi+yi*yi)*SQRT(xj*xj+yj*yj)
            alph   = ACOS(spro/aval)
            dq     = -dampa1 * ATAN(angles(i,j)-alph)
            dqmax  = MAX(dqmax, ABS(dq))
            q(i,j) = q(i,j) + dq
          ENDIF
        ENDDO
      ENDIF
      IF (ica.EQ.10 .or. ica.EQ.11) THEN
        i   = ip
        ip1 = ip - 1
        DO j=2,jp-1
          IF (angles(i,j) .GT. 0.D0) THEN
            xi     = x(ip1,j) - x(i,j)
            yi     = y(ip1,j) - y(i,j)
            xj     = 0.5D0*(x(i,j+1)-x(i,j-1))
            yj     = 0.5D0*(y(i,j+1)-y(i,j-1))
            spro   = xi*xj + yi*yj
            aval   = SQRT(xi*xi+yi*yi)*SQRT(xj*xj+yj*yj)
            alph   = ACOS(spro/aval)
            dq     = -dampa2 * ATAN(angles(i,j)-alph)
            dqmax  = MAX(dqmax, ABS(dq))
            q(i,j) = q(i,j) + dq
          ENDIF
        ENDDO
      ENDIF

C --- angle control along j=const.

      IF (jca.EQ.01 .OR. jca.EQ.11) THEN
        j   = 1
        jp1 = 2
        DO i=2,ip-1
          IF (angles(i,j) .GT. 0.D0) THEN
            xi     = 0.5D0*(x(i+1,j)-x(i-1,j))
            yi     = 0.5D0*(y(i+1,j)-y(i-1,j))
            xj     = x(i,jp1) - x(i,j)
            yj     = y(i,jp1) - y(i,j)
            spro   = xi*xj + yi*yj
            aval   = SQRT(xi*xi+yi*yi)*SQRT(xj*xj+yj*yj)
            alph   = ACOS(spro/aval)
            dp     = -dampa1 * ATAN(angles(i,j)-alph)
            dpmax  = MAX(dpmax, ABS(dp))
            p(i,j) = p(i,j) + dp
          ENDIF
        ENDDO
      ENDIF
      IF (jca.EQ.10 .OR. jca.EQ.11) THEN
        j   = jp
        jp1 = jp - 1
        DO i=2,ip-1
          IF (angles(i,j) .GT. 0.D0) THEN
            xi     = 0.5D0*(x(i+1,j)-x(i-1,j))
            yi     = 0.5D0*(y(i+1,j)-y(i-1,j))
            xj     = x(i,jp1) - x(i,j)
            yj     = y(i,jp1) - y(i,j)
            spro   = xi*xj + yi*yj
            aval   = SQRT(xi*xi+yi*yi)*SQRT(xj*xj+yj*yj)
            alph   = ACOS(spro/aval)
            dp     = -dampa2 * ATAN(angles(i,j)-alph)
            dpmax  = MAX(dpmax, ABS(dp))
            p(i,j) = p(i,j) + dp
          ENDIF
        ENDDO
      ENDIF

      darmax = MAX(dpmax, dqmax)

      RETURN
      END

C #############################################################################

      SUBROUTINE Interp( im,jm,ip,jp,p,q,icd,jcd,ica,jca,
     &                   pangle1,pangle2,pspace1,pspace2 )

C *****************************************************************************
C
C  1-D interpolation by power-law functions
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameter list
      INTEGER im, jm, ip, jp, icd, jcd, ica, jca
      REAL*8 p(im,jm), q(im,jm)
      REAL*8 pangle1, pangle2, pspace1, pspace2

C ... loop variables
      INTEGER i, j

C ... local variables
      REAL*8 rat

C *****************************************************************************

      IF (icd .GT. 0) THEN
        DO i=2,ip-1
          rat = REAL(i-1)/REAL(ip-1)
          DO j=2,jp-1
            p(i,j) = p(1,j)*(1.D0-rat)**pspace1 + p(ip,j)*rat**pspace2
          ENDDO
        ENDDO
      ENDIF

      IF (ica .GT. 0) THEN
        DO i=2,ip-1
          rat = REAL(i-1)/REAL(ip-1)
          DO j=2,jp-1
            q(i,j) = q(1,j)*(1.D0-rat)**pangle1 + q(ip,j)*rat**pangle2
          ENDDO
        ENDDO
      ENDIF

      IF (jcd .GT. 0) THEN
        DO j=2,jp-1
          rat = REAL(j-1)/REAL(jp-1)
          DO i=2,ip-1
            q(i,j) = q(i,1)*(1.D0-rat)**pspace1 + q(i,jp)*rat**pspace2
          ENDDO
        ENDDO
      ENDIF

      IF (jca .GT. 0) THEN
        DO j=2,jp-1
          rat = REAL(j-1)/REAL(jp-1)
          DO i=2,ip-1
            p(i,j) = p(i,1)*(1.D0-rat)**pangle1 + p(i,jp)*rat**pangle2
          ENDDO
        ENDDO
      ENDIF

      RETURN
      END

C #############################################################################

      SUBROUTINE Gauss( im,jm,ip,jp,itgauss,x,y,p,q,omega,rx,ry,it )

C *****************************************************************************
C
C  Gauss-Seidel scheme for solving the Laplace/Poisson equation
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameter list
      INTEGER im, jm, ip, jp, itgauss, it
      REAL*8 x(im,jm), y(im,jm), p(im,jm), q(im,jm), omega, rx, ry

C ... loop variables
      INTEGER i, j, ip1, jp1

C ... local variables
      REAL*8 eps
      PARAMETER (eps=1.D-5)

      REAL*8 xksi, xeta, yksi, yeta, x2ksi, x2eta, xksieta, y2ksi,
     &       y2eta, yksieta, alpha, beta, gamma, resx, resy, ag2,
     &       rx0, ry0

C *****************************************************************************

      ip1 = ip - 1
      jp1 = jp - 1

      DO it=1,itgauss

        rx = 0.D0
        ry = 0.D0

        DO j=2,jp1
          DO i=2,ip1
            xksi = 0.5D0*(x(i+1,j)-x(i-1,j))
            xeta = 0.5D0*(x(i,j+1)-x(i,j-1))
            yksi = 0.5D0*(y(i+1,j)-y(i-1,j))
            yeta = 0.5D0*(y(i,j+1)-y(i,j-1))

            x2ksi   = x(i+1,j) + x(i-1,j) - 2.D0*x(i,j)
            x2eta   = x(i,j+1) + x(i,j-1) - 2.D0*x(i,j)
            xksieta = 0.25D0*(x(i+1,j+1)-x(i+1,j-1)-
     &                        x(i-1,j+1)+x(i-1,j-1))

            y2ksi   = y(i+1,j) + y(i-1,j) - 2.D0*y(i,j)
            y2eta   = y(i,j+1) + y(i,j-1) - 2.D0*y(i,j)
            yksieta = 0.25D0*(y(i+1,j+1)-y(i+1,j-1)-
     &                        y(i-1,j+1)+y(i-1,j-1))

            alpha = xeta*xeta + yeta*yeta
            beta  = xksi*xeta + yksi*yeta
            gamma = xksi*xksi + yksi*yksi
            ag2   = 2.D0*(alpha+gamma)

C --------- Residuals

            resx = alpha*(x2ksi+p(i,j)*xksi) - 2.D0*beta*xksieta +
     &             gamma*(x2eta+q(i,j)*xeta)

            resy = alpha*(y2ksi+p(i,j)*yksi) - 2.D0*beta*yksieta +
     &             gamma*(y2eta+q(i,j)*yeta)

            rx = rx + resx*resx
            ry = ry + resy*resy

C --------- Gauss-Seidel

            x(i,j) = x(i,j) + omega*resx/ag2
            y(i,j) = y(i,j) + omega*resy/ag2
          ENDDO
        ENDDO

        IF (it .EQ. 1) THEN
          rx0 = rx
          ry0 = ry
        ENDIF
        rx = rx/rx0
        ry = ry/ry0

        IF (rx.LE.eps .AND. ry.LE.eps) GOTO 9999

      ENDDO

9999  CONTINUE
      RETURN
      END

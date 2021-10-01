      PROGRAM Mstage
C
C *****************************************************************************
C
C   CALCULATION OF THE FOURIER SYMBOL AND THE AMPLIFICATION FACTOR
C   FOR AN EXPLICIT MULTISTAGE TIME-STEPPING SCHEME
C   ==============================================================
C
C   Model = 1:     du/dt + du/dx = 0
C
C   Model = 2:     du/dt + du/dx = nue*d^2u/dx^2
C
C *****************************************************************************
C
C   (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
C   Created : February 1992
C   Modified: February 6, 2014
C
C
C   This program is free software; you can redistribute it and/or
C   modify it under the terms of the GNU General Public License
C   as published by the Free Software Foundation; either version 2
C   of the License, or (at your option) any later version.
C
C   This program is distributed in the hope that it will be useful,
C   but WITHOUT ANY WARRANTY; without even the implied warranty of
C   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
C   GNU General Public License for more details.
C
C   You should have received a copy of the GNU General Public License
C   along with this program; if not, write to the Free Software
C   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
C
C *****************************************************************************
C
      IMPLICIT NONE
C
C ... input parameters
      CHARACTER fnstab*(80), fnamplif*(80)
      INTEGER modeq, kdis, ksmoo, ksch
      REAL cflu, cfls, ratio, eps4r, esmoo, a(5),
     &     xbeg, xend, ybeg, yend
C
C ... loop variables
      INTEGER i, j
C
C ... local variables
      COMPLEX z, ze, g, ge, s
      CHARACTER title*(125), discr*(2), scheme*(5), smooth*(1),
     &          model*(2)
      INTEGER nstages
      REAL pi, rad, eps4, dtrat, ksi, zr, zi, zrv, x, y,
     &     deltx, delty
C
C *****************************************************************************
C
      pi = 2.*ASIN(1.)
      rad = pi/180.
C
      WRITE(*,*) '   FOURIER SYMBOL AND AMPLIFICATION FACTOR'
      WRITE(*,*) ' OF EXPLICIT MULTISTAGE TIME-STEPPING SCHEME'
      WRITE(*,*) ' ==========================================='
      WRITE(*,*) ' '
C
      READ(*,'(A80)') fnstab
      READ(*,'(A80)') fnamplif
      READ(*,*) modeq
      READ(*,*) kdis
      READ(*,*) ksmoo
      READ(*,*) ksch
      READ(*,*) cflu
      READ(*,*) cfls
      READ(*,*) ratio
      READ(*,*) eps4r
      READ(*,*) esmoo
      nstages = ksch/10
      READ(*,*) (a(i), i=1,nstages)
      READ(*,*) xbeg, xend
      READ(*,*) ybeg, yend
C
      eps4 = eps4r
      IF (ABS(eps4r) .GE. 1.E-6) eps4 = 1./eps4r
C
      IF (modeq .EQ. 1) ratio = 0.
      IF (kdis .EQ. 0) THEN
        dtrat = 1./(1.+4.*ratio)
      ELSE IF (kdis .EQ. 1) THEN
        dtrat = 1./(1.+2.*ratio)
      ELSE
        dtrat = 1./(1.+ratio)
      ENDIF
C
      IF (modeq .EQ. 1) model = 'EU'
      IF (modeq .EQ. 2) model = 'NS'
C
      IF (kdis .EQ. 0) discr = 'C2'
      IF (kdis .EQ. 1) discr = 'U1'
      IF (kdis .EQ. 2) discr = 'U2'
C
      IF (ksch .EQ. 31) scheme = '(3,1)'
      IF (ksch .EQ. 32) scheme = '(3,2)'
      IF (ksch .EQ. 33) scheme = '(3,3)'
      IF (ksch .EQ. 41) scheme = '(4,1)'
      IF (ksch .EQ. 42) scheme = '(4,2)'
      IF (ksch .EQ. 44) scheme = '(4,4)'
      IF (ksch .EQ. 52) scheme = '(5,2)'
      IF (ksch .EQ. 53) scheme = '(5,3)'
      IF (ksch .EQ. 55) scheme = '(5,5)'
C
      IF (ksmoo .EQ. 1) smooth = 'C'
      IF (ksmoo .EQ. 2) smooth = 'U'
C
      IF (kdis .EQ. 0) THEN
        WRITE(title,1000) model,scheme,discr,smooth,cflu,cfls,ratio,
     &                    eps4r,esmoo,(a(i), i=1,nstages)
      ELSE
        WRITE(title,1001) model,scheme,discr,smooth,cflu,cfls,ratio,
     &                    esmoo,(a(i), i=1,nstages)
      ENDIF
C
C --- open Vis2D files (stability region, amplification factors)
C
      OPEN(10,file=fnstab,form='formatted',status='unknown')
      WRITE(10,1005) title
      WRITE(10,1010)
C
      OPEN(20,file=fnamplif,form='formatted',status='unknown')
      WRITE(20,1005) title
      WRITE(20,1011)
C
C --- Lines of constant damping (|g|=const) -----------------------------------
C
      WRITE(*,*) ' Stability region ...'
      WRITE(10,1015) 100,100,'stability region'
C
      deltx = (xend-xbeg)/99.
      delty = (yend-ybeg)/99.
C
      y = ybeg
      DO j=1,100
        x = xbeg
        DO i=1,100
          z = -CMPLX(x,y)
          CALL Amplif( ksch,z,a,g )
          WRITE(10,1020) x,y,CABS(g)
          x = x + deltx
        ENDDO
        y = y + delty
      ENDDO
C
C --- Fourier symbol ----------------------------------------------------------
C
      WRITE(*,*) ' Fourier symbol ...'
      WRITE(10,1015) 3601,1,'Fourier symbol'
C
      DO i=0,3600
        ksi = rad*FLOAT(i)/10.
C
        CALL Diffop ( ksi,kdis,eps4,zr,zi )
        CALL Diffopv( ksi,modeq,zrv )
C
        z = dtrat*cflu*(ratio*zrv-CMPLX(zr,zi))
        x = REAL (z)
        y = AIMAG(z)
        WRITE(10,1020) x,y,1.
      ENDDO
C
C --- smoothed scheme
C
      WRITE(10,1015) 18001,1,'Fourier symbol (smoothed)'
C
      DO i=0,18000
        ksi = rad*FLOAT(i)/50.
C
        CALL Diffop ( ksi,kdis,eps4,zr,zi )
        CALL Diffopv( ksi,modeq,zrv )
        CALL Irsop  ( ksi,ksmoo,esmoo,s )
C
        z  = dtrat*cflu*(ratio*zrv-CMPLX(zr,zi))
        ze = (cfls/cflu)*z*s
        x  = REAL (ze)
        y  = AIMAG(ze)
        WRITE(10,1020) x,y,1.
      ENDDO
C
C --- amplification factor ----------------------------------------------------
C
      WRITE(*,*) ' Amplification factor ...'
      WRITE(20,1016) 1801,'no smoothing'
C
      DO i=0,1800
        ksi = rad*FLOAT(i)/10.
C
        CALL Diffop ( ksi,kdis,eps4,zr,zi )
        CALL Diffopv( ksi,modeq,zrv )
C
        z = dtrat*cflu*(-ratio*zrv+CMPLX(zr,zi))
C
        CALL Amplif( ksch,z,a,g  )
        WRITE(20,1021) ksi,CABS(g)
      ENDDO
C
C --- smoothed scheme
C
      WRITE(20,1016) 1801,'residual smoothing'
C
      DO i=0,1800
        ksi = rad*FLOAT(i)/10.
C
        CALL Diffop ( ksi,kdis,eps4,zr,zi )
        CALL Diffopv( ksi,modeq,zrv )
        CALL Irsop  ( ksi,ksmoo,esmoo,s )
C
        z  = dtrat*cflu*(-ratio*zrv+CMPLX(zr,zi))
        ze = (cfls/cflu)*z*s
C
        CALL Amplif( ksch,ze,a,ge )
        WRITE(20,1021) ksi,CABS(ge)
      ENDDO
C
      CLOSE(10)
      CLOSE(20)
      WRITE(*,*) ' Finished.'
C
C
1000  FORMAT(A2,': ',A5,'/',A2,'/',A1,', CFL=',F5.2,'/',F5.1,', LV/LC=',
     &       F6.2,', 1/E4=',F5.0,', ESMOO=',F5.1,', KOEF:',5F8.5)
1001  FORMAT(A2,': ',A5,'/',A2,'/',A1,', CFL=',F5.2,'/',F5.1,', LV/LC=',
     &       F6.2,', ESMOO=',F5.1,', KOEF:',5F8.5)
1005  FORMAT(A)
1010  FORMAT('1',/,'MSTAGE',/,'3 3',/,'x',/,'y',/,'|g|')
1011  FORMAT('1',/,'MSTAGE',/,'2 2',/,'phase angle',/,'|g|')
1015  FORMAT(I5,I5,/,'0 0 0',/,A)
1016  FORMAT(I5,' 0',/,'0 0 0',/,A)
1020  FORMAT(3E13.5)
1021  FORMAT(2E13.5)
C
      STOP
      END
C
C #############################################################################
C
      SUBROUTINE Diffop( ksi,kdis,eps4,zr,zi )
C
C  Fourier symbol of spatial discretization - convection term
C  ==========================================================
C
      IMPLICIT NONE
C
C ... parameters
      INTEGER kdis
      REAL ksi, eps4, zr, zi
C
C ... local variables
      REAL c, s
C
C *****************************************************************************
C
      c = COS(ksi)
      s = SIN(ksi)
C
      IF (kdis .EQ. 0) THEN
C
C ----- central scheme: 2nd order, 3rd-order dissipation
C
        zr = 4.*eps4*(1.-c)**2
        zi = s
C
      ELSE IF (kdis .EQ. 1) THEN
C
C ----- 1st-order upwind
C
        zr = 1. - c
        zi = s
C
      ELSE IF (kdis .EQ. 2) THEN
C
C ----- 2nd-order upwind
C
        zr = (1.-c)*(1.-c)
        zi = s*(2.-c)
C
      ELSE
        PRINT*,' Error - no such spatial discretisation: ',kdis
        STOP
      ENDIF
C
      RETURN
      END
C
C #############################################################################
C
      SUBROUTINE Diffopv( ksi,modeq,zr )
C
C  Fourier symbol of spatial discretization - diffusion term
C  =========================================================
C
      IMPLICIT NONE
C
C ... parameters
      INTEGER modeq
      REAL ksi, zr
C
C *****************************************************************************
C
      IF (modeq .EQ. 1) THEN
        zr = 0.0
      ELSE
        zr = 2.*(COS(ksi)-1.)
      ENDIF
C
      RETURN
      END
C
C #############################################################################
C
      SUBROUTINE Irsop( ksi,ksmoo,esmoo,s )
C
C  Fourier symbol of residual smoothing operator
C  =============================================
C
      IMPLICIT NONE
C
C ... parameters
      COMPLEX s
      INTEGER ksmoo
      REAL ksi, esmoo
C
C ... local variables
      REAL sr, si
C
C *****************************************************************************
C
      IF (ksmoo .EQ. 1) THEN
C
C ----- central IRS
C
        s = 1./(1.+2.*esmoo*(1.-COS(ksi)))
C
      ELSE IF (ksmoo .EQ. 2) THEN
C
C ----- upwind IRS
C
        sr  = 1. + esmoo*(1.-COS(ksi))
        si  = esmoo*SIN(ksi)
        s   = 1./CMPLX(sr,si)
C
      ELSE
        PRINT*,' Error - no such IRS available: ',ksmoo
        STOP
      ENDIF
C
      RETURN
      END
C
C #############################################################################
C
      SUBROUTINE Amplif( ksch,z,a,g )
C
C  Amplification factor
C  ====================
C
      IMPLICIT NONE
C
C ... parameters
      COMPLEX z, g
      INTEGER ksch
      REAL a(5)
C
C ... local variables
      COMPLEX zi, f, b1, p1, p2
      REAL zr, d2, d4, dd4
C
C *****************************************************************************
C
      IF (ksch .EQ. 31) THEN
C
C ---   (3,1)
C
        zi = CMPLX(0.,AIMAG(z))
        f  = a(3) - a(2)*a(3)*zi + a(1)*a(2)*a(3)*zi*zi
C
      ELSE IF (ksch .EQ. 32) THEN
C
C ---   (3,2)
C
        zi = CMPLX(0.,AIMAG(z))
        zr = REAL(z)
        f  = a(3) - a(3)*(a(1)*zr+a(2)*zi) + a(1)*a(2)*a(3)*z*zi
C
      ELSE IF (ksch .EQ. 33) THEN
C
C ---   (3,3)
C
        f = a(3) - a(2)*a(3)*z + a(1)*a(2)*a(3)*z*z
C
      ELSE IF (ksch .EQ. 41) THEN
C
C ---   (4,1)
C
        zi = CMPLX(0.,AIMAG(z))
        f  = a(4) - a(3)*a(4)*zi + a(2)*a(3)*a(4)*zi*zi -
     &       a(1)*a(2)*a(3)*a(4)*zi*zi*zi
C
      ELSE IF (ksch .EQ. 42) THEN
C
C ---   (4,2)
C
        zi = CMPLX(0.,AIMAG(z))
        zr = REAL(z)
        f  = a(4) - a(4)*(a(1)*zr+a(3)*zi) +
     &       a(3)*a(4)*(a(1)*zr+a(2)*zi)*zi -
     &       a(1)*a(2)*a(3)*a(4)*z*zi*zi
C
      ELSE IF (KSCH .EQ. 44) THEN
C
C ---   (4,4)
C
        f = a(4) - a(3)*a(4)*z + a(2)*a(3)*a(4)*z*z -
     &      a(1)*a(2)*a(3)*a(4)*z*z*z
C
      ELSE IF (ksch .EQ. 52) THEN
C
C ---   (5,2)
C
        zi = CMPLX(0.,AIMAG(z))
        zr = REAL(z)
        f  = a(5) - a(5)*(a(1)*zr+a(4)*zi) +
     &       a(4)*a(5)*(a(1)*zr+a(3)*zi)*zi -
     &       a(3)*a(4)*a(5)*(a(1)*zr+a(2)*zi)*zi*zi +
     &       a(1)*a(2)*a(3)*a(4)*a(5)*z*zi*zi*zi
C
      ELSE IF (ksch .EQ. 53) THEN
C
C ---   (5,3) with blended dissipation (hybrid scheme)
C
        zi  = CMPLX(0.,AIMAG(z))
        zr  = REAL(z)
        d2  = 0.56
        d4  = 0.44
        dd4 = 1. - d4
        b1  = a(2)*(1.-a(1)*zi)
        p1  = zi + d4*zr
        p2  = zi + d2*zr
        f   = a(5) - a(4)*a(5)*p1*(1.-a(3)*zi) -
     &        a(4)*a(5)*b1*p1*(a(3)*p2*zi-d2*zr) -
     &        a(5)*dd4*d2*b1*zr
C
      ELSE IF (ksch .EQ. 55) THEN
C
C ---   (5,5)
C
        f = a(5) - a(4)*a(5)*z + a(3)*a(4)*a(5)*z*z -
     &      a(2)*a(3)*a(4)*a(5)*z*z*z +
     &      a(1)*a(2)*a(3)*a(4)*a(5)*z*z*z*z
C
      ELSE
        PRINT*,' Error - no such time-stepping scheme: ',ksch
        STOP
      ENDIF
C
      g = 1. - f*z
C
C
      RETURN
      END

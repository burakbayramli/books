      PROGRAM Implic
C
C *****************************************************************************
C
C   CALCULATION OF AMPLIFICATION FACTOR FOR AN IMPLICIT TIME-STEPPING SCHEME
C   ========================================================================
C
C   Model = 1:     du/dt + du/dx = 0
C
C   Model = 2:     du/dt + du/dx = nue*d^2u/dx^2
C
C *****************************************************************************
C
C   (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
C   Created : October 1992
C   Modified: February 4, 2014
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
      INTEGER mod, kdis_impl, kdis_expl, diffterm
      REAL cfl, ratio, eps_i2, eps_e4, beta
      CHARACTER fnplot*(80)
C
C ... loop variables
      INTEGER i
C
C ... local variables
      CHARACTER title*(84), model*(2), discr_e*(2), discr_i*(2)
      COMPLEX z_impl, z_expl, g
      REAL eps_e4r, eps_i2r, ksi, sn, cs, zr, zi, zrve, zrvi, pi, af
C
C *****************************************************************************
C
      pi = 2.*Asin(1.)
C
      WRITE(*,*) 'AMPLIFICATION FACTOR OF IMPLICIT TIME-STEPPING SCHEME'
      WRITE(*,*) '====================================================='
      WRITE(*,*) ' '
C
      READ(*,'(A80)') fnplot
      READ(*,*) mod
      READ(*,*) kdis_expl
      READ(*,*) eps_e4r
      READ(*,*) kdis_impl
      READ(*,*) eps_i2r
      READ(*,*) beta
      READ(*,*) cfl
      READ(*,*) ratio
      READ(*,*) diffterm
C
      IF (mod .EQ. 1) ratio = 0.
      eps_e4 = eps_e4r
      IF (ABS(eps_e4r) .GE. 1.E-6) eps_e4 = 1./eps_e4r
      eps_i2 = eps_i2r
      IF (ABS(eps_i2r) .GE. 1.E-6) eps_i2 = 1./eps_i2r
C
      IF (mod .EQ. 1) model = 'EU'
      IF (mod .EQ. 2) model = 'NS'
C
      IF (kdis_expl .EQ. 0) discr_e = 'C2'
      IF (kdis_expl .EQ. 1) discr_e = 'U1'
      IF (kdis_expl .EQ. 2) discr_e = 'U2'
C
      IF (kdis_impl .EQ. 0) discr_i = 'C2'
      IF (kdis_impl .EQ. 1) discr_i = 'U1'
      IF (kdis_impl .EQ. 2) discr_i = 'U2'
C
      WRITE(title,1000) model,discr_e,discr_i,beta,
     &                  cfl,ratio,eps_e4r,eps_i2r
C
C --- open Vis2D file
C
      OPEN(10,file=fnplot,form='formatted',status='unknown')
      WRITE(10,1005) title
      WRITE(10,1010)
C
C --- computation of the amplification factor
C
      WRITE(*,*) ' Computing amplification factor ...'
      WRITE(10,1015) 361
C
      DO i=0,360
        ksi = FLOAT(i)*pi/360.
        sn  = SIN(ksi)
        cs  = COS(ksi)
C
        IF (mod .EQ. 1) THEN
          zrve = 0.
        ELSE
          zrve = 2.*ratio*(cs-1.)
        ENDIF
        IF (diffterm .EQ. 0) THEN
          zrvi = 0.
        ELSE
          zrvi = zrve
        ENDIF
C
C ----- implicit operator
C
        IF (kdis_impl .EQ. 0) THEN
          zr = 1./cfl + beta*(2.*eps_i2*(1.-cs)-zrvi)
          zi = beta*sn
        ELSE IF (kdis_impl .EQ. 1) THEN
          zr = 1./cfl + beta*((1.-cs)-zrvi)
          zi = beta*sn
        ELSE
          zr = 1./cfl + beta*((1.-cs)**2-zrvi)
          zi = beta*sn*(2.-cs)
        ENDIF
        z_impl = CMPLX(zr,zi)
C
C ----- explicit operator
C
        IF (kdis_expl .EQ. 0) THEN
          zr = 4.*eps_e4*(1.-cs)**2 - zrve
          zi = sn
        ELSE IF (kdis_expl .EQ. 1) THEN
          zr = 1. - cs - zrve
          zi = sn
        ELSE
          zr = (1.-cs)**2 - zrve
          zi = sn*(2.-cs)
        ENDIF
        z_expl = CMPLX(zr,zi)
C
        g  = 1. - z_expl/z_impl
        af = CABS(g)

        WRITE(10,1020) ksi,af
      ENDDO
C
      CLOSE(10)
      WRITE(*,*) ' Finished.'
C
C
1000  FORMAT(A2,': ',A2,' on RHS, ',A2,' on LHS, beta=',F3.1,', CFL=',
     &       1PE8.2,0P,', LV/LC=',F6.2,', e4=',F6.1,', e2=',F6.1)
1005  FORMAT(A)
1010  FORMAT('1',/,'IMPLIC',/,'1 2',/,'phase angle',/,'|g|')
1015  FORMAT(I5,' 0',/,'0 0 0',/,'amplification factor')
1020  FORMAT(2E13.5)
C
      STOP
      END

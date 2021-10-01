      SUBROUTINE Flux_roe( imax,ib2,a,ls,rs,f,rhs )

C *****************************************************************************
C
C  Computes convective fluxes and the dissipation according to Roe's
C  flux-difference splitting scheme and forms the residual. Source term
C  is added later.
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: Entropy_corr
C
C *****************************************************************************
C
C  Created    : Jul. 25, 1997; (c) Jiri Blazek
C  Last update: Nov. 26, 2004
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
      INCLUDE 'param.inc'

C ... parameter list
      INTEGER imax, ib2
      REAL*8  a(imax), rhs(imax,3)
      REAL*8  ls(imax,3), rs(imax,3), f(imax,3)

C ... local variables
      INTEGER i
      REAL*8  fcav(3), fdiss(3)
      REAL*8  ggm1, si, rl, ul, pl, hl, qrl, rr, ur, pr, hr, qrr,
     &        rav, dd, dd1, uav, hav, q2a, c2a, cav, du, delta,
     &        h1, h2, h3, h5, eabs1, eabs2, eabs3

C ... functions
      REAL*8 Entropy_corr

C *****************************************************************************

      ggm1 = gamma/gam1

      DO i=1,ib2
        si = 0.5D0*(a(i+1)+a(i))

C ----- average of left and right convective fluxes

        rl  = ls(i,1)
        ul  = ls(i,2)
        pl  = ls(i,3)
        hl  = ggm1*pl/rl + 0.5D0*ul*ul
        qrl = ul*rl

        rr  = rs(i,1)
        ur  = rs(i,2)
        pr  = rs(i,3)
        hr  = ggm1*pr/rr + 0.5D0*ur*ur
        qrr = ur*rr

        fcav(1) = qrl    + qrr
        fcav(2) = qrl*ul + qrr*ur + pl + pr
        fcav(3) = qrl*hl + qrr*hr

C ----- dissipative fluxes

        rav   = SQRT(rl*rr)
        dd    = rav/rl
        dd1   = 1.D0/(1.D0+dd)
        uav   = (ul+dd*ur)*dd1
        hav   = (hl+dd*hr)*dd1
        q2a   = 0.5D0*uav*uav
        c2a   = gam1*(hav-q2a)
        cav   = SQRT(c2a)
        du    = ur - ul

        h1    = ABS(uav - cav)
        h2    = ABS(uav)
        h3    = ABS(uav + cav)
        delta = epsentr*cav

        eabs1 = Entropy_corr( h1,delta )
        eabs2 = Entropy_corr( h2,delta )
        eabs3 = Entropy_corr( h3,delta )

        h1 = rav*cav*du
        h2 = eabs1*(pr-pl - h1)/(2.D0*c2a)
        h3 = eabs2*(rr-rl - (pr-pl)/c2a)
        h5 = eabs3*(pr-pl + h1)/(2.D0*c2a)

        fdiss(1) = h2 + h3 + h5
        fdiss(2) = h2*(uav-cav) + h3*uav + h5*(uav+cav)
        fdiss(3) = h2*(hav-cav*uav) + h3*q2a + h5*(hav+cav*uav)

C ----- total fluxes at i+1/2

        f(i,1) = 0.5D0*(fcav(1)-fdiss(1))*si
        f(i,2) = 0.5D0*(fcav(2)-fdiss(2))*si
        f(i,3) = 0.5D0*(fcav(3)-fdiss(3))*si
      ENDDO

C --- sum of fluxes = RHS -----------------------------------------------------

      DO i=2,ib2
        rhs(i,1) = f(i,1) - f(i-1,1)
        rhs(i,2) = f(i,2) - f(i-1,2)
        rhs(i,3) = f(i,3) - f(i-1,3)
      ENDDO

      RETURN
      END

C =============================================================================

      REAL*8 FUNCTION Entropy_corr( z,d )
C
C  Entropy correction function
C
      IMPLICIT NONE

      REAL*8 z, d

      IF (z .GT. d) THEN
        Entropy_corr = z
      ELSE
        Entropy_corr = 0.5D0*(z*z+d*d)/d
      ENDIF

      RETURN
      END

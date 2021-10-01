      SUBROUTINE Flux_cusp( imax,ib2,a,cv,p,ls,rs,f,rhs )

C *****************************************************************************
C
C  Computes convective fluxes and the dissipation according to the H-CUSP
C  flux-vector splitting scheme and forms the residual. Source term
C  is added later.
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
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
      REAL*8  a(imax)
      REAL*8  cv(imax,3), p(imax), rhs(imax,3)
      REAL*8  ls(imax,3), rs(imax,3), f(imax,3)

C ... local variables
      INTEGER i
      REAL*8  fcav(3), fdiss(3)
      REAL*8  ggm1, si, rl, ul, pl, hl, qrl, rr, ur,
     &        pr, hr, qrr, rav, uav, pav, cav, machn, afac, bfac, h1

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

        rav   = 0.5D0*(cv(i+1,1)/a(i+1)+cv(i,1)/a(i))
        uav   = 0.5D0*(cv(i+1,2)/cv(i+1,1)+cv(i,2)/cv(i,1))
        pav   = 0.5D0*(p(i+1)+p(i))
        cav   = SQRT(gamma*pav/rav)
        machn = uav/cav

        IF (machn.GE.0.D0 .AND. machn.LT.1.D0) THEN
          h1   = 2.D0*machn - 1.D0
          bfac = MAX(0.D0,h1)
        ELSE IF (machn.GT.-1.D0 .AND. machn.LT.0.D0) THEN
          h1   = 2.D0*machn + 1.D0
          bfac = MIN(0.D0,h1)
        ELSE
          bfac = SIGN(1.D0,machn)
        ENDIF
        afac = ABS(uav) - bfac*uav

        fdiss(1) = afac*(rr   -rl   ) + bfac*(qrr   -qrl)
        fdiss(2) = afac*(rr*ur-rl*ul) + bfac*(qrr*ur-qrl*ul+pr-pl)
        fdiss(3) = afac*(rr*hr-rl*hl) + bfac*(qrr*hr-qrl*hl)

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

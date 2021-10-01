      SUBROUTINE Bcond( imax,ib2,a,cv,p )

C *****************************************************************************
C
C  Sets the boundary conditions at the inlet and outlet (in dummy points).
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
C  Last update: Dec. 22, 1999
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
      REAL*8 a(imax), cv(imax,3), p(imax)

C ... local variables
      REAL*8 u, cs2, c02, rinv, dis, cb, cc02,
     &       tb, pb, rhob, rho, cs, ub

C *****************************************************************************
C --- inlet (subsonic):
C
C     - speed of sound from Riemann invariant
C     - temperature from isentropic relation
C     - pressure from isentropic relation
C     - density from gas equation
C     - velocity from energy equation

      u    = cv(2,2)/cv(2,1)
      cs2  = gamma*p(2)*a(2)/cv(2,1)
      c02  = cs2 + 0.5D0*gam1*u*u
      rinv = u - 2.D0*SQRT(cs2)/gam1
      dis  = gap1*c02/(gam1*rinv*rinv) - 0.5D0*gam1
      IF (dis .LT. 0.D0) THEN
        WRITE(*,1000) dis
        dis = 1.D-20
      ENDIF
      cb   = -rinv*(gam1/gap1)*(1.D0+SQRT(dis))
      cc02 = MIN(cb*cb/c02, 1.D0)
      tb   = t01*cc02
      pb   = p01*(tb/t01)**(gamma/gam1)
      rhob = pb/(rgas*tb)
      ub   = SQRT(2.D0*cpgas*(t01-tb))

      cv(1,1) = rhob*a(2)
      cv(1,2) = rhob*a(2)*ub
      cv(1,3) = (pb/gam1+0.5D0*rhob*ub*ub)*a(2)
      p(1)    = pb

C --- outlet - subsonic:
C
C     - pressure = given back pressure
C     - density from characteristic b.c.
C     - velocity from characteristic b.c.
C
C --- outlet - supersonic:
C
C     - pressure extrapolated from the interior
C     - density extrapolated from the interior
C     - velocity extrapolated from the interior

      rho = cv(ib2,1)/a(ib2)
      u   = cv(ib2,2)/cv(ib2,1)
      cs  = SQRT(gamma*p(ib2)/rho)

      IF (u .GE. cs) THEN
C ----- supersonic flow
        pb   = p(ib2)
        rhob = rho
        ub   = u
      ELSE
C ----- subsonic flow
        pb   = p2
        rhob = rho + (p2-p(ib2))/(cs*cs)
        ub   = u   - (p2-p(ib2))/(cs*rho)
      ENDIF

      cv(imax,1) = rhob*a(ib2)
      cv(imax,2) = rhob*ub*a(ib2)
      cv(imax,3) = (pb/gam1+0.5D0*rhob*ub*ub)*a(ib2)
      p (imax)   = pb

1000  FORMAT(' Warning (bcond.f) - discriminant<0 at inflow boundary',
     &       ' (',1PE13.5,')')
      RETURN
      END

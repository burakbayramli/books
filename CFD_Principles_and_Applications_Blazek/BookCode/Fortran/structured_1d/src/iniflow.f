      SUBROUTINE Iniflow( imax,a,cv,p )

C *****************************************************************************
C
C  Initializes the flow field using total pressure and temperature (inlet),
C  and static pressure (outlet). Initializes reference values of the limiter
C  (CUSP scheme or Roe's flux-difference splitting scheme).
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
C  Last update: Nov. 25, 2004
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
      INTEGER imax
      REAL*8  a(imax), cv(imax,3), p(imax)

C ... local variables
      INTEGER i
      REAL*8  temp, rho, mach, cs, u, mass, e

C *****************************************************************************

      gam1 = gamma - 1.D0
      gap1 = gamma + 1.D0
      rgas = gam1*cpgas/gamma
      temp = t01 * (p2/p01)**(gam1/gamma)
      rho  = p2/(rgas*temp)
      mach = SQRT(2.D0*((t01/temp)-1.D0)/gam1)
      cs   = SQRT(gamma*p2/rho)
      u    = cs*mach
      mass = rho*u*a(2)
      e    = (cpgas-rgas)*t01

C --- flow field

      DO i=1,imax
        cv(i,1) = rho   * a(i)
        cv(i,2) = mass
        cv(i,3) = rho*e * a(i)
        p (i)   = p2
      ENDDO

C --- limiter reference values

      volref = 1.D0    ! characteristic length**2
      rhoref = rho
      uref   = u
      pref   = p2

      RETURN
      END

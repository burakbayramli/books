      SUBROUTINE Irsmoo( imax,ib2,rhs,d )

C *****************************************************************************
C
C  Conducts implicit residual smoothing (based on central operator).
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
C  Last update: Dec. 29, 2004
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
      INCLUDE 'numer.inc'

C ... parameter list
      INTEGER imax, ib2
      REAL*8  rhs(imax,3), d(imax)

C ... local variables
      INTEGER i, ii
      REAL*8  eps2, t

C *****************************************************************************

      eps2 = 2.D0*epsirs + 1.D0
      d(1) = 0.D0

C --- elimination step

      DO i=2,ib2
        t        = 1.D0/(eps2-epsirs*d(i-1))
        d(i)     = t*epsirs
        rhs(i,1) = t*(rhs(i,1) + epsirs*rhs(i-1,1))
        rhs(i,2) = t*(rhs(i,2) + epsirs*rhs(i-1,2))
        rhs(i,3) = t*(rhs(i,3) + epsirs*rhs(i-1,3))
      ENDDO

C --- backward substitution

      i = ib2
      DO ii=3,ib2
        i        = i - 1
        rhs(i,1) = rhs(i,1) + d(i)*rhs(i+1,1)
        rhs(i,2) = rhs(i,2) + d(i)*rhs(i+1,2)
        rhs(i,3) = rhs(i,3) + d(i)*rhs(i+1,3)
      ENDDO

      RETURN
      END

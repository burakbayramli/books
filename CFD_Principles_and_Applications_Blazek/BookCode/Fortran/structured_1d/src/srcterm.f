      SUBROUTINE Srcterm( imax,ib2,a,p,rhs )

C *****************************************************************************
C
C  Computes source term and adds it to the right-hand side.
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
C  Last update: Feb. 28, 1999
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
      INTEGER imax, ib2
      REAL*8  a(imax), p(imax), rhs(imax,3)

C ... local variables
      INTEGER i
      REAL*8  da

C *****************************************************************************

      DO i=2,ib2
        da       = 0.5D0*(a(i+1)-a(i-1))
        rhs(i,2) = rhs(i,2) - p(i)*da
      ENDDO

      RETURN
      END

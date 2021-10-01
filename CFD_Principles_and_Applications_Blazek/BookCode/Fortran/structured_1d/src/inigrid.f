      SUBROUTINE Inigrid( imax,ib2,x,a,vol )

C *****************************************************************************
C
C  Computes control volumes.
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
      REAL*8  x(imax), a(imax), vol(imax)

C ... local variables
      INTEGER i
      REAL*8  da

C *****************************************************************************

      DO i=2,ib2
        da     = a(i-1) + 2.D0*a(i) + a(i+1)
        vol(i) = (x(i+1)-x(i-1))*da/8.D0
      ENDDO
      vol(1   ) = vol(2  )
      vol(imax) = vol(ib2)

      RETURN
      END

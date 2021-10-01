      SUBROUTINE Wsolut( imax,cv,p )

C *****************************************************************************
C
C  Stores the solution for restart.
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
      INCLUDE 'param.inc'

C ... parameter list
      INTEGER imax
      REAL*8  cv(imax,3), p(imax)

C ... local variables
      INTEGER i

C *****************************************************************************

      OPEN(40,file=fnrest,form='unformatted',status='unknown')
      WRITE(40) (cv(i,1),cv(i,2),cv(i,3), i=1,imax)
      WRITE(40) (p(i), i=1,imax)
      CLOSE(40)

      RETURN
      END

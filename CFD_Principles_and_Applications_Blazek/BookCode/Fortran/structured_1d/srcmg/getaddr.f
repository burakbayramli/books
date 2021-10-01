      INTEGER FUNCTION Get_addr( nlevels,ilev,neqs,imax )

C *****************************************************************************
C
C  Returns the start address of the grid level "ilev" within a vector
C  containing "neqs" equations.
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************
C
C  Created    : Dec. 28, 2004; (c) Jiri Blazek
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

C ... parameter list
      INTEGER nlevels, ilev, neqs
      INTEGER imax(nlevels)

C ... local variables
      INTEGER i

C *****************************************************************************

      Get_addr = 1

      IF (ilev .GT. 1) THEN
        DO i=1,ilev-1
          Get_addr = Get_addr + neqs*imax(i)
        ENDDO
      ENDIF

      RETURN
      END

      INTEGER FUNCTION Length( str )

C *****************************************************************************
C
C  Delivers length of the string 'str' (without trailing blanks).
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************
C
C  Created    : Sep. 20, 1997; (c) Jiri Blazek
C  Last update: Sep. 20, 1997
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
      CHARACTER*(*) str

C ... loop variables
      INTEGER i

C *****************************************************************************

      Length = 0
      DO i=1,LEN(str)
        IF (str(i:i) .NE. ' ') Length = i
      ENDDO

      RETURN
      END

      SUBROUTINE Restrict( imaxf,ib2f,rhs,imaxc,ib2c,fterm )

C *****************************************************************************
C
C  Restricts the residual (+ forcing function) from fine to the next
C  coarse level. The result is stored as the forcing term for the
C  coarse level.
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
C  Last update: Jan. 04, 2005
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
      INTEGER imaxf, ib2f, imaxc, ib2c
      REAL*8  rhs(imaxf,3), fterm(imaxc,3)

C ... local variables
      INTEGER i, ii

C *****************************************************************************

      DO i=2,ib2c
        ii         = 2*i - 2
        fterm(i,1) = 0.5D0*(rhs(ii-1,1)+rhs(ii+1,1)) + rhs(ii,1)
        fterm(i,2) = 0.5D0*(rhs(ii-1,2)+rhs(ii+1,2)) + rhs(ii,2)
        fterm(i,3) = 0.5D0*(rhs(ii-1,3)+rhs(ii+1,3)) + rhs(ii,3)
      ENDDO

C --- zero out at dummy cells and boundaries

      fterm(1    ,1) = 0.D0
      fterm(1    ,2) = 0.D0
      fterm(1    ,3) = 0.D0
      fterm(imaxc,1) = 0.D0
      fterm(imaxc,2) = 0.D0
      fterm(imaxc,3) = 0.D0

      fterm(2   ,1) = 0.D0
      fterm(2   ,2) = 0.D0
      fterm(2   ,3) = 0.D0
      fterm(ib2c,1) = 0.D0
      fterm(ib2c,2) = 0.D0
      fterm(ib2c,3) = 0.D0

      RETURN
      END

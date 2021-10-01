      SUBROUTINE Inject( imaxf,ib2f,cvf,imaxc,ib2c,ac,cvc,pc )

C *****************************************************************************
C
C  Injects the solution from fine to the next coarse level.
C
C *****************************************************************************
C
C  Subroutines called: Bcond
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
      INCLUDE 'param.inc'

C ... parameter list
      INTEGER imaxf, ib2f, imaxc, ib2c
      REAL*8  ac(imaxc)
      REAL*8  cvf(imaxf,3), cvc(imaxc,3), pc(imaxc)

C ... local variables
      INTEGER i
      REAL*8  rrho, rhou, rhoe

C *****************************************************************************
C --- inject solution to coarser grid

      DO i=2,ib2c
        cvc(i,1) = cvf(2*i-2,1)
        cvc(i,2) = cvf(2*i-2,2)
        cvc(i,3) = cvf(2*i-2,3)
      ENDDO
      cvc(1    ,1) = cvf(1,1)
      cvc(1    ,2) = cvf(1,2)
      cvc(1    ,3) = cvf(1,3)
      cvc(imaxc,1) = cvf(imaxf,1)
      cvc(imaxc,2) = cvf(imaxf,2)
      cvc(imaxc,3) = cvf(imaxf,3)

C --- update pressure on coarser grid

      DO i=1,imaxc
        rrho  = ac(i)/cvc(i,1)
        rhou  = cvc(i,2)/ac(i)
        rhoe  = cvc(i,3)/ac(i)
        pc(i) = gam1*(rhoe-0.5D0*rhou*rhou*rrho)
      ENDDO

      RETURN
      END

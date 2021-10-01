      SUBROUTINE Interpolate( imaxc,ib2c,ac,cvc,
     &                        imaxf,ib2f,af,cvf,pf )

C *****************************************************************************
C
C  Interpolates the solution from coarse to the next fine level.
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
C  Last update: Jan. 08, 2005
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
      INTEGER imaxc, ib2c, imaxf, ib2f
      REAL*8  ac(imaxc), af(imaxf)
      REAL*8  cvc(imaxc,3), cvf(imaxf,3), pf(imaxf)

C ... local variables
      INTEGER i, ii
      REAL*8  rho, rrho, rhou, rhoe

C *****************************************************************************
C --- initialize cvf to 0

      DO i=1,imaxf
        cvf(i,1) = 0.D0
        cvf(i,2) = 0.D0
        cvf(i,3) = 0.D0
      ENDDO

C --- interpolate solution to finer grid (without areas A!)

      DO i=2,ib2c
        ii   = 2*i - 2
        rho  = cvc(i,1)/ac(i)
        rhou = cvc(i,2)/ac(i)
        rhoe = cvc(i,3)/ac(i)

        cvf(ii-1,1) = cvf(ii-1,1) + 0.5D0*rho
        cvf(ii-1,2) = cvf(ii-1,2) + 0.5D0*rhou
        cvf(ii-1,3) = cvf(ii-1,3) + 0.5D0*rhoe
        cvf(ii  ,1) = rho
        cvf(ii  ,2) = rhou
        cvf(ii  ,3) = rhoe
        cvf(ii+1,1) = cvf(ii+1,1) + 0.5D0*rho
        cvf(ii+1,2) = cvf(ii+1,2) + 0.5D0*rhou
        cvf(ii+1,3) = cvf(ii+1,3) + 0.5D0*rhoe
      ENDDO

C --- update pressure and dummy points on finer grid; multiply W by A

      DO i=2,ib2f
        rrho  = 1.D0/cvf(i,1)
        rhou  = cvf(i,2)
        rhoe  = cvf(i,3)
        pf(i) = gam1*(rhoe-0.5D0*rhou*rhou*rrho)

        cvf(i,1) = cvf(i,1)*af(i)
        cvf(i,2) = cvf(i,2)*af(i)
        cvf(i,3) = cvf(i,3)*af(i)
      ENDDO

      CALL Bcond( imaxf,ib2f,af,cvf,pf )

      RETURN
      END

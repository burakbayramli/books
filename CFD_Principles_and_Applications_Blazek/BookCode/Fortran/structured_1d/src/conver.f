      SUBROUTINE Conver( imax,ib2,a,cv,cvold,p )

C *****************************************************************************
C
C  Monitors the residual and prints it out.
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
      INTEGER imax, ib2
      REAL*8  a(imax), cv(imax,3), cvold(imax,3), p(imax)

C ... local variables
      INTEGER i, idrho, nsup
      REAL*8  dr, drmax, rho, u, c, avms

C *****************************************************************************
C --- get the change of density and the mass flow

      drho  = 0.D0
      drmax = -1.D+20
      avms  = 0.D0
      nsup  = 0

      DO i=2,ib2
        dr   = cv(i,1) - cvold(i,1)
        avms = avms + cv(i,2)
        drho = drho + dr**2
        IF (ABS(dr) .GE. drmax) THEN
          drmax = ABS(dr)
          idrho = i
        ENDIF
        rho = cv(i,1)/a(i)
        u   = cv(i,2)/cv(i,1)
        c   = SQRT(gamma*p(i)/rho)
        IF (u .GT. c) nsup = nsup + 1
      ENDDO
      avms = avms/DBLE(ncells+1)

C --- print convergence history

      IF (iter .EQ. 1) drho1 = SQRT(drho/DBLE(ncells+1))
      drho = SQRT(drho/DBLE(ncells+1))/drho1

      WRITE( *,1000) iter,LOG10(drho),drmax,idrho,avms,
     &               cv(2,2)-cv(ib2,2),nsup
      WRITE(30,1000) iter,LOG10(drho),drmax,idrho,avms,
     &               cv(2,2)-cv(ib2,2),nsup

1000  FORMAT(1X,I6,2X,1P,E12.5,2X,E12.5,2X,I5,2X,E12.5,2X,E12.5,2X,I5)
      RETURN
      END

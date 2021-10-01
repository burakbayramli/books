      SUBROUTINE Dissip( imax,ib2,beta,vol,cv,p,dt,dp,d,diss )

C *****************************************************************************
C
C  Computes scalar artificial dissipation (JST-scheme).
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
      REAL*8  beta
      REAL*8  vol(imax), cv(imax,3), p(imax), dt(imax), diss(imax,3)
      REAL*8  dp(imax), d(imax,3)

C ... local variables
      INTEGER i, im1, ip1, ip2
      REAL*8  eval, pmax, eps2, eps4, beta1

C *****************************************************************************
C --- pressure sensor (divided second differences)

      DO i=2,ib2
        dp(i) = ABS((p(i+1)-2.D0*p(i)+p(i-1))/
     &              (p(i+1)+2.D0*p(i)+p(i-1)))
      ENDDO
      dp(1   ) = dp(2  )
      dp(imax) = dp(ib2)

C --- dissipation fluxes (at i+1/2)

      DO i=1,ib2
        im1    = MAX(i-1,   1)
        ip1    =     i+1
        ip2    = MIN(i+2,imax)
        eval   = 0.5D0*(vol(i)/dt(i)+vol(ip1)/dt(ip1))
        pmax   = MAX(dp(i),dp(ip1))
        eps2   = eval*vis2*pmax
        eps4   = eval*vis4
        eps4   = DIM(eps4,eps2)
        d(i,1) = eps2*(cv(ip1,1)-cv(i,1)) -
     &           eps4*(cv(ip2,1)-3.D0*cv(ip1,1)+3.D0*cv(i,1)-cv(im1,1))
        d(i,2) = eps2*(cv(ip1,2)-cv(i,2)) -
     &           eps4*(cv(ip2,2)-3.D0*cv(ip1,2)+3.D0*cv(i,2)-cv(im1,2))
        d(i,3) = eps2*(cv(ip1,3)-cv(i,3)) -
     &           eps4*(cv(ip2,3)-3.D0*cv(ip1,3)+3.D0*cv(i,3)-cv(im1,3))
      ENDDO

C --- dissipation term

      beta1 = 1.D0 - beta
      DO i=2,ib2
        diss(i,1) = beta*(d(i,1)-d(i-1,1)) + beta1*diss(i,1)
        diss(i,2) = beta*(d(i,2)-d(i-1,2)) + beta1*diss(i,2)
        diss(i,3) = beta*(d(i,3)-d(i-1,3)) + beta1*diss(i,3)
      ENDDO

      RETURN
      END

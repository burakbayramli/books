      SUBROUTINE LR_state_cusp( imax,ib2,a,vol,cv,p,du,ls,rs )

C *****************************************************************************
C
C  Computes left and right states using limiter formulated
C  for the CUSP scheme.
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: CUSPLIM
C
C *****************************************************************************
C
C  Created    : Jul. 25, 1997; (c) Jiri Blazek
C  Last update: Jan. 15, 2005
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
      INCLUDE 'numer.inc'

C ... parameter list
      INTEGER imax, ib2
      REAL*8  a(imax), vol(imax), cv(imax,3), p(imax)
      REAL*8  du(0:imax,3), ls(imax,3), rs(imax,3)

C ... local variables
      INTEGER i
      REAL*8  delt(3), af, bf

C ... functions
      REAL*8 CUSPLIM

C *****************************************************************************
C --- limiter functions
C
C     CUSPLIM = original CUSP (SLIP) limiter (Eq. (4.121))

      CUSPLIM(af,bf) = 1.D0 - ((af-bf)/(ABS(af)+ABS(bf)+1.D-20))**2

C --- first differences of rho, u, p

      DO i=1,ib2
        du(i,1) = cv(i+1,1)/a(i+1)    - cv(i,1)/a(i)
        du(i,2) = cv(i+1,2)/cv(i+1,1) - cv(i,2)/cv(i,1)
        du(i,3) = p(i+1)              - p(i)
      ENDDO
      du(0   ,1) = du(1  ,1)
      du(0   ,2) = du(1  ,2)
      du(0   ,3) = du(1  ,3)
      du(imax,1) = du(ib2,1)
      du(imax,2) = du(ib2,2)
      du(imax,3) = du(ib2,3)

C --- left / right state

      DO i=1,ib2
        delt(1) = 0.25D0*(du(i+1,1)+du(i-1,1))*
     &                   CUSPLIM( du(i+1,1),du(i-1,1) )
        delt(2) = 0.25D0*(du(i+1,2)+du(i-1,2))*
     &                   CUSPLIM( du(i+1,2),du(i-1,2) )
        delt(3) = 0.25D0*(du(i+1,3)+du(i-1,3))*
     &                   CUSPLIM( du(i+1,3),du(i-1,3) )
        rs(i,1)  = cv(i+1,1)/a(i+1)    - delt(1)
        rs(i,2)  = cv(i+1,2)/cv(i+1,1) - delt(2)
        rs(i,3)  = p(i+1)              - delt(3)
        ls(i,1)  = cv(i  ,1)/a(i  )    + delt(1)
        ls(i,2)  = cv(i  ,2)/cv(i  ,1) + delt(2)
        ls(i,3)  = p(i  )              + delt(3)
      ENDDO

      RETURN
      END

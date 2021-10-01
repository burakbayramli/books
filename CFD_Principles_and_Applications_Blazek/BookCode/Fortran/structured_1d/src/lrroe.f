      SUBROUTINE LR_state_roe( imax,ib2,a,vol,cv,p,du,ls,rs )

C *****************************************************************************
C
C  Computes (limited) left and right states using the MUSCL approach
C  (suitable for Roe's upwind scheme).
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: MUSCL0, MUSCL3
C
C *****************************************************************************
C
C  Created    : Jul. 25, 1997; (c) Jiri Blazek
C  Last update: Nov. 26, 2004
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
      REAL*8  a(imax), vol(imax), cv(imax,3), p(imax)
      REAL*8  du(0:imax,3), ls(imax,3), rs(imax,3)

C ... local variables
      INTEGER i
      REAL*8  eps2(3), deltl(3), deltr(3)
      REAL*8  limfac3, rvolref, vola, eps2n

C ... functions
      REAL*8 MUSCL0, MUSCL3
      REAL*8 af, bf, eps

C *****************************************************************************
C --- limiter functions
C
C     MUSCL0 = MUSCL scheme with kappa=0   (Eq. (4.114))
C     MUSCL3 = MUSCL scheme with kappa=1/3 (Eq. (4.117))

      MUSCL0(af,bf,eps) = (af*(bf*bf+eps)+bf*(af*af+eps))/
     &                    (af*af+bf*bf+2.D0*eps+1.D-30)
      MUSCL3(af,bf,eps) = (bf*(2.D0*af*af+eps)+af*(bf*bf+2.D0*eps))/
     &                    (2.D0*af*af+2.D0*bf*bf-af*bf+3.D0*eps+1.D-30)

C --- normalised epsilon^2 for all limited variables (rho, u, p)

      IF (iorder .GE. 2) THEN
        limfac3 = limfac*limfac*limfac
        rvolref = 1.D0/volref**1.5D0
        eps2(1) = limfac3*rhoref*rhoref*rvolref     ! rho
        eps2(2) = limfac3*uref*uref    *rvolref     ! u
        eps2(3) = limfac3*pref*pref    *rvolref     ! p
      ENDIF

C --- first differences of rho, u, p

      IF (iorder .GE. 2) THEN
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
      ENDIF

C --- left / right state

      IF (iorder .EQ. 2) THEN         ! kappa = 0
        DO i=1,ib2
          vola     = (0.5D0*(vol(i+1)+vol(i)))**1.5D0
          eps2n    = eps2(1)*vola
          deltr(1) = 0.5D0*MUSCL0( du(i+1,1),du(i  ,1),eps2n )
          deltl(1) = 0.5D0*MUSCL0( du(i  ,1),du(i-1,1),eps2n )
          eps2n    = eps2(2)*vola
          deltr(2) = 0.5D0*MUSCL0( du(i+1,2),du(i  ,2),eps2n )
          deltl(2) = 0.5D0*MUSCL0( du(i  ,2),du(i-1,2),eps2n )
          eps2n    = eps2(3)*vola
          deltr(3) = 0.5D0*MUSCL0( du(i+1,3),du(i  ,3),eps2n )
          deltl(3) = 0.5D0*MUSCL0( du(i  ,3),du(i-1,3),eps2n )
          rs(i,1)  = cv(i+1,1)/a(i+1)    - deltr(1)
          rs(i,2)  = cv(i+1,2)/cv(i+1,1) - deltr(2)
          rs(i,3)  = p(i+1)              - deltr(3)
          ls(i,1)  = cv(i  ,1)/a(i  )    + deltl(1)
          ls(i,2)  = cv(i  ,2)/cv(i  ,1) + deltl(2)
          ls(i,3)  = p(i  )              + deltl(3)
        ENDDO
      ELSE IF (iorder .EQ. 3) THEN    ! kappa = 1/3
        DO i=1,ib2
          vola     = (0.5D0*(vol(i+1)+vol(i)))**1.5D0
          eps2n    = eps2(1)*vola
          deltr(1) = 0.5D0*MUSCL3( du(i+1,1),du(i  ,1),eps2n )
          deltl(1) = 0.5D0*MUSCL3( du(i  ,1),du(i-1,1),eps2n )
          eps2n    = eps2(2)*vola
          deltr(2) = 0.5D0*MUSCL3( du(i+1,2),du(i  ,2),eps2n )
          deltl(2) = 0.5D0*MUSCL3( du(i  ,2),du(i-1,2),eps2n )
          eps2n    = eps2(3)*vola
          deltr(3) = 0.5D0*MUSCL3( du(i+1,3),du(i  ,3),eps2n )
          deltl(3) = 0.5D0*MUSCL3( du(i  ,3),du(i-1,3),eps2n )
          rs(i,1)  = cv(i+1,1)/a(i+1)    - deltr(1)
          rs(i,2)  = cv(i+1,2)/cv(i+1,1) - deltr(2)
          rs(i,3)  = p(i+1)    - deltr(3)
          ls(i,1)  = cv(i  ,1)/a(i  )    + deltl(1)
          ls(i,2)  = cv(i  ,2)/cv(i  ,1) + deltl(2)
          ls(i,3)  = p(i  )    + deltl(3)
        ENDDO
      ELSE                            ! 1st order
        DO i=1,ib2
          rs(i,1) = cv(i+1,1)/a(i+1)
          rs(i,2) = cv(i+1,2)/cv(i+1,1)
          rs(i,3) = p(i+1)
          ls(i,1) = cv(i,1)/a(i)
          ls(i,2) = cv(i,2)/cv(i,1)
          ls(i,3) = p(i)
        ENDDO
      ENDIF

      RETURN
      END

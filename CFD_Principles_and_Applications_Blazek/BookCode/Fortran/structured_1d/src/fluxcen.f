      SUBROUTINE Flux_cen( imax,ib2,a,cv,p,diss,f,rhs )

C *****************************************************************************
C
C  Computes convective fluxes (central) and adds them to the scalar artificial
C  dissipation in order to form the residual (source term is included later).
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

C ... parameter list
      INTEGER imax, ib2
      REAL*8  a(imax), cv(imax,3), p(imax), diss(imax,3), rhs(imax,3)
      REAL*8  f(imax,3)

C ... local variables
      INTEGER i
      REAL*8  si, rav, ruav, reav, pav, rhav, qs

C *****************************************************************************
C --- flux term (average of variables)

      DO i=1,ib2
        si   = 0.5D0*(a(i+1)+a(i))
        rav  = 0.5D0*(cv(i+1,1)/a(i+1)+cv(i,1)/a(i))
        ruav = 0.5D0*(cv(i+1,2)/a(i+1)+cv(i,2)/a(i))
        reav = 0.5D0*(cv(i+1,3)/a(i+1)+cv(i,3)/a(i))
        pav  = 0.5D0*(p(i+1)+p(i))
        rhav = reav + pav
        qs   = ruav*si/rav

        f(i,1) = rav *qs
        f(i,2) = ruav*qs + pav*si
        f(i,3) = rhav*qs
      ENDDO

C --- flux + dissipation = RHS

      DO i=2,ib2
        rhs(i,1) = f(i,1) - f(i-1,1) - diss(i,1)
        rhs(i,2) = f(i,2) - f(i-1,2) - diss(i,2)
        rhs(i,3) = f(i,3) - f(i-1,3) - diss(i,3)
      ENDDO

      RETURN
      END

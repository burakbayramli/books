      SUBROUTINE Output( imax,ib2,x,a,cv,p )

C *****************************************************************************
C
C  Writes out the solution (in Vis2d format).
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
C  Last update: Feb. 19, 2014
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
      REAL*8  x(imax), a(imax), cv(imax,3), p(imax)

C ... local variables
      INTEGER i
      REAL*8  rho, u, temp, c, mach

C *****************************************************************************

      OPEN(20,file=fnplot,form='formatted',status='unknown')
      WRITE(20,1000) ib2-1
      DO i=2,ib2
        rho  = cv(i,1)/a(i)
        u    = cv(i,2)/cv(i,1)
        temp = p(i)/(rgas*rho)
        c    = SQRT(gamma*p(i)/rho)
        mach = u/c
        WRITE(20,1010) x(i),a(i),rho,u,p(i),temp,mach,cv(i,2)
      ENDDO
      CLOSE(20)

1000  FORMAT('Flow in a nozzle',/,'1',/,'Results',/,'1 8',/,
     &       'x [m]',/,'A [m]',/,'rho [kg/m^3]',/,'u [m/s]',/,
     &       'p [Pa]',/,'T [K]',/,'Mach',/,'mass flow [kg/s]',/,
     &       I4,' 0',/,'0 0 0',/,'Nozzle')
1010  FORMAT(8E14.6)

      RETURN
      END

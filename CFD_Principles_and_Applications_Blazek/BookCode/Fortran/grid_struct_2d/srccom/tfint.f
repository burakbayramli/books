      SUBROUTINE Tfint( im,jm,is,ie,js,je,
     &                  s1,s2,s3,s4,
     &                  x,y )

C *****************************************************************************
C
C  Generates grid inside a domain (is-ie, js-je) using linear transfinite
C  interpolation:
C
C               4 (j=je)
C           ----------------
C           |              |
C  1 (i=is) |              | 2 (i=ie)
C           |              |
C           ----------------
C               3 (j=js)
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************
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
      INTEGER im, jm, is, ie, js, je
      REAL*8 s1(*), s2(*), s3(*), s4(*)
      REAL*8 x(im,jm), y(im,jm)

C ... loop variables
      INTEGER i, j

C ... local variables
      INTEGER is1, ie1, js1, je1, ii, jj
      REAL*8 a1, a2, b1, b2, phij1, phij2, phii1, phii2

C *****************************************************************************

      is1 = is + 1
      js1 = js + 1
      ie1 = ie - 1
      je1 = je - 1

C --- distances of points along boundaries

      s1(js) = 0.D0
      s2(js) = 0.D0
      s3(is) = 0.D0
      s4(is) = 0.D0

      DO i=is1,ie              ! boundaries 3 & 4
        ii    = i - 1
        s3(i) = s3(ii) +
     &           SQRT( (x(i,js)-x(ii,js))**2+(y(i,js)-y(ii,js))**2 )
        s4(i) = s4(ii) +
     &           SQRT( (x(i,je)-x(ii,je))**2+(y(i,je)-y(ii,je))**2 )
      ENDDO
      DO i=is1,ie
        s3(i) = s3(i)/s3(ie)
        s4(i) = s4(i)/s4(ie)
      ENDDO

      DO j=js1,je              ! boundaries 1 & 2
        jj    = j - 1
        s1(j) = s1(jj) +
     &           SQRT( (x(is,j)-x(is,jj))**2+(y(is,j)-y(is,jj))**2 )
        s2(j) = s2(jj) +
     &           SQRT( (x(ie,j)-x(ie,jj))**2+(y(ie,j)-y(ie,jj))**2 )
      ENDDO
      DO j=js1,je
        s1(j) = s1(j)/s1(je)
        s2(j) = s2(j)/s2(je)
      ENDDO

C --- interpolation

      DO j=js1,je1
        DO i=is1,ie1
          a1     = s1(j) - s2(j)
          a2     = s3(i) - s4(i)
          b1     = s1(j)
          b2     = s3(i)
          phij1  = (b1-a1*b2)/(1.D0-a1*a2)
          phij2  = 1.D0 - phij1
          phii1  = (b2-a2*b1)/(1.D0-a1*a2)
          phii2  = 1.D0 - phii1
          x(i,j) =       phij2*x( i,js) +       phij1*x( i,je) +
     &                   phii2*x(is,j ) +       phii1*x(ie, j) -
     &             phij2*phii2*x(is,js) - phij1*phii1*x(ie,je) -
     &             phij2*phii1*x(ie,js) - phij1*phii2*x(is,je)
          y(i,j) =       phij2*y( i,js) +       phij1*y( i,je) +
     &                   phii2*y(is,j ) +       phii1*y(ie, j) -
     &             phij2*phii2*y(is,js) - phij1*phii1*y(ie,je) -
     &             phij2*phii1*y(ie,js) - phij1*phii2*y(is,je)
        ENDDO
      ENDDO

      RETURN
      END

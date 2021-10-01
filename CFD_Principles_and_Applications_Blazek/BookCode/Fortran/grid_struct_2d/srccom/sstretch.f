      SUBROUTINE Sstretch( is,i1,i2,ie,xs,x1,x2,xe,dxs,dxe,x )

C *****************************************************************************
C
C  Produces an "S-type" function for stretching a coordinate
C  at both ends of an interval. A separate geometric progression
C  is used at each end to give either a decreasing or increasing
C  grid density. The matching in the interior is done by a cubic
C  polynomial.
C
C  is,ie = first & last index of the whole interval (in x)
C  i1,i2 = (is-i1), (i2-ie) stretching intervals at both ends
C  xs,xe = first & last coordinate of the whole interval
C  x1,x2 = (xs-x1), (x2-xe) stretching intervals at the ends
C  dxs   = spacing at the first coordinate
C  dxe   = spacing at the last coordinate
C  x     = point distribution (starting at xs, ending at xe)
C
C *****************************************************************************
C
C  Subroutines called: Stretch
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
      INTEGER is, i1, i2, ie
      REAL*8 xs, x1, x2, xe, dxs, dxe, x(*)

C ... loop variables
      INTEGER i

C ... local variables
      INTEGER isp1, iemi2
      REAL*8 dx, rs, re, dx1, dx2, x1p, x2p, di12, di1

C *****************************************************************************

      isp1  = is + 1
      iemi2 = ie - i2

C --- starting sector

      dx = dxs
      CALL Stretch( dx,x1-xs,i1-is,rs )
      x(is) = xs
      DO i=isp1,i1
        x(i) = x(i-1) + dx
        dx   = rs*dx
      ENDDO

C --- final sector (reverse order)

      dx = -dxe
      CALL Stretch( dx,x2-xe,ie-i2,re )
      x(ie) = xe
      DO i=1,iemi2
        x(ie-i) = x(ie+1-i) + dx
        dx      = re*dx
      ENDDO

C --- mid-sector (matching cubic polynomial)

      dx1  = x(i1) - x(i1-1)
      dx2  = x(i2+1) - x(i2)
      x1p  = dx1*rs*LOG(rs)/(rs-1.D0)
      x2p  = dx2*re*LOG(re)/(re-1.D0)
      di12 = i2 - i1
      DO i=i1,i2
        di1  = REAL(i-i1)
        x(i) = x1 + x1p*di1 + (3.D0*(x2-x1)-(x2p+2.D0*x1p)*di12)
     &                *(di1/di12)**2
     &            - (2.D0*(x2-x1)-(x2p+x1p)*di12)
     &                *(di1/di12)**3
      ENDDO

      RETURN
      END

      SUBROUTINE Get_params( ilev )

C *****************************************************************************
C
C  Writes actual values into the common block with numerical parameters
C  "numer.inc".
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************
C
C  Created    : Dec. 28, 2004; (c) Jiri Blazek
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
      INCLUDE 'param.inc'
      INCLUDE 'numer.inc'

C ... parameter list
      INTEGER ilev

C ... local variables
      INTEGER irk

C *****************************************************************************

C --- fine grid

      IF (ilev .EQ. ilevbase) THEN
        cfl      = cflf
        epsirs   = epsirsf
        vis2     = vis2f
        vis4     = vis4f
        limfac   = limfacf
        epsentr  = epsentrf
        kdissip  = kdissipf
        iorder   = iorderf
        nrk      = nrkf
        DO irk=1,nrk
          ark(irk)   = arkf(irk)
          betrk(irk) = betrkf(irk)
          ldiss(irk) = ldissf(irk)
          lsmoo(irk) = lsmoof(irk)
        ENDDO

C --- coarse grid

      ELSE
        cfl      = cflc
        epsirs   = epsirsc
        vis2     = vis2c
        vis4     = vis4c
        limfac   = limfacc
        epsentr  = epsentrc
        kdissip  = kdissipc
        iorder   = iorderc
        nrk      = nrkc
        DO irk=1,nrk
          ark(irk)   = arkc(irk)
          betrk(irk) = betrkc(irk)
          ldiss(irk) = ldissc(irk)
          lsmoo(irk) = lsmooc(irk)
        ENDDO
      ENDIF

      RETURN
      END

      SUBROUTINE Multigrid( idim,mgdim,nlevels,imax,ib2,mxdum,icycle,
     &                      x,a,vol,cv,p,fterm,ffunc,cvold,diss,rhs,
     &                      dt,ls,rs,dum )

C *****************************************************************************
C
C  One multigrid cycle starting at grid level "ilevbase".
C
C *****************************************************************************
C
C  Subroutines called: Solver, Inject, Restrict, Prolongate
C
C  Functions called: Get_addr
C
C *****************************************************************************
C
C  Created    : Dec. 29, 2004; (c) Jiri Blazek
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
      INCLUDE 'numer.inc'

C ... parameter list
      INTEGER idim, mgdim, nlevels, mxdum
      INTEGER imax(nlevels), ib2(nlevels), icycle(nlevels)
      REAL*8  x(mgdim*idim), a(mgdim*idim), vol(mgdim*idim)
      REAL*8  cv(mgdim*idim*3), p(idim), fterm(mgdim*idim*3)
      REAL*8  cvold(mgdim*idim*3), diss(idim*3), rhs(idim*3), dt(idim)
      REAL*8  ffunc(idim*3), ls(idim*3), rs(idim*3)
      REAL*8  dum(mxdum)

C ... local variables
      INTEGER ilev, mgtype, iaddr, iaddrc, iaddrf,
     &        iaddr3, iaddr3f, iaddr3c
      LOGICAL lrhs, lffun, lrsum

C ... functions
      INTEGER Get_addr

C *****************************************************************************
C --- store cycle type

      IF (kmgcyc.EQ.'v' .OR. kmgcyc.EQ.'V') THEN
        mgtype = 1
      ELSE
        mgtype = 2
      ENDIF

C --- reset fine-coarse switch

      DO ilev=ilevbase,nlevels
        icycle(ilev) = 0
      ENDDO

C --- time step, restriction and injection ------------------------------------

      ilev = ilevbase
10    CONTINUE
        icycle(ilev) = icycle(ilev) + 1

C ----- time step

        iaddr   = Get_addr( nlevels,ilev  ,1,imax )
        iaddrc  = Get_addr( nlevels,ilev+1,1,imax )
        iaddr3  = Get_addr( nlevels,ilev  ,3,imax )
        iaddr3c = Get_addr( nlevels,ilev+1,3,imax )
        lrhs  = .FALSE.
        IF (ilev .EQ. ilevbase) THEN
          lffun = .FALSE.
          lrsum = .FALSE.
        ELSE
          lffun = .TRUE.
          lrsum = .TRUE.
        ENDIF
        CALL Solver( imax(ilev),ib2(ilev),ilev,mxdum,
     &               lrhs,lffun,lrsum,
     &               x(iaddr),a(iaddr),vol(iaddr),cv(iaddr3),p,
     &               fterm(iaddr3),ffunc,cvold(iaddr3),diss,rhs,
     &               dt,ls,rs,dum )

C ----- new residual (with W^(n+1))

        lrhs  = .TRUE.
        lffun = .FALSE.
        lrsum = .FALSE.
        CALL Solver( imax(ilev),ib2(ilev),ilev,mxdum,
     &               lrhs,lffun,lrsum,
     &               x(iaddr),a(iaddr),vol(iaddr),cv(iaddr3),p,
     &               fterm(iaddr3),ffunc,cvold(iaddr3),diss,rhs,
     &               dt,ls,rs,dum )

C ----- injection from h to 2h

        CALL Inject( imax(ilev  ),ib2(ilev  ),cv(iaddr3 ),
     &               imax(ilev+1),ib2(ilev+1),a(iaddrc),
     &               cv(iaddr3c),p )

C ----- restriction from h to 2h

        CALL Restrict( imax(ilev  ),ib2(ilev  ),rhs,
     &                 imax(ilev+1),ib2(ilev+1),fterm(iaddr3c) )

        ilev = ilev + 1
      IF (ilev .LT. nlevels) GOTO 10

C --- time step on the coarsest grid

      iaddr  = Get_addr( nlevels,ilev,1,imax )
      iaddr3 = Get_addr( nlevels,ilev,3,imax )
      lrhs  = .FALSE.
      lffun = .TRUE.
      lrsum = .TRUE.
      CALL Solver( imax(ilev),ib2(ilev),ilev,mxdum,
     &             lrhs,lffun,lrsum,
     &             x(iaddr),a(iaddr),vol(iaddr),cv(iaddr3),p,
     &             fterm(iaddr3),ffunc,cvold(iaddr3),diss,rhs,
     &             dt,ls,rs,dum )

C --- prolongation, solution correction ---------------------------------------

20    CONTINUE
        iaddr   = Get_addr( nlevels,ilev  ,1,imax )
        iaddrf  = Get_addr( nlevels,ilev-1,1,imax )
        iaddr3  = Get_addr( nlevels,ilev  ,3,imax )
        iaddr3f = Get_addr( nlevels,ilev-1,3,imax )
        CALL Prolongate( imax(ilev  ),ib2(ilev  ),
     &                   a(iaddr ),cv(iaddr3 ),cvold(iaddr3),
     &                   imax(ilev-1),ib2(ilev-1),
     &                   a(iaddrf),cv(iaddr3f),p )
        ilev = ilev - 1
      IF (ilev .GT. ilevbase) THEN
        IF (icycle(ilev) .NE. mgtype) GOTO 10
        icycle(ilev) = 0
        GOTO 20
      ENDIF

      RETURN
      END

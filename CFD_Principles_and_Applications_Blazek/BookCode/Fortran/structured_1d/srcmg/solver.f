      SUBROUTINE Solver( imax,ib2,ilev,mxdum,lrhs,lffun,lrsum,
     &                   x,a,vol,cv,p,fterm,ffunc,cvold,diss,rhs,
     &                   dt,ls,rs,dum )

C *****************************************************************************
C
C  One time step on grid level "ilev" using explicit multistage scheme.
C  If "lrhs"=true, it computes the residual which is restricted to the
C  next coarse level.
C
C *****************************************************************************
C
C  Subroutines called: Bcond, Dissip, Flux_cen, LR_state_cusp, LR_state_roe,
C                      Flux_cusp, Flux_roe, Irsmoo, Srcterm, Tstep, Get_params
C
C  Functions called: none
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
      INTEGER imax, ib2, ilev, mxdum
      LOGICAL lrhs, lffun, lrsum
      REAL*8  x(imax), a(imax), vol(imax)
      REAL*8  cv(imax,3), p(imax)
      REAL*8  cvold(imax,3), diss(imax,3), rhs(imax,3), dt(imax)
      REAL*8  fterm(imax,3), ffunc(imax,3), ls(imax,3), rs(imax,3)
      REAL*8  dum(mxdum)

C ... local variables
      INTEGER i, irk, idim
      REAL*8  fac, adtv, rrho, rhou, rhoe

C *****************************************************************************
C --- check dimension of dummy array (Dissip, Flux_, LR_state_, Irsmoo)

      idim = 4*imax                  ! Dissip needs the most
      IF (idim .GT. mxdum) THEN      ! LR_state needs 3*(imax+1)
        WRITE(*,1000) idim,mxdum
        STOP
      ENDIF

C --- get parameters

      CALL Get_params( ilev )

      IF (lrhs) nrk = 1    ! compute new residual with W^(n+1)

C --- store previous solution; set dissipation = 0

      IF (lrhs) THEN
        DO i=1,imax
          diss(i,1) = 0.D0
          diss(i,2) = 0.D0
          diss(i,3) = 0.D0
        ENDDO
      ELSE
        DO i=1,imax
          cvold(i,1) = cv(i,1)
          cvold(i,2) = cv(i,2)
          cvold(i,3) = cv(i,3)
          diss (i,1) = 0.D0
          diss (i,2) = 0.D0
          diss (i,3) = 0.D0
        ENDDO
      ENDIF

C --- calculate time step -----------------------------------------------------

      IF (.NOT. lrhs) CALL Tstep( imax,ib2,x,a,vol,cv,p,dt )

C --- loop over the R.-K. stages: ---------------------------------------------

      DO irk=1,nrk

C ----- central scheme with artificial dissipation

        IF (kdissip.EQ.'c' .OR. kdissip.EQ.'C') THEN

C ------- artificial dissipation
          IF (ldiss(irk) .EQ. 1) THEN
            IF (iorder .GE. 2) THEN
              CALL Dissip( imax,ib2,betrk(irk),vol,cv,p,dt,
     &                     dum(1),dum(1+imax),diss )
            ELSE
              CALL Dissip2( imax,ib2,betrk(irk),vol,cv,p,dt,
     &                      dum(1),dum(1+imax),diss )
            ENDIF
          ENDIF

C ------- convective flux
          CALL Flux_cen( imax,ib2,a,cv,p,diss,dum,rhs )

C ----- CUSP scheme (flux-vector splitting)

        ELSE IF (kdissip.EQ.'u' .OR. kdissip.EQ.'U') THEN

          CALL LR_state_cusp( imax,ib2,a,vol,cv,p,dum,ls,rs )
          CALL Flux_cusp( imax,ib2,a,cv,p,ls,rs,dum,rhs )

C ----- Roe's flux-difference splitting scheme
C       (called in EVERY stage)

        ELSE

          CALL LR_state_roe( imax,ib2,a,vol,cv,p,dum,ls,rs )
          CALL Flux_roe( imax,ib2,a,ls,rs,dum,rhs )

        ENDIF

C ----- source term

        CALL Srcterm( imax,ib2,a,p,rhs )

C ----- zero out residuals at dummy points

        rhs(1   ,1) = 0.D0
        rhs(1   ,2) = 0.D0
        rhs(1   ,3) = 0.D0
        rhs(imax,1) = 0.D0
        rhs(imax,2) = 0.D0
        rhs(imax,3) = 0.D0

C ----- new residual formed with W^(n+1); forcing function added

        IF (lrhs .AND. ilev.GT.ilevbase) THEN
          DO i=2,ib2
            rhs(i,1) = rhs(i,1) + ffunc(i,1)
            rhs(i,2) = rhs(i,2) + ffunc(i,2)
            rhs(i,3) = rhs(i,3) + ffunc(i,3)
          ENDDO
        ENDIF

        IF (lrhs) RETURN

C ----- forcing function

        IF (lffun) THEN
          DO i=2,ib2
            ffunc(i,1) = fterm(i,1) - rhs(i,1)
            ffunc(i,2) = fterm(i,2) - rhs(i,2)
            ffunc(i,3) = fterm(i,3) - rhs(i,3)
          ENDDO
          ffunc(1   ,1) = 0.D0
          ffunc(1   ,2) = 0.D0
          ffunc(1   ,3) = 0.D0
          ffunc(imax,1) = 0.D0
          ffunc(imax,2) = 0.D0
          ffunc(imax,3) = 0.D0
          lffun         = .FALSE.
        ENDIF

C ----- residual * time step

        fac = ark(irk)*cfl
        IF (lrsum) THEN
          DO i=2,ib2
            adtv     = fac*dt(i)/vol(i)
            rhs(i,1) = adtv*(rhs(i,1)+ffunc(i,1))
            rhs(i,2) = adtv*(rhs(i,2)+ffunc(i,2))
            rhs(i,3) = adtv*(rhs(i,3)+ffunc(i,3))
          ENDDO
        ELSE
          DO i=2,ib2
            adtv     = fac*dt(i)/vol(i)
            rhs(i,1) = adtv*rhs(i,1)
            rhs(i,2) = adtv*rhs(i,2)
            rhs(i,3) = adtv*rhs(i,3)
          ENDDO
        ENDIF

C ----- implicit residual smoothing

        IF (lsmoo(irk).GT.0 .AND. epsirs.GT.0.D0)
     &    CALL Irsmoo( imax,ib2,rhs,dum )

C ----- update (conservative variables and pressure)

        DO i=2,ib2
          cv(i,1) = cvold(i,1) - rhs(i,1)
          cv(i,2) = cvold(i,2) - rhs(i,2)
          cv(i,3) = cvold(i,3) - rhs(i,3)

          rrho = a(i)/cv(i,1)
          rhou = cv(i,2)/a(i)
          rhoe = cv(i,3)/a(i)
          p(i) = gam1*(rhoe-0.5D0*rhou*rhou*rrho)
        ENDDO

C ----- boundary conditions
        IF (ilev .EQ. ilevbase) CALL Bcond( imax,ib2,a,cv,p )

C ----- next stage (irk)
      ENDDO

1000  FORMAT(/,' Error (solver.f) - max. dimension of work space',
     &       ' exeeded: ',I6,' > ',I6)
      RETURN
      END

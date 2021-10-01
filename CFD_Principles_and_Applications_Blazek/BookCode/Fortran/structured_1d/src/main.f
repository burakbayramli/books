      PROGRAM Eul1D

C *****************************************************************************
C
C   SIMULATION OF QUASI 1-D INVISCID FLOW IN A TUBE/NOZZLE
C   ======================================================
C
C   (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
C   Created July 25, 1997
C   Version 2.3 from February 18, 2014
C
C *****************************************************************************
C
C   Features:
C   ~~~~~~~~~
C   # finite-volume scheme (dual control volume)
C   # central differences with Jameson's artificial dissipation
C   # CUSP flux-vector splitting scheme with SLIP limiter
C   # flux-difference splitting scheme due to Roe (MUSCL interpolation)
C   # explicit multistage time-marching scheme (Runge-Kutta type)
C   # characteristic inlet/outlet boundary conditions
C   # central implicit residual smoothing
C
C   I/O channels:
C   ~~~~~~~~~~~~~
C   5  = user parameters (input)
C   6  = control output
C   10 = grid
C   20 = results (Vis2d format)
C   30 = convergence history (Vis2d format)
C   40 = restart solution (unformatted)
C
C *****************************************************************************
C
C  Subroutines called: Read_char, Iniflow, Inigrid, Output, Rgrid,
C                      Rsolut, Solver, Wsolut
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

C ... global parameters - set as appropriate (idim = max. no. of nodes)
      INTEGER idim, mxdum

      PARAMETER (idim =300)
      PARAMETER (mxdum=4*idim)
C     ========================

C ... include files
      INCLUDE 'param.inc'

C ... global variables
      INTEGER imax, ib2

      REAL*8 cv(idim*3), p(idim)
      REAL*8 x(idim), a(idim), vol(idim)
      REAL*8 cvold(idim*3), diss(idim*3), rhs(idim*3), dt(idim)
      REAL*8 ls(idim*3), rs(idim*3)
      REAL*8 dum(mxdum)

C ... local variables
      CHARACTER*(80) str
      INTEGER irk

C *****************************************************************************

      WRITE(*,*) ' '
      WRITE(*,*) '*************************************************'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*      INVISCID QUASI 1-D FLOW IN A NOZZLE      *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*  (c) Jiri Blazek, CFD Consulting & Analysis   *'
      WRITE(*,*) '*                 www.cfd-ca.de                 *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*          Version 2.3 from 02/18/2014          *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*************************************************'
      WRITE(*,*) ' '

C --- read input parameters ---------------------------------------------------

      READ(*,'(1X)')
      READ(*,'(A256)') fngrid
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fnplot
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fnconv
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fnrest

C --- physics

      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,*) p01
      READ(*,*) t01
      READ(*,*) p2
      READ(*,*) gamma
      READ(*,*) cpgas

C --- iteration control

      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,*) maxiter
      READ(*,*) convtol
      READ(*,'(A)') str
      CALL Read_char( str,lrest )

C --- numerical parameters

      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,*) cfl
      READ(*,*) epsirs
      READ(*,'(A)') str
      CALL Read_char( str,kdissip )
      READ(*,*) vis2
      READ(*,*) vis4
      READ(*,*) iorder
      READ(*,*) limfac
      READ(*,*) epsentr
      READ(*,*) nrk
      READ(*,*) (ark(irk)  , irk=1,nrk)
      READ(*,*) (betrk(irk), irk=1,nrk)
      READ(*,*) (ldiss(irk), irk=1,nrk)
      READ(*,*) (lsmoo(irk), irk=1,nrk)

      IF (kdissip.EQ.'c' .OR. kdissip.EQ.'C') vis4 = 1.D0/vis4

C --- write out input parameters ----------------------------------------------

      WRITE(*,1005) p01,t01,p2,gamma,cpgas
      IF (kdissip.EQ.'c' .OR. kdissip.EQ.'C') THEN
        WRITE(*,1010) cfl,epsirs,vis2,1.D0/vis4
      ELSE IF (kdissip.EQ.'u' .OR. kdissip.EQ.'U') THEN
        WRITE(*,1011) cfl,epsirs
      ELSE IF (kdissip.EQ.'r' .OR. kdissip.EQ.'R') THEN
        WRITE(*,1013) cfl,epsirs,iorder,limfac,epsentr
      ENDIF

C --- read grid (x,A(x)); compute cell volumes --------------------------------

      CALL Rgrid( idim,imax,ib2,x,a )

      CALL Inigrid( imax,ib2,x,a,vol )

C --- initialize flow field and some constants --------------------------------

      CALL Iniflow( imax,a,cv,p )

      IF (lrest.EQ.'y' .OR. lrest.EQ.'Y') THEN
        CALL Rsolut( imax,cv,p )
      ELSE
        CALL Bcond( imax,ib2,a,cv,p )
      ENDIF

C --- solve the equations -----------------------------------------------------

      OPEN(30,file=fnconv,form='formatted',status='unknown')
      WRITE( *,1015)
      WRITE(30,1020)
      iter = 0

10    CONTINUE
        iter = iter + 1
        CALL Solver( imax,ib2,mxdum,
     &               x,a,vol,cv,p,
     &               cvold,diss,rhs,dt,ls,rs,
     &               dum )
      IF (iter.LT.maxiter .AND. drho.GT.convtol) GOTO 10

      CLOSE(30)

C --- store the results -------------------------------------------------------

      WRITE(*,1025)

      CALL Output( imax,ib2,x,a,cv,p )

      CALL Wsolut( imax,cv,p )

      WRITE(*,'(A,/)') ' Finished.'

1005  FORMAT(' Flow parameters:',/,1X,16('~'),/,
     &       ' p01   = ',E12.6,' [Pa]',/,
     &       ' T01   = ',E12.6,' [K]',/,
     &       ' p2    = ',E12.6,' [Pa]',/,
     &       ' gamma = ',E12.6,' [-]',/,
     &       ' cp    = ',E12.6,' [J/kgK]',/)
1010  FORMAT(' Numerical parameters: (central scheme)',/,1X,21('~'),/,
     &       ' CFL = ',F5.2,', epsirs = ',F4.2,', vis2 = ',F4.2,
     &       ', 1/vis4 = ',F5.1)
1011  FORMAT(' Numerical parameters: (CUSP scheme)',/,1X,21('~'),/,
     &       ' CFL = ',F5.2,', epsirs = ',F5.2)
1013  FORMAT(' Numerical parameters: (Roe scheme)',/,1X,21('~'),/,
     &       ' CFL = ',F5.2,', epsirs = ',F5.2,', iorder = ',I1,
     &       ', limfac = ',F5.2,', epsentr = ',F5.3)
1015  FORMAT(//,3X,'step',7X,'drho',9X,'drmax',9X,'i',6X,'mass',
     &       11X,'dm',8X,'nsup',/,1X,78('-'))
1020  FORMAT('Flow in a nozzle',/,'1',/,'Convergence History',/,'1 7',
     &       /,'iteration',/,'log(resid)',/,'drho_max [kg/m^3]',/,
     &       'node',/,'mass flow [kg/s]',/,'mass diff [kg/s]',/,
     &       'nsup pts',/,'-1 0',/,'0 0 0',/,'Nozzle')
1025  FORMAT(/,1X,78('-'),//,' Storing results ...',/)

      STOP
      END

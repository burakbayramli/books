      PROGRAM Eul1Dmg

C *****************************************************************************
C
C   SIMULATION OF QUASI 1-D INVISCID FLOW IN A TUBE/NOZZLE
C   USING GEOMETRIC MULTIGRID IN FAS FORMULATION
C   ======================================================
C
C   (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
C   Created December 22, 2004
C   Version 1.1 from February 19, 2014
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
C   # geometric multigrid in Full Approximation Storage (FAS) formulation
C   # Full Multigrid (FMG) scheme for the initial solution
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
C                      Rsolut, Solver, Conver, Multigrid, Wsolut
C
C  Functions called: Get_addr
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

C ... global parameters - set as appropriate:
C     idim   = max. no. of nodes on the finest grid
C     mxlevs = max. no. of multigrid levels
C     mgdim  = factor for number of nodes on all grids = (1+2+4+8+16)/16

      INTEGER idim, mxlevs, mgdim, mxdum

      PARAMETER (idim=200, mxlevs=5, mgdim=2)
      PARAMETER (mxdum=4*idim)
C     ========================

C ... include files
      INCLUDE 'numer.inc'
      INCLUDE 'param.inc'

C ... global variables
      INTEGER nlevels
      INTEGER imax(mxlevs), ib2(mxlevs), ncells(mxlevs), icycle(mxlevs)
      REAL*8  cv(mgdim*idim*3), p(idim)
      REAL*8  x(mgdim*idim), a(mgdim*idim), vol(mgdim*idim)
      REAL*8  cvold(mgdim*idim*3), diss(idim*3), rhs(idim*3), dt(idim)
      REAL*8  fterm(mgdim*idim*3), ffunc(idim*3)
      REAL*8  ls(idim*3), rs(idim*3)
      REAL*8  dum(mxdum)

C ... local variables
      CHARACTER*(80) str
      INTEGER irk, ilev, iter, itermg, iaddr, iaddr3, iaddrf, iaddr3f
      LOGICAL lrhs, lffun, lrsum

C ... functions
      INTEGER Get_addr

C *****************************************************************************

      WRITE(*,*) ' '
      WRITE(*,*) '*************************************************'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*      INVISCID QUASI 1-D FLOW IN A NOZZLE      *'
      WRITE(*,*) '*    SOLVED BY MULTIGRID IN FAS FORMULATION     *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*  (c) Jiri Blazek, CFD Consulting & Analysis   *'
      WRITE(*,*) '*                 www.cfd-ca.de                 *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*          Version 1.1 from 02/19/2014          *'
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
      READ(*,*) nlevels
      READ(*,*) ilevbase
      READ(*,*) maxsgiter
      READ(*,*) maxcgiter
      READ(*,*) maxfgiter
      READ(*,'(A)') str
      CALL Read_char( str,kmgcyc )
      READ(*,*) convtol
      READ(*,'(A)') str
      CALL Read_char( str,lrest )

      ilevbase = MAX(1,MIN(nlevels,ilevbase))
      IF (nlevels .GT. mxlevs) THEN
        WRITE(*,1000) nlevels,mxlevs
        STOP
      ENDIF

C --- numerical parameters - finest grid

      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,*) cflf
      READ(*,*) epsirsf
      READ(*,'(A)') str
      CALL Read_char( str,kdissipf )
      READ(*,*) vis2f
      READ(*,*) vis4f
      READ(*,*) iorderf
      READ(*,*) limfacf
      READ(*,*) epsentrf
      READ(*,*) nrkf
      READ(*,*) (arkf(irk)  , irk=1,nrkf)
      READ(*,*) (betrkf(irk), irk=1,nrkf)
      READ(*,*) (ldissf(irk), irk=1,nrkf)
      READ(*,*) (lsmoof(irk), irk=1,nrkf)
      IF (kdissipf.EQ.'c' .OR. kdissipf.EQ.'C') vis4f = 1.D0/vis4f

C --- numerical parameters - coarse grids

      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,*) cflc
      READ(*,*) epsirsc
      READ(*,'(A)') str
      CALL Read_char( str,kdissipc )
      READ(*,*) vis2c
      READ(*,*) vis4c
      READ(*,*) iorderc
      READ(*,*) limfacc
      READ(*,*) epsentrc
      READ(*,*) nrkc
      READ(*,*) (arkc(irk)  , irk=1,nrkc)
      READ(*,*) (betrkc(irk), irk=1,nrkc)
      READ(*,*) (ldissc(irk), irk=1,nrkc)
      READ(*,*) (lsmooc(irk), irk=1,nrkc)
      IF (kdissipc.EQ.'c' .OR. kdissipc.EQ.'C') vis4c = 1.D0/vis4c

C --- write out input parameters ----------------------------------------------

      WRITE(*,1005) p01,t01,p2,gamma,cpgas

      WRITE(*,1010) nlevels,ilevbase,maxsgiter,maxcgiter,maxfgiter,
     &              kmgcyc,convtol

      WRITE(*,1015)
      IF (kdissipf.EQ.'c' .OR. kdissipf.EQ.'C') THEN
        WRITE(*,1020) cflf,epsirsf,vis2f,1.D0/vis4f
      ELSE IF (kdissipf.EQ.'u' .OR. kdissipf.EQ.'U') THEN
        WRITE(*,1025) cflf,epsirsf
      ELSE IF (kdissipf.EQ.'r' .OR. kdissipf.EQ.'R') THEN
        WRITE(*,1035) cflf,epsirsf,iorderf,limfacf,epsentrf
      ENDIF

      WRITE(*,1016)
      IF (kdissipc.EQ.'c' .OR. kdissipc.EQ.'C') THEN
        WRITE(*,1020) cflc,epsirsc,vis2c,1.D0/vis4c
      ELSE IF (kdissipc.EQ.'u' .OR. kdissipc.EQ.'U') THEN
        WRITE(*,1025) cflc,epsirsc
      ELSE IF (kdissipc.EQ.'r' .OR. kdissipc.EQ.'R') THEN
        WRITE(*,1035) cflc,epsirsc,iorderc,limfacc,epsentrc
      ENDIF

C --- read grid (x,A(x)); compute cell volumes --------------------------------

      CALL Rgrid( idim,mgdim,nlevels,imax,ib2,ncells,x,a )

      DO ilev=1,nlevels
        iaddr = Get_addr( nlevels,ilev,1,imax )
        CALL Inigrid( imax(ilev),ib2(ilev),x(iaddr),a(iaddr),
     &                vol(iaddr) )
      ENDDO

C --- initialize flow field and some constants --------------------------------

      iaddr  = Get_addr( nlevels,ilevbase,1,imax )
      iaddr3 = Get_addr( nlevels,ilevbase,3,imax )

      CALL Iniflow( imax(ilevbase),a(iaddr),cv(iaddr3),p )

      IF (lrest.EQ.'y' .OR. lrest.EQ.'Y') THEN
        CALL Rsolut( imax(ilevbase),cv(iaddr3),p )
      ELSE
        CALL Bcond( imax(ilevbase),ib2(ilevbase),a(iaddr),
     &              cv(iaddr3),p )
      ENDIF

C --- solve the equations -----------------------------------------------------

      OPEN(30,file=fnconv,form='formatted',status='unknown')
      WRITE( *,1040)
      WRITE(30,1045)
      iter   = 0
      iaddr  = Get_addr( nlevels,ilevbase,1,imax )
      iaddr3 = Get_addr( nlevels,ilevbase,3,imax )

C --- single-grid iterations

      IF (maxsgiter .GT. 0) THEN
        WRITE(*,1050) 'single grid iterations',ilevbase
        lrhs  = .FALSE.
        lffun = .FALSE.
        lrsum = .FALSE.
10      CONTINUE
          iter = iter + 1
          CALL Solver( imax(ilevbase),ib2(ilevbase),ilevbase,mxdum,
     &                 lrhs,lffun,lrsum,
     &                 x(iaddr),a(iaddr),vol(iaddr),cv(iaddr3),p,
     &                 fterm(iaddr3),ffunc,cvold(iaddr3),diss,rhs,
     &                 dt,ls,rs,dum )
          CALL Conver( iter,imax(ilevbase),ib2(ilevbase),
     &                 ncells(ilevbase),a(iaddr),cv(iaddr3),
     &                 cvold(iaddr3),p )
        IF (iter.LT.maxsgiter .AND. drho.GT.convtol) GOTO 10
      ENDIF

C --- multigrid iterations from coarse levels (FMG)

      IF (maxcgiter.GT.0 .AND. ilevbase.LT.nlevels) THEN
15      WRITE(*,1050) 'multigrid iterations',ilevbase
        itermg = 0
20      CONTINUE
          iter   = iter + 1
          itermg = itermg + 1
          CALL Multigrid( idim,mgdim,nlevels,imax,ib2,mxdum,icycle,
     &                    x,a,vol,cv,p,fterm,ffunc,cvold,diss,rhs,
     &                    dt,ls,rs,dum )
          CALL Conver( iter,imax(ilevbase),ib2(ilevbase),
     &                 ncells(ilevbase),a(iaddr),cv(iaddr3),
     &                 cvold(iaddr3),p )
        IF (itermg.LT.maxcgiter .AND. drho.GT.convtol) GOTO 20

        iaddrf  = Get_addr( nlevels,ilevbase-1,1,imax )
        iaddr3f = Get_addr( nlevels,ilevbase-1,3,imax )
        CALL Interpolate( imax(ilevbase  ),ib2(ilevbase  ),
     &                    a(iaddr ),cv(iaddr3 ),
     &                    imax(ilevbase-1),ib2(ilevbase-1),
     &                    a(iaddrf),cv(iaddr3f),p )
        ilevbase = ilevbase - 1
        iaddr    = iaddrf
        iaddr3   = iaddr3f
        IF (ilevbase .GT. 1) GOTO 15
      ENDIF

C --- multigrid iterations from the finest grid

      IF (maxfgiter.GT.0 .AND. ilevbase.LT.nlevels) THEN
        WRITE(*,1050) 'multigrid iterations',ilevbase
        itermg = 0
30      CONTINUE
          iter   = iter + 1
          itermg = itermg + 1
          CALL Multigrid( idim,mgdim,nlevels,imax,ib2,mxdum,icycle,
     &                    x,a,vol,cv,p,fterm,ffunc,cvold,diss,rhs,
     &                    dt,ls,rs,dum )
          CALL Conver( iter,imax(ilevbase),ib2(ilevbase),
     &                 ncells(ilevbase),a(iaddr),cv(iaddr3),
     &                 cvold(iaddr3),p )
        IF (itermg.LT.maxfgiter .AND. drho.GT.convtol) GOTO 30
      ENDIF

      CLOSE(30)

C --- store the results -------------------------------------------------------

      WRITE(*,1055)

      iaddr  = Get_addr( nlevels,ilevbase,1,imax )
      iaddr3 = Get_addr( nlevels,ilevbase,3,imax )

      CALL Output( imax(ilevbase),ib2(ilevbase),x(iaddr),a(iaddr),
     &             cv(iaddr3),p )

      CALL Wsolut( imax(ilevbase),cv(iaddr3),p )

      WRITE(*,'(A,/)') ' Finished.'

1000  FORMAT(/,' Error (main.f) - too many grid levels! (',I3,
     &       ' but max. ',I3,')')
1005  FORMAT(' Flow parameters:',/,1X,16('~'),/,
     &       ' p01   = ',E12.6,' [Pa]',/,
     &       ' T01   = ',E12.6,' [K]',/,
     &       ' p2    = ',E12.6,' [Pa]',/,
     &       ' gamma = ',E12.6,' [-]',/,
     &       ' cp    = ',E12.6,' [J/kgK]',/)
1010  FORMAT(' Control parameters:',/,1X,19('~'),/,
     &       ' levels = ',I1,/,
     &       ' start  = ',I1,/,
     &       ' sgiter = ',I5,/,
     &       ' mgiter = ',I5,/,
     &       ' fgiter = ',I5,/,
     &       ' mgtype = ',A1,/,
     &       ' tol    = ',E12.6,/)
1015  FORMAT(' Finest grid:',/,1X,12('~'))
1016  FORMAT(/,' Coarse grids:',/,1X,13('~'))
1020  FORMAT(' Central scheme:',/,
     &       ' CFL = ',F5.2,', epsirs = ',F4.2,', vis2 = ',F4.2,
     &       ', 1/vis4 = ',F5.1)
1025  FORMAT(' CUSP scheme:',/,
     &       ' CFL = ',F5.2,', epsirs = ',F5.2)
1035  FORMAT(' Roe scheme:',/,
     &       ' CFL = ',F5.2,', epsirs = ',F5.2,', iorder = ',I1,
     &       ', limfac = ',F5.2,', epsentr = ',F5.3)
1040  FORMAT(//,3X,'step',7X,'drho',9X,'drmax',9X,'i',6X,'mass',
     &       11X,'dm',8X,'nsup',/,1X,78('-'))
1045  FORMAT('Flow in a nozzle',/,'1',/,'Convergence History',/,'1 7',
     &       /,'iteration',/,'log(resid)',/,'drho_max [kg/m^3]',/,
     &       'node',/,'mass flow [kg/s]',/,'mass diff [kg/s]',/,
     &       'nsup pts',/,'-1 0',/,'0 0 0',/,'Nozzle')
1050  FORMAT('*** ',A,' on level ',I1,' ***')
1055  FORMAT(/,1X,78('-'),//,' Storing results ...',/)

      STOP
      END

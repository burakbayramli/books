      PROGRAM Channel

C *****************************************************************************
C
C   GENERATES 2-D GRID FOR A CHANNEL WITH CIRCULAR BUMP
C   ===================================================
C
C   (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
C   Created March 7, 1990
C   Version 1.4 from February 17, 2014
C
C *****************************************************************************
C
C   Features:
C   ~~~~~~~~~
C   # algebraically generated H-grid
C   # point clustering at leading and trailing edge
C   # grid stretched in vertical direction
C   # writes grid and topology file in STRUCT2D format
C   # writes plot file in Vis2D format
C
C   I/O channels:
C   ~~~~~~~~~~~~~
C   5  = user parameters (input)
C   6  = control output
C   10 = grid data (fn_grid)
C   20 = grid topology (fn_gtop)
C   30 = plot data (fn_plot, Vis2D format)
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: Length
C
C *****************************************************************************
C
C   This program is free software; you can redistribute it and/or
C   modify it under the terms of the GNU General Public License
C   as published by the Free Software Foundation; either version 2
C   of the License, or (at your option) any later version.
C
C   This program is distributed in the hope that it will be useful,
C   but WITHOUT ANY WARRANTY; without even the implied warranty of
C   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
C   GNU General Public License for more details.
C
C   You should have received a copy of the GNU General Public License
C   along with this program; if not, write to the Free Software
C   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameters - set as appropriate
      INTEGER im, jm
      PARAMETER (im=200, jm=100)

C ... loop variables
      INTEGER i, j

C ... global variables
      CHARACTER*80 title
      CHARACTER*256 fn_grid, fn_gtop, fn_plot
      INTEGER nx, ny, nc
      REAL*8 x(0:im,0:jm), y(0:im,0:jm)
      REAL*8 thick, height, beta1, beta2, ksi, eta, ksit, etat,
     &       rat, dum

C ... functions
      INTEGER Length

C *****************************************************************************

      WRITE(*,*) ' '
      WRITE(*,*) '*************************************************'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*     GENERATION OF 2-D H-GRID IN A CHANNEL     *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*  (c) Jiri Blazek, CFD Consulting & Analysis   *'
      WRITE(*,*) '*                 www.cfd-ca.de                 *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*          Version 1.4 from 02/17/2014          *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*************************************************'
      WRITE(*,*) ' '

C --- initialise constants

      beta1 = 1.26D0
      beta2 = 1.2D0

C --- read input parameters

      READ(*,'(1X)')
      READ(*,'(A80)') title
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fn_grid
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fn_gtop
      READ(*,'(1X)')
      READ(*,'(1X)')
      READ(*,'(A256)') fn_plot
      READ(*,'(1X)')
      READ(*,*) nx        ! no. of cells in x-direction (channel length)
      READ(*,*) ny        ! no. of cells in y-direction (channel height)
      READ(*,*) thick     ! height of the circular bump (% of its length)
      READ(*,*) height    ! 1/2 height of the channel

      thick = thick/100.D0
      IF (nx.GT.im .OR. ny.GT.jm) THEN
        WRITE(*,'(A)') ' ERROR - too many grid points'
        STOP
      ENDIF

C --- generate grid

      WRITE(*,'(A)') ' Generating grid ...'

      DO j=0,ny
        eta  = REAL(j)/REAL(ny)
        etat = height*(EXP(beta2*eta)-1.D0)/(EXP(beta2)-1.D0)

        DO i=0,nx
          ksi = 3.D0*REAL(i)/REAL(nx) - 1.D0
          IF ((ksi.GE.-1.D0) .AND. (ksi.LE.-0.25D0)) THEN
            ksit = (4.D0*ksi+1.D0)/3.D0
          ELSEIF ((ksi.GT.-0.25D0) .AND. (ksi.LE.1.25D0)) THEN
            ksit = (4.D0*ksi+1.D0)/6.D0
          ELSEIF ((ksi.GT.1.25D0) .AND. (ksi.LE.2.D0)) THEN
            ksit = (4.D0*ksi-2.D0)/3.D0
          ENDIF

          IF ((ksit.GE.-1.D0) .AND. (ksit.LE.0.D0)) THEN
            x(i,j) = (EXP(-beta1*(ksit+1.D0))-1.D0)/
     &               (EXP(-beta1)-1.D0) - 1.D0
          ELSE IF ((ksit.GT.0.D0) .AND. (ksit.LE.1.D0)) THEN
            x(i,j) = ksit
          ELSE
            x(i,j) = 2.D0 - (EXP(beta1*(ksit-2.D0))-1.D0)/
     &                      (EXP(-beta1)-1.D0)
          ENDIF

          IF ((x(i,j).GE.0.D0) .AND. (x(i,j).LE.1.D0)) THEN
            rat    = 1.D0/(4.D0*thick) - thick
            dum    = -0.5D0*(rat-SQRT(rat**2-4.D0*((x(i,j)-0.5D0)**2-
     &                       0.25D0)))
            y(i,j) = etat + dum*(1.D0-etat/height)
          ELSE
            y(i,j) = etat
          ENDIF
        ENDDO
      ENDDO

C --- store grid

      WRITE(*,'(A)') ' Storing grid file ...'

      nc = Length(title)

      OPEN (10,FILE=fn_grid,STATUS='unknown',FORM='formatted')
      WRITE(10,1000) title(1:nc),nx,ny
      WRITE(10,1010) ((x(i,j),y(i,j), i=0,nx), j=0,ny)
      CLOSE(10)

C --- store grid topology

      WRITE(*,'(A)') ' Storing topology file ...'

      OPEN(20,FILE=fn_gtop,FORM='formatted',STATUS='unknown')
      WRITE(20,1020) title(1:nc),4,nx,ny
      WRITE(20,1025) 'wall'    ,300,1,2,nx+1,0,0,0
      WRITE(20,1025) 'outflow' ,200,2,2,ny+1,0,0,0
      WRITE(20,1025) 'symmetry',502,3,2,nx+1,0,0,0
      WRITE(20,1025) 'inflow'  ,100,4,2,ny+1,0,0,0
      CLOSE(20)

C --- store plot file

      WRITE(*,'(A)') ' Storing plot file ...'

      OPEN(30,FILE=fn_plot,FORM='formatted',STATUS='unknown')
      WRITE(30,1045) title(1:nc),nx+1,ny+1
      DO j=0,ny
        DO i=0,nx
          WRITE(30,1035) x(i,j),y(i,j)
        ENDDO
      ENDDO
      CLOSE(30)

      WRITE(*,'(/,A,/)') ' Finished.'

1000  FORMAT('# ',A,/,'#',/,'# no. of cells in i-, j-direction',/,
     &       I6,2X,I6,/,'# coordinates (x, y):')
1010  FORMAT(2E17.9)
1020  FORMAT('# ',A,/,'#',/,'# no. of segments',/,I6,/,'#',/,
     &       '# no. of cells in i-, j-direction',/,I6,I6,/,'#',/,
     &       '# segments (cell-centred index!):',/,'#',/,
     &       '# itype lb  lbeg  lend  lbs  lbegs  lends',/,'#')
1025  FORMAT('# ',A,/,I6,I4,I6,I6,I5,2I7)
1035  FORMAT(2E17.9)
1045  FORMAT(A,/,'1',/,'Structured Grid',/,'1 2',/,'x',/,'y',/,
     &       I4,I4,/,'0 0 0',/,'grid')
      STOP
      END

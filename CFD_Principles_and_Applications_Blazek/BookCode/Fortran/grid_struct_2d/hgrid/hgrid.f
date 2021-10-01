      PROGRAM Hgrid

C *****************************************************************************
C
C   GENERATION OF 2-D STRUCTURED H-TYPE GRID AROUND A BLADE
C   =======================================================
C
C   (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
C   Created January 1, 1995
C   Version 1.5 from February 17, 2014
C
C *****************************************************************************
C
C   Features:
C   ~~~~~~~~~
C   # straight H-grid
C   # wall contour approximated using Bezier spline
C   # point clustering at leading and trailing edge
C   # adjustable wall spacing and stretching ratio in wall layer
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
C  Subroutines called: Bezier, Bezier_interpol, Bezier_x, Sstretch,
C                      Stretch, Tfint
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

C ... global dimensions - set as appropriate:
      INTEGER im, jm
      PARAMETER (im=400, jm=100)

C ... global variables
      CHARACTER*80 title
      CHARACTER*256 fn_grid, fn_gtop, fn_plot
      INTEGER nxa, nxi, nxo, ny, ncwy, ncoo, ip, jp, nle, nte
      REAL*8 xyb(2,im), x(im,jm), y(im,jm), bc(2,3*im+1)
      REAL*8 t, beta1, beta2, betas, d12, d34, strfacw, dxle, dyle,
     &       dxte, dytep, dytes, dyout

C ... loop variables
      INTEGER i, j

C ... local variables
      INTEGER nc
      REAL*8 s1(jm), s2(jm), s3(im), s4(im)
      REAL*8 pi, ang, xp1, yp1, vp, vple, vpte,
     &       xle, xte, s, sp, ss, ds, rat

C ... functions
      INTEGER Length

C *****************************************************************************

      pi = 4.D0*ATAN(1.D0)

      WRITE(*,*) ' '
      WRITE(*,*) '*************************************************'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*    GENERATION OF 2-D H-GRID AROUND A BLADE    *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*  (c) Jiri Blazek, CFD Consulting & Analysis   *'
      WRITE(*,*) '*                 www.cfd-ca.de                 *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*          Version 1.5 from 02/17/2014          *'
      WRITE(*,*) '*                                               *'
      WRITE(*,*) '*************************************************'
      WRITE(*,*) ' '

C --- read parameters and blade coordinates -----------------------------------

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
      READ(*,*) nxa      ! no. of cells around blade (new point distrib.)
      READ(*,*) nxi      ! no. of cells in inlet region
      READ(*,*) nxo      ! no. of cells in outlet region
      READ(*,*) ny       ! no. of cells in normal direction
      READ(*,*) t        ! pitch
      READ(*,*) beta1    ! inlet angle
      READ(*,*) beta2    ! outlet angle
      READ(*,*) betas    ! stagger angle
      READ(*,*) d12      ! dist. between leading edge and inlet boundary
      READ(*,*) d34      !     - " -     trailing edge and outlet boundary
      READ(*,*) ncwy     ! no. of cells in wall layer (>1)
      READ(*,*) strfacw  ! stretching ratio in wall layer
      READ(*,*) dxle     ! leading edge (l.e.) spacing in i-dir.
      READ(*,*) dyle     ! l.e. spacing in j-dir.
      READ(*,*) dxte     ! trailing edge (t.e.) spacing in i-dir.
      READ(*,*) dytep    ! t.e. spacing in j-dir. - pressure side
      READ(*,*) dytes    ! t.e. spacing in j-dir. - suction side
      READ(*,*) dyout    ! spacing at outlet in j-dir.
      READ(*,*) ncoo     ! no. of coord. pairs

      ip = nxi + nxa/2 + nxo + 1    ! no. of points in i-, j-direction
      jp = ny + 1
      IF (ip.GT.im .OR. jp.GT.jm) THEN
        WRITE(*,'(A)') ' ERROR - too many grid points'
        GOTO 9999
      ENDIF
      nle = nxi + 1            ! l.e. index
      nte = nxi + nxa/2 + 1    ! t.e. (lower surface) index

      DO i=1,ncoo              ! coordinates (from lower t.e. to upper t.e)
        READ(*,*) xyb(1,i),xyb(2,i)
      ENDDO

      WRITE(*,1000) ip,jp,nle,nte

C --- rotate the blade

      ang = pi*betas/180.D0
      DO i=1,ncoo
        xp1      = xyb(1,i)*COS(ang) + xyb(2,i)*SIN(ang)
        yp1      = xyb(2,i)*COS(ang) - xyb(1,i)*SIN(ang)
        xyb(1,i) = xp1
        xyb(2,i) = yp1
      ENDDO

C --- find coefficients of the Bezier spline ----------------------------------

      WRITE(*,'(A)') ' Distributing points on blade ...'

      CALL Bezier_interpol( xyb,ncoo-1,bc )

C --- get location of leading and trailing edge -------------------------------

      CALL Bezier( bc,ncoo-1,0.D0,xp1,yp1 )
      xle  = 1.D20
      xte  = xp1
      vpte = 0.D0

      DO i=1,5000
        vp = REAL(i)/5000.D0
        CALL Bezier( bc,ncoo-1,vp,xp1,yp1 )
        IF (xp1 .LE. xle) THEN     ! leading edge = x-min
          xle  = xp1
          vple = vp
        ENDIF
        IF (xp1 .GE. xte) THEN     ! trailing edge = x-max
          xte  = xp1
          vpte = vp
        ENDIF
      ENDDO

C --- set leading & trailing edge points

      CALL Bezier( bc,ncoo-1,vple,x(nle,1),y(nle,1) )
      x(nle,jp) = x(nle,1)
      y(nle,jp) = y(nle,1) + t

      CALL Bezier( bc,ncoo-1,vpte,x(nte,1),y(nte,1) )
      x(nte,jp) = x(nte,1)
      y(nte,jp) = y(nte,1) + t

      WRITE(*,1005) x(nle,1),y(nle,1),x(nte,1),y(nte,1)

C --- distribute points in i-direction ----------------------------------------

      WRITE(*,'(A)') ' Distributing points in i-direction ...'

C --- inlet to leading edge

      CALL Stretch( dxle,d12,nxi,rat )
      ang = pi*beta1/180.D0
      ds  = dxle
      s   = 0.D0
      DO i=nle-1,1,-1
        s       = s + ds
        x(i, 1) = x(nle,1) - s
        y(i, 1) = y(nle,1) - s*TAN(ang)
        x(i,jp) = x(i,1)
        y(i,jp) = y(i,1) + t
        ds      = ds*rat
      ENDDO

C --- blade surface

      CALL Sstretch( nle,nle+2,nte-2,nte,
     &               x(nle,1),x(nle,1)+2.1D0*dxle,
     &               x(nte,1)-2.1D0*dxte,x(nte,1),
     &               dxle,dxte,s3 )

      CALL Bezier( bc,ncoo-1,1.D0,xp1,yp1 )

      DO i=nle+1,nte-1
        x(i ,1) = s3(i)
        CALL Bezier_x( bc,ncoo-1,vple,vpte,1.D-10,x(i, 1),y(i, 1) )
        x(i,jp) = s3(i)
        IF (x(i,jp).GE.xp1 .AND. x(i,jp).LE.x(nte,jp)) THEN
          CALL Bezier_x( bc,ncoo-1,vpte,1.D0,1.D-10,x(i,jp),y(i,jp) )
        ELSE
          CALL Bezier_x( bc,ncoo-1,0.D0,vple,1.D-10,x(i,jp),y(i,jp) )
        ENDIF
        y(i,jp) = y(i,jp) + t
      ENDDO

C --- trailing edge to outlet

      CALL Stretch( dxte,d34,nxo,rat )
      ang = pi*beta2/180.D0
      ds  = dxte
      s   = 0.D0
      DO i=nte+1,ip
        s       = s + ds
        x(i, 1) = s + x(nte,1)
        y(i, 1) = s*TAN(ang) + y(nte,1)
        x(i,jp) = x(i,1)
        y(i,jp) = y(i,1) + t
        ds      = ds*rat
      ENDDO

C --- distribute points in j-direction ----------------------------------------

      WRITE(*,'(A)') ' Distributing points in j-direction ...'

C --- inlet

      ds = t/REAL(ny)
      DO j=2,jp-1
        x(1,j) = x(1,1)
        y(1,j) = y(1,j-1) + ds
      ENDDO

C --- leading edge

      s  = 0.D0
      ds = dyle
      DO j=1,ncwy
        s  = s + ds
        ds = ds*strfacw
      ENDDO

      CALL Sstretch( 1,1+ncwy,jp-ncwy,jp,0.D0,s,t-s,t,dyle,dyle,s1 )

      DO j=2,jp-1
        x(nle,j) = x(nle,1)
        y(nle,j) = y(nle,1) + s1(j)
        IF ((j.GT.2) .AND. (s1(j).LE.s1(j-1))) THEN
          WRITE(*,*) 'ERROR - failed at leading edge'
          GOTO 9999
        ENDIF
      ENDDO

C --- trailing edge

      sp = 0.D0
      ds = dytep
      DO j=1,ncwy
        sp = sp + ds
        ds = ds*strfacw
      ENDDO
      ss = 0.D0
      ds = dytes
      DO j=1,ncwy
        ss = ss + ds
        ds = ds*strfacw
      ENDDO

      CALL Sstretch( 1,1+ncwy,jp-ncwy,jp,0.D0,ss,t-sp,t,dytes,dytep,s1 )

      DO j=2,jp-1
        x(nte,j) = x(nte,1)
        y(nte,j) = y(nte,1) + s1(j)
        IF ((j.GT.2) .AND. (s1(j).LE.s1(j-1))) THEN
          WRITE(*,*) 'ERROR - failed at trailing edge'
          GOTO 9999
        ENDIF
      ENDDO

C --- outlet

      CALL Stretch( dyout,0.5D0*t,ny/2,rat )
      CALL Sstretch( 1,3,jp-2,jp,0.D0,(1.D0+rat)*dyout,
     &               t-(1.D0+rat)*dyout,t,dyout,dyout,s1 )

      DO j=2,jp-1
        x(ip,j) = x(ip,1)
        y(ip,j) = y(ip,1) + s1(j)
        IF ((j.GT.2) .AND. (s1(j).LE.s1(j-1))) THEN
          WRITE(*,*) 'ERROR - failed at outlet'
          GOTO 9999
        ENDIF
      ENDDO

C --- distribute points inside the domain -------------------------------------

      WRITE(*,'(A)') ' Generating interior grid ...'

C --- inlet to leading edge

      CALL Tfint( im,jm,1,nle,1,jp,s1,s2,s3,s4,x,y )

C --- leading to trailing edge

      CALL Tfint( im,jm,nle,nte,1,jp,s1,s2,s3,s4,x,y )

C --- trailing edge to outlet

      CALL Tfint( im,jm,nte,ip,1,jp,s1,s2,s3,s4,x,y )

C --- save files --------------------------------------------------------------

      nc = Length(title)

C --- write out grid

      WRITE(*,'(A)') ' Storing grid file ...'

      OPEN(10,FILE=fn_grid,FORM='formatted',STATUS='unknown')
      WRITE(10,1010) title(1:nc),ip-1,jp-1
      WRITE(10,1015) ((x(i,j),y(i,j), i=1,ip), j=1,jp)
      CLOSE(10)

C --- write out topology

      WRITE(*,'(A)') ' Storing topology file ...'

      OPEN(20,FILE=fn_gtop,FORM='formatted',STATUS='unknown')
      WRITE(20,1020) title(1:nc),8,ip-1,jp-1
      WRITE(20,1025) 'periodic 1'   ,700,1,2    ,nle,3,2    ,nle
      WRITE(20,1025) 'suction side' ,300,1,nle+1,nte,0,0    ,0
      WRITE(20,1025) 'periodic 2'   ,700,1,nte+1,ip ,3,nte+1,ip
      WRITE(20,1025) 'outflow'      ,200,2,2    ,jp ,0,0    ,0
      WRITE(20,1025) 'shadow 2'     ,700,3,nte+1,ip ,1,nte+1,ip
      WRITE(20,1025) 'pressure side',300,3,nle+1,nte,0,0    ,0
      WRITE(20,1025) 'shadow 1'     ,700,3,2    ,nle,1,2    ,nle
      WRITE(20,1025) 'inflow'       ,100,4,2    ,jp ,0,0    ,0
      CLOSE(20)

C --- plot file

      WRITE(*,'(A)') ' Storing plot file ...'

      OPEN(30,FILE=fn_plot,FORM='formatted',STATUS='unknown')
      WRITE(30,1045) title(1:nc),ip,jp
      DO j=1,jp
        DO i=1,ip
          WRITE(30,1035) x(i,j),y(i,j)
        ENDDO
      ENDDO
      WRITE(30,1050) ncoo
      DO i=1,ncoo
        WRITE(30,1035) xyb(1,i),xyb(2,i)
      ENDDO
      CLOSE(30)

      WRITE(*,'(/,A,/)') ' Finished.'


1000  FORMAT(' Number of grid points: ',I3,' x ',I3,/,
     &       ' Leading edge index   : ',I3,/,
     &       ' Trailing edge index  : ',I3)
1005  FORMAT(' Leading edge position : ',E12.5,', ',E12.5,/,
     &       ' Trailing edge position: ',E12.5,', ',E12.5,/)
1010  FORMAT('# ',A,/,'#',/,'# no. of cells in i-, j-direction',/,
     &       I6,2X,I6,/,'# coordinates (x, y):')
1015  FORMAT(2E17.9)
1020  FORMAT('# ',A,/,'#',/,'# no. of segments',/,I6,/,'#',/,
     &       '# no. of cells in i-, j-direction',/,I6,I6,/,'#',/,
     &       '# segments (cell-centred index!):',/,'#',/,
     &       '# itype lb  lbeg  lend  lbs  lbegs  lends',/,'#')
1025  FORMAT('# ',A,/,I6,I4,I6,I6,I5,2I7)
1035  FORMAT(2E17.9)
1045  FORMAT(A,/,'1',/,'Structured Grid',/,'2 2',/,'x',/,'y',/,
     &       I4,I4,/,'0 0 0',/,'grid')
1050  FORMAT(I4,' 1',/,'0 0 0',/,'orig. coords.')
9999  STOP
      END


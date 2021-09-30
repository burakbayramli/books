C#######################################################################
      PROGRAM PLOTP
C#######################################################################
C     This code produces plots of grid, velocity vectors, and profiles, 
C     contour lines and color fills for any quantity. The output are 
C     postscript files for each page. The code can easily be adapted for 
C     interactive use and screen window output, but as this is to some    
C     extent hardware-dependent, only postscript output is provided here. 
C     The same code is used for both single-grid and multi-grid solutions. 
C     Array dimensions NX and NY should be equal to or greater than the 
C     maximum number of nodes in respective directions on the finest grid. 
C     Up to 128 contours can be ploted. A zoom factor (magnification) and 
C     the center point coordinates can be specified. The plots are saved 
C     on files which carry the name + data set number (data set number 
C     contain four digits), eg. "vect0001.ps" for the plot of velocity 
C     vectors from data set 1.
C                                                             
C     This is Version 1.2 of the code, October 1996.          
C                                                             
C     The user may modify the code and give it to third       
C     parties, provided that an acknowledgement to the        
C     source of the original version is retained.             
C                                                             
C                     M. Peric, IfS, Hamburg, 1996            
C-----------------------------------------------------------------------
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'
      COMMON /VEL/ U(NXY),V(NXY)
      DIMENSION P(NXY),T(NXY),PSI(NXY),F2(NXY)
      LOGICAL   GRID,VEL,PRES,SF,TEMP,LPROF,UMAG,VMAG
      parameter (mnm=30)
      character name*(mnm),filin*(mnm+4),filref*(mnm),filpos*(mnm+8)
*     CHARACTER FILPOS*42,FILIN*34,FILREF*30,NAME*30
      logical lco
C=============================================================
C.....OPEN FILES
C=============================================================
C     Problem name must be that used in the calculation.
C     Input file contains control parameters (what to plot)
C     and must be prepared by the user in advance;
C     results file contains data written by the flow solver
C     (grid, variable values); file with reference data
C     contains profiles of some variables (from experiment or
C     other computations) which shall be plotted along with
C     profiles from result file. A dummy name should be given 
C     if no such data is used.     
C-------------------------------------------------------------
C
      WRITE(*,*) ' ENTER PROBLEM NAME (UP TO ',MNM,' CHARACTERS):  '
      READ(*,'(A)') NAME
      WRITE(*,*) ' ENTER NAME OF FILE WITH REFERENCE DATA ',
     &           '(UP TO ',MNM,' CHAR.):'
      READ(*,'(A)') FILREF
C
      CALL STRLEN(NAME,NAME,NNAME)
      FILIN=NAME(1:NNAME)//'.pin'
C
      lco=.true.
      ifec=6
C==========================================================
C.....CHOOSE DATA SET TO PLOT (GRID NUMBER FOR STEADY FLOW)
C==========================================================
C
      DO LK=1,MDSN
C
C==========================================================
c
      OPEN (UNIT=2,FILE=FILREF,STATUS='OLD',ERR=101)
      REWIND 2
  101 CONTINUE
C
      WRITE(*,'(a,$)') ' ENTER DATA SET NUMBER:  '
      READ (*,*) ICOUNT
      WRITE(*,*) ICOUNT
      IF(ICOUNT.EQ.0) GO TO 555
C
      write(filpos,'(a,i4.4,a4)') name(1:nname),icount,'.pos'
      OPEN (UNIT=8,FILE=FILPOS,FORM='UNFORMATTED',STATUS='OLD',ERR=102)
      REWIND 8
      OPEN (UNIT=1,FILE=FILIN,STATUS='OLD',ERR=103)
      REWIND 1
C
C-----------------------------------------------------------------
C     Magnification factor FMAG is used to magnify the figure.
C     XOR and YOR are the coordinates (in physical space) of
C     the point which should be in the center of the plot. The
C     whole plot will be produced always, but if FMAG is large,
C     large portion of the figure will be off paper bounds and
C     will not be seen on printed figure. One can also play with
C     Bounding Box numbers in the PostScript file to adjust what
C     should be seen when a figure is included in text, e.g. using
C     'psfig' in LaTeX. Note that too large a magnification factor
C     may lead to resolution problems.
C----------------------------------------------------------------- 
C
      if(lco) write(ifec,*) 'ENTER: FMAG,XOR,YOR'
      READ(1,*,err=205) FMAG,XOR,YOR
      IF(ABS(FMAG-1.0).LT.0.1) THEN
        XOR=0.
        YOR=0.
      ENDIF
C
C.....READ GRID DATA AND RESULTS FROM UNIT 8
C
      READ(8) ITIM,TIME,NI,NJ,NIM,NJM,NIJ,
     *        (X(IJ),IJ=1,NIJ),(Y(IJ),IJ=1,NIJ),
     *        (XC(IJ),IJ=1,NIJ),(YC(IJ),IJ=1,NIJ),
     *        (PSI(IJ),IJ=1,NIJ),(F2(IJ),IJ=1,NIJ),
     *        (U(IJ),IJ=1,NIJ),(V(IJ),IJ=1,NIJ),
     *        (P(IJ),IJ=1,NIJ),(T(IJ),IJ=1,NIJ)
      REWIND 8
      CLOSE(UNIT=8)
C
      DO I=1,NI
	LI(I)=(I-1)*NJ
      END DO
C
C=============================================================
C.....CONTROL PARAMETERS AND FLUID PROPERTIES
C=============================================================
C     Logical variables GRID, VEL, PRES, TEMP and SF define
C     whether the grid, velocity vectors or profiles, pressure
C     contours or colour fill, temperature contours or
C     colour fill, and streamlines or colour fill will be 
C     ploted. If one grid level is to be skiped, set all
C     these variables to FALSE.
C     LPROF will plot the profils for velocity and temperature
C     as well.
C-------------------------------------------------------------
C
      if(lco) write(ifec,*) 'ENTER: GRID,VEL,UMAG,VMAG,PRES,TEMP,SF'
      READ(1,*,err=205) GRID,VEL,UMAG,VMAG,PRES,TEMP,SF
C
      IF(GRID.OR.VEL.OR.PRES.OR.TEMP.OR.SF.OR.UMAG.OR.VMAG) THEN 
C
C------------------------------------------------------------
C     UIN and DENSIT are the reference velocity and density
C     used to scale pressure.
C------------------------------------------------------------
C
      if(lco) write(ifec,*) 'ENTER: UIN,DENSIT,AROMAX,CWITH,BWITH'
      READ(1,*,err=205) UIN,DENSIT,AROMAX,CWITH,BWITH
C
C-------------------------------------------------------------
C  XBOX and YBOX define the position of the bottom left corner
C  of the box which contains information about contours 
C  (colour scale interpretation). The values are given in inches
C  relative to bottom left corner of the ploting area (e.g. if
C  the geometry is long in X and shorter in Y direction, there
C  is room within the 7 x 7 inches area for the info-box).
C  Maximum velocity vector or profile width will be AROMAX inches.
C-------------------------------------------------------------
C
      if(lco) write(ifec,*) 'ENTER: XBOX,YBOX,XTIM,YTIM,SCFT'
      READ(1,*,err=205) XBOX,YBOX,XTIM,YTIM,SCFT
      XBOX=XBOX*2400.
      YBOX=YBOX*2400.
      XTIM=XTIM*2400.
      YTIM=YTIM*2400.
      AROMAX=AROMAX*2400.
C
C-------------------------------------------------------------
C  Every IARth vector in I-direction and JARth vector in 
C  J-direction will be ploted if VEL=TRUE; NPRX profiles at 
C  constant X and NPRY profiles at constant Y will be ploted;
C-------------------------------------------------------------
C
      if(lco) write(ifec,*) 'ENTER: IAR,JAR,NPRX,NPRY'
      READ(1,*,err=205) IAR,JAR,NPRX,NPRY
C
C---------------------------------------------------------------
C  XPRO(I) are the coordinates X at which profiles of variables
C  are to be ploted. IEPRX(I) defines for which profile there is
C  comparison data (1 -> comparison data exists, 0 -> no data).
C  XPRO(I) is to be given in meters.
C----------------------------------------------------------------
C
      if(lco) write(ifec,*) 'ENTER: (XPRO(I),I=1,NPRX)'
      READ(1,*,err=205) (XPRO(I),I=1,NPRX)
      if(lco) write(ifec,*) 'ENTER: (IEPRX(I),I=1,NPRX)'
      READ(1,*,err=205) (IEPRX(I),I=1,NPRX)
C
C-----------------------------------------------------------------
C     YPRO(I) are the coordinates Y at which profiles of variables
C     are to be ploted. IEPRY(I) defines for which profile there is
C     comparison data (1 -> comparison data exists, 0 -> no data).
C     YPRO(I) is to be given in meters.
C-----------------------------------------------------------------
C
      if(lco) write(ifec,*) 'ENTER: (YPRO(J),J=1,NPRY)'
      READ(1,*,err=205) (YPRO(J),J=1,NPRY)
      if(lco) write(ifec,*) 'ENTER: (IEPRY(J),J=1,NPRY)'
      READ(1,*,err=205) (IEPRY(J),J=1,NPRY)
      LPROF=(NPRX.GT.0.OR.NPRY.GT.0)
C
C==========================================================
C.....SOLUTION DOMAIN DIMENSIONS, SCALING FACTOR
C==========================================================
C     The solution domain is maped on an area 7 x 7 inches;
C     Postscript unit is a point (72 points in an inch), and
C     in order to enable accurate drawing on printers with
C     a resolution of 2400 x 2400 dot per inch, the coordinates
C     are scaled appropriately (7 x 2400 = 16800, thus 7 inches
C     are represented by 16800 dots; a scaling factor of 0.03
C     is used to convert dots of maximum printer resolution
C     to postscript units). XPAGE and YPAGE define the ploting
C     area of 7 x 7 inches.
C---------------------------------------------------------------
C
      XMAX =-1.E20
      YMAX =-1.E20
      XMIN = 1.E20
      YMIN = 1.E20
      XPAGE= 16800.
      YPAGE= 16800.
C
      DO IJ=1,NIJ
	XMAX=MAX(XMAX,X(IJ))
	YMAX=MAX(YMAX,Y(IJ))
	XMIN=MIN(XMIN,X(IJ))
	YMIN=MIN(YMIN,Y(IJ))
      END DO
C
      XTOT=XMAX-XMIN
      YTOT=YMAX-YMIN
      SCFX=XPAGE/XTOT
      SCFY=XPAGE/YTOT
      SCFG=MIN(SCFX,SCFY)
      XTOT=XTOT*SCFG
      YTOT=YTOT*SCFG
C
C.....CALCULATE SHIFT IN COORDINATES IN CASE OF ZOOM
C
      IF(ABS(FMAG-1.0).LT.0.1) THEN
        XOR=0.5*(XMAX+XMIN)
        YOR=0.5*(YMAX+YMIN)
      ENDIF
      XOFFS=0.5*XTOT-(XOR-XMIN)*SCFG*FMAG
      YOFFS=0.5*YTOT-(YOR-XMIN)*SCFG*FMAG
C
C.....GRID COORDINATES AND CENTRAL POINTS COORINATES ON PLOTING PAGE
C
      DO IJ=1,NIJ
	XC(IJ)=(XC(IJ)-XMIN)*SCFG*FMAG+XOFFS
	YC(IJ)=(YC(IJ)-YMIN)*SCFG*FMAG+YOFFS
	X(IJ)=(X(IJ)-XMIN)*SCFG*FMAG+XOFFS
	Y(IJ)=(Y(IJ)-YMIN)*SCFG*FMAG+YOFFS
      END DO
C
C.....CALCULATE SCALING OF PROFILE COORDINATES
C
      DO I=1,NPRX
         XPRO(I)=(XPRO(I)-XMIN)*SCFG*FMAG+XOFFS
      END DO
      DO J=1,NPRY
         YPRO(J)=(YPRO(J)-YMIN)*SCFG*FMAG+YOFFS
      END DO
C
      XMIN=0.
      YMIN=0.
      XMAX=XTOT*0.03
      YMAX=YTOT*0.03
C
C======================================================
C.....PLOT THE GRID
C======================================================
C
      IF(GRID) THEN
        WRITE(*,*) '    plot the grid '
	CALL BPLOT('grid',ICOUNT)
	CALL GRIDPL
	WRITE(7,*) 'p'
	CLOSE(UNIT=7)
      ENDIF
C
C======================================================
C.....PLOT VELOCITY VECTORS 
C======================================================
C     NCOL is the number of colours used for colouring
C     velocity vectors according to their magnitude; if
C     NCOL=1, black vectors are ploted.
C------------------------------------------------------
      if(lco) write(ifec,*) 'ENTER: NCOL,IVP'
      READ(1,*,err=205) NCOL,IVP
      IF(VEL) THEN
        WRITE(*,*) '    plot velocity vectors '
	CALL SETCOL(NCOL)
	CALL BPLOT('vect',ICOUNT)
	CALL VELPl(NCOL,IVP)
	WRITE(7,*) 'p'
	CLOSE(UNIT=7)
      END IF 
C
C======================================================
c.....PLOT U-VELOCITY CONTOURS
C======================================================
C     ICON defines how the contour values are defined:
C     if ICON=0, the values are read from input file,
C     otherwise they are calculated between maximum and
C     minimum value, see the routine CONT. NCON is the 
C     number of contours to be ploted. ICOL defines 
C     whether the contours are to be ploted in colour
C     or black (ICOL=0 -> black, otherwise colour).
C     IVEC and IGRID define whether the vector or/and
C     grid plot is to be overlayed over the colour fill
C     plot (0 -> no overlay, 1 -> yes).
C-------------------------------------------------------
      if(lco) write(ifec,49) 'U-Velocity'
      READ(1,*,err=205) ICON,NCON,ICOL,IVEC,IGRID
      IF(UMAG) THEN
        IF(ICON.EQ.0) THEN
          if(lco) write(ifec,*) 'ENTER: (CVAL(I),I=1,NCON)'
          READ(1,*,err=205) (CVAL(I),I=1,NCON)
        ENDIF
        DO IJ=1,NIJ
          FI(IJ)=U(IJ)
        END DO
C..plot conturs
        WRITE(*,*) '    plot u-velocity conturs '
C
        CALL BPLOT('isou',ICOUNT)
        CALL CONT(ICON,NCON,ICOL,NIM,NJM,'U-VELOCITY     ')
        WRITE(7,*) 'p'
        CLOSE(UNIT=7)
C======================================================
C.....PLOT U-VELOCITY COLOR FILL (IF ICOL.GT.0)
C======================================================
        IF(ICOL.GT.0) THEN
          WRITE(*,*) '    plot u-velocity color fill'
          CALL SETCOL(NCON)
          CALL BPLOT('uvel',ICOUNT)
          CALL PAINT(NCON,NIM,NJM,'U-VELOCITY     ')
          IF(IVEC.EQ.1) CALL VELPL(0,0)
          IF(IGRID.EQ.1) CALL GRIDPL
          WRITE(7,*) 'p'
          CLOSE(UNIT=7)
        ENDIF
C======================================================
C.....PLOT U VELOCITY PROFILES
C======================================================
        IF(LPROF) THEN
          WRITE(*,*) '    plot u-velocity profiles'  
          CALL PROFIL('U-VELOCITY     ','upr',ICOUNT)
        ENDIF
      ENDIF
C======================================================
C.....PLOT V-VELOCITY CONTOURS
C======================================================
      if(lco) write(ifec,49) 'V-Velocity'
      READ(1,*,err=205) ICON,NCON,ICOL,IVEC,IGRID
      IF(VMAG) THEN
        IF(ICON.EQ.0) THEN
          if(lco) write(ifec,*) 'ENTER: (CVAL(I),I=1,NCON)'
          READ(1,*,err=205) (CVAL(I),I=1,NCON)
        ENDIF
        DO IJ=1,NIJ
          FI(IJ)=V(IJ)
        END DO
c..plot conturs
        WRITE(*,*) '    plot v-velocity conturs'   
C
        CALL BPLOT('isov',ICOUNT)
        CALL CONT(ICON,NCON,ICOL,NIM,NJM,'V-VELOCITY     ')
        WRITE(7,*) 'p'
        CLOSE(UNIT=7)
C======================================================
C.....PLOT V-VELOCITY COLOR FILL
C======================================================
        IF(ICOL.GT.0) THEN
          WRITE(*,*) '    plot v-velocity color fill'
          CALL SETCOL(NCON)
          CALL BPLOT('vvel',ICOUNT)
          CALL PAINT(NCON,NIM,NJM,'V-VELOCITY     ')
          IF(IVEC.EQ.1) CALL VELPL(0,0)
          IF(IGRID.EQ.1) CALL GRIDPL
          WRITE(7,*) 'p'
          CLOSE(UNIT=7)
        ENDIF
C======================================================
C.....PLOT V VELOCITY PROFILES
C======================================================
        IF(LPROF) THEN
          WRITE(*,*) '    plot v-velocity profiles'
          CALL PROFIL('V-VELOCITY     ','vpr',ICOUNT)
        ENDIF
      ENDIF
C======================================================
C.....PLOT ISOBARS
C======================================================
      if(lco) write(ifec,49) 'Pressure'
      READ(1,*,err=205) ICON,NCON,ICOL,IVEC,IGRID
      IF(PRES) THEN
        IF (ICON.EQ.0) THEN
          write(99,*) 'ENTER: (CVAL(I),I=1,NCON)'
          READ(1,*,err=205) (CVAL(I),I=1,NCON)
        END IF
        SCFV=1./(DENSIT*UIN**2)
c
        DO IJ=1,NIJ
          FI(IJ)=P(IJ)*SCFV
        END DO
c..plot conturs
        WRITE(*,*) '    plot pressure conturs'
C
        CALL BPLOT('isob',ICOUNT)
        CALL CONT(ICON,NCON,ICOL,NIM,NJM,'ISOBARS        ')
        WRITE(7,*) 'p'
        CLOSE(UNIT=7)
C======================================================
C.....PLOT PRESSURE COLOR FILL
C======================================================
	IF(ICOL.GT.0) THEN
          WRITE(*,*) '    plot pressure color fill '
	  CALL SETCOL(NCON)
	  CALL BPLOT('pres',ICOUNT)
	  CALL PAINT(NCON,NIM,NJM,'ISOBARS        ')
	  IF(IVEC.EQ.1) CALL VELPL(0,0)
	  IF(IGRID.EQ.1) CALL GRIDPL
	  WRITE(7,*) 'p'
	  CLOSE(UNIT=7)
	ENDIF
      ENDIF
C======================================================
C.....PLOT TEMPERATURE CONTOURS
C======================================================
      if(lco) write(ifec,49) 'Temperature'
      READ(1,*,err=205) ICON,NCON,ICOL,IVEC,IGRID
      IF(TEMP) THEN
        IF(ICON.EQ.0) THEN
          if(lco) write(ifec,*) ' ENTER: (CVAL(I),I=1,NCON)'
          READ(1,*,err=205) (CVAL(I),I=1,NCON)
        ENDIF
        DO IJ=1,NIJ
          FI(IJ)=T(IJ)
        END DO
c..plot conturs
        WRITE(*,*) '    plot temperature contours '
C
        CALL BPLOT('isot',ICOUNT)
        CALL CONT(ICON,NCON,ICOL,NIM,NJM,'ISOTHERMS      ')
        WRITE(7,*) 'p'
        CLOSE(UNIT=7)
C======================================================
C.....PLOT TEMPERATURE COLOR FILL
C======================================================
	IF(ICOL.GT.0) THEN
          WRITE(*,*) '    plot temperature color fill'
	  CALL SETCOL(NCON)
	  CALL BPLOT('temp',ICOUNT)
	  CALL PAINT(NCON,NIM,NJM,'ISOTHERMS      ')
	  IF(IVEC.EQ.1) CALL VELPL(0,0)
	  IF(IGRID.EQ.1) CALL GRIDPL
	  WRITE(7,*) 'p'
	  CLOSE(UNIT=7)
	ENDIF
C======================================================
C.....PLOT TEMPERATURE PROFILES
C======================================================
	IF(LPROF) THEN
          WRITE(*,*) '    plot temperature profiles '
	  CALL PROFIL('TEMPERATURE    ','tpr',ICOUNT)
	ENDIF
      ENDIF
C===========================================================
C.....CALCULATE STREAM FUNCTION VALUES
C===========================================================
C     Streamfunction values are calculated at CV corners
C     using mass fluxes throufg cell faces (since the
C     mass flow rate between two streamlines is constant).
C     The value at the south-west corner is zero. Since
C     the contour and colour fill routines operate on
C     values at XC and YC, and PSI is calculated at 
C     locations defined by X and Y, the true XC and YC
C     are copied into P and T arrays and then overwritten
C     by X and Y.    
C-----------------------------------------------------------
   49 format(' ENTER: ICON,NCON,ICOL,IVEC,IGRID,JCON,JPLO -> ',a)
c
      if(lco) write(ifec,49) 'Streamfunction'
      READ(1,*,err=205) ICON,NCON,ICOL,IVEC,IGRID
      IF(SF) THEN
	PSI(1)=0.
	DO I=1,NIM
	  II=LI(I)
	  IF(I.NE.1) PSI(II+1)=PSI(II-NJ+1)-F2(II+1)
	  DO J=2,NJM
	    IJ=II+J
	    PSI(IJ)=PSI(IJ-1)+PSI(IJ)
	  END DO
	END DO
C===========================================================
C.....SET XC=X AND YC=Y FOR PSI-PLOTS
C===========================================================
	DO IJ=1,NIJ
	  FI(IJ)=PSI(IJ)
	  P(IJ)=XC(IJ)
	  T(IJ)=YC(IJ)
	  XC(IJ)=X(IJ)
	  YC(IJ)=Y(IJ)
	END DO
C=================================================================
c.....READ CONTROL PARAMETERS AND CONTOUR LEVELS
C=================================================================
C     If ICON=0, contour levels are read from input file; otherwise
C     they are calculated below considering the minimum and 
C     maximum value at the interior and at boundaries.
C-------------------------------------------------------------------
	IF(ICON.EQ.0) THEN
	  if(lco) write(ifec,*) ' ENTER: (CVAL(I),I=1,NCON)'
	  READ(1,*,err=205) (CVAL(I),I=1,NCON)
	ELSE
C
C.....FIND MIN AND MAX VALUES OF PSI IN INTERIOR
C
	  PSIMAX=-1.E20
	  PSIMIN=1.E20
	  DO I=1,NIM
	  DO J=1,NJM
	    IJ=LI(I)+J
	    IF(FI(IJ).GT.PSIMAX) THEN
	      XPSMAX=XC(IJ)
	      YPSMAX=YC(IJ)
	      PSIMAX=FI(IJ)
	    ENDIF
	    IF(FI(IJ).LT.PSIMIN) THEN
	      PSIMIN=FI(IJ)
	      XPSMIN=XC(IJ)
	      YPSMIN=YC(IJ)
	    ENDIF
	  END DO
	  END DO
C-----------------------------------------------------------
C.....FIND MIN AND MAX VALUES OF PSI AT BOUNDARY
C-----------------------------------------------------------
	  PSBMIN= 1.E20
	  PSBMAX=-1.E20
	  DO I=1,NIM
	    IJ=LI(I)+1
	    PSBMIN=MIN(PSBMIN,FI(IJ))
	    PSBMAX=MAX(PSBMAX,FI(IJ))
	    IJ=LI(I)+NJM
	    PSBMIN=MIN(PSBMIN,FI(IJ))
	    PSBMAX=MAX(PSBMAX,FI(IJ))
	  END DO
C
	  DO J=1,NJM
	    IJ=LI(1)+J
	    PSBMIN=MIN(PSBMIN,FI(IJ))
	    PSBMAX=MAX(PSBMAX,FI(IJ))
	    IJ=LI(NIM)+J
	    PSBMIN=MIN(PSBMIN,FI(IJ))
	    PSBMAX=MAX(PSBMAX,FI(IJ))
	  END DO
C=================================================================
C.....RANGES OF PSI VALUES, SET NO. OF CONTOURS IN RECIRC. REGIONS
C=================================================================
C     NCMIN contours are defined between the minimum value of PSI
C     at boundary and in the interior; it is chosen as a fraction
C     of NCON, depending on how big is the range DPSMIN.
C-----------------------------------------------------------------   
	  DELPSB=PSBMAX-PSBMIN
	  DPSMIN=PSBMIN-PSIMIN
	  DPSMAX=PSIMAX-PSBMAX
C
	  DM1=MAX(DELPSB,DPSMAX)
	  IF(DPSMIN.LT.0.01*DM1) THEN
	    NCMIN=0
	  ELSEIF(DPSMIN.GE.0.01*DM1.AND.DPSMIN.LT.0.05*DM1) THEN
	    NCMIN=NCON/4
	  ELSEIF(DPSMIN.GE.0.05*DM1.AND.DPSMIN.LT.0.1*DM1) THEN
	    NCMIN=NCON/3
	  ELSEIF(DPSMIN.GE.0.1*DM1.AND.DPSMIN.LT.0.5*DM1) THEN
	    NCMIN=NCON/2
	  ELSE
	    NCMIN=NCON
	  ENDIF
C====================================================================
C     NCMAX contours are defined between the maximum value of PSI
C     at boundary and in the interior; it is chosen as a fraction
C     of NCON, depending on how big is the range DPSMAX.
C====================================================================   
	  DM1=MAX(DELPSB,DPSMIN)
	  IF(DPSMAX.LT.0.01*DM1) THEN
	    NCMAX=0
	  ELSEIF(DPSMAX.GE.0.01*DM1.AND.DPSMAX.LT.0.05*DM1) THEN
	    NCMAX=NCON/4
	  ELSEIF(DPSMAX.GE.0.05*DM1.AND.DPSMAX.LT.0.1*DM1) THEN
	    NCMAX=NCON/3
	  ELSEIF(DPSMAX.GE.0.1*DM1.AND.DPSMAX.LT.0.5*DM1) THEN
	    NCMAX=NCON/2
	  ELSE
	    NCMAX=NCON
	  ENDIF
C
C.....SET STREAMFUNCTION CONTOUR LEVELS
C
	  LC=0
	  DPSI=DPSMIN/REAL(NCMIN+1)    
	  DO L=1,NCMIN
	    LC=LC+1
	    CVAL(LC)=PSIMIN+DPSI*REAL(L)
	  END DO 
	  LC=LC+1
	  CVAL(LC)=PSBMIN+1.E-20
C-----------------------------------------------------------
C     Boundary values are defined as contour lines. If
C     the difference between maximum and minimum values at 
C     boundary is small compared to the full range, no
C     contour values are set between these two values;
C     otherwise, NCON values are defined.
C-----------------------------------------------------------
	  IF(DELPSB.GT.0.1*(DPSMIN+DPSMAX)) THEN
	    DPSI=DELPSB/REAL(NCON+1)
	    DO L=1,NCON
	      LC=LC+1
	      CVAL(LC)=PSBMIN+DPSI*REAL(L)
	    END DO
C
	    LC=LC+1
	    CVAL(LC)=PSBMAX-1.E-20
	  ENDIF
C
	  DPSI=DPSMAX/REAL(NCMAX+1)
	  DO L=1,NCMAX
	    LC=LC+1
	    CVAL(LC)=PSBMAX+DPSI*REAL(L)
	  END DO
C
	  ICON=0
	  NCON=LC
	ENDIF
C======================================================
C.....PLOT STREAMLINES
C======================================================
        WRITE(*,*) '    plot streamlines'
        CALL BPLOT('strl',ICOUNT)
        CALL CONT(ICON,NCON,ICOL,NIM-1,NJM-1,'STREAMLINES   ')
        WRITE(7,*) 'p'
        CLOSE(UNIT=7)
C======================================================
C.....PLOT STREAMLINE COLOR FILL
C======================================================
	IF(ICOL.GT.0) THEN
          WRITE(*,*) '    plot streamline color fill'
	  CALL SETCOL(NCON)
	  CALL BPLOT('strc',ICOUNT)
	  CALL PAINT(NCON,NIM-1,NJM-1,'STREAMLINES    ')
	  IF(IVEC.EQ.1) THEN
	    ICOL=1
	    DO IJ=1,NIJ
	      XC(IJ)=P(IJ)
	      YC(IJ)=T(IJ)
	    END DO
	    CALL VELPL(0,0)
	  ENDIF
	  IF(IGRID.EQ.1) CALL GRIDPL
	  WRITE(7,*) 'p'
	  CLOSE(UNIT=7)
	ENDIF
      ENDIF
C
      ENDIF
C============================================================
C.....END OF OPERATIONS ON THIS DATA SET
C============================================================
      CLOSE(UNIT=1)
      CLOSE(UNIT=2)
      lco=.false.
C-----------------------------
      END DO
C-----------------------------
C
  555 CONTINUE
C
      STOP
  205 write(*,*) ' '
      write(*,*) ' error with reading input file '
      write(*,*) ' '
      stop
  102 WRITE(*,'(A,A10)') '  ERROR: CANNOT OPEN FILE ',FILPOS
      STOP
  103 WRITE(*,'(A,A10)') '  ERROR: CANNOT OPEN FILE ',FILIN
      STOP
      END
C##################################################################
      SUBROUTINE VELPL(NCOL,IVP)
C##################################################################
C     This routine plots velocity vectors (every IARth and JARth
C     value in I and J direction, respectively). Colour is defined
C     by RGB values, which range between 0. and 1. for each of 
C     the three base colours. The appropriate values for R, G and B
C     are calculated in the routine SETRGB.
C==================================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /VEL/ U(NXY),V(NXY)
      COMMON /RGB/ R(255),G(255),B(255)
      CHARACTER TT*17
C
C.....CALCULATE SCALING FACTOR FOR VELOCITIES (MAX. VALUE = AROMAX)
C
      VMAX=-1.0E10
      VMIN=1.E10
      DO IJ=1,NIJ
	VMEAN=SQRT(U(IJ)**2+V(IJ)**2)
	VMIN=MIN(VMIN,VMEAN)
	VMAX=MAX(VMAX,VMEAN)
      END DO
      SCF=AROMAX/(VMAX+1.E-15)
C
C.....SET COLOUR LEVELS (FOR NCOL=1, BLACK IS USED)
C
      DELV=(VMAX-VMIN)/MAX(REAL(NCOL),1.)
      DO N=1,NCOL+1
	CVAL(N)=VMIN+REAL(N-1)*DELV
      END DO
      WRITE(7,*) CWITH,' w'
      IF(NCOL.EQ.1) WRITE(7,*) '0. 0. 0. col'
C
C=======================================================
c.....PLOT VECTORS AT EACH  IAR-TH AND JAR-TH POSITION
C=======================================================
C
      ISTA=2
      IF(IVP.EQ.1) ISTA=1
C
      DO I=ISTA,NI-ISTA+1,IAR
      DO J=ISTA,NJ-ISTA+1,JAR
	IJ=LI(I)+J
	X1=U(IJ)*SCF+XC(IJ)
	Y1=V(IJ)*SCF+YC(IJ)
C
C.....ASSIGN COLOUR ACCORDING TO VELOCITY VECTOR MAGNITUDE
C
	VM=SQRT(U(IJ)**2+V(IJ)**2)
	IF(NCOL.GT.1) THEN
	  DO N=2,NCOL+1
	    IF(VM.LE.CVAL(N).AND.VM.GE.CVAL(N-1)) THEN
	      WRITE(7,*) R(N-1),G(N-1),B(N-1),' col'
	    ENDIF
	  END DO
	ENDIF
C
C....DRAW THE ARROW
C 
	CALL ARROW(XC(IJ),YC(IJ),X1,Y1)
      END DO
      END DO
C
C=======================================================
C.....WRITE TITLE AND VECTOR SCALE TO POSTSCRIPT FILE
C=======================================================
c
      WRITE(7,*) '0. 0. 0. col'
      TSIZE=600.
      X1=0.35*XTOT
      Y1=YTOT+3.*TSIZE
      X2=X1+AROMAX
      Y2=Y1
      CALL ARROW(X1,Y1,X2,Y2)
      X2=X2+TSIZE
      Y2=Y2-0.5*TSIZE
      WRITE(7,*) INT(X2),INT(Y2),'  m'
      WRITE(7,*) '/Times-Roman findfont 500.00 scalefont setfont'
      WRITE(tt,'(4h =  ,1pe8.2,5h m/s )') vmax
      WRITE(7,*) '(',tt,') show'
      Y1=Y1+2.*TSIZE
      WRITE(7,*) INT(X1),INT(Y1),'  m'
      WRITE(7,*) '/Helvetica findfont 600.00 scalefont setfont'
      WRITE(tt,'(17hVelocity  Vectors)')
      WRITE(7,*) '(',tt,') show s'
C
      RETURN
      END
C
C##################################################
      SUBROUTINE SETCOL(NCOL)
C##################################################
C     This routine sets the values of R, G, and B
C     composition of the colour table. NCOL values
C     are set so that the range of values from MAX
C     to MIN corresponds to the colour scale from
C     pink to deep blue.
C==================================================
      INCLUDE 'float.inc'
      COMMON /CBWITH/ CWITH,BWITH 
      COMMON /RGB/ R(255),G(255),B(255)
      DIMENSION RT(255),GT(255),BT(255)
C
      IF(NCOL.LT.5) RETURN
C
C----------------------------------
C.....FIVE RANGES IN SPECTRUM
C----------------------------------
      NDC=NCOL/5
      DC=1./REAL(NDC)
C----------------------------------
C.....RANGE 1: PINK TO RED
C----------------------------------
      DO L=1,NDC
	RT(L)=1.
	GT(L)=0.
	BT(L)=1.-(L-1)*DC
      END DO
      NL=NDC
C----------------------------------
C.....RANGE 2: RED TO YELLOW
C----------------------------------
      DO L=1,NDC
	RT(NL+L)=1.
	GT(NL+L)=(L-1)*DC
	BT(NL+L)=0.
      END DO
      NL=NL+NDC
C----------------------------------
C.....RANGE 3: YELLOW TO GREEN
C----------------------------------
      DO L=1,NDC
	RT(NL+L)=1.-(L-1)*DC
	GT(NL+L)=1.
	BT(NL+L)=0.
      END DO
      NL=NL+NDC
C----------------------------------
C.....RANGE 4: GREEN TO LIGHT BLUE
C----------------------------------
      DO L=1,NDC
	RT(NL+L)=0.
	GT(NL+L)=1.
	BT(NL+L)=(L-1)*DC
      END DO
      NL=NL+NDC
C
      NDC=NCOL-4*NDC
      DC=1./REAL(NDC)
C-------------------------------------
C.....RANGE 5: LIGHT BLUE TO DARK BLUE
C-------------------------------------
      DO L=1,NDC
	RT(NL+L)=0.
	GT(NL+L)=1.-(L-1)*DC
	BT(NL+L)=1.
      END DO
C
C.....SWAP THE ORDERING: DARK BLUE TO BE MINIMUM
C
      DO L=1,NCOL
	LL=NCOL-L+1
	R(L)=RT(LL)
	G(L)=GT(LL)
	B(L)=BT(LL)
      END DO
C
      RETURN
      END
C
C#####################################################################
      SUBROUTINE PROFIL(TITLE,pron,LK)
C#####################################################################
C     This routine draws profiles of the variable FI at given X and
C     Y cross-sections. The cross-sections are defined in input data
C     as XPRO(I) and YPRO(I); ten profiles can be ploted. Calculated
C     profiles can ce compared to reference data (e.g. from an
C     experiment). Reference data should be stored on a file in a
C     format given below (up to 100 points per profile; increase
C     dimension if necessary).
C=====================================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /PRO/ XP(2),YP(2)
      COMMON /RGB/ R(255),G(255),B(255)
      DIMENSION YPR(100),FIPR(100),XPR(100)
      CHARACTER TITLE*15,TT*17,FILN*4,pron*3
C
C.....CALCULATE SCALING FACTOR, SCALE VARIABLE (accorning to AROMAX)
C
      FIMAX=0.0
      DO IJ=1,NIJ
	PC=ABS(FI(IJ))
	FIMAX=MAX(FIMAX,PC)
      END DO
      SCF=AROMAX/(FIMAX+1.E-20)
      DO IJ=1,NIJ
	FI(IJ)=SCF*FI(IJ)
      END DO
C
C===============================================================
C.....PROFILES AT GIVEN  X - COORDINATE
C===============================================================
C     We consider quadrilateral elements, made by CV centers;
C     if the line X=XINT crosses two sides of such an element,
C     a segment of the profile between the two crossing points
C     is obtained (since nodes are connected by straight lines
C     and linear interpolation is used, the same crossing points
C     will be found in neighbor elements, so the profile will be
C     continuous). The variable values at the crossing points
C     are obtained by interpolation from two corner nodes. The
C     profile line is thick, the base line is thin.
C
      IF(NPRX.NE.0) THEN
        filn=pron//'x'
	CALL BPLOT(FILN,LK)
C
	DO K=1,NPRX
	  XINT=XPRO(K)
	  DO I=1,NIM
	  DO J=1,NJM
	    IJ=LI(I)+J
	    CALL PROFX(IJ,IP,XINT)
C
C     IP is the number of crossing points; it has to be two if the
C     line crosses the element. 
C
	    IF(IP.EQ.2) THEN
	      WRITE(7,*) '10 w ',INT(XINT+XP(1)),INT(YP(1)),' m ',
     *                    INT(XINT+XP(2)),INT(YP(2)),' l s'
	      WRITE(7,*) '10 w ',INT(XINT),INT(YP(1)),' m ',
     *                    INT(XINT),INT(YP(2)),' l s'
	    ENDIF
	  END DO
	  END DO
C
C------------------------------------------------------------
C.....COMPARISON WITH REFERENCE DATA
C------------------------------------------------------------
C     Reference data for each profile for which IEPRX=1 should
C     be provided on a file in the following format:
C     The first line is the number of reference points, KPR. It
C     can be zero, so one does not have to have data for each
C     variable at all cross-sections.
C     There follow KPR lines with the Y-coordinate of the reference
C     point and the variable value at that point, FIPR. A diamond
C     symbol is ploted at each point.
C
	  IF(IEPRX(K).NE.0) THEN
	  WRITE(7,*) '32 w'
	  READ(2,*) KPR
	  IF(KPR.NE.0) THEN
	    DO KK=1,KPR
	      READ(2,*) YPR(KK),FIPR(KK)
	    END DO
C
	    DO KK=1,KPR
	      YPR(KK)=YPR(KK)*SCFG
	      FIPR(KK)=XINT+FIPR(KK)*SCF*SCFV
	      WRITE(7,*) int(FIPR(KK)-200.),int(YPR(KK)),' m'
	      WRITE(7,*) int(FIPR(KK)),int(YPR(KK)-200.),' l'
	      WRITE(7,*) int(FIPR(KK)+200.),int(YPR(KK)),' l'
	      WRITE(7,*) int(FIPR(KK)),int(YPR(KK)+200.),' l'
	      WRITE(7,*) int(FIPR(KK)-200.),int(YPR(KK)),' l s'
	    END DO
	  ENDIF
	  ENDIF
	END DO
C
C------------------------------------------------------------
C.....WRITE THE TITLE AND PROFILE SCALE
C------------------------------------------------------------
C
      write(7,*) '0. 0. 0. col'
      TSIZE=600.
      x1=0.35*xtot
      y1=ytot+2.*TSIZE
      x2=x1+aromax
      y2=y1
      write(7,*) x1,y1,' m ',x2,y2,' l s'
      x2=x2+tsize
      y2=y2-0.5*tsize
      write(7,*) '/Helvetica findfont 660.00 scalefont setfont'
      write(7,*) int(x2),int(y2),'  m'
      write(tt,'(5h  =  ,1pe8.2,4h m/s)') fimax
      write(7,*) '(',tt,') show'
      y1=y1+2.*tsize
      write(7,*) int(x1),int(y1),'  m'
      write(7,*) '(',title,') show'
      WRITE(7,*) 's p'
      CLOSE(UNIT=7)
C
      ENDIF
C
C===============================================================
C.....PROFILES AT SPECIFIED Y-CROSS-SECTION
C===============================================================
C
      IF(NPRY.NE.0) THEN
        filn=pron//'y'
	CALL BPLOT(FILN,LK)
C
	DO K=1,NPRY
	  YINT=YPRO(K)
	  DO I=1,NIM
	  DO J=1,NJM
	    IJ=LI(I)+J
	    CALL PROFY(IJ,IP,YINT)
C
	    IF(IP.EQ.2) THEN
	      WRITE(7,*) '10 w ',INT(XP(1)),INT(YINT+YP(1)),' m ',
     *                    INT(XP(2)),INT(YINT+YP(2)),' l s'
	      WRITE(7,*) '10 w ',INT(XP(1)),INT(YINT),' m ',
     *                    INT(XP(2)),INT(YINT),' l s'
	    ENDIF
	  END DO
	  END DO
C
C.....COMPARISON WITH REFERENCE DATA
C
	  IF(IEPRY(K).NE.0) THEN
	  WRITE(7,*) CWITH,' w'
	  READ(2,*) KPR
	  IF(KPR.NE.0) THEN
	    READ(2,*) (XPR(KK),KK=1,KPR)
	    READ(2,*) (FIPR(KK),KK=1,KPR)
	    DO KK=1,KPR
	      XPR(KK)=XPR(KK)*SCFG
	      FIPR(KK)=XINT+FIPR(KK)*SCF*SCFV
	      WRITE(7,*) int(XPR(KK)),int(FIPR(KK)-200.),' m'
	      WRITE(7,*) int(XPR(KK)-200.),int(FIPR(KK)),' l'
	      WRITE(7,*) int(XPR(KK)),int(FIPR(KK)+200.),' l'
	      WRITE(7,*) int(XPR(KK)+200.),int(FIPR(KK)),' l'
	      WRITE(7,*) int(XPR(KK)),int(FIPR(KK)-200.),' l s'
	    END DO
	  ENDIF
	  ENDIF
	END DO
C
C.....WRITE THE TITLE AND PROFILE SCALE
C
      write(7,*) '0. 0. 0. col'
      x1=0.35*xtot
      y1=ytot+2.*tsize
      x2=x1+aromax
      y2=y1
      write(7,*) x1,y1,' m ',x2,y2,' l s'
      x2=x2+tsize
      y2=y2-0.5*tsize
      write(7,*) '/Helvetica findfont 660.00 scalefont setfont'
      write(7,*) int(x2),int(y2),'  m'
      write(tt,'(5h  =  ,1pe8.2,4h m/s)') fimax
      write(7,*) '(',tt,') show'
      y1=y1+2.*tsize
      write(7,*) int(x1),int(y1),'  m'
      write(7,*) '(',title,') show'
      write(7,*) 's p'
      CLOSE(UNIT=7)
C
      ENDIF
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE PROFX(IJ,IP,XINT)
C##########################################################
C     This routine checks if the line X=XINT crosses an
C     element, defined by the south-west corner index IJ.
C     For each side of the element, it calls routine PROPX
C     to find the crossing point. The coordinates of the
C     crossing point are stored as XP and YP. IP is the
C     number of crossing points found.
C==========================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /PRO/ XP(2),YP(2)
C
C.....FIND IF XINT CROSSES THE ELEMENT WHOSE SW-CORNER IS IJ
C
      IP=0
      XMN=MIN(XC(IJ),XC(IJ+NJ),XC(IJ+NJ+1),XC(IJ+1))
      XMX=MAX(XC(IJ),XC(IJ+NJ),XC(IJ+NJ+1),XC(IJ+1))
      IF(XINT.GE.XMN.AND.XINT.LE.XMX) THEN
	CALL PROPX(IJ,IJ+NJ,IP,XINT)
	CALL PROPX(IJ+NJ,IJ+NJ+1,IP,XINT)
	IF(IP.NE.2) CALL PROPX(IJ+NJ+1,IJ+1,IP,XINT)
	IF(IP.NE.2) CALL PROPX(IJ+1,IJ,IP,XINT)
      ENDIF
      RETURN
      END
C
C#############################################################
      SUBROUTINE PROPX(IJ1,IJ2,IP,XINT)
C#############################################################
C     This routine finds the crossing point of the line X=XINT
C     with an element side defined by node indices at its ends,
C     IJ1 and IJ2. IP is incremented by one if the crossing
C     point is found, and the coordinates are stored in XP(IP)
C     and YP(IP).
C==============================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /PRO/ XP(2),YP(2)
C
C.....SEARCH FOR CROSSING POINT BETWEEN IJ1 AND IJ2 NODES
C
      IF(XINT.GE.MIN(XC(IJ1),XC(IJ2)).AND.
     *   XINT.LE.MAX(XC(IJ1),XC(IJ2))) THEN
	IP=IP+1
	FAC=(XINT-XC(IJ1))/(XC(IJ2)-XC(IJ1)+1.E-20)
	YP(IP)=YC(IJ1)+FAC*(YC(IJ2)-YC(IJ1))
	XP(IP)=FI(IJ1)+FAC*(FI(IJ2)-FI(IJ1))
      ENDIF
      RETURN
      END
C         
C##########################################################
      SUBROUTINE PROFY(IJ,IP,YINT)
C##########################################################
C     This routine is analogous to PROFX, only for Y=YINT.
C==========================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /PRO/ XP(2),YP(2)
C
C.....FIND IF YINT CROSSES THE CELL WHOSE SW-CORNER IS IJ
C
      IP=0
      YMN=MIN(YC(IJ),YC(IJ+NJ),YC(IJ+NJ+1),YC(IJ+1))
      YMX=MAX(YC(IJ),YC(IJ+NJ),YC(IJ+NJ+1),YC(IJ+1))
      IF(YINT.GE.YMN.AND.YINT.LE.YMX) THEN
	CALL PROPY(IJ,IJ+NJ,IP,YINT)
	CALL PROPY(IJ+NJ,IJ+NJ+1,IP,YINT)
	IF(IP.NE.2) CALL PROPY(IJ+NJ+1,IJ+1,IP,YINT)
	IF(IP.NE.2) CALL PROPY(IJ+1,IJ,IP,YINT)
      ENDIF
      RETURN
      END
C
C##########################################################
      SUBROUTINE PROPY(IJ1,IJ2,IP,YINT)
C##########################################################
C     This routine is analogous to PROPX, only for Y=YINT.
C==========================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /PRO/ XP(2),YP(2)
C
C.....SEARCH FOR CROSSING POINT BETWEEN IJ1 AND IJ2 NODES
C
      IF(YINT.GE.MIN(YC(IJ1),YC(IJ2)).AND.
     *   YINT.LE.MAX(YC(IJ1),YC(IJ2))) THEN
	IP=IP+1
	FAC=(YINT-YC(IJ1))/(YC(IJ2)-YC(IJ1)+1.E-20)
	XP(IP)=XC(IJ1)+FAC*(XC(IJ2)-XC(IJ1))
	YP(IP)=FI(IJ1)+FAC*(FI(IJ2)-FI(IJ1))
      ENDIF
      RETURN
      END
C
C
C###############################################################
      SUBROUTINE ARROW(XC1,YC1,X,Y)
C###############################################################
C     This routine draws the arrow representing velocity vector.
C     XC1 and YC1 are page coordinates of vector start, X and Y
C     are coordinates of vector end. The arrow is long 20% of the
C     vector length and wide 5% of the vector length.
C===============================================================
      INCLUDE 'float.inc'
C
      VL=SQRT((X-XC1)**2+(Y-YC1)**2)
      IF(VL.GT.1.E-8) THEN
	WRITE(7,*) int(XC1),int(YC1),' m',int(X),int(Y),' l'
	DX=X-XC1
	DY=Y-YC1
	X1=X-0.2*DX
	Y1=Y-0.2*DY
	DA=0.025*VL
	SAL=DY/VL
	CAL=DX/VL
	DX=DA*SAL
	DY=DA*CAL
	X1=X1-DX
	Y1=Y1+DY
	X2=X1+2.*DX
	Y2=Y1-2.*DY
	WRITE(7,*) int(X1),int(Y1),' l',int(X2),int(Y2),' l',
     *             int(X),int(Y),' l s'
      ENDIF
      RETURN
      END
C
C
C###########################################################
      SUBROUTINE GRIDPL
C###########################################################
C     This routine plots the grid lines other than boundary
C     lines.
C===========================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'
C
      WRITE(7,*) CWITH,' w'
C
C.....PLOT I-LINES (J=CONST)
C
      DO J=2,NJM-1
	IJ=LI(1)+J
	WRITE(7,*) INT(X(IJ)),INT(Y(IJ)),' m '
C
	DO I=2,NIM
	  IJ=LI(I)+J
	  WRITE(7,*) INT(X(IJ)),INT(Y(IJ)),' l '
	END DO
	WRITE(7,*) 's'
      END DO
C
C.....PLOT  J - LINES (I=CONST)
C
      DO I=2,NIM-1
	IJ=LI(I)+1
	WRITE(7,*) int(X(IJ)),int(Y(IJ)),' m '
C
	DO J=2,NJM
	  IJ=LI(I)+J
	  WRITE(7,*) int(X(IJ)),int(Y(IJ)),' l '
	END DO
	WRITE(7,*) 's'
      END DO
   21 format(2i12,a3)



C
C.....TOP LEFT CORNER
C
      ncvs=(ni-2)*(nj-2)
      x1=xbox
      y1=ybox   !  +6600.
C
C.....PLOT INFO BOX TITLE
C
      write(7,*) '0. 0. 0. col'
      write(7,*) '/Helvetica findfont 500.00 scalefont setfont'
      write(7,*) int(x1),int(y1),
     & ' m (',ni-2,'x',nj-2,'=',ncvs,' CELLS) show '  
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE BPLOT(HEAD,L)
C##########################################################
C     This routine opens the postscript file with the name
C     specified by HEAD and L (e.g. isob1.ps if HEAD=isob,
C     and L=1), writes the postscript header to it and plots
C     the boundary grid lines with a thick line. This is
C     the first routine that has to be called for any plot.
C==========================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /RGB/ R(255),G(255),B(255)
      CHARACTER FILOUT*11,HEAD*4
C
C.....OPEN PLOT FILE, WRITE HEADER
C
      write(filout,'(a,i4.4,a)') head,l,'.ps'
C
      OPEN(UNIT=7,FILE=FILOUT,ERR=101)
      REWIND 7
      CALL PSHEAD(XMIN,XMAX,YMIN,YMAX)
      WRITE(7,*) '0. 0. 0. col'
C
C.....MOVE TO SOUTH-WEST CORNER
C
      WRITE(7,*) BWITH,' w'
      IJ=LI(1)+1
      WRITE(7,*) int(X(IJ)),int(Y(IJ)),' m '
C
C.....Draw west boundary
C
      DO J=2,NJM
	IJ=LI(1)+J
	WRITE(7,*) int(X(IJ)),int(Y(IJ)),' l '
      END DO
C
C.....Draw north boundary
C
      DO I=2,NIM
	IJ=LI(I)+NJM
	WRITE(7,*) int(X(IJ)),int(Y(IJ)),' l '
      END DO
C
C.....DRAW EAST BOUNDARY
C
      DO J=NJM-1,1,-1
	IJ=LI(NIM)+J
	WRITE(7,*) int(X(IJ)),int(Y(IJ)),' l '
      END DO
C
C.....DRAW SOUTH BOUNDARY
C
      DO I=NIM-1,1,-1
	IJ=LI(I)+1
	WRITE(7,*) int(X(IJ)),int(Y(IJ)),' l '
      END DO
   21 format(2i12,a3)
C
      WRITE(7,*) 's'
C
      RETURN
  101 write(*,'(a,a)') '  ** Error: cannot open file ',filout
      stop
      END
c
C############################################################
      SUBROUTINE CONT(ICON,NCON,ICOL,NIB,NJB,TITLE)
C############################################################
C     This routine plots contour lines of a constant value of
C     the variable stored in the array FI(NXY). It operates on
C     elements defined by CV centers. Contour lines are
C     made of segments of straight lines connecting points at
C     element edges where the interpolated varible value equals
C     the contour level. Since these segments are small, the
C     lines apear smooth for grids with more than 20 nodes in
C     one direction.
C=============================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /RGB/ R(255),G(255),B(255)
      CHARACTER TITLE*15
C
C.... FIND EXTREMA
C
      FIMAX=-1.E20
      FIMIN= 1.E20
      DO IJ=1,NIJ
	FIMAX=MAX(FIMAX,FI(IJ))
	FIMIN=MIN(FIMIN,FI(IJ))
      END DO
C
C.....SET CONTOUR LEVELS (IF ICON.NE.0; LINEAR DISTRIBUTION)
C
      IF(ICON.NE.0) THEN
	DFI=(FIMAX-FIMIN)/REAL(NCON)
	CVAL(1)=FIMIN+0.5*DFI
	DO NC=2,NCON
	  CVAL(NC)=CVAL(NC-1)+DFI
	END DO
      ENDIF
C
C.....SET LINE THICKNES AND COLORS (BLACK IF ICOL=0)
C   
      WRITE(7,*) CWITH,' w '
      IF(ICOL.EQ.0) THEN
	WRITE(7,*) '0. 0. 0. COL'
      ELSE
	CALL SETCOL(NCON)
      ENDIF
C
      DFITOL=1.E-20
      DFIMM=ABS(FIMAX-FIMIN)
      IF(DFIMM.LT.DFITOL) GO TO 921
C
C=====================================================================
C.....Search each quadrilateral element (IJ node is south-west corner)
C=====================================================================
C     There are NIM elements in I and NJM elements in J direction
C     (boundary nodes are also used in defining the elements).
C     IJ is the south-west, IJE the south-east, IJNE the north-east
C     and IJN the north-west corner of the element. The variable
C     values are available at corners (these are CV centers). If the
C     contour level is within the minimum and maximum value at the
C     four vertices of the element, the four element edges are
C     checked to find out if the contour level falls between values
C     at ends (routine NEWC). If so, the location is found by linear
C     interpolation, the coordinates are stored in CVX and CVY arrays
C     and the counter IVC is increased. The lines between points are
C     then drawn. If more than two points are found, the line is
C     closed from last to first point.
C---------------------------------------------------------------------
      DO K=1,NCON
	IF(ICOL.GT.0) WRITE(7,*) R(K),G(K),B(K),' col'
C
      DO I=1,NIB
      DO J=1,NJB
	IJ=LI(I)+J
	IJE=IJ+NJ
	IJN=IJ+1
	IJNE=IJE+1
C
C.....SEARCH FOR CONTOUR AND PLOT IT
C
	FI1=CVAL(K)
	FIMN=MIN(FI(IJ),FI(IJE),FI(IJN),FI(IJNE))
	FIMX=MAX(FI(IJ),FI(IJE),FI(IJN),FI(IJNE))
	IF(.NOT.((FI1.LT.FIMN).OR.(FI1.GT.FIMX))) THEN
	  IVC=0
	  CALL NEWC(IJ,IJE,IVC)
	  CALL NEWC(IJE,IJNE,IVC)
	  CALL NEWC(IJNE,IJN,IVC)
	  CALL NEWC(IJN,IJ,IVC)
	  IF(IVC.GT.1) THEN
	    WRITE(7,*) INT(CVX(1)),INT(CVY(1)),' m'
	    DO KK=2,IVC
	      WRITE(7,*) INT(CVX(KK)),INT(CVY(KK)),' l'
	    END DO
	    IF(IVC.GT.2) WRITE(7,*) INT(CVX(1)),INT(CVY(1)),' l'
	    WRITE(7,*) 's'
	  ENDIF
	ENDIF
      END DO
      END DO
C
      END DO
C
C.....PLOT THE BOX WITH CONTOUR LEVELS AND COLOUR SCALE
C
  921 CONTINUE
      WRITE(7,*) '0. 0. 0. col'
      CALL PLBOX(NCON,ICOL,TITLE)
C
      RETURN
      END
C
C############################################################
      SUBROUTINE NEWC(IJ1,IJ2,IVC)
C############################################################
C     This routine finds the contour point on one element
C     edge (it checks also if the corner value at IJ1 equals
C     the contour level value).
C============================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

C
c.....CHECK IF VALUE AT IJ1 EQUALS CONTOUR VALUE
C
      IF(FI(IJ1).EQ.FI1) THEN
      IVC=IVC+1
      CVX(IVC)=XC(IJ1)
      CVY(IVC)=YC(IJ1)
C
C.....CHECK IF CONTUR VALUE FI1 LIES BETWEEN VALUES AT IJ1 and IJ2;
C.....IF SO, INTERPOLATE TO FIND LOCATION BETWEEN NODES IJ1 and IJ2
C
      ELSEIF((FI1.GE.MIN(FI(IJ1),FI(IJ2))).AND.
     *    (FI1.LE.MAX(FI(IJ1),FI(IJ2)))) THEN
	IVC=IVC+1
	FAC1=(FI1-FI(IJ1))/(FI(IJ2)-FI(IJ1)+1.E-20)
	CVX(IVC)=XC(IJ1)+FAC1*(XC(IJ2)-XC(IJ1))
	CVY(IVC)=YC(IJ1)+FAC1*(YC(IJ2)-YC(IJ1))
      ENDIF
C
      RETURN
      END
C
C#############################################################
      SUBROUTINE PLBOX(NCON,ICOL,TITLE)
C#############################################################
C     This routine plots the box with contour levels and color 
C     scale.
C=============================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /RGB/ R(255),G(255),B(255)
      CHARACTER TITLE*15,TT*9
C
C.....TOP LEFT CORNER
C
      X1=XBOX
      Y1=YBOX+6600.
C
C.....PLOT INFO BOX TITLE
C
      WRITE(7,*) '0. 0. 0. col'
      WRITE(7,*) '/Helvetica findfont 500.00 scalefont setfont'
      WRITE(7,*) int(x1),int(y1),' m (',title,') show'
      Y1=Y1-600.
      WRITE(7,*) '/Helvetica findfont 400.00 scalefont setfont'
      WRITE(7,*) int(x1),int(y1),' m (Contour levels:) show'
      WRITE(7,*) '200 w'
      X2=X1+1400.
      Y1=Y1-520.
C
C.....PLOT COLORS AND CONTOUR LEVELS
C
      ISTEP=(NCON+5)/10
C
      DO N=1,NCON,ISTEP
	IF(ICOL.EQ.1) WRITE(7,*) R(N),G(N),B(N),' col'
	WRITE(7,*) int(x1),int(y1),' m ',int(x2),int(y1),' l s' 
	WRITE(TT,'(1PE9.2)') CVAL(N)
	WRITE(7,*) int(x2+400.),int(y1-160.),' m ','(',tt,') show'
	Y1=Y1-500.
      END DO
C
      IF(ITIM.NE.0) THEN
        sfont=scft*500.
        WRITE(7,*) '0. 0. 0. col'
        WRITE(7,*) '/Helvetica findfont ',sfont,' scalefont setfont'
        WRITE(7,*) INT(XTIM),INT(YTIM),' m ( TIME= ',time,' SEC) show'
      END IF
C
      RETURN
      END
C
C#############################################################
      SUBROUTINE PAINT(NCON,NIB,NJB,TITLE)
c#############################################################
C     This routine paints each element by filling one colour 
C     between two contour levels. It operates on the same
C     element as the contouring routine. Two contour levels
C     are considered, and the polygon is defined using contour
C     lines and element edges.
C=============================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

      COMMON /RGB/ R(255),G(255),B(255)
      CHARACTER TITLE*15
C      
c.....FIND EXTREMA, CHOOSE CONTOUR COLOURS, PLOT THE BOX
C
      FIMAX=-1.E20
      FIMIN=1.E20
      DO IJ=1,NIJ
	FIMAX=MAX(FIMAX,FI(IJ))
	FIMIN=MIN(FIMIN,FI(IJ))
      END DO
      DFI=(FIMAX-FIMIN)/REAL(NCON)
C
      CVAL(1)=FIMIN+0.5*DFI
      DO NC=2,NCON
	CVAL(NC)=CVAL(NC-1)+DFI
      END DO
C
      CALL SETCOL(NCON)
C
      DFITOL=1.E-20
      DFIMM=ABS(FIMAX-FIMIN)
      IF(DFIMM.LT.DFITOL) GO TO 922
C
C============================================================
c.....Paint each element (south-west corner is node IJ)
C============================================================
c     One fills first the colour between FIMIN and the first
C     contour level, then between contours, and finally between
C     the last contour level and PHIMAX.
C------------------------------------------------------------
      WRITE(7,*) '40 w '
      FI1=FIMIN
      DO K=1,NCON
	WRITE(7,*) R(K),G(K),B(K),' col'
	FI2=FI1+DFI
	IF(K.EQ.NCON) FI2=FIMAX
C
	DO I=1,NIB
	DO J=1,NJB
	  IJ=LI(I)+J
	  IJE=IJ+NJ
	  IJN=IJ+1
	  IJNE=IJE+1
C
c.....SEARCH FOR FILLING RANGE BETWEEN FI1 AND FI2
C
	  FIMN=MIN(FI(IJ),FI(IJE),FI(IJN),FI(IJNE))
	  FIMX=MAX(FI(IJ),FI(IJE),FI(IJN),FI(IJNE))
	  IF(.NOT.(((FI1.LT.FIMN).AND.(FI2.LT.FIMN)).OR. 
     *            ((FI1.GT.FIMX).AND.(FI2.GT.FIMX)))) THEN 
	    IVC=0
	    CALL NEWVC(IJ,IJE,IVC)
	    CALL NEWVC(IJE,IJNE,IVC)
	    CALL NEWVC(IJNE,IJN,IVC)
	    CALL NEWVC(IJN,IJ,IVC)
	    IF(IVC.GT.2) THEN
	      WRITE(7,*) 'n ',int(cvx(1)),int(cvy(1)),' m'
	      DO L=2,IVC
		WRITE(7,*) int(cvx(l)),int(cvy(l)),' l'
	      END DO
	      WRITE(7,*) int(cvx(1)),int(cvy(1)),' l cp f s'
	    ENDIF
	  ENDIF
	END DO
	END DO
C
      FI1=FI2
      END DO
C
  922 CONTINUE
      ICOL=1
      CALL PLBOX(NCON,ICOL,TITLE)
C
      RETURN
      END
C
C#############################################################
      SUBROUTINE NEWVC(IJ1,IJ2,IVC)
C#############################################################
C     This routine defines and fills with colour a polygon
C     bounded by two contours (FI1 and FI2) and element edges.
C     If an element corner is within the range, it becomes a
C     vertex in the polygon. When points corresponding to both
C     FI1 and FI2 are found on one edge, they are sorted out so
C     that polygon edges define a closed surface (one searces
C     along edges of an element counterclockwise; on one edge,
C     from IJ1 node to IJ2 node).
C=============================================================
      INCLUDE 'float.inc'
      INCLUDE 'comon.inp'

C
c.....CHECK IF NODE IJ1 IS WITHIN PAINTED RANGE 
C
      IF(FI(IJ1).GE.FI1.AND.FI(IJ1).LE.FI2) THEN
	IVC=IVC+1
	CVX(IVC)=XC(IJ1)
	CVY(IVC)=YC(IJ1)
      ENDIF
C
C.....VALUE FI1 (LOWER BOUND) BETWEEN NODES IJ1 and IJ2
C
      FAC1=0.
      IF((FI1.GE.MIN(FI(IJ1),FI(IJ2))).AND.
     *   (FI1.LE.MAX(FI(IJ1),FI(IJ2)))) THEN
	IVC=IVC+1
	FAC1=(FI1-FI(IJ1))/(FI(IJ2)-FI(IJ1)+1.E-20)
	CVX(IVC)=XC(IJ1)+FAC1*(XC(IJ2)-XC(IJ1))
	CVY(IVC)=YC(IJ1)+FAC1*(YC(IJ2)-YC(IJ1))
      ENDIF
C
C....VALUE FI2 (UPPER BOUND) BETWEEN NODES IJ1 and IJ2
C
      FAC2=0.
      IF((FI2.GE.MIN(FI(IJ1),FI(IJ2))).AND.
     *   (FI2.LE.MAX(FI(IJ1),FI(IJ2)))) THEN
	IVC=IVC+1
	FAC2=(FI2-FI(IJ1))/(FI(IJ2)-FI(IJ1)+1.E-20)
	CVX(IVC)=XC(IJ1)+FAC2*(XC(IJ2)-XC(IJ1))
	CVY(IVC)=YC(IJ1)+FAC2*(YC(IJ2)-YC(IJ1))
C
C.....IF BOTH POINTS FOUND, SORT THEM OUT
C
	IF(FAC2.LT.FAC1) THEN
	  X1=CVX(IVC-1)
	  Y1=CVY(IVC-1)
	  CVX(IVC-1)=CVX(IVC)
	  CVY(IVC-1)=CVY(IVC)
	  CVX(IVC)=X1
	  CVY(IVC)=Y1
	ENDIF
      ENDIF
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE PSHEAD(XMIN,XMAX,YMIN,YMAX)
C##########################################################
C     This routine writes postscript header to the plot
C     file. Bounding box coveres only the solution domain
C     boundary, not the contour information. 
C==========================================================
      INCLUDE 'float.inc'
C
      X1=XMIN+100.
      Y1=YMIN+100.
      X2=XMAX+110.
      Y2=YMAX+110.
C
      WRITE(7,101) X1,Y1,X2,Y2
  101 FORMAT('%!Ps-Adobe-2.0',/,
     *       '%%Creator: PLOTGR',/,
     *       '%%BoundingBox: ',4f6.0,/,
     *       '%%EndComments',/,
     *       '/c {currentpoint} def',/,
     *       '/f {fill} def ',/,
     *       '/gr {grestore} def',/,
     *       '/gs {gsave} def',/,
     *       '/l {lineto} def ',/,
     *       '/m {moveto} def',/,
     *       '/n {newpath} def',/,
     *       '/p {showpage} def ',/,
     *       '/s {stroke} def ',/,
     *       '/sg {setgray} def ',/,
     *       '/w {setlinewidth} def',/,
     *       '/cp {closepath} def',/,
     *       '/col {setrgbcolor} def',/,
     *       '/sh {show} def',/,
     *       '/sds  {setdash} def',/,
     *       '100 100 translate 0.03 0.03 scale ',/,
     *       '1 setlinecap 1 setlinejoin ')                                           
C
      RETURN
      END
C#################################################
      subroutine strlen(input,output,lout)
C#################################################
C     This routine finds string length.
C-------------------------------------------------
      character*(*) input,output
C&&&
      lin=len(input)
      do 10 i=lin,1,-1
      lout=i
      if(input(lout:lout).ne.' ') go to 11
   10 continue
      lout=0
      output=' '
      return
   11 output(1:lout)=input(1:lout)
      return
      end

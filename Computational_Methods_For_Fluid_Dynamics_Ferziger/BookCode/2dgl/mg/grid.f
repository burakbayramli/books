C#########################################################
      PROGRAM GRGEN
C#########################################################
C     This code generates non-orthogonal body-fitted grids
C     (one block only). The grid is automatically refined
C     up to the number of grid levels prescribed. This
C     version includes C- and O-type grids. See 
C     readme-file for a description.
C
C     This is Version 1.3 of the code, December 1996.
C
C     The user may modify the code and give it to third
C     parties, provided that an acknowledgement to the
C     source of the original version is retained.
C
C                M. Peric, Hamburg, 1996
C                Milovan.Peric@t-online.de
C=========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'logico.ing'
      INCLUDE 'grid.ing'
      INCLUDE 'bound.ing'
      INCLUDE 'lines.ing'
      CHARACTER*10 FILIN,FILTO,FILOUT,FILGRD,NAME*6
C
C.....DEFINE FILES
C
      PRINT *, ' ENTER PROBLEM NAME (SIX CHARACTERS):  '
      READ(*,'(A6)') NAME
      PRINT *, ' INPUT FROM KEYBOARD (1 - YES, 0 - NO)?   '
      READ(*,*) INPTYP
C
      IF(INPTYP.EQ.0) THEN
        WRITE( FILIN,'(A6,4H.gin)') NAME
        OPEN (UNIT=5,FILE=FILIN)
        REWIND 5
      ELSE
        WRITE( FILTO,'(A6,4H.gin)') NAME
        OPEN (UNIT=1,FILE=FILTO)
        REWIND 1
      ENDIF
C
      WRITE(FILOUT,'(A6,4H.got)') NAME
      WRITE(FILGRD,'(A6,4H.grd)') NAME
      OPEN (UNIT=4,FILE=FILGRD,FORM='UNFORMATTED')
      OPEN (UNIT=2,FILE=FILOUT)
      REWIND 4
      REWIND 2
C
C.....GENERATE THE COARSEST GRID AND SMOOTH IT
C
      K=1
      CALL GRIDGN
      CALL SMOG(K)
C
C.....GENERATE FINE GRIDS AND SMOOTH THEM
C
      DO K=2,NGR
        CALL GRIDG(K-1)
        CALL SMOG(K)
      END DO
C
C.....DEDUCE COARSE GRID COORDINATES FROM THE FINEST GRID
C
      IF(NGR.GT.1) THEN
        DO K=NGR-1,1,-1
          CALL SETIND(K)
          ISTF=IGR(K+1)
C
          DO I=1,NIM
          IF=2*I-1
          DO J=1,NJM
            JF=2*J-1
            IJ=LI(I+IST)+J
            IJF=LI(IF+ISTF)+JF
            X(IJ)=X(IJF)
            Y(IJ)=Y(IJF)
          END DO
          END DO
        END DO
      ENDIF
C
C.....PLOT THE GRID (ALL LEVELS)
C
      IF(LPLOT) CALL GRIDPL
C
C.....CALCULATE GRID DATA AND COLLECT BOUNDARY DATA
C
      IF(LCALG) THEN
        CALL CALCG
        CALL SETBC
      ENDIF
C
C.....CALCULATE BOUND. CELL FACE AREA DIVIDED BY DISTANCE AND UNIT VECTORS
C
      CALL SODW
      CALL SODS
C
C.....CREATE A FILE WITH CORRECT DIMENSION PARAMETERS
C
      OPEN(UNIT=9,FILE='param.inc')
      REWIND 9
C
      NIAL=IGR(NGR)+NIGR(NGR)
      NJAL=JGR(NGR)+NJGR(NGR)
      NIJA=IJGR(NGR)+NIGR(NGR)*NJGR(NGR)
      NXYG=MAX(IJGR(NGR),1)
      NMIJ=MAX(NMIJ,1)
      NINA=MAX(NINA,1)
      NOT=MAX(NOT,1)
      NST=MAX(NST,1)
      NWT=MAX(NWT,1)
      NOCT=MAX(NOCT,1)
C
      WRITE(9,99) '      PARAMETER (NGR= ',NGR, ',NXA=  ',NIAL,','
      WRITE(9,99) '     *           NYA= ',NJAL,',NMXY= ',NMIJ,','
      WRITE(9,99) '     *           NXYA=',NIJA,',NXYG= ',NXYG,','
      WRITE(9,99) '     *           NIA= ',NINA,',NOA=  ',NOT,','
      WRITE(9,99) '     *           NSA= ',NST, ',NWA=  ',NWT,','
      WRITE(9,99) '     *           NOCA=',NOCT,',NFI=4)'
   99 FORMAT(A22,I5,A7,I5,A1)
C
C.....STORE GRID DATA
C
      IF(LSTORE) THEN
        IA=0
        IF(LAXIS) IA=1
        WRITE(4) IA,(ITB(1,I),I=1,NIAL),(ITB(2,I),I=1,NIAL),
     *        (JTB(1,J),J=1,NJAL),(JTB(2,J),J=1,NJAL),
     *        (LI(I),I=1,NIAL),(NIGR(K),K=1,NGR),(NJGR(K),K=1,NGR),
     *        (IGR(K),K=1,NGR),(JGR(K),K=1,NGR),(IJGR(K),K=1,NGR),
     *        NINA,(NINL(K),K=1,NGR),(IIS(K),K=1,NGR),(IJI(I),I=1,NINA),
     *        (IJPI(I),I=1,NINA),(IJI1(I),I=1,NINA),(IJI2(I),I=1,NINA),
     *        NOT,(NOUT(K),K=1,NGR),(IOS(K),K=1,NGR),(IJO(I),I=1,NOT),
     *        (IJPO(I),I=1,NOT),(IJO1(I),I=1,NOT),(IJO2(I),I=1,NOT),
     *        NWT,(NWAL(K),K=1,NGR),(IWS(K),K=1,NGR),(IJW(I),I=1,NWT),
     *        (IJPW(I),I=1,NWT),(IJW1(I),I=1,NWT),(IJW2(I),I=1,NWT),
     *        (NWALI(K),K=1,NGR),(IWAS(K),K=1,NGR),(NWALA(K),K=1,NGR),
     *        NST,(NSYM(K),K=1,NGR),(ISS(K),K=1,NGR),(IJS(I),I=1,NST),
     *        (IJPS(I),I=1,NST),(IJS1(I),I=1,NST),(IJS2(I),I=1,NST),
     *        NOCT,(NOC(K),K=1,NGR),(IOCS(K),K=1,NGR),(IJL(I),I=1,NOCT),
     *        (IJR(I),I=1,NOCT),(IJOC1(I),I=1,NOCT),(IJOC2(I),I=1,NOCT)
        WRITE(4) (X(I),I=1,NIJA),(Y(I),I=1,NIJA),(XC(I),I=1,NIJA),
     *        (YC(I),I=1,NIJA),(FX(I),I=1,NIJA),(FY(I),I=1,NIJA),
     *        (VOL(I),I=1,NIJA),(SRDW(I),I=1,NWT),(XTW(I),I=1,NWT),
     *        (YTW(I),I=1,NWT),(SRDS(I),I=1,NST),(XNS(I),I=1,NST),
     *        (YNS(I),I=1,NST),(FOC(I),I=1,NOCT)
      ENDIF
C
C.....PRINT GRID DATA
C
      IF(LPRINT) CALL PRINTX
C
      CLOSE(UNIT=1)
      CLOSE(UNIT=2)
      CLOSE(UNIT=4)
      CLOSE(UNIT=9)
C
      STOP
      END
C
C
C######################################################
      SUBROUTINE GRIDGN
C######################################################
C     This routine generates the coarsest grid.
C======================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'logico.ing'
      INCLUDE 'grid.ing'
      INCLUDE 'bound.ing'
C
C.....READ CONTROL PARAMETERS
C
      PRINT *,' ENTER> LSTORE, LCALG, LPRINT, LPLOT, LAXIS:  '
      READ(5,*) LSTORE,LCALG,LPRINT,LPLOT,LAXIS
      PRINT *,' ENTER> NUMBER OF GRID LEVELS, NGR:  '
      READ(5,*) NGR
      PRINT *,' SELECTION OF STRAIGHT LINES: 0 -> S-N, 1 -> W-E:  '
      READ(5,*) IDIR 
      WRITE(1,*) LSTORE,LCALG,LPRINT,LPLOT,LAXIS,
     *        '  LSTORE,LCALG,LPRINT,LPLOT,LAXIS'
      WRITE(1,*) NGR,' NGR'
      WRITE(1,*) IDIR,'  IDIR'
C
      IGR(1)=0
      JGR(1)=0
      IJGR(1)=0
C
C.....GENERATE GRID POINTS ALONG BOUNDARY
C
      CALL BGRID
C
      NIGR(1)=NI
      NJGR(1)=NJ
C
C.....DEFINE FINE GRID PARAMETERS
C
      DO K=2,NGR
        KK=K-1
        IGR(K)=IGR(KK)+NIGR(KK)
        JGR(K)=JGR(KK)+NJGR(KK)
        IJGR(K)=IJGR(KK)+NIGR(KK)*NJGR(KK)
        NIGR(K)=2*NIGR(KK)-2
        NJGR(K)=2*NJGR(KK)-2
      END DO
C
C.....CHECK DIMENSION PARAMETERS
C
      NIAL=IGR(NGR)+NIGR(NGR)
      NJAL=JGR(NGR)+NJGR(NGR)
      NIJA=IJGR(NGR)+NIGR(NGR)*NJGR(NGR)
C
      PRINT *,'NXA_MAX = ',NIAL,  ' , DIMENSIONED AS:  NXA = ',NXA
      PRINT *,'NYA_MAX = ',NJAL,  ' , DIMENSIONED AS:  NYA = ',NYA
      PRINT *,'NXYA_MAX = ',NIJA,' , DIMENSIONED AS: NXYA = ',NXYA
      IF(NIAL.GT.NXA.OR.NJAL.GT.NYA.OR.NIJA.GT.NXYA) THEN
        PRINT *,'  ARRAY DIMENSIONS (PARAMETERS) INSUFFICIENT '
        PRINT *,'  EXECUTION STOPED **** INCREASE PARAMETERS, '
        PRINT *,'  RE-COMPILE GRID GENERATION CODE AND RE-RUN '
        STOP
      ENDIF
C
C.....SET POINTER LI(I) FOR ALL FINER GRIDS
C
      DO K=2,NGR
        CALL SETIND(K)
        DO I=1,NI
          LI(IST+I)=(I-1)*NJ+IJGR(K)
        END DO
      END DO

C
C.....CALCULATE GRID POINTS IN THE INTERIOR
C
      CALL SETIND(1)
      CALL CALXY(IDIR)
C
C.....SCALE COORDINATES TO METERS
C
      PRINT*,' ENTER SCALING FACTOR (CONVERT COORD. TO METERS):   '
      READ(5,*) SCF
      WRITE(1,*) SCF,'   SCF'
      DO IJ=1,NIJA
        X(IJ)=X(IJ)*SCF
        Y(IJ)=Y(IJ)*SCF
      END DO
C
      RETURN                                   
      END
C
C
C##########################################################
      BLOCK DATA
C##########################################################
      COMMON /CHCS/ CHS(4)
      CHARACTER CHS*5
      DATA CHS /'SOUTH','NORTH','WEST ','EAST '/
      END
C
C
C##########################################################
      SUBROUTINE BGRID 
C##########################################################
C     This routine generates grid points along solution
C     domain boundaries. Boundaries are subdivided into
C     line segments (straight, circle, or arbitrary); data
C     is provided for each line interactivly.
C==========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'lines.ing'
      LOGICAL LREPT
C
C.....NUMBER OF CONTROL VOLUMES IN I AND J DIRECTION
C
      PRINT *,' ENTER NO. OF CVs IN I AND J DIRECTION:  '
      READ(5,*) NICV,NJCV
      WRITE(1,*) NICV,NJCV,'  NICV,NJCV'
C
C.....INDEX CONTROL AND CONVERSION
C
      NI=NICV+2
      NJ=NJCV+2
      NIM=NI-1
      NJM=NJ-1
      DO I=1,NI
        LI(I)=(I-1)*NJ
      END DO
C
   10 CONTINUE
      NCIR=0
      NARB=0
C
C.....LINES ALONG BOUNDARY; COORD. OF BEGIN OF LINE 1
C
      DO L=1,4
        NCF(L)=0
        PRINT *,' NO. OF LINES ON ',CHS(L),' SIDE:  '
        READ(5,*) NLINES(L)
        WRITE(1,*) NLINES(L),'  NLINES(L)'
        PRINT *,' COORDINATES OF BEGIN OF LINE 1:  '
        READ(5,*) XLS,YLS
        WRITE(1,*) XLS,YLS,'  XLS,YLS'
C
C.....COORD. OF LINE END 
C
        DO LL=1,NLINES(L)
          PRINT *,' COORDINATES OF LINE ',LL,'  END:  '
          READ(5,*) XLE,YLE
          WRITE(1,*) XLE,YLE,'  XLE,YLE'
C
C.....READ LINE SPECIFICATIONS
C
   50     PRINT *,' NUMBER OF SEGMENTS, LINE TYPE, BOUNDARY TYPE:  '
          READ(5,*) NSEG,LTYP,LBTYP
          WRITE(1,*) NSEG,LTYP,LBTYP,'  NSEG, LTYP, LBTYP'
          NCF(L)=NCF(L)+NSEG
          IF((L.LE.2.AND.NCF(L).GT.NICV).OR.
     *       (L.GT.2.AND.NCF(L).GT.NJCV)) THEN
            PRINT *,' TOO MANY SEGMENTS - MORE THAN NICV OR NJCV !!! '
            PRINT *,' REDUCE NSEG TO MATCH NICV OR NJCV AND RE-TYPE!!'
            GO TO 50
          ENDIF
          PRINT *,' SIZE OF SEGMENT AT LINE BEGIN, EXPANSION FACTOR:  '
          READ(5,*) DX1,EXP
          WRITE(1,*) DX1,EXP,'  DX1,EXP'
C
C.....STRAIGHT LINE: CALCULATE COORD. OF BOUNDARY GRID POINTS
C
          IF(LTYP.EQ.1) CALL STRLINE(L)
C
C.....CIRCLE LINES: DEFINE CIRCLE SEGMENT AND CALCULATE GRID COORD. 
C
          IF(LTYP.EQ.2) THEN
            PRINT *,' ENTER COORDINATES OF A THIRD POINT ON CIRCLE;'
            PRINT *,' IF FULL CIRCLE, COORD. OF CENTER INSTEAD:  '
            READ(5,*) XLM,YLM
            WRITE(1,*) XLM,YLM,'  XLM,YLM'
            PRINT *,' ENTER ANGLES AT LINE BEGIN AND END:  '
            READ(5,*) FIS,FIE
            WRITE(1,*) FIS,FIE,'  FIS,FIE'
C
            CALL CIRCLIN(L)
          ENDIF
C
C.....ARBITRARY LINE: READ COORDINATES OF GRID POINTS
C
          IF(LTYP.EQ.3) CALL ARBLINE(L)
C
C.....SET COORD. OF THE BEGIN OF NEXT LINE
C
          XLS=XLE
          YLS=YLE
        END DO
C
      END DO
C
C.....CHECK NUMBERS OF CELL FACES ON OPOSITE BLOCK SIDES
C
      LREPT=.FALSE.
C
      DO NL=1,3,2
        IF(NCF(NL).NE.NCF(NL+1)) THEN
          PRINT *,' ERROR: NUMBER OF CV-FACES ON SIDES ',CHS(NL),
     *            ' AND ',CHS(NL+1),' DO NOT MATCH  '
          PRINT *,' REPEAT DATA INPUT!!! '
          LREPT=.TRUE.
        ENDIF
      END DO
C
      IF(NCF(1).NE.NICV.OR.NCF(3).NE.NJCV) THEN
        PRINT *,' NUMBER OF SEGMENTS ON BOUNDARIES DOES NOT MATCH'
        PRINT *,' SPECIFIED NICV AND NJCV; ADJUST EITHER AND RE-TYPE'
        LREPT=.TRUE.
      ENDIF
C
      IF(LREPT) GO TO 10
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE STRLINE(L)
C############################################################
C     This routine deals with straight lines.
C============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'lines.ing'
C
C.....CALCULATE COORDINATES OF GRID POINTS ALONG THE LINE
C
      XYL=SQRT((XLE-XLS)**2+(YLE-YLS)**2)
C
      CALL DIVLINE(XYL)
C
C.....ASSIGN LINE POINTS TO GRID POINTS
C
      CALL SETPT(L,IE,IS,JE,JS)
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE SETPT(L,IE,IS,JE,JS)
C##########################################################
C     This routine assignes line points to boundary points.
C==========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'lines.ing'
      INCLUDE 'grid.ing'
C
C.....ASSIGN LINE POINTS TO GRID POINTS FOR SOUTH AND NORTH BOUND.
C.....SET BOUNDARY TYPE FOR BOUNDARY CELL FACES
C
      IF(L.LE.2) THEN
        JS=1
        IF(L.EQ.2) JS=NJ-1
        IE=NCF(L)+1
        IS=IE-NSEG
        JE=JS
C
        DO I=IS,IE
          IJ=LI(I)+JS
          X(IJ)=XPT(I-IS+1)
          Y(IJ)=YPT(I-IS+1)
          IF(I.LT.IE) ITB(L,I+1)=LBTYP
        END DO
C
      ELSE
C
C.....ASSIGN LINE POINTS TO GRID POINTS FOR WEST AND EAST BOUND.
C.....SET BOUNDARY TYPE FOR BOUNDARY CELL FACES
C
        IS=1
        IF(L.EQ.4) IS=NI-1
        JE=NCF(L)+1
        JS=JE-NSEG
        IE=IS
        IND=L-2
C
        DO J=JS,JE
          IJ=LI(IS)+J
          X(IJ)=XPT(J-JS+1)
          Y(IJ)=YPT(J-JS+1)
          IF(J.LT.JE) JTB(IND,J+1)=LBTYP
        END DO
C
      ENDIF
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE CIRCLIN(L)
C############################################################
C     This routine deals with lines which are circle segments.
C============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'lines.ing'
      INCLUDE 'circl.ing'
C
C.....CHECK IF FULL CIRCLE, SET CENTER COORDINATES AND RADIUS
C
      XYL=SQRT((XLE-XLS)**2+(YLE-YLS)**2)
      IF(XYL.LT.1.E-20) THEN
        XCC=XLM
        YCC=YLM
        RCC=SQRT((XLM-XLS)**2+(YLM-YLS)**2)
      ELSE
C
C.....FIND CIRCLE CENTER AND RADIUS
C
        C1=(XLM-XLS)/(YLM-YLS+1.E-20)
        C2=(XLE-XLM)/(YLE-YLM+1.E-20)
        X1=0.5*(XLS+XLM)
        Y1=0.5*(YLS+YLM)
        X2=0.5*(XLE+XLM)
        Y2=0.5*(YLE+YLM)
        XCC=(Y1-Y2+C1*X1-C2*X2)/(C1-C2+1.E-20)
        IF(ABS(C1).LT.ABS(C2)) THEN
          YCC=Y1-C1*(XCC-X1)
        ELSE
          YCC=Y2-C2*(XCC-X2)
        ENDIF
C
        RCC=SQRT((XCC-XLS)**2+(YCC-YLS)**2)
      ENDIF
C
      XYL=ABS(FIE-FIS)      
C
C.....CALCULATE COORDINATES OF GRID POINTS ALONG THE LINE
C
      CALL DIVLINE(XYL)
C
C.....ASSIGN LINE POINTS TO GRID POINTS 
C
      CALL SETPT(L,IE,IS,JE,JS)
C
C.....STORE CIRCLE DATA FOR GRID REFINEMENT
C
      NCIR=NCIR+1
      JSCIR(NCIR)=JS
      ISCIR(NCIR)=IS
      JECIR(NCIR)=JE
      IECIR(NCIR)=IE
      RCIR(NCIR)=RCC     
      XCCIR(NCIR)=XCC
      YCCIR(NCIR)=YCC
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE ARBLINE(L)
C############################################################
C     This routine deals with arbitrary lines which are 
C     defined by specifying coordinates of each point.
C============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'lines.ing'
      INCLUDE 'circl.ing'
C
C.....READ COORDINATES OF GRID POINTS ALONG THE LINE
C
      DO I=1,NSEG+1
        PRINT*,' ENTER COORD. (X,Y) OF POINT NO.',I,' :  '
        READ(5,*) XPT(I),YPT(I)
        WRITE(1,*) XPT(I),YPT(I),'  XPT(I),YPT(I)'
      END DO
C
C.....ASSIGN LINE POINTS TO GRID POINTS
C
      CALL SETPT(L,IE,IS,JE,JS)
C
C.....STORE LINE DATA FOR SMOOTHING OF REFINED GRIDS
C
      NARB=NARB+1
      ISARB(NARB)=IS
      JSARB(NARB)=JS
      JEARB(NARB)=JE
      IEARB(NARB)=IE
C
      RETURN
      END
C
C
C#########################################################
      SUBROUTINE DIVLINE(XYL)
C#########################################################
C     This routine subdivides a line into a specified
C     number of segments and creates coordinates of points
C     along the line.
C=========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'lines.ing'
      INCLUDE 'circl.ing'
      DIMENSION DL(500),FD(500)
C
C.....UNIFORM SUBDIVISION (ZERO INPUT FOR BOTH DX1 AND EXP)
C
      IF(DX1.LT.1.E-20.AND.EXP.LT.1.E-20) THEN
        DX1=XYL/REAL(NSEG)
        EXP=1.0
C
C.....SPECIFIED SIZE OF THE FIRST SEGMENT
C
      ELSEIF(DX1.GT.1.E-20) THEN
        CALL EXPAND(XYL)
C
C.....SPECIFIED EXPANSION RATIO
C
      ELSEIF(EXP.GT.1.E-20) THEN
        IF(ABS(EXP-1.0).LT.0.001) THEN
          DX1=XYL/REAL(NSEG)
        ELSE
          DX1=XYL*(1.-EXP)/(1.-EXP**NSEG)
        ENDIF
      ENDIF
C
C.....CALCULATE SIZE OF INTERVALS AND SCALE TO THE RANGE (0 ... 1)
C
      XYLR=1./(XYL+1.E-20)
      DL(1)=DX1
      FD(1)=0.
      SUML=0.
C
      DO I=2,NSEG
        DL(I)=DL(I-1)*EXP
        SUML=SUML+DL(I-1)
        FD(I)=SUML*XYLR
      END DO
      FD(NSEG+1)=1.0 
C
C.....CALCULATE COORDINATES OF GRID POINTS FOR STRAIGHT LINES
C
      IF(LTYP.EQ.1) THEN
        DO I=1,NSEG+1
          XPT(I)=XLS+FD(I)*(XLE-XLS)
          YPT(I)=YLS+FD(I)*(YLE-YLS)
        END DO
C
C.....CALCULATE COORDINATES OF GRID POINTS FOR CIRCLE LINES 
C
      ELSEIF(LTYP.EQ.2) THEN
        FIP=ATAN(1.)/45.
        XPT(1)=XLS
        YPT(1)=YLS
        XPT(NSEG+1)=XLE
        YPT(NSEG+1)=YLE
        DO I=2,NSEG
          ANGLE=(FIS+FD(I)*(FIE-FIS))*FIP
          XPT(I)=XCC+RCC*COS(ANGLE)
          YPT(I)=YCC+RCC*SIN(ANGLE)
        ENDDO
      ENDIF
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE EXPAND(XYL)
C########################################################
C     This routine calculates the expansion factor along
C     a line when the size of the first segment is 
C     specified.
C========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'lines.ing'
      FI(A,B,G,N)=A*G**N-B*G+B-A
C
C.....TOLERANCE, RANGE
C
      EPS=1.E-4
      EMAX=2.
      EMIN=.5
      I=0
C 
C.....CALC. AVERAGE DX AND DETERMINE WHETHER SUBDIVISION IS UNIFORM
C
      AR=XYL/REAL(NSEG)
      IF(ABS(DX1-AR)/AR.LT.0.01) THEN
        EXP=1.0
        DX1=AR
C
C.....NON-UNIFORM DISTRIBUTION, FIND WHETHER EXPANSION OR CONTRACTION
C
      ELSE
        IF(DX1.LT.AR) THEN
          E2=EMAX
          E1=1.0001
        ELSE
          E1=EMIN
          E2=0.9999
        ENDIF
C
C.....CALCULATE EXPANSION RATIO USING INTERVAL HALVING METHOD
C
        FI1=FI(DX1,XYL,E1,NSEG)
        FI2=FI(DX1,XYL,E2,NSEG)
        ONE=1.
        IF(SIGN(ONE,FI1).NE.SIGN(ONE,FI2)) THEN
C
C.....ITERATE UNTIL EXP IS FOUND WHICH FITS SPECIFIED DX1 AND XYL
C
   10     I=I+1
          E=0.5*(E1+E2)
          FIP=FI(DX1,XYL,E,NSEG)
          IF(SIGN(ONE,FIP).EQ.SIGN(ONE,FI2)) THEN
            E2=E
            FI2=FIP
          ELSE
            E1=E
            FI1=FIP
          ENDIF
          IF(ABS(FI2-FI1).GT.EPS.AND.I.LT.200) GO TO 10
C
C.....PRINT MESSAGE (EXP OR OUT OF RANGE)
C
          EXP=E
          PRINT *, '   EXP. FACTOR FOR THIS LINE:  EXP = ',EXP
          PRINT *, '     ',I,'  ITERATIONS PERFORMED '
        ELSE
          PRINT *, '  *** ERROR: EXPANSION FACTOR > 2 OR < 0.5 ***'
          PRINT *, '  I TAKE EXP=1.0; CHANGE DATA FOR THIS LINE?  '
          EXP=1.0
          DX1=AR
        ENDIF
C
      ENDIF
C
      RETURN
      END
C
C
C###########################################################
      SUBROUTINE CALXY(IDD) 
C###########################################################
C     This routine calculates the coordinates of grid points
C     in the interior of the solution domain once the 
C     coordinates of grid points along boundaries are known.
C===========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'grid.ing'
      DIMENSION FL(NXA),FR(NXA)
C
C.....OFFSETS AT WEST AND EAST BOUNDARY
C
      IIE=LI(NIM)
      IIW=LI(1)
C
C==================================================================
C.....CALCULATE COORDINATES OF INTERIOR POINTS (STRAIGHT LINES N-S)
C==================================================================
C
      IF(IDD.EQ.0) THEN
C
C.....DISTANCE BETWEEN CORNER POINTS (NORTH - SOUTH)
C
        DLR=1./SQRT((X(IIE+NJM)-X(IIE+1))**2+(Y(IIE+NJM)-Y(IIE+1))**2)
        DLL=1./SQRT((X(IIW+NJM)-X(IIW+1))**2+(Y(IIW+NJM)-Y(IIW+1))**2)
C
C.....DISTRIBUTION FUNCTION ON WEST AND EAST SIDE
C
        DO J=1,NJM
          FL(J)=SQRT((X(IIW+J)-X(IIW+1))**2+(Y(IIW+J)-Y(IIW+1))**2)*DLL
          FR(J)=SQRT((X(IIE+J)-X(IIE+1))**2+(Y(IIE+J)-Y(IIE+1))**2)*DLR
        END DO
C
C.....DISTANCE BETWEEN OPOSITE POINTS ON SOUTH AND NORTH SIDE
C
        RNIM=1./REAL(NIM-1)
        DO I=2,NIM-1
          II=LI(I)
          DX=X(II+NJM)-X(II+1)
          DY=Y(II+NJM)-Y(II+1)
C
C.....DISTRIBUTE POINTS USING INTERPOLATED WEST AND EAST DISTRIBUTIONS
C
          DO J=2,NJM-1
            FAC=(REAL(I-1)*FR(J)+REAL(NIM-I)*FL(J))*RNIM
            X(II+J)=X(II+1)+FAC*DX
            Y(II+J)=Y(II+1)+FAC*DY
          END DO
        END DO
C
C==================================================================
C.....CALCULATE COORDINATES OF INTERIOR POINTS (STRAIGHT LINES E-W)
C==================================================================
C
      ELSEIF(IDD.EQ.1) THEN
C
C.....DISTANCE BETWEEN CORNERS (EAST - WEST)
C
        DLR=1./SQRT((X(IIE+NJM)-X(IIW+NJM))**2+
     *              (Y(IIE+NJM)-Y(IIW+NJM))**2)
        DLL=1./SQRT((X(IIE+1)-X(IIW+1))**2+(Y(IIE+1)-Y(IIW+1))**2)
C
C.....DISTRIBUTION FUNCTION ALONG SOUTH AND NORTH BOUNDARY
C
        DO I=1,NIM
          II=LI(I+IST)
          FL(I)=SQRT((X(II+1)-X(IIW+1))**2+(Y(II+1)-Y(IIW+1))**2)*DLL
          FR(I)=SQRT((X(II+NJM)-X(IIW+NJM))**2+
     *            (Y(II+NJM)-Y(IIW+NJM))**2)*DLR
        END DO
C
C.....DISTANCE BETWEEN OPOSITE POINTS ON EAST AND WEST BOUNDARY
C
        RNJM=1./REAL(NJM-1)
        DO J=2,NJM-1
          DX=X(IIE+J)-X(IIW+J)
          DY=Y(IIE+J)-Y(IIW+J)
C
C.....DISTRIBUTE POINTS USING INTERPOLATED NORTH AND SOUTH DISTRIBUTIONS
C
          DO I=2,NIM-1
            IJ=LI(I+IST)+J
            FAC=(REAL(J-1)*FR(I)+REAL(NJM-J)*FL(I))*RNJM
            X(IJ)=X(IIW+J)+FAC*DX
            Y(IJ)=Y(IIW+J)+FAC*DY
          END DO
        END DO
C
      ENDIF
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE GRIDG(KK)
C##########################################################
C     This routine creates refined grid from a coarse grid.
C==========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'lines.ing'
      INCLUDE 'circl.ing'
      INCLUDE 'grid.ing'
C
C.....FINE GRID INDEX
C
      K=KK+1
C
C.....DEFINE COARSE GRID
C
      NIG=NIGR(KK)
      NJG=NJGR(KK)
      NIMG=NIG-1
      NJMG=NJG-1
      ISTG=IGR(KK)
      JSTG=JGR(KK)
C
C.....DEFINE FINE GRID
C
      NI=NIG*2-2
      NJ=NJG*2-2
      NIM=NI-1
      NJM=NJ-1
      IST=IGR(K)
      JST=JGR(K)
C
C===============================================================
C.....INTERPOLATION ALONG J-LINES
C===============================================================
C
      DO JG=1,NJMG
        J=2*JG-1
        IJG=LI(ISTG+1)+JG
        IJ=LI(IST+1)+J
        X(IJ)=X(IJG)
        Y(IJ)=Y(IJG)
        DXE=X(IJG+NJG)-X(IJG)
        DYE=Y(IJG+NJG)-Y(IJG)
        DLE=SQRT(DXE**2+DYE**2)
C
        DO IG=2,NIMG-1
          I=2*IG-1
          IJG=LI(ISTG+IG)+JG
          IJ=LI(IST+I)+J
          IMJ=IJ-NJ
          X(IJ)=X(IJG)
          Y(IJ)=Y(IJG)
          DXP=DXE
          DYP=DYE
          DLW=DLP
          DLP=DLE
C
          DXE=X(IJG+NJG)-X(IJG)
          DYE=Y(IJG+NJG)-Y(IJG)
          DLE=SQRT(DXE**2+DYE**2)
C
          FXP=SQRT(DLP/(DLE+1.E-20))
          FAC=FXP/(1.+FXP)
          FXR=SQRT(DLP/(DLW+1.E-20))
          FACR=FXR/(1.+FXR)
          FAC=0.5*(1.+FAC-FACR)
          IF(IG.EQ.2) FAC=.5
          X(IMJ)=X(IMJ-NJ)+DXP*FAC
          Y(IMJ)=Y(IMJ-NJ)+DYP*FAC
        END DO
C
        IPJ=IJ+NJ
        X(IPJ)=X(IJ)+0.5*DXE
        Y(IPJ)=Y(IJ)+0.5*DYE
        X(IPJ+NJ)=X(IJG+NJG)
        Y(IPJ+NJ)=Y(IJG+NJG)
      END DO
C
C===============================================================
C.....INTERPOLATION ALONG I-LINES
C===============================================================
C
      DO I=1,NIM
        II=LI(IST+I)
        IJ=II+3
        DXE=X(IJ)-X(IJ-2)
        DYE=Y(IJ)-Y(IJ-2)
        DLE=SQRT(DXE**2+DYE**2)
C
        DO JG=2,NJMG-1
          J=2*JG-1
          IJ=II+J
          DXP=DXE
          DYP=DYE
          DLW=DLP
          DLP=DLE
C
          DXE=X(IJ+2)-X(IJ)
          DYE=Y(IJ+2)-Y(IJ)
          DLE=SQRT(DXE**2+DYE**2)
C
          FYP=SQRT(DLP/(DLE+1.E-20))
          FAC=FYP/(1.+FYP)
          FYR=SQRT(DLP/(DLW+1.E-20))
          FACR=FYR/(1.+FYR)
          FAC=0.5*(1.+FAC-FACR)
          IF(JG.EQ.2) FAC=0.5
          X(IJ-1)=X(IJ-2)+DXP*FAC
          Y(IJ-1)=Y(IJ-2)+DYP*FAC
        END DO
C
        X(IJ+1)=X(IJ)+0.5*DXE
        Y(IJ+1)=Y(IJ)+0.5*DYE
      END DO
C
C===============================================================
C.....MODIFICATIONS ON CIRCLE LINES  
C===============================================================
C
      IF(NCIR.NE.0) THEN
C
      DO NC=1,NCIR
C
C.....SCALE CENTER COORD. AND RADIUS
C
        IF(K.EQ.2) THEN
          RCIR(NC) =RCIR(NC)*SCF   
          XCCIR(NC)=XCCIR(NC)*SCF
          YCCIR(NC)=YCCIR(NC)*SCF
        ENDIF
C
C.....CALCULATE START AND END INDICES ON THE FINER GRID
C
        JSCIR(NC)=2*JSCIR(NC)-1
        JECIR(NC)=2*JECIR(NC)-1
        ISCIR(NC)=2*ISCIR(NC)-1
        IECIR(NC)=2*IECIR(NC)-1
C
C.....ADJUST COORDINATES OF FINE GRID POINTS TO LIE ON CIRCLE
C
        IF(JSCIR(NC).EQ.JECIR(NC)) THEN
          DO I=ISCIR(NC),IECIR(NC)
            IJ=LI(I+IST)+JSCIR(NC)
            CALL CIRMOD(X(IJ),Y(IJ),XCCIR(NC),YCCIR(NC),RCIR(NC))
          END DO
C
        ELSEIF(ISCIR(NC).EQ.IECIR(NC)) THEN
          DO J=JSCIR(NC),JECIR(NC)
            IJ=LI(IST+ISCIR(NC))+J
            CALL CIRMOD(X(IJ),Y(IJ),XCCIR(NC),YCCIR(NC),RCIR(NC))
          END DO
        ENDIF
C
      END DO
      ENDIF
C
C===============================================================
C.....SMOOTHING OF BOUNDARIES MADE OF ARBITRARY LINES
C===============================================================
C
      IF(NARB.NE.0) THEN
C
      DO L=1,NARB
C
C.....CALCULATE START AND END INDICES ON THE FINER GRID
C
        ISARB(L)=2*ISARB(L)-1
        IEARB(L)=2*IEARB(L)-1
        JSARB(L)=2*JSARB(L)-1
        JEARB(L)=2*JEARB(L)-1
C
C.....SMOOTHING OF BOUNDARY LINES 
C
        IF(ISARB(L).EQ.IEARB(L)) THEN
          IJBEG=LI(IST+ISARB(L))+JSARB(L)+1
          IJEND=LI(IST+ISARB(L))+JEARB(L)-1
          CALL SMBL(IJBEG,IJEND,1)
C
        ELSEIF(JSARB(L).EQ.JEARB(L)) THEN
          IJBEG=LI(IST+ISARB(L)+1)+JSARB(L)
          IJEND=LI(IST+IEARB(L)-1)+JSARB(L)
          CALL SMBL(IJBEG,IJEND,NJ)
        ENDIF
C
      END DO
      ENDIF
C
C===============================================================
C.....SET BOUNDARY CONDITIONS FOR FINE GRID BOUNDARY CELL FACES
C===============================================================
C
      DO L=1,2
        DO IG=2,NIMG
          I1=IST+2*IG-2
          ITB(L,I1)=ITB(L,IG+ISTG)
          ITB(L,I1+1)=ITB(L,I1)
        END DO
C
        DO JG=2,NJMG
          J1=JST+2*JG-2
          JTB(L,J1)=JTB(L,JG+JSTG)
          JTB(L,J1+1)=JTB(L,J1)
        END DO
      END DO
C
      RETURN
      END
C
C
C########################################################
       SUBROUTINE CIRMOD(XP,YP,XCC,YCC,RC)
C########################################################
C     This routine modifies coordinates on boundary lines
C     which are circle segments after refinement.
C========================================================
      INCLUDE 'float.inc'
C
      DXO=XP-XCC
      DYO=YP-YCC
      RO=SQRT(DXO**2+DYO**2)
      FAC=(RC-RO)/RO
      XP=XP+DXO*FAC
      YP=YP+DYO*FAC
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE SMBL(IJS,IJE,INC)
C########################################################
C     This routine smooths boundary lines defined by 
C     arbitrary points; now empty -- a spline interpolation
C     should be implemented
C========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'grid.ing'
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE SMOG(K)
C########################################################
C     This routine smooths grid in the interior.
C========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
C
      CALL SETIND(K)
C
C.....SMOOTH WEST-EAST LINES
C
      CALL SMOGP(NJ)

C.....SMOOTH SOUTH-NORTH LINES
C
      IND=1
      CALL SMOGP(IND)
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE SMOGP(IND)
C########################################################
C     This routine adjusts coordinates of interior grid
C     points to make the grid smoother. Two neighbor grid
C     points (L and R) of point P are connected and an 
C     auxilliary point is set on this line at a location
C     corresponding to the position of P between L and R.
C     The new grid point is put midway between the old
C     location P and the auxilliary point. The points are
C     moving inwards towards center of curvature.
C========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'grid.ing'
      INCLUDE 'indexg.ing'
      DIMENSION XN(NXYA),YN(NXYA)
C
C.....COPY OLD COORDINATES
C
      DO I=2,NIM-1
        DO J=2,NJM-1
          IJ=LI(I+IST)+J
          XN(IJ)=X(IJ)
          YN(IJ)=Y(IJ)
        END DO
      END DO
C
C.....SMOOTH LINES
C
       DO I=2,NIM-1
        DO J=2,NJM-1
          IJ=LI(I+IST)+J
          IJL=IJ-IND
          IJR=IJ+IND
C
C.....CONNECTIONS IJ <--> IJL AND IJ <--> IJR
C
          TL=SQRT((X(IJ)-X(IJL))**2+(Y(IJ)-Y(IJL))**2)
          TR=SQRT((X(IJ)-X(IJR))**2+(Y(IJ)-Y(IJR))**2)
C
C.....Auxilliary point on line IJL - IJR, modified grid node
C
          FL=TL/(TL+TR)
          XP=X(IJL)+FL*(X(IJR)-X(IJL))
          YP=Y(IJL)+FL*(Y(IJR)-Y(IJL))
          XN(IJ)=0.5*(X(IJ)+XP)
          YN(IJ)=0.5*(Y(IJ)+YP)
        END DO
      END DO
C
C.....RESET COORDINATES OF GRID NODES
C
      DO I=2,NIM-1
        DO J=2,NJM-1
          IJ=LI(I+IST)+J
          X(IJ)=XN(IJ)
          Y(IJ)=YN(IJ)
        END DO
      END DO
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE CALCG
C########################################################
C     This routine calculates grid data for flow solver.
C========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'grid.ing'
      INCLUDE 'logico.ing'
C
C.....SET GRID PARAMETERS AND CALCULATE ON ALL GRIDS
C
      DO K=1,NGR
        CALL SETIND(K)
C
C.....CALCULATION OF CELL VOLUMES FOR AXI-SYMMETRIC GEOMETRY
C
        IF (LAXIS) THEN
          SIXR=1./6.
          DO I=2,NIM
          DO J=2,NJM
            IJ=LI(I+IST)+J
            IMJ=IJ-NJ
            IMJM=IMJ-1
            IJM=IJ-1
            RIJ=Y(IJ)**2
            RIMJ=Y(IMJ)**2
            RIMJM=Y(IMJM)**2
            RIJM=Y(IJM)**2
            VOL(IJ)=SIXR*((X(IJ)-X(IMJ))*(RIJ+RIMJ+Y(IJ)*Y(IMJ))+
     *              (X(IMJ)-X(IMJM))*(RIMJ+RIMJM+Y(IMJ)*Y(IMJM))+
     *              (X(IMJM)-X(IJM))*(RIMJM+RIJM+Y(IMJM)*Y(IJM))+
     *              (X(IJM)-X(IJ))*(RIJM+RIJ+Y(IJM)*Y(IJ)))
          END DO
          END DO
C
        ELSE
C
C.....CALCULATION OF CELL VOLUMES FOR PLANE GEOMETRY
C
          DO I=2,NIM
          DO J=2,NJM
            IJ=LI(I+IST)+J
            DXNESW=X(IJ)-X(IJ-NJ-1)
            DYNESW=Y(IJ)-Y(IJ-NJ-1)
            DXNWSE=X(IJ-NJ)-X(IJ-1)
            DYNWSE=Y(IJ-NJ)-Y(IJ-1)
            VOL(IJ)=0.5*ABS(DXNESW*DYNWSE-DXNWSE*DYNESW)
          END DO
          END DO
C
        ENDIF
C
C.....CALCULATION OF NODE COORDINATES: CORNER (DUMMY) NODES
C
        IJ=LI(1+IST)+1
        XC(IJ)=X(IJ)
        YC(IJ)=Y(IJ)
C
        IJ=LI(NIM+IST)+1
        XC(IJ+NJ)=X(IJ)
        YC(IJ+NJ)=Y(IJ)
C
        IJ=LI(1+IST)+NJM
        XC(IJ+1)=X(IJ)
        YC(IJ+1)=Y(IJ)
C
        IJ=LI(NIM+IST)+NJM
        XC(IJ+NJ+1)=X(IJ)
        YC(IJ+NJ+1)=Y(IJ)
C
C.....CALCULATION OF NODE COORDINATES: BOUNDARY NODES
C
        DO J=2,NJM
          IJ=LI(IST+NIM)+J
          XC(IJ+NJ)=0.5*(X(IJ)+X(IJ-1))
          YC(IJ+NJ)=0.5*(Y(IJ)+Y(IJ-1))
          IJ=LI(IST+1)+J
          XC(IJ)=0.5*(X(IJ)+X(IJ-1))
          YC(IJ)=0.5*(Y(IJ)+Y(IJ-1))
        END DO
C
        DO I=2,NIM
          IJ=LI(I+IST)+1
          XC(IJ)=0.5*(X(IJ)+X(IJ-NJ))
          YC(IJ)=0.5*(Y(IJ)+Y(IJ-NJ))
          IJ=LI(I+IST)+NJM
          XC(IJ+1)=0.5*(X(IJ)+X(IJ-NJ))
          YC(IJ+1)=0.5*(Y(IJ)+Y(IJ-NJ))
        END DO
C
C.....CALCULATION OF NODE COORDINATES: CELL CENTERS
C
        DO I=2,NIM
        DO J=2,NJM
          IJ=LI(I+IST)+J
          XC(IJ)=0.25*(X(IJ)+X(IJ-1)+X(IJ-NJ)+X(IJ-NJ-1))
          YC(IJ)=0.25*(Y(IJ)+Y(IJ-1)+Y(IJ-NJ)+Y(IJ-NJ-1))
        END DO
        END DO
C 
C==========================================================     
C..... CALCULATION OF INTERPOLATION FACTORS
C==========================================================     
C
        DO I=2,NIM
        DO J=2,NJM
          IJ=LI(I+IST)+J
C
C.....INTERPOLATION IN I-DIRECTION: FX = Pe/PE
C
          XE=0.5*(X(IJ)+X(IJ-1))
          YE=0.5*(Y(IJ)+Y(IJ-1))
          DLPE=SQRT((XE-XC(IJ))**2+(YE-YC(IJ))**2)
          DLEE=SQRT((XC(IJ+NJ)-XE)**2+(YC(IJ+NJ)-YE)**2)
          FX(IJ)=DLPE/(DLPE+DLEE+1.E-20)
C
C.....INTERPOLATION IN J-DIRECTION: FY = Pn/PN
C
          XN=0.5*(X(IJ)+X(IJ-NJ))
          YN=0.5*(Y(IJ)+Y(IJ-NJ))
          DLPN=SQRT((XN-XC(IJ))**2+(YN-YC(IJ))**2)
          DLNN=SQRT((XC(IJ+1)-XN)**2+(YC(IJ+1)-YN)**2)
          FY(IJ)=DLPN/(DLPN+DLNN+1.E-20)
        END DO
        END DO
C
C.....Interpolation factors along south and north boundary
C
        DO I=2,NIM
          IJ=LI(I+IST)+1
          XE=X(IJ)
          YE=Y(IJ)
          DLPE=SQRT((XE-XC(IJ))**2+(YE-YC(IJ))**2)
          DLEE=SQRT((XC(IJ+NJ)-XE)**2+(YC(IJ+NJ)-YE)**2)
          FX(IJ)=DLPE/(DLPE+DLEE+1.E-20)
          IJ=LI(I+IST)+NJ
          XE=X(IJ-1)
          YE=Y(IJ-1)
          DLPE=SQRT((XE-XC(IJ))**2+(YE-YC(IJ))**2)
          DLEE=SQRT((XC(IJ+NJ)-XE)**2+(YC(IJ+NJ)-YE)**2)
          FX(IJ)=DLPE/(DLPE+DLEE+1.E-20)
        END DO
C
C.....Interpolation factors along east and west boundary
C
        DO J=2,NJM
          IJ=LI(1+IST)+J
          XN=X(IJ)
          YN=Y(IJ)
          DLPN=SQRT((XN-XC(IJ))**2+(YN-YC(IJ))**2)
          DLNN=SQRT((XC(IJ+1)-XN)**2+(YC(IJ+1)-YN)**2)
          FY(IJ)=DLPN/(DLPN+DLNN+1.E-20)
          IJ=LI(NI+IST)+J
          XN=X(IJ-NJ)
          YN=Y(IJ-NJ)
          DLPN=SQRT((XN-XC(IJ))**2+(YN-YC(IJ))**2)
          DLNN=SQRT((XC(IJ+1)-XN)**2+(YC(IJ+1)-YN)**2)
          FY(IJ)=DLPN/(DLPN+DLNN+1.E-20)
        END DO
C
      END DO
C
      RETURN
      END    
C
C
C###########################################################
      SUBROUTINE PRINT(PHI,TITLE)
C###########################################################
C     This routine prints grid coordinates and other fields.
C===========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      DIMENSION PHI(NXYA)
C
      CHARACTER*6 TITLE
      WRITE(2,20) TITLE
      IE=1
      NL=NI/12+1
      IF(MOD(NI,12).EQ.0) NL=NI/12
C
      DO L=1,NL
        IS=IE
        IE=MIN(NI,IS+11)
        WRITE(2,'(3X,4HI = ,I3,11I10)') (I,I=IS,IE)
        WRITE(2,*) '  J'
        DO J=NJ,1,-1
          WRITE(2,'(1X,I3,1P12E10.2)') J,(PHI(LI(I+IST)+J),I=IS,IE)
        END DO
      END DO
C
   20 FORMAT(2X,26('*-'),7X,A6,7X,26('-*'))
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE SETIND(K)
C##########################################################
C     This routine sets some indices for the given grid.
C==========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
C
      NI=NIGR(K)
      NJ=NJGR(K)
      IST=IGR(K)
      JST=JGR(K)
      NIM=NI-1
      NJM=NJ-1
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE SETBC
C############################################################
C     This routine collects information about boundary cell
C     faces of the same type.
C============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'bound.ing'
C
C.....SEARCH EACH GRID FOR BOUNDARIES OF GIVEN TYPE
C
      NINA=0
      NOT=0
      NST=0
      NWT=0
      NOCT=0
C
      DO K=1,NGR
C
C.....INLET BOUNDARIES
C
        IIS(K)=NINA
        CALL DEFBC(1,K,NINA,NINL,IJI,IJPI,IJI1,IJI2)
        NINA=NINA+NINL(K)
C
C.....OUTLET BOUNDARIES
C
        IOS(K)=NOT
        CALL DEFBC(2,K,NOT,NOUT,IJO,IJPO,IJO1,IJO2)
        NOT=NOT+NOUT(K)
C
C.....SYMMETRIY BOUNDARIES
C
        ISS(K)=NST
        CALL DEFBC(3,K,NST,NSYM,IJS,IJPS,IJS1,IJS2)
        NST=NST+NSYM(K)
C
C.....ISOTHERMAL WALLS
C
        IWS(K)=NWT
        CALL DEFBC(4,K,NWT,NWALI,IJW,IJPW,IJW1,IJW2)
        NWT=NWT+NWALI(K)
C
C.....ADIABATIC WALLS
C
        IWAS(K)=NWT
        CALL DEFBC(5,K,NWT,NWALA,IJW,IJPW,IJW1,IJW2)
        NWT=NWT+NWALA(K)
        NWAL(K)=NWALI(K)+NWALA(K) 
C
C.....O-GRID & C-GRID CUTS 
C
        IOCS(K)=NOCT
        CALL DEFOCB(K,NOCT,NOC)
        NOCT=NOCT+NOC(K)
C
      END DO
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE DEFBC(LT,K,NBC,NB,IJB,IJBP,IJ1,IJ2)
C############################################################
C     This routine prepares data about boundary conditions
C     as required by the flow solver.
C============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'bound.ing'
      INCLUDE 'lines.ing'
C
      DIMENSION NB(MNG),IJB(1000),IJBP(1000),
     *          IJ1(1000),IJ2(1000)
C
C.....COLLECT BOUNDARY CELL FACES OF TYPE 'LT' IN A LIST FOR EACH GRID
C
      CALL SETIND(K)
      NB(K)=0
      NBCF=NBC
C
C.....SOUTH SIDE
C
      DO I=2,NIM
        IF(ITB(1,I+IST).EQ.LT) THEN
          NBCF=NBCF+1
          NB(K)=NB(K)+1
          IJB(NBCF)=LI(I+IST)+1
          IJBP(NBCF)=IJB(NBCF)+1
          IJ1(NBCF)=IJB(NBCF)
          IJ2(NBCF)=IJ1(NBCF)-NJ
        ENDIF
      END DO
C
C.....NORTH SIDE
C
      DO I=2,NIM
        IF(ITB(2,I+IST).EQ.LT) THEN
          NBCF=NBCF+1
          NB(K)=NB(K)+1
          IJB(NBCF)=LI(I+IST)+NJ
          IJBP(NBCF)=IJB(NBCF)-1
          IJ2(NBCF)=IJBP(NBCF)
          IJ1(NBCF)=IJ2(NBCF)-NJ
        ENDIF
      END DO
C
C.....WEST SIDE
C
      DO J=2,NJM
        IF(JTB(1,J+JST).EQ.LT) THEN
          NBCF=NBCF+1
          NB(K)=NB(K)+1
          IJB(NBCF)=LI(1+IST)+J
          IJBP(NBCF)=IJB(NBCF)+NJ
          IJ2(NBCF)=IJB(NBCF)
          IJ1(NBCF)=IJ2(NBCF)-1
        ENDIF
      END DO
C
C.....EAST SIDE
C
      DO J=2,NJM
        IF(JTB(2,J+JST).EQ.LT) THEN
          NBCF=NBCF+1
          NB(K)=NB(K)+1
          IJB(NBCF)=LI(NI+IST)+J
          IJBP(NBCF)=IJB(NBCF)-NJ
          IJ1(NBCF)=IJBP(NBCF)
          IJ2(NBCF)=IJ1(NBCF)-1
        ENDIF
      END DO
C
      RETURN
      END
C
C               
C############################################################
      SUBROUTINE DEFOCB(K,NBC,NB)
C############################################################
C     This routine prepares data about O- and C-boundaries
C     as required by the flow solver. For every boundary cell
C     face of type 10 not already checked, the check is
C     performed; if another face with same cell face center
C     is found, it is also flagged and one face is included
C     in the list of interface faces.
C============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'bound.ing'
      INCLUDE 'lines.ing'
      INCLUDE 'grid.ing'
C
      DIMENSION NB(MNG)
      COMMON /OCB/ LITBOC(2,NXA),LJTBOC(2,NYA)
      LOGICAL LITBOC,LJTBOC
C
C.....COLLECT BOUNDARY CELL FACE PAIRS OF TYPE '10' IN A LIST FOR EACH GRID
C
      CALL SETIND(K)
      NB(K)=0
      NBCF=NBC
C
C.....NONE OF BOUNDARY CELL FACES SEARCHED SO FAR:
C
      DO L=1,2
        DO I=2,NIM
          LITBOC(L,I+IST)=.TRUE.
        END DO
        DO J=2,NJM
          LJTBOC(L,J+JST)=.TRUE.
        END DO
      END DO
C
C.....SOUTH SIDE: SEARCHED FACES FLAGGED (LITBOC or LJTBOC set FALSE)
C
      DO I=2,NIM
        IF(ITB(1,I+IST).EQ.10.AND.LITBOC(1,I+IST)) THEN
          IJB=LI(I+IST)+2
          XB=0.5*(X(IJB-1)+X(IJB-NJ-1))
          YB=0.5*(Y(IJB-1)+Y(IJB-NJ-1))
          LITBOC(1,I+IST)=.FALSE.
          CALL OCFIND(IJNB,XB,YB)
          IF(IJNB.EQ.0) PRINT *,' INTERFACE PAIR NOT FOUND...'
          NBCF=NBCF+1
          NB(K)=NB(K)+1
          IJL(NBCF)=IJB
          IJR(NBCF)=IJNB
          IJOC1(NBCF)=IJB-1
          IJOC2(NBCF)=IJB-NJ-1
        ENDIF
      END DO
C
C.....NORTH SIDE
C
      DO I=2,NIM
        IF(ITB(2,I+IST).EQ.10.AND.LITBOC(2,I+IST)) THEN
          IJB=LI(I+IST)+NJM
          XB=0.5*(X(IJB)+X(IJB-NJ))
          YB=0.5*(Y(IJB)+Y(IJB-NJ))
          LITBOC(2,I+IST)=.FALSE.
          CALL OCFIND(IJNB,XB,YB)
          IF(IJNB.EQ.0) PRINT *,' INTERFACE PAIR NOT FOUND...'
          NBCF=NBCF+1
          NB(K)=NB(K)+1
          IJL(NBCF)=IJB
          IJR(NBCF)=IJNB
          IJOC1(NBCF)=IJB-NJ
          IJOC2(NBCF)=IJB
        ENDIF
      END DO
C
C.....WEST SIDE
C
      DO J=2,NJM
        IF(JTB(1,J+JST).EQ.10.AND.LJTBOC(1,J+JST)) THEN
          IJB=LI(2+IST)+J
          XB=0.5*(X(IJB-NJ)+X(IJB-NJ-1))
          YB=0.5*(Y(IJB-NJ)+Y(IJB-NJ-1))
          LJTBOC(1,J+JST)=.FALSE.
          CALL OCFIND(IJNB,XB,YB)
          IF(IJNB.EQ.0) PRINT *,' INTERFACE PAIR NOT FOUND...'
          NBCF=NBCF+1
          NB(K)=NB(K)+1
          IJL(NBCF)=IJB
          IJR(NBCF)=IJNB
          IJOC1(NBCF)=IJB-NJ-1
          IJOC2(NBCF)=IJB-NJ
        ENDIF
      END DO
C
C.....EAST SIDE
C
      DO J=2,NJM
        IF(JTB(2,J+JST).EQ.10.AND.LJTBOC(2,J+JST)) THEN
          IJB=LI(NIM+IST)+J
          XB=0.5*(X(IJB)+X(IJB-1))
          YB=0.5*(Y(IJB)+Y(IJB-1))
          LJTBOC(2,J+JST)=.FALSE.
          CALL OCFIND(IJNB,XB,YB)
          IF(IJNB.EQ.0) PRINT *,' INTERFACE PAIR NOT FOUND...'
          NBCF=NBCF+1
          NB(K)=NB(K)+1
          IJL(NBCF)=IJB
          IJR(NBCF)=IJNB
          IJOC1(NBCF)=IJB
          IJOC2(NBCF)=IJB-1
        ENDIF
      END DO
C
C.....CALCULATE INTERPOLATION FACTORS AT CUT INTERFACES
C
      DO IOC=NBC+1,NBC+NB(K)
        XB=0.5*(X(IJOC1(IOC))+X(IJOC2(IOC)))
        YB=0.5*(Y(IJOC1(IOC))+Y(IJOC2(IOC)))
        DL1=SQRT((XB-XC(IJL(IOC)))**2+(YB-YC(IJL(IOC)))**2)
        DL2=SQRT((XC(IJR(IOC))-XC(IJL(IOC)))**2+
     *           (YC(IJR(IOC))-YC(IJL(IOC)))**2)
        FOC(IOC)=DL1/DL2
      END DO
C
      RETURN
      END
C
C               
C############################################################
      SUBROUTINE OCFIND(IJNB,XB,YB)
C############################################################
C     This routine searches for pair faces at O- and C-cuts.
C     The search goes over all boundary faces of type 10 which
C     have not been flagges yet (they are flagged when a pair
C     has already been found by searching from the other side).
C     XB and YB are the coordinates of cell-face center whose
C     pair is beeing sought; XN and YN are the coordinates
C     of cell-face center for the face being checked. If the
C     two match within EPSREL of the cell-face length, they 
C     are assumed to match. The face being checked is labeled
C     and the index of CV-center on the other side, IJNB, is
C     returned to the calling routine. 
C============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'bound.ing'
      INCLUDE 'lines.ing'
      INCLUDE 'grid.ing'
C
      COMMON /OCB/ LITBOC(2,NXA),LJTBOC(2,NYA)
      LOGICAL LITBOC,LJTBOC
      EPSREL=1.E-1
      IJNB=0
C
C.....SEARCH SOUTH SIDE
C
      DO I=2,NIM
        IF(ITB(1,I+IST).EQ.10.AND.LITBOC(1,I+IST)) THEN
          IJN=LI(I+IST)+2
          XN=0.5*(X(IJN-1)+X(IJN-NJ-1))
          YN=0.5*(Y(IJN-1)+Y(IJN-NJ-1))
          DL=SQRT((X(IJN-1)-X(IJN-NJ-1))**2+(Y(IJN-1)-Y(IJN-NJ-1))**2)
          EPS=DL*EPSREL
          IF(ABS(XN-XB).LT.EPS.AND.ABS(YN-YB).LT.EPS) THEN
            IJNB=IJN
            LITBOC(1,I+IST)=.FALSE.
          ENDIF
        ENDIF
      END DO
C
C.....SEARCH NORTH SIDE
C
      DO I=2,NIM
        IF(ITB(2,I+IST).EQ.10.AND.LITBOC(2,I+IST)) THEN
          IJN=LI(I+IST)+NJM
          XN=0.5*(X(IJN)+X(IJN-NJ))
          YN=0.5*(Y(IJN)+Y(IJN-NJ))
          DL=SQRT((X(IJN)-X(IJN-NJ))**2+(Y(IJN)-Y(IJN-NJ))**2)
          EPS=DL*EPSREL
          IF(ABS(XN-XB).LT.EPS.AND.ABS(YN-YB).LT.EPS) THEN
            IJNB=IJN
            LITBOC(2,I+IST)=.FALSE.
          ENDIF
        ENDIF
      END DO
C
C.....SEARCH WEST SIDE
C
      DO J=2,NJM
        IF(JTB(1,J+JST).EQ.10.AND.LJTBOC(1,J+JST)) THEN
          IJN=LI(2+IST)+J
          XN=0.5*(X(IJN-NJ)+X(IJN-NJ-1))
          YN=0.5*(Y(IJN-NJ)+Y(IJN-NJ-1))
          DL=SQRT((X(IJN-NJ)-X(IJN-NJ-1))**2+(Y(IJN-NJ)-Y(IJN-NJ-1))**2)
          EPS=DL*EPSREL
          IF(ABS(XN-XB).LT.EPS.AND.ABS(YN-YB).LT.EPS) THEN
            IJNB=IJN
            LJTBOC(1,J+JST)=.FALSE.
          ENDIF
        ENDIF
      END DO
C
C.....EAST SIDE
C
      DO J=2,NJM
        IF(JTB(2,J+JST).EQ.10.AND.LJTBOC(2,J+JST)) THEN
          IJN=LI(NIM+IST)+J
          XN=0.5*(X(IJN)+X(IJN-1))
          YN=0.5*(Y(IJN)+Y(IJN-1))
          DL=SQRT((X(IJN)-X(IJN-1))**2+(Y(IJN)-Y(IJN-1))**2)
          EPS=DL*EPSREL
          IF(ABS(XN-XB).LT.EPS.AND.ABS(YN-YB).LT.EPS) THEN
            IJNB=IJN
            LJTBOC(2,J+JST)=.FALSE.
          ENDIF
        ENDIF
      END DO
C
      RETURN
      END
C
C               
C###########################################################
      SUBROUTINE SODW
C###########################################################
C     This routine calculates components of the unit vector
C     normal to wall boundary faces and area of that face
C     divided by the distance of cell center to the wall.
C===========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'grid.ing'
      INCLUDE 'bound.ing'
      INCLUDE 'logico.ing'
C
C.....CALCULATE COMPONENTS OF THE UNIT VECTOR TANGENTIAL TO CELL FACE
C
      DO IW=IWS(1)+1,IWS(1)+NWT
        IJB=IJW(IW)
        IJP=IJPW(IW)
        IJ1=IJW1(IW)
        IJ2=IJW2(IW)
C
        DX=X(IJ1)-X(IJ2)
        DY=Y(IJ1)-Y(IJ2)
        AR=SQRT(DX**2+DY**2)
        XTW(IW)=DX/(AR+1.E-20)
        YTW(IW)=DY/(AR+1.E-20)
        XN=YTW(IW)
        YN=-XTW(IW)
C
C.....NORMAL DISTANBE FROM CELL FACE CENTER TO CELL CENTER
C
        DN=(XC(IJB)-XC(IJP))*XN+(YC(IJB)-YC(IJP))*YN
C
C.....CELL FACE AREA DIVIDED BY DISTANCE TO THE CELL CENTER
C
        SRDW(IW)=AR/(DN+1.E-20)
        IF(LAXIS) SRDW(IW)=SRDW(IW)*YC(IJB)
C
      END DO
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE SODS
C#############################################################
C     This routine calculates components of the unit vector
C     parallel to symmetry boundary faces and area of that 
C     face divided by the distance of cell center to boundary.
C=============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'grid.ing'
      INCLUDE 'bound.ing'
      INCLUDE 'logico.ing'
C
C.....CALCULATE COMPONENTS OF THE UNIT VECTOR TANGENTIAL TO CELL FACE
C
      DO IS=ISS(1)+1,ISS(1)+NST
        IJB=IJS(IS)
        IJP=IJPS(IS)
        IJ1=IJS1(IS)
        IJ2=IJS2(IS)
C
        DX=X(IJ1)-X(IJ2)
        DY=Y(IJ1)-Y(IJ2)
        AR=SQRT(DX**2+DY**2)
        XT=DX/(AR+1.E-20)
        YT=DY/(AR+1.E-20)
        XNS(IS)=YT
        YNS(IS)=-XT
C
C.....NORMAL DISTANBE FROM CELL FACE CENTER TO CELL CENTER
C
        DN=(XC(IJB)-XC(IJP))*XNS(IS)+(YC(IJB)-YC(IJP))*YNS(IS)
C
C.....CELL FACE AREA DIVIDED BY DISTANCE TO THE CELL CENTER
C
        SRDS(IS)=AR/(DN+1.E-20)
        IF(LAXIS) SRDS(IS)=SRDS(IS)*YC(IJB)
C
      END DO
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE PRINTX
C#############################################################
C     This routine prints some parameters about the grid.
C=============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'grid.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'bound.ing'
      INCLUDE 'logico.ing'
      INCLUDE 'lines.ing'
C
      DO K=1,NGR
C
C.....WRITE BOUNDARY TYPE SPECIFICATION FOR BOUNDARY CELL FACES
C
        WRITE(2,*) '   BOUNDARY CONDITIONS'
        WRITE(2,*) '   ==================='
        WRITE(2,*) '  '
        CALL SETIND(K)
C
C.....SOUTH SIDE
C   
        WRITE(2,*) '   SOUTH SIDE:'
        WRITE(2,*) '  '
C
        LPI=NIM/20
        IF(MOD(NIM,20).NE.0) LPI=LPI+1
        DO L=1,LPI
          N=(L-1)*20
          NE=MIN(NI,N+20)
          WRITE(2,'(A7,20I4)') '    I: ',(I,I=N+1,NE)
          WRITE(2,'(A7,20I4)') ' ITBS: ',(ITB(1,I+IST),I=N+1,NE)
          WRITE(2,*) '  '
        END DO
        WRITE(2,*) '  '
C
C.....NORTH SIDE
C   
        WRITE(2,*) '   NORTH SIDE:'
        WRITE(2,*) '  '
C
        DO L=1,LPI
          N=(L-1)*20
          NE=MIN(NI,N+20)
          WRITE(2,'(A7,20I4)') '    I: ',(I,I=N+1,NE)
          WRITE(2,'(A7,20I4)') ' ITBN: ',(ITB(2,I+IST),I=N+1,NE)
          WRITE(2,*) '  '
        END DO
        WRITE(2,*) '  '
C
C.....WEST SIDE
C   
        WRITE(2,*) '   WEST SIDE:'
        WRITE(2,*) '  '
C
        LPJ=NJM/20
        IF(MOD(NJM,20).NE.0) LPJ=LPJ+1
        DO L=1,LPJ
          N=(L-1)*20
          NE=MIN(NJ,N+20)
          WRITE(2,'(A7,20I4)') '    J: ',(J,J=N+1,NE)
          WRITE(2,'(A7,20I4)') ' JTBW: ',(JTB(1,J+JST),J=N+1,NE)
          WRITE(2,*) '  '
        END DO
        WRITE(2,*) '  '
C
C.....EAST SIDE
C   
        WRITE(2,*) '   EAST SIDE:'
        WRITE(2,*) '  '
C
        DO L=1,LPJ
          N=(L-1)*20
          NE=MIN(NJ,N+20)
          WRITE(2,'(A7,20I4)') '    J: ',(J,J=N+1,NE)
          WRITE(2,'(A7,20I4)') ' JTBE: ',(JTB(2,J+JST),J=N+1,NE)
          WRITE(2,*) '  '
        END DO
        WRITE(2,*) '  '
C
C.....O- AND C-GRID CUTS
C
        WRITE(2,*) '   O- AND C-GRID CUTS:'
        WRITE(2,*) '  '
C
        LPI=NOC(K)/12
        IF(MOD(NOC(K),12).NE.0) LPI=LPI+1
        IB=IOCS(K)
        DO L=1,LPI
          N=(L-1)*12
          NE=MIN(NOC(K),N+12)
          WRITE(2,'(A7,12I6)') '  IOC: ',(I,I=N+1,NE)
          WRITE(2,'(A7,12I6)') '  IJL: ',(IJL(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)') '  IJR: ',(IJR(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12F6.3)') '  FOC: ',(FOC(I),I=IB+N+1,IB+NE)
          WRITE(2,*) '  '
        END DO
        WRITE(2,*) '  '
C
C.....WALLS
C
        WRITE(2,*) '   WALLS:'
        WRITE(2,*) '  '
C
        LPI=NWAL(K)/12
        IF(MOD(NWAL(K),12).NE.0) LPI=LPI+1
        IB=IWS(K)
        DO L=1,LPI
          N=(L-1)*12
          NE=MIN(NWAL(K),N+12)
          WRITE(2,'(A7,12I6)')   '    IW: ',(I,I=N+1,NE)
          WRITE(2,'(A7,12I6)')   '   IJW: ', (IJW(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJPW: ',(IJPW(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJW1: ',(IJW1(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJW2: ',(IJW2(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12F6.3)') '  SRDW: ',(SRDW(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12F6.3)') '   XTW: ', (XTW(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12F6.3)') '   YTW: ', (YTW(I),I=IB+N+1,IB+NE)
          WRITE(2,*) '  '
        END DO
        WRITE(2,*) '  '
C
C.....SYMMETRY
C
        WRITE(2,*) '   SYMMETRY PLANES:'
        WRITE(2,*) '  '
C
        LPI=NSYM(K)/12
        IF(MOD(NSYM(K),12).NE.0) LPI=LPI+1
        IB=ISS(K)
        DO L=1,LPI
          N=(L-1)*12
          NE=MIN(NSYM(K),N+12)
          WRITE(2,'(A7,12I6)')   '    IS: ',(I,I=N+1,NE)
          WRITE(2,'(A7,12I6)')   '   IJS: ', (IJS(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJPS: ',(IJPS(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJS1: ',(IJS1(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJS2: ',(IJS2(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12F6.3)') '  SRDS: ',(SRDS(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12F6.3)') '   XNS: ', (XNS(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12F6.3)') '   YNS: ', (YNS(I),I=IB+N+1,IB+NE)
          WRITE(2,*) '  '
        END DO
        WRITE(2,*) '  '
C
C.....INLETS
C
        WRITE(2,*) '   INLETS:'
        WRITE(2,*) '  '
C
        LPI=NINL(K)/12
        IF(MOD(NINL(K),12).NE.0) LPI=LPI+1
        IB=IIS(K)
        DO L=1,LPI
          N=(L-1)*12
          NE=MIN(NINL(K),N+12)
          WRITE(2,'(A7,12I6)')   '    II: ',(I,I=N+1,NE)
          WRITE(2,'(A7,12I6)')   '   IJI: ', (IJI(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJPI: ',(IJPI(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJI1: ',(IJI1(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJI2: ',(IJI2(I),I=IB+N+1,IB+NE)
          WRITE(2,*) '  '
        END DO
        WRITE(2,*) '  '
C
C.....OUTLETS
C
        WRITE(2,*) '   OUTLETS:'
        WRITE(2,*) '  '
C
        LPI=NOUT(K)/12
        IF(MOD(NOUT(K),12).NE.0) LPI=LPI+1
        IB=IOS(K)
        DO L=1,LPI
          N=(L-1)*12
          NE=MIN(NOUT(K),N+12)
          WRITE(2,'(A7,12I6)')   '    IO: ',(I,I=N+1,NE)
          WRITE(2,'(A7,12I6)')   '   IJO: ', (IJO(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJPO: ',(IJPO(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJO1: ',(IJO1(I),I=IB+N+1,IB+NE)
          WRITE(2,'(A7,12I6)')   '  IJO2: ',(IJO2(I),I=IB+N+1,IB+NE)
          WRITE(2,*) '  '
        END DO
        WRITE(2,*) '  '
C
C.....WRITE ARRAYS WITH CELL DATA
C
        CALL PRINT(X,' X-COO')
        CALL PRINT(Y,' Y-COO')
        CALL PRINT(XC,'XC-COO')
        CALL PRINT(YC,'YC-COO')
        CALL PRINT(FX,'  FX  ')
        CALL PRINT(FY,'  FY  ')
        CALL PRINT(VOL,' VOL  ')
        WRITE(2,*) '  '
C
      END DO
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE GRIDPL
C#############################################################
C     This routine plots the grids in postscript format.
C=============================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.ing'
      INCLUDE 'indexg.ing'
      INCLUDE 'grid.ing'
      DIMENSION IX(NXYA),IY(NXYA)
      CHARACTER FILOUT*8
C
C.....ALL GRIDS TO BE PLOTTED
C
      DO K=1,NGR
        CALL SETIND(K)
        NIJ=NI*NJ
        IJST=LI(1+IST)+1
        IJEN=IJST+NIJ-1
C
C....FIND MAXIMUM AND MINIMUM COORDINATES
C
        YMAX=-1.E20
        XMAX=-1.E20
        XMIN=1.E20
        YMIN=XMIN
C
        DO IJ=IJST,IJEN
          XMAX=MAX(XMAX,X(IJ))
          XMIN=MIN(XMIN,X(IJ))
          YMIN=MIN(YMIN,Y(IJ))
          YMAX=MAX(YMAX,Y(IJ))
        END DO
C
C.....SCALING FACTOR, SCALE COORDINATES, CONVERT TO INTEGER
C
        XL=XMAX-XMIN
        YL=YMAX-YMIN
        SCFX=8400.0/XL
        SCFY=8400.0/YL
        SCF=MIN(SCFX,SCFY)
        DO IJ=IJST,IJEN
          IX(IJ)=INT((X(IJ)-XMIN)*SCF)
          IY(IJ)=INT((Y(IJ)-YMIN)*SCF)
        END DO
C
C....WRITE POSTSCRIPT FILE HEADER
C
        XMIN=50.
        YMIN=50.
        XMAX=XMIN+XL*SCF*0.06
        YMAX=YMIN+YL*SCF*0.06
C
        WRITE(FILOUT,'(4Hgrid,I1,3H.ps)') K
        OPEN(UNIT=7,FILE=FILOUT)
        REWIND 7
        CALL PSHEAD(XMIN,XMAX,YMIN,YMAX)
C
C.....PLOT BOUNDARY OF SOLUTION DOMAIN WITH THICK LINE
C
        WRITE(7,*) '4 w'
        IJ=LI(1+IST)+1
        WRITE(7,'(2I5,A3)') IX(IJ),IY(IJ),' m '
C
        DO J=2,NJM
          IJ=LI(1+IST)+J
          WRITE(7,'(2I5,A3)') IX(IJ),IY(IJ),' l '
        END DO
C
        DO I=2,NIM
          IJ=LI(I+IST)+NJM
          WRITE(7,'(2I5,A3)') IX(IJ),IY(IJ),' l '
        END DO
C
        DO J=NJM-1,1,-1
          IJ=LI(NIM+IST)+J
          WRITE(7,'(2I5,A3)') IX(IJ),IY(IJ),' l '
        END DO
C
        DO I=NIM-1,1,-1
          IJ=LI(I+IST)+1
          WRITE(7,'(2I5,A3)') IX(IJ),IY(IJ),' l '
        END DO
C
        WRITE(7,*) 's'
C
C.....PLOT INNER GRID LINES WITH THIN LINES
C
        WRITE(7,*) '1 w'
C
C.....PLOT I-LINES (J=CONST)
C
        DO J=2,NJM-1
          IJ=LI(1+IST)+J
          WRITE(7,'(2I5,A3)') IX(IJ),IY(IJ),' m '
          DO I=2,NIM
            IJ=LI(I+IST)+J
            WRITE(7,'(2I5,A3)') IX(IJ),IY(IJ),' l '
          END DO
          WRITE(7,*) 's'
        END DO
C
C.....PLOT  J - LINES (I=CONST)
C
        DO I=2,NIM-1
          IJ=LI(I+IST)+1
          WRITE(7,'(2I5,A3)') IX(IJ),IY(IJ),' m '
          DO J=2,NJM
            IJ=LI(I+IST)+J
            WRITE(7,'(2I5,A3)') IX(IJ),IY(IJ),' l '
          END DO
          WRITE(7,*) 's'
        END DO
C
C.....CLOSE PLOT FOR ONE GRID
C
        WRITE(7,*) 'p'
        CLOSE(UNIT=7)
C
      END DO
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE PSHEAD(XMIN,XMAX,YMIN,YMAX)
C##########################################################
C     This routine prints the header of postscript files.
C==========================================================
      INCLUDE 'float.inc'                                                                                                                   
      WRITE(7,*) '%!PS-Adobe-2.0'
      WRITE(7,*) '%%Creator: GRIDPL'
      WRITE(7,*) '%%BoundingBox: ',XMIN,YMIN,XMAX,YMAX
      WRITE(7,*) '%%EndComments'
      WRITE(7,*) '/c {currentpoint} def /f {fill} def '
      WRITE(7,*) '/gr {grestore} def /gs {gsave} def /l {lineto} def '
      WRITE(7,*) '/m {moveto} def /n {newpath} def /p {showpage} def '
      WRITE(7,*) '/s {stroke} def /sg {setgray} def '
      WRITE(7,*) '/w {setlinewidth} def '
      WRITE(7,*) '50 50 translate 0.06 0.06 scale '
      WRITE(7,*) '1 setlinecap 1 setlinejoin '
      RETURN
      END

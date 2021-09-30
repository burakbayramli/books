C###################################################################      
      PROGRAM EXPA
C###################################################################
C#                                                                 #
C#    PROGRAM FOR INTERACTIVE GENERATION OF RECTILINEAR ORTHOGONAL #
C#    GRIDS FOR THE 2D MULTIGRID FLOW PREDICTION CODE              #
C#                                                                 #
C#                                    M. PERIC, IfS Hamburg, 1995  #
C###################################################################
      COMMON /COOR/ X(2000),Y(2000),
     *       XG(1000),YG(1000),XALL(2000),YALL(2000)
      COMMON /PARAM/ NI,NJ,NIM,NJM,NIGR(8),NJGR(8),
     *       IGR(8),JGR(8),IJGR(8),KG1,NGR,LPLOT
      CHARACTER FILIN*12,FILOUT*12
C
C----------------------------------------
C.....READ INPUT FILE NAME AND OPEN FILES
C----------------------------------------
C
      PRINT *, ' INPUT FILE NAME (* - KEYBOARD):  '
      READ(*,1) FILIN
    1 FORMAT(A12)
      IF(FILIN.NE.'*') THEN
        OPEN (UNIT=2,FILE=FILIN)
        REWIND 2
        ITYP=0
      ELSE
        OPEN (UNIT=1,FILE='grid.inp')
        REWIND 1
        ITYP=1
      ENDIF
C
      PRINT *, ' OUTPUT FILE NAME:  '
      IF(ITYP.EQ.1) THEN
        READ(*,1) FILOUT
        WRITE(1,1) FILOUT
      ELSE
        READ(2,1) FILOUT
      ENDIF
C
      OPEN (UNIT=3,FILE=FILOUT)
      REWIND 3
C-----------------------------------------------------------
C          READ INPUT DATA AND GENERATE KG1-TH LEVEL GRID
C          LPLOT -> 1 - PLOT THE GRID; 0 - DON'T
C          KG1   -  GRID LEVEL TO BE GENERATED
C          NGR  -   TOTAL NUMBER OF GRID LEVELS
C          GRIDN -  ROUTINE WHICH GENERATES THE BASE GRID
C          IF DATA IS ENTERED FROM A KEYBOARD, IT IS WRITTEN
C          ONTO FILE 'grid.inp' FOR FUTURE USE
C-----------------------------------------------------------
      PRINT *, ' ENTER> LPLOT, KG1, NGR:  '
      IF(ITYP.EQ.1) THEN
        READ(*,*) LPLOT,KG1,NGR
        WRITE(1,*) LPLOT,KG1,NGR,'  LPLOT,KG1,NGR '
      ELSE
        READ(2,*) LPLOT,KG1,NGR
      ENDIF
C
      CALL GRIDN(ITYP)
C-------------------------------------------------
C.....REFINE GENERATED GRID UP TO THE FINEST LEVEL
C-------------------------------------------------
      DO K=KG1+1,NGR
        CALL GRIDF(K)
      END DO
C-------------------------------------------------
C.....DEFINE ALL GRIDS, FROM LEVEL 1 TO LEVEL NGR
C-------------------------------------------------
      CALL GRIDM
C
      STOP
      END
C
C####################################################################
      SUBROUTINE GRIDN(ITYP)
C####################################################################
      COMMON /COOR/ X(2000),Y(2000),
     *       XG(1000),YG(1000),XALL(2000),YALL(2000)
      COMMON /PARAM/ NI,NJ,NIM,NJM,NIGR(8),NJGR(8),
     *       IGR(8),JGR(8),IJGR(8),KG1,NGR,LPLOT
      DIMENSION XX(2000)
      FI(A,B,G,N)=A*G**N-B*G+B-A
C
C------------------------------------------------------
C.....CALCULATION OF GRID COORDINATES FOR ONE DIRECTION
C------------------------------------------------------
      PRINT *, ' *** GENERATING  X-COORDINATES ***'
C---------------------------------------------------------------
C          THE GRID IS IN EACH DIRECTION SUBDIVIDED INTO NSUB
C          SUBDOMAINS (NSUB IS NOT SAME FOR X AND Y DIRECTION).
C          IN EACH SUBDOMAIN, GRID SPACING EXPANDS (OR CONTRACTS)
C          BY A CONSTANT FACTOR. SUBDIVISION IN SUBDOMAINS
C          ALLOWS CONTROLL OVER DISTRIBUTION OF GRID LINES.
C          FOR THE FIRST SUBDOMAIN, COORDINATES OF BEGIN AND
C          END, THE NUMBER OF CVs WITHIN SUBDOMAIN, AND EITHER
C          THE SIZE OF REFERENCE CV AND ITS POSITION (BEGIN OR
C          END OF THE SUBDOMAIN) OR THE EXPANSION FACTOR NEED 
C          BE SPECIFIED. FOR SUBSEQUENT SUBDOMAINS, ONLY THE END
C          COORDINATE AND THE NUMBER OF CVs ARE REQUIRED; THE 
C          WIDTH OF THE INITIAL CV IS THE SAME AS THE WIDTH OF
C          THE LAST CV IN THE PRECEDING SUBDOMAIN.
C---------------------------------------------------------------
      DO LL=1,2
        PRINT *, ' NUMBER OF SUBDOMAINS?  '
        IF(ITYP.EQ.1) THEN
          READ(*,*) NSUB
          WRITE(1,*) NSUB,'   NSUB '
        ELSE
          READ(2,*) NSUB
        ENDIF
c
        NP=1
C
        DO L=1,NSUB
    5     CONTINUE
C-------------------------------------------
C         INPUT DATA FOR THE FIRST SUBDOMAIN
C-------------------------------------------
          IF (L.EQ.1) THEN
            PRINT *, ' ENTER> XSTART, XEND, NUMBER OF CVS:  '
            IF(ITYP.EQ.1) THEN
              READ(*,*) XXS,XXE,N
              WRITE(1,*) XXS,XXE,N,'   XXS,XXE,N '
            ELSE
              READ(2,*) XXS,XXE,N
            ENDIF
C
            PRINT *, ' ENTER> EXPANSION FACTOR (ZERO OR SPEC. VALUE)  '
            IF(ITYP.EQ.1) THEN
              READ(*,*) G
              WRITE(1,*) G,'   G '
            ELSE
              READ(2,*) G
            ENDIF
C
            IF (G.EQ.0.) THEN
              PRINT *, ' REF. CV SIZE, POSITION (1 - BEG., 2 - END)  '
              IF(ITYP.EQ.1) THEN
                READ(*,*) A,IP 
                WRITE(1,*) A,IP, '   A,IP '
              ELSE
                READ(2,*) A,IP 
              ENDIF
            ENDIF
          ELSE
C----------------------------------------
C         INPUT DATA FOR OTHER SUBDOMAINS
C----------------------------------------
            XXS=XXE
            PRINT *, ' ENTER: XEND, NUMBER OF CVs:  '
            IF(ITYP.EQ.1) THEN
              READ(*,*) XXE,N
              WRITE(1,*) XXE,N,'   XXE,N '
            ELSE
              READ(2,*) XXE,N
            ENDIF
C
            A=XX(NP)-XX(NP-1)
            IP=1
          ENDIF
C--------------------------------------------------------------------
C         ABOVE, 'XX(I)' ARE THE COORDINATES OF GRID POINTS (USED
C         FOR BOTH DIRECTIONS), 'A' IS THE WIDTH OF THE REFERENCE CV,
C         AND 'G' IS THE SPECIFIED EXPANSION FACTOR. IF 'G=0.0' WAS
C         SPECIFIED, EXPANSION FACTOR IS CALCULATED USING KNOWN
C         INTERWAL WIDTH AND THE SIZE OF THE REFERENCE CV AT ONE
C         END.
C         BELOW, 'G1' AND 'G2' ARE THE LIMITING EXPANSION RATIOS, 'EPS'
C         IS CONVERGENCE TOLERANCE FOR THE SECANT METHOD WHICH IS
C         USED TO CALCULATE THE EXPANSION FACTOR WHEN THE FIRST
C         INTERVAL IS GIVEN.
C--------------------------------------------------------------------
          B=XXE-XXS
          IF (L.GT.1.OR.(L.EQ.1.AND.G.EQ.0.0)) THEN
            EPS=1.E-8
            AR=B/REAL(N)
            IF(A.LT.AR) THEN
              G1=1.000001
              G2=2.0
            ELSE
              G1=0.5
              G2=0.999999
            ENDIF
C
            G=1.
            IF(ABS((A-AR)/A).GT.1.E-4) THEN
              FI1=FI(A,B,G1,N)
              FI2=FI(A,B,G2,N)
              IF(SIGN(1.,FI1).NE.SIGN(1.,FI2)) THEN
                K=0
   10           K=K+1
                G=0.5*(G1+G2)
                FIP=FI(A,B,G,N)
                IF(SIGN(1.,FIP).EQ.SIGN(1.,FI2)) THEN
                  G2=G
                  FI2=FIP
                ELSE
                  G1=G
                  FI1=FIP
                ENDIF
                IF(ABS(FI2-FI1).GT.EPS.AND.K.LT.200) GO TO 10
                PRINT *, '   EXP. FACTOR IN INTERVAL ',L, ' G = ',G
                PRINT *, '     ',K,'  ITERATIONS PERFORMED '
              ELSE
                PRINT *, '  * ERROR: EXPANSION FACTOR > 2 OR < 0.5 *'
                PRINT *, '  INCREASE NO. OF CVs OR CHANGE INITIAL ONE'
                GO TO 5
              ENDIF
            ENDIF
            IF (IP.EQ.2) G=1./G
C---------------------------------------------------------------
C         IF 'G' WAS SPECIFIED FOR THE FIRST INTERVAL, CALCULATE
C         THE SIZE OF THE FIRST CV
C---------------------------------------------------------------   
          ELSE
            S=1.
            DO I=1,N-1
              S=S+G**I
            END DO
            A=B/S
          ENDIF
C----------------------------------------
C.....CALCULATE COODINATES OF GRID POINTS
C----------------------------------------
          IST=NP
          NP=NP+N
          XX(IST)=XXS
          DO I=1,N
            XX(IST+I)=XX(IST+I-1)+A*G**(I-1)
          END DO
        END DO
C------------------------------------
C.....FINISH ONE COORDINATE DIRECTION
C------------------------------------
        NP=NP+1
        XX(NP)=XX(NP-1)
        IF(LL.EQ.1) THEN
          NI=NP
          NIM=NI-1
          DO I=1,NI
            X(I)=XX(I)
          END DO
          PRINT *, ' *** GENERATING  Y-COORDINATES *** '
C
        ELSE
          NJ=NP
          NJM=NJ-1
          DO J=1,NJ
            Y(J)=XX(J)
          END DO
        ENDIF  
C
      END DO
C
      RETURN
      END
C
C####################################################################
      SUBROUTINE GRIDF(K)
C####################################################################
      COMMON /COOR/ X(2000),Y(2000),
     *       XG(1000),YG(1000),XALL(2000),YALL(2000)
      COMMON /PARAM/ NI,NJ,NIM,NJM,NIGR(8),NJGR(8),
     *       IGR(8),JGR(8),IJGR(8),KG1,NGR,LPLOT
C
C-------------------------------------------------------------
C         GRID LEVEL K HAS (NI-2) CVs IN X AND (NJ-2) CVs IN Y
C         DIRECTION; COORDINATES OF GRID LINES ARE STORED AS
C         X(I), I=1,NI AND Y(J), J=1,NJ. HERE LEVEL (K+1) GRID
C         WILL BE CREATED; X(I) AND Y(J) WILL BE OVERWRITEN BY
C         THE REFINED GRID DATA.
C-------------------------------------------------------------
      DO I=1,NI
        XG(I)=X(I)
      END DO
      DO J=1,NJ
        YG(J)=Y(J)
      END DO
      NIG=NI
      NJG=NJ
C-------------------------------------------------------------------
C         REFINED GRID HAS TWICE AS MANY CVs; WITH TWO BOUNDARY
C         NODES, STORAGE FOR (NIG*2-2) AND (NJG*2-2) NODES IS NEEDED
C         'X(I)' AND 'Y(J)' ARE OVERWRITTEN BY REFINED GRID
C-------------------------------------------------------------------
      NI=NIG*2-2
      NJ=NJG*2-2
      NIM=NI-1
      NJM=NJ-1
C-----------------------------------------------------------------
C        REFINE X-COORDINATES; IF THE ORIGINAL GRID WAS SYMMETRIC,
C        RETAIN SYMMETRY. EXPANSION FACTOR OF THE FINE GRID IS
C        EQUAL TO SQRT OF THE COARSE GRID EXPANSION FACTOR
C-----------------------------------------------------------------
      X(1)=XG(1)
      DLP=XG(2)-XG(1)
      DLE=DLP
C
      DO IG=2,NIG-2
        I=2*IG-1
        X(I)=XG(IG)
        DLW=DLP
        DLP=DLE
        DLE=XG(IG+1)-XG(IG)
        FX=SQRT(DLP/DLE)
        FAC=FX/(1.+FX)
        FXR=SQRT(DLP/DLW)
        FACR=FXR/(1.+FXR)
        FAC=0.5*(1.+FAC-FACR)
        X(I-1)=X(I-2)+DLP*FAC
      END DO 
C
      DLW=DLP
      DLP=DLE
      FX=SQRT(DLP/DLE)
      FAC=FX/(1.+FX)
      FXR=SQRT(DLP/DLW)
      FACR=FXR/(1.+FXR)
      FAC=0.5*(1.+FAC-FACR)
      X(I+1)=X(I)+FAC*DLP
      X(NIM)=XG(NIG-1)
      X(NI)=X(NIM)
C-----------------------------
C         REFINE Y-COORDINATES
C-----------------------------
      Y(1)=YG(1)
      DLP=YG(2)-YG(1)
      DLE=DLP
C
      DO JG=2,NJG-2
        J=2*JG-1
        Y(J)=YG(JG)
        DLW=DLP
        DLP=DLE
        DLE=YG(JG+1)-YG(JG)
        FX=SQRT(DLP/DLE)
        FAC=FX/(1.+FX)
        FXR=SQRT(DLP/DLW)
        FACR=FXR/(1.+FXR)
        FAC=0.5*(1.+FAC-FACR)
        Y(J-1)=Y(J-2)+DLP*FAC
      END DO 
C
      DLW=DLP
      DLP=DLE
      FX=SQRT(DLP/DLE)
      FAC=FX/(1.+FX)
      FXR=SQRT(DLP/DLW)
      FACR=FXR/(1.+FXR)
      FAC=0.5*(1.+FAC-FACR)
      Y(J+1)=Y(J)+FAC*DLP
      Y(NJM)=YG(NJG-1)
      Y(NJ)=Y(NJM)
C
      RETURN
      END
C
C####################################################################
      SUBROUTINE GRIDM
C####################################################################
      COMMON /COOR/ X(2000),Y(2000),
     *       XG(1000),YG(1000),XALL(2000),YALL(2000)
      COMMON /PARAM/ NI,NJ,NIM,NJM,NIGR(8),NJGR(8),
     *       IGR(8),JGR(8),IJGR(8),KG1,NGR,LPLOT
C
C-----------------------------------------------------------------
C          DEFINE CONTROL PARAMETERS; THE LATEST REFINED GRID IS
C          THE FINEST. 'NIGR(K)' IS 'NI' OF THE GRID LEVEL 'K';
C          'NJGR(K)' IS 'NJ' OF GRID LEVEL 'K'. 'IGR(K)' AND
C          'JGR(K)' ARE THE 'I' AND 'J' OFFSET FOR GRID 'K'  
C          (POINTER TO 'I=1' AND 'J=1' OF GRID 'K' IN THE VECTOR 
C          CONTAINING ALL GRIDS), AND 'IJGR(K)' IS THE OFFSET
C          OF NODAL INDICES (POINTER TO THE FIRST NODE OF GRID 'K'
C          IN THE VECTOR CONTAINING ALL GRIDS)
C-----------------------------------------------------------------
      NIGR(NGR)=NI
      NJGR(NGR)=NJ
      DO K=NGR-1,1,-1
        NIGR(K)=(NIGR(K+1)-2)/2+2
        NJGR(K)=(NJGR(K+1)-2)/2+2
      END DO
      IGR(1)=0
      JGR(1)=0
      IJGR(1)=0
      DO K=2,NGR
        IGR(K)=IGR(K-1)+NIGR(K-1)
        JGR(K)=JGR(K-1)+NJGR(K-1)
        IJGR(K)=IJGR(K-1)+NIGR(K-1)*NJGR(K-1)
      END DO
C
      WRITE(3,*) (IGR(I),I=1,NGR),'  (IGR(I),I=1,NGR) '
      WRITE(3,*) (JGR(I),I=1,NGR),'  (JGR(I),I=1,NGR) '
      WRITE(3,*) (NIGR(I),I=1,NGR),'  (NIGR(I),I=1,NGR) '
      WRITE(3,*) (NJGR(I),I=1,NGR),'  (NJGR(I),I=1,NGR) '
      WRITE(3,*) (IJGR(I),I=1,NGR),'  (IJGR(I),I=1,NGR) '
C
C----------------------------------------------------------------
C          DEFINE GRIDS AND WRITE GRID COORDINATES FOR EACH LEVEL
C          IF 'LPLOT = 1', PLOT EACH GRID
C---------------------------------------------------------------- 
      DO L=1,NGR
        ISTEP=2**(NGR-L)
        IA=0
        DO I=1,NIGR(NGR),ISTEP
          IA=IA+1
          XALL(IA)=X(I)
        END DO
        XALL(NIGR(L))=XALL(NIGR(L)-1)
C
        JA=0
        DO J=1,NJGR(NGR),ISTEP
          JA=JA+1
          YALL(JA)=Y(J)
        END DO
        YALL(NJGR(L))=YALL(NJGR(L)-1)
C
        WRITE(3,*) (XALL(I),I=1,NIGR(L))
        WRITE(3,*) (YALL(J),J=1,NJGR(L))
        IF(LPLOT.EQ.1) CALL GRIDPL(L)
      END DO
C
      RETURN
      END
C
C#############################################################
      SUBROUTINE GRIDPL(L)
C#############################################################
      COMMON /COOR/ X(2000),Y(2000),
     *       XG(1000),YG(1000),XALL(2000),YALL(2000)
      COMMON /PARAM/ NI,NJ,NIM,NJM,NIGR(8),NJGR(8),
     *       IGR(8),JGR(8),IJGR(8),KG1,NGR,LPLOT
      DIMENSION IXALL(2000),IYALL(2000)
      CHARACTER FILOUT*12
C
C-----------------------------------------------------------
C         CREATE POSTSCRIPT FILE WITH PLOT OF GRID LINES FOR
C         EACH GRID LEVEL (FILE NAMED GRID?.PS). ROUTINE
C         'PSHEAD' WRITES HEADER OF THE POSCRIPT FILE. 
C         PLOT AREA IS 7 x 7 INCHES, ORIENTATION IS PORTRAIT;
C         COORDINATES ARE SCALED TO ALLOW ACCURATE PRINTING
C         ON PRINTERS WITH 1200 x 1200 DPI 
C-----------------------------------------------------------
      IE=NIGR(L)-1
      JE=NJGR(L)-1
      XMIN=XALL(L)
      XMAX=XALL(IE)
      YMIN=YALL(L)
      YMAX=YALL(JE)
      XL=XMAX-XMIN
      YL=YMAX-YMIN
      SCFX=8400.0/XL
      SCFY=8400.0/YL
      SCF=MIN(SCFX,SCFY)
      XMIN=50.
      YMIN=50.
      XMAX=XMIN+XL*SCF*0.06
      YMAX=YMIN+YL*SCF*0.06
C
      WRITE(FILOUT,'(4Hgrid,I1,3H.ps)') L
      OPEN(UNIT=7,FILE=FILOUT)
      REWIND 7
      CALL PSHEAD(XMIN,XMAX,YMIN,YMAX)
C
      DO I=1,IE
        IXALL(I)=INT(XALL(I)*SCF)
      END DO
      DO J=1,JE
        IYALL(J)=INT(YALL(J)*SCF)
      END DO
C---------------------------------------------------------
C         PLOT BOUNDARY OF SOLUTION DOMAIN WITH THICK LINE
C---------------------------------------------------------
      WRITE(7,*) '4 w'
      WRITE(7,71) IXALL(1),IYALL(1),' m ',IXALL(IE),IYALL(1),' l '
      WRITE(7,71) IXALL(IE),IYALL(JE),' l ',IXALL(1),IYALL(JE),' l '
      WRITE(7,72) IXALL(1),IYALL(1),' l  s '
   71 FORMAT(2I5,A3,2I5,A3)
   72 FORMAT(2I5,A6)
C---------------------------------------------------------
C         PLOT INNER GRID LINES WITH THIN LINES
C---------------------------------------------------------
      WRITE(7,*) '1 w'
      DO J=2,JE-1
        WRITE(7,73) IXALL(1),IYALL(J),' m ',IXALL(IE),IYALL(J),' l  s '
      END DO
      DO I=2,IE-1
        WRITE(7,73) IXALL(I),IYALL(1),' m ',IXALL(I),IYALL(JE),' l  s '
      END DO
   73 FORMAT(2I5,A3,2I5,A6)
      WRITE(7,*) 'p '
C
      CLOSE(UNIT=7)
      RETURN
      END
C
C#######################################################
      SUBROUTINE PSHEAD(XMIN,XMAX,YMIN,YMAX)
C#######################################################
C
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

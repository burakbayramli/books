      PROGRAM EXPA
C###################################################################
C#         GENERATION OF RECTILINEAR GRIDS                         #
C#         FOR THE MULTIGRID FLOW PREDICTION CODE                  #
C#         Z. Lilek, M. Peric, Institut fuer Schiffbau, 1995       #
C###################################################################
      PARAMETER (NGIT=4,NICV=8,NJCV=8,NKCV=8,
     *          NX=NICV*2**(NGIT-1)+2,NY=NJCV*2**(NGIT-1)+2,
     *          NZ=NKCV*2**(NGIT-1)+2,
     *          NXY=NX*NY,NXZ=NX*NZ,NYZ=NY*NZ,NXYZ=NXY*NZ,
     *          NXA=NICV*(2**NGIT-1)+2*NGIT,NYA=NJCV*(2**NGIT-1)+2*NGIT,
     *          NZA=NKCV*(2**NGIT-1)+2*NGIT,
     *          NXYA=NICV*NJCV*(4**NGIT-1)/3+2*(NXA+NYA-2*NGIT),
     *          NXZA=NICV*NKCV*(4**NGIT-1)/3+2*(NXA+NZA-2*NGIT),
     *          NYZA=NJCV*NKCV*(4**NGIT-1)/3+2*(NYA+NZA-2*NGIT),
     *          NXYZA=NICV*NJCV*NKCV*(8**NGIT-1)/7+
     *          2*(4**NGIT-1)/3*(NICV*NJCV+NICV*NKCV+NJCV*NKCV)+
     *          4*(NXA+NYA+NZA-4*NGIT))
      COMMON /COOR/ F(NXYZA),X(NXA),Y(NYA),Z(NZA),XG(NXA),
     *       YG(NYA),ZG(NZA),XALL(NXA),YALL(NYA),ZALL(NZA)
      COMMON /PARAM/ NI,NJ,NK,NIM,NJM,NKM,NIGIT(NGIT),NJGIT(NGIT),
     *       NKGIT(NGIT),IGIT(NGIT),JGIT(NGIT),KGIT(NGIT),IJKGIT(NGIT),
     *       KG1
      common /logic/ LOB,LOT,LOS,LON,LOW,LOE
      LOGICAL LOB,LOT,LOS,LON,LOW,LOE
      COMMON /BC/ LBS(NXZA),LBN(NXZA),LBW(NYZA),LBE(NYZA),
     *            LBB(NXYA),LBT(NXYA),
     *            ISBIJ(NGIT),ISBIK(NGIT),ISBJK(NGIT)
      LOGICAL LPLOT
      CHARACTER*10 FILIN,FILOUT,FILRES
C
C.....OPEN FILES
C
      PRINT *, ' ENTER:  INPUT FILE NAME '
      READ(*,5) FILIN
      PRINT *, ' ENTER: OUTPUT FILE NAME '
      READ(*,5) FILOUT
      PRINT *, ' ENTER: RESULT FILE NAME '
      READ(*,5) FILRES
    5 FORMAT(A10)
      OPEN (UNIT=1,FILE=FILIN)
      OPEN (UNIT=2,FILE=FILOUT)
      OPEN (UNIT=4,FILE=FILRES)
      REWIND 1
      REWIND 2
      REWIND 4
C
C.....GENERATE ONE GRID
C
      PRINT *, ' ENTER GRID LEVEL FOR WHICH DATA WILL BE GIVEN:  '
      READ(1,*) KG1
      CALL GRIDN
C
C.....REFINE GRID TO THE  FINEST LEVEL
C
      DO 50 K=KG1+1,NGIT
   50 CALL GRIDF
C
C.....DEFINE ALL GRIDS
C
      CALL GRIDM
      CALL SETB
C
C
C.....STORE GRID DATA
C
      WRITE(4,*) LOB,LOT,LOS,LON,LOW,LOE,LBS,LBN,LBW,LBE,
     *           LBB,LBT,ISBIJ,ISBIK,ISBJK,
     *           NIGIT,NJGIT,NKGIT,IGIT,JGIT,KGIT,IJKGIT 
      WRITE(4,*) XALL,YALL,ZALL
C
      STOP
      END
C
C####################################################################
C#                GENERATION OF NEW GRID                            #
C####################################################################
      SUBROUTINE GRIDN
      PARAMETER (NGIT=4,NICV=8,NJCV=8,NKCV=8,
     *          NX=NICV*2**(NGIT-1)+2,NY=NJCV*2**(NGIT-1)+2,
     *          NZ=NKCV*2**(NGIT-1)+2,
     *          NXY=NX*NY,NXZ=NX*NZ,NYZ=NY*NZ,NXYZ=NXY*NZ,
     *          NXA=NICV*(2**NGIT-1)+2*NGIT,NYA=NJCV*(2**NGIT-1)+2*NGIT,
     *          NZA=NKCV*(2**NGIT-1)+2*NGIT,
     *          NXYA=NICV*NJCV*(4**NGIT-1)/3+2*(NXA+NYA-2*NGIT),
     *          NXZA=NICV*NKCV*(4**NGIT-1)/3+2*(NXA+NZA-2*NGIT),
     *          NYZA=NJCV*NKCV*(4**NGIT-1)/3+2*(NYA+NZA-2*NGIT),
     *          NXYZA=NICV*NJCV*NKCV*(8**NGIT-1)/7+
     *          2*(4**NGIT-1)/3*(NICV*NJCV+NICV*NKCV+NJCV*NKCV)+
     *          4*(NXA+NYA+NZA-4*NGIT))
      COMMON /COOR/ F(NXYZA),X(NXA),Y(NYA),Z(NZA),XG(NXA),
     *       YG(NYA),ZG(NZA),XALL(NXA),YALL(NYA),ZALL(NZA)
      COMMON /PARAM/ NI,NJ,NK,NIM,NJM,NKM,NIGIT(NGIT),NJGIT(NGIT),
     *       NKGIT(NGIT),IGIT(NGIT),JGIT(NGIT),KGIT(NGIT),IJKGIT(NGIT),
     *       KG1
      common /logic/ LOB,LOT,LOS,LON,LOW,LOE
      LOGICAL LOB,LOT,LOS,LON,LOW,LOE
      COMMON /BC/ LBS(NXZA),LBN(NXZA),LBW(NYZA),LBE(NYZA),
     *            LBB(NXYA),LBT(NXYA),
     *            ISBIJ(NGIT),ISBIK(NGIT),ISBJK(NGIT)
      DIMENSION YY(500),E(200)
C
C.....CALCULATION OF COORDINATES FOR ONE DIRECTION
C
      PRINT *, ' X-coordinates'
C
      DO 200 LL=1,3
      PRINT *, ' Number of subdomains?'
      READ(1,*) NSUB
      NP=1
C
C.....INPUT DATA FOR ONE SUBDOMAIN
C
      DO 100 L=1,NSUB
    5 PRINT *, ' Starting and ending coordinate, number of CV?'
      READ(1,*) XS,XE,N
      IF(L.EQ.1) THEN
        PRINT *, ' ENTER EXPANSION FACTOR: '
        READ(1,*) G
      ELSE
        G=1./G
      ENDIF
      B=XE-XS
C
C.....CALCULATE SIZE OF FIRST INTERVAL
C
      S=1.
      DO I=1,N-1
        S=S+G**I
      END DO
      A=B/S
C
C......CALCULATE SIZE OF INTERVALS
C
      IST=NP
      NP=NP+N
      F(1)=A
      DO 20 I=2,N
   20 F(I)=A*G**(I-1)
C
C.....CALCULATE COODINATES OF GRID POINTS
C
      YY(IST)=XS
      DO 40 I=1,N
      II=IST+I
   40 YY(II)=YY(II-1)+F(I)
  100 CONTINUE
C
C.....FINISH ONE INTERVAL
C
      NP=NP+1
      YY(NP)=YY(NP-1)
      IF(LL.EQ.1) THEN
      NI=NP
      NIM=NI-1
      DO 120 I=1,NI
  120 X(I)=YY(I)
      PRINT *, ' Y - Coordinates'
      ELSEIF(LL.EQ.2) THEN
      NJ=NP
      NJM=NJ-1
      DO 125 J=1,NJ
  125 Y(J)=YY(J)
      PRINT *, ' Z - Coordinates'
      ELSEIF(LL.EQ.3) THEN
      NK=NP
      NKM=NK-1
      DO 135 K=1,NK
  135 Z(K)=YY(K)
      ENDIF
  200 CONTINUE
      RETURN
      END
C
C####################################################################
C#              REFINING COARSE GRID                                #
C####################################################################
      SUBROUTINE GRIDF
      PARAMETER (NGIT=4,NICV=8,NJCV=8,NKCV=8,
     *          NX=NICV*2**(NGIT-1)+2,NY=NJCV*2**(NGIT-1)+2,
     *          NZ=NKCV*2**(NGIT-1)+2,
     *          NXY=NX*NY,NXZ=NX*NZ,NYZ=NY*NZ,NXYZ=NXY*NZ,
     *          NXA=NICV*(2**NGIT-1)+2*NGIT,NYA=NJCV*(2**NGIT-1)+2*NGIT,
     *          NZA=NKCV*(2**NGIT-1)+2*NGIT,
     *          NXYA=NICV*NJCV*(4**NGIT-1)/3+2*(NXA+NYA-2*NGIT),
     *          NXZA=NICV*NKCV*(4**NGIT-1)/3+2*(NXA+NZA-2*NGIT),
     *          NYZA=NJCV*NKCV*(4**NGIT-1)/3+2*(NYA+NZA-2*NGIT),
     *          NXYZA=NICV*NJCV*NKCV*(8**NGIT-1)/7+
     *          2*(4**NGIT-1)/3*(NICV*NJCV+NICV*NKCV+NJCV*NKCV)+
     *          4*(NXA+NYA+NZA-4*NGIT))
      COMMON /COOR/ F(NXYZA),X(NXA),Y(NYA),Z(NZA),XG(NXA),
     *       YG(NYA),ZG(NZA),XALL(NXA),YALL(NYA),ZALL(NZA)
      COMMON /PARAM/ NI,NJ,NK,NIM,NJM,NKM,NIGIT(NGIT),NJGIT(NGIT),
     *       NKGIT(NGIT),IGIT(NGIT),JGIT(NGIT),KGIT(NGIT),IJKGIT(NGIT),
     *       KG1
      common /logic/ LOB,LOT,LOS,LON,LOW,LOE
      LOGICAL LOB,LOT,LOS,LON,LOW,LOE
      COMMON /BC/ LBS(NXZA),LBN(NXZA),LBW(NYZA),LBE(NYZA),
     *            LBB(NXYA),LBT(NXYA),
     *            ISBIJ(NGIT),ISBIK(NGIT),ISBJK(NGIT)
C
      DO 10 I=1,NI
   10 XG(I)=X(I)
      DO 20 J=1,NJ
   20 YG(J)=Y(J)
      DO 30 K=1,NK
   30 ZG(K)=Z(K)
      NIG=NI
      NJG=NJ
      NKG=NK
C
C.....CALCULATE FINE GRID PARAMETERS
C
      NI=NIG*2-2
      NJ=NJG*2-2
      NK=NKG*2-2
      NIM=NI-1
      NJM=NJ-1
      NKM=NK-1
C
C.....REFINE X-COORDINATES
C
      X(1)=XG(1)
      DLE=XG(2)-XG(1)
      DO 300 IG=2,NIG-2
      I=2*IG-1
      X(I)=XG(IG)
      DLP=DLE
      DLE=XG(IG+1)-XG(IG)
      FX=SQRT(DLP/DLE)
      FAC=FX/(1.+FX)
      X(I-1)=X(I-2)+DLP*FAC
  300 CONTINUE
      X(I+1)=X(I)+0.5*DLE
      X(NIM)=XG(NIG-1)
      X(NI)=X(NIM)
C
C.....REFINE Y-COORDINATES
C
      Y(1)=YG(1)
      DLN=YG(2)-YG(1)
      DO 400 JG=2,NJG-2
      J=2*JG-1
      Y(J)=YG(JG)
      DLP=DLN
      DLN=YG(JG+1)-YG(JG)
      FY=SQRT(DLP/DLN)
      FAC=FY/(1.+FY)
      Y(J-1)=Y(J-2)+DLP*FAC
  400 CONTINUE
      Y(J+1)=Y(J)+0.5*DLN
      Y(NJM)=YG(NJG-1)
      Y(NJ)=Y(NJM)
C
C.....REFINE Z-COORDINATES
C
      Z(1)=ZG(1)
      DLT=ZG(2)-ZG(1)
      DO 500 KG=2,NKG-2
      K=2*KG-1
      Z(K)=ZG(KG)
      DLP=DLT
      DLT=ZG(KG+1)-ZG(KG)
      FZ=SQRT(DLP/DLT)
      FAC=FZ/(1.+FZ)
      Z(K-1)=Z(K-2)+DLP*FAC
  500 CONTINUE
      Z(K+1)=Z(K)+0.5*DLT
      Z(NKM)=ZG(NKG-1)
      Z(NK)=Z(NKM)
      RETURN
      END
C
C####################################################################
C#                GENERATE NGIT GRIDS                               #
C####################################################################
      SUBROUTINE GRIDM
      PARAMETER (NGIT=4,NICV=8,NJCV=8,NKCV=8,
     *          NX=NICV*2**(NGIT-1)+2,NY=NJCV*2**(NGIT-1)+2,
     *          NZ=NKCV*2**(NGIT-1)+2,
     *          NXY=NX*NY,NXZ=NX*NZ,NYZ=NY*NZ,NXYZ=NXY*NZ,
     *          NXA=NICV*(2**NGIT-1)+2*NGIT,NYA=NJCV*(2**NGIT-1)+2*NGIT,
     *          NZA=NKCV*(2**NGIT-1)+2*NGIT,
     *          NXYA=NICV*NJCV*(4**NGIT-1)/3+2*(NXA+NYA-2*NGIT),
     *          NXZA=NICV*NKCV*(4**NGIT-1)/3+2*(NXA+NZA-2*NGIT),
     *          NYZA=NJCV*NKCV*(4**NGIT-1)/3+2*(NYA+NZA-2*NGIT),
     *          NXYZA=NICV*NJCV*NKCV*(8**NGIT-1)/7+
     *          2*(4**NGIT-1)/3*(NICV*NJCV+NICV*NKCV+NJCV*NKCV)+
     *          4*(NXA+NYA+NZA-4*NGIT))
      COMMON /COOR/ F(NXYZA),X(NXA),Y(NYA),Z(NZA),XG(NXA),
     *       YG(NYA),ZG(NZA),XALL(NXA),YALL(NYA),ZALL(NZA)
      COMMON /PARAM/ NI,NJ,NK,NIM,NJM,NKM,NIGIT(NGIT),NJGIT(NGIT),
     *       NKGIT(NGIT),IGIT(NGIT),JGIT(NGIT),KGIT(NGIT),IJKGIT(NGIT),
     *       KG1
      common /logic/ LOB,LOT,LOS,LON,LOW,LOE
      LOGICAL LOB,LOT,LOS,LON,LOW,LOE
      COMMON /BC/ LBS(NXZA),LBN(NXZA),LBW(NYZA),LBE(NYZA),
     *            LBB(NXYA),LBT(NXYA),
     *            ISBIJ(NGIT),ISBIK(NGIT),ISBJK(NGIT)
C
C.....DEFINE CONTROL PARAMETERS
C
      NIGIT(NGIT)=NI
      NJGIT(NGIT)=NJ
      NKGIT(NGIT)=NK
      DO 230 K=NGIT-1,1,-1
      NIGIT(K)=(NIGIT(K+1)-2)/2+2
      NJGIT(K)=(NJGIT(K+1)-2)/2+2
      NKGIT(K)=(NKGIT(K+1)-2)/2+2
  230 CONTINUE
      IGIT(1)=0
      JGIT(1)=0
      KGIT(1)=0
      IJKGIT(1)=0
      DO 240 K=2,NGIT
      IGIT(K)=IGIT(K-1)+NIGIT(K-1)
      JGIT(K)=JGIT(K-1)+NJGIT(K-1)
      KGIT(K)=KGIT(K-1)+NKGIT(K-1)
  240 IJKGIT(K)=IJKGIT(K-1)+NIGIT(K-1)*NJGIT(K-1)*NKGIT(K-1)
      IALL=IGIT(NGIT)+NI
      JALL=JGIT(NGIT)+NJ
      KALL=KGIT(NGIT)+NK
C
C.....DEFINE GRIDS
C
      DO 270 L=1,NGIT
      ISTEP=2**(NGIT-L)
      IGI=IGIT(L)
      IGJ=JGIT(L)
      IGK=KGIT(L)
      DO 250 I=1,NI,ISTEP
      IGI=IGI+1
      XALL(IGI)=X(I)
  250 CONTINUE
      IF(L.NE.NGIT) XALL(IGI+1)=XALL(IGI)
      DO 260 J=1,NJ,ISTEP
      IGJ=IGJ+1
  260 YALL(IGJ)=Y(J)
      IF(L.NE.NGIT) YALL(IGJ+1)=YALL(IGJ)
C
      DO 265 K=1,NK,ISTEP
      IGK=IGK+1
  265 ZALL(IGK)=Z(K)
      IF(L.NE.NGIT) ZALL(IGK+1)=ZALL(IGK)
  270 CONTINUE
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE  SETB
C##########################################################
      PARAMETER (NGIT=4,NICV=8,NJCV=8,NKCV=8,
     *          NX=NICV*2**(NGIT-1)+2,NY=NJCV*2**(NGIT-1)+2,
     *          NZ=NKCV*2**(NGIT-1)+2,
     *          NXY=NX*NY,NXZ=NX*NZ,NYZ=NY*NZ,NXYZ=NXY*NZ,
     *          NXA=NICV*(2**NGIT-1)+2*NGIT,NYA=NJCV*(2**NGIT-1)+2*NGIT,
     *          NZA=NKCV*(2**NGIT-1)+2*NGIT,
     *          NXYA=NICV*NJCV*(4**NGIT-1)/3+2*(NXA+NYA-2*NGIT),
     *          NXZA=NICV*NKCV*(4**NGIT-1)/3+2*(NXA+NZA-2*NGIT),
     *          NYZA=NJCV*NKCV*(4**NGIT-1)/3+2*(NYA+NZA-2*NGIT),
     *          NXYZA=NICV*NJCV*NKCV*(8**NGIT-1)/7+
     *          2*(4**NGIT-1)/3*(NICV*NJCV+NICV*NKCV+NJCV*NKCV)+
     *          4*(NXA+NYA+NZA-4*NGIT))
      COMMON /COOR/ F(NXYZA),X(NXA),Y(NYA),Z(NZA),XG(NXA),
     *       YG(NYA),ZG(NZA),XALL(NXA),YALL(NYA),ZALL(NZA)
      COMMON /PARAM/ NI,NJ,NK,NIM,NJM,NKM,NIGIT(NGIT),NJGIT(NGIT),
     *       NKGIT(NGIT),IGIT(NGIT),JGIT(NGIT),KGIT(NGIT),IJKGIT(NGIT),
     *       KG1
      common /logic/ LOB,LOT,LOS,LON,LOW,LOE
      LOGICAL LOB,LOT,LOS,LON,LOW,LOE
      COMMON /BC/ LBS(NXZA),LBN(NXZA),LBW(NYZA),LBE(NYZA),
     *            LBB(NXYA),LBT(NXYA),
     *            ISBIJ(NGIT),ISBIK(NGIT),ISBJK(NGIT)
C
C.....OPEN FILE FOR INLET VALUES
      OPEN(7,FILE='INLET')
      REWIND 7
C
      NI=NIGIT(1)
      NJ=NJGIT(1)
      NK=NKGIT(1)
      NIJ=NI*NJ
C
C....SET TYPES OF BOUNDARY CONDITIONS
C    
C.....BOTTOM
      READ(1,*) NBTB
      DO 60 N=1,NBTB
      READ(1,*) LB,IS,IE,JS,JE
      IF(LB.EQ.2) LOB = .TRUE.
C.....IF INLET
      IF(LB.EQ.1) READ(1,*) UIN,VIN,WIN,TIN
      DO 55 I=IS,IE
      II=(I-1)*NJ
      DO 55 J=JS,JE
      IJ=II+J
C.....WRITE IN FILE "INLET"
      IF(LB.EQ.1) WRITE(7,*) UIN,VIN,WIN,TIN,'    BOTTOM',NBTB,'GRID= 1'
   55 LBB(IJ)=LB
   60 CONTINUE
C     
C.....TOP
      READ(1,*) NBTT
      DO 70 N=1,NBTT
      READ(1,*) LB,IS,IE,JS,JE
      IF(LB.EQ.2) LOT=.TRUE.
      DO 65 I=IS,IE
      II=(I-1)*NJ
      DO 65 J=JS,JE
      IJ=II+J
   65 LBT(IJ)=LB
   70 CONTINUE
C     
C.....SOUTH
      READ(1,*) NBTS
      DO 80 N=1,NBTS
      READ(1,*) LB,IS,IE,KS,KE
      IF(LB.EQ.2) LOS=.TRUE.
      DO 75 I=IS,IE
      II=(I-1)*NK
      DO 75 K=KS,KE
      IK=II+K
   75 LBS(IK)=LB
   80 CONTINUE
C     
C.....NORTH
      READ(1,*) NBTN
      DO 90 N=1,NBTN
      READ(1,*) LB,IS,IE,KS,KE
      IF(LB.EQ.2) LON=.TRUE.
      DO 85 I=IS,IE
      II=(I-1)*NK
      DO 85 K=KS,KE
      IK=II+K
   85 LBN(IK)=LB
   90 CONTINUE
C     
C.....WEST
      READ(1,*) NBTW
      DO 100 N=1,NBTW
      READ(1,*) LB,JS,JE,KS,KE
      IF(LB.EQ.2) LOW=.TRUE.
      DO 95 J=JS,JE
      JJ=(J-1)*NK
      DO 95 K=KS,KE
      JK=JJ+K
   95 LBW(JK)=LB
  100 CONTINUE
C
C.....EAST
      READ(1,*) NBTE
      DO 110 N=1,NBTE
      READ(1,*) LB,JS,JE,KS,KE
      IF(LB.EQ.2) LOE=.TRUE.
      DO 105 J=JS,JE
      JJ=(J-1)*NK
      DO 105 K=KS,KE
      JK=JJ+K
  105 LBE(JK)=LB
  110 CONTINUE
C.....
      ISBIJ(1)=0
      ISBIK(1)=0
      ISBJK(1)=0
      DO 150 MGR=2,NGIT
      NI=NIGIT(MGR-1)
      NJ=NJGIT(MGR-1)
      NK=NKGIT(MGR-1)
C.....
      NIJ=NI*NJ
      NIK=NI*NK
      NJK=NJ*NK
C.....
      ISBIJ(MGR)=ISBIJ(MGR-1)+NIJ 
      ISBIK(MGR)=ISBIK(MGR-1)+NIK 
      ISBJK(MGR)=ISBJK(MGR-1)+NJK 
  150 CONTINUE
C
C.....DEFINE BOUNDARY TYPES
      DO 200 MGR=1,NGIT-1
C.....
      NI=NIGIT(MGR)
      NJ=NJGIT(MGR)
      NK=NKGIT(MGR)
      ISTIJG=ISBIJ(MGR)
      ISTIKG=ISBIK(MGR)
      ISTJKG=ISBJK(MGR)
      ISTIJF=ISBIJ(MGR+1)
      ISTIKF=ISBIK(MGR+1)
      ISTJKF=ISBJK(MGR+1)
      CALL SETGF(LBB,NI,NJ,ISTIJG,ISTIJF)
      CALL SETGF(LBT,NI,NJ,ISTIJG,ISTIJF)
      CALL SETGF(LBS,NI,NK,ISTIKG,ISTIKF)
      CALL SETGF(LBN,NI,NK,ISTIKG,ISTIKF)
      CALL SETGF(LBW,NJ,NK,ISTJKG,ISTJKF)
      CALL SETGF(LBE,NJ,NK,ISTJKG,ISTJKF)
 200  CONTINUE
      RETURN
      END
      SUBROUTINE SETGF(LFI,NIG,NJG,ISTG,ISTF)
      DIMENSION LFI(1)
C
      NJF=(NJG-2)*2+2
      NIMG=NIG-1
      NJMG=NJG-1
      DO 10 I=2,NIMG
      II=(I-1)*NJG
      IF=I*2-2
      IIF=(IF-1)*NJF
      DO 10 J=2,NJMG
      IJG=ISTG+II+J
      IJF=ISTF+IIF+2*J-2
      LFI(IJF)=LFI(IJG)
      LFI(IJF+1)=LFI(IJG)
      LFI(IJF+NJF)=LFI(IJG)
      LFI(IJF+NJF+1)=LFI(IJG)
   10 CONTINUE
      RETURN
      END

*   DSMC0F.FOR
*
      PROGRAM DSMC0F
*
*--for the study of the fluctuations in a uniform simple gas
*
*--SI units are used throughout
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
*--MNM  is the maximum number of molecules
*--MNC  is the maximum number of cells
*--MNSC is the maximum number of sub-cells
*--MTC the number of forward and backward steps in the temporal correl.
*--MXC the number of forward and backward steps in the temporal correl.
*--MAXD	is the maximum possible (say 4 standard deviations) departure
*----of the number of molecules in a cell from the mean number per cell
*----(FNUM should be chosen such that the mean per cell is an integer)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
*--variables as in DSMC0S.FOR
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
*
*--variables as in DSMC0S.FOR
*
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
*
*--variables as in DSMC0S.FOR
*
      COMMON /GASS  / SP(2),SPM(5)
*
*--variables as in DSMC0S.FOR
*
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP,VMP
*
*--variables as in DSMC0S.FOR
*
      COMMON /SAMPF / CVT(5,-MTC:MTC),CSX(5,-MXC:MXC),CST(6,-MTC:MTC),
     &                NSMPX,NSMPT,CNN(-MAXD:MAXD),NSMPN,AVM,AVT,AVU,AVV,
     &                AVW,CSF(4,-MXC:MXC),NCO
*
*--CVT(N,-MTC:MTC) cell NCO values at 2*MTC+1 successive time steps
*----the current value in this cell is in MTC+1
*----N=1 the number
*----N=2 the x velocity component
*----N=3 the y velocity component
*----N=4 the z velocity component
*----N=5 the temperature
*--CSX(N,M) the sum of q1(NCO)*q2(M) where M is the cell number
*----N=1 q1=q2=dN for the <dn dn> spatial correlation
*----N=2 q1=q2=du for the <du du> spatial correlation
*----N=3 q1=q2=dv for the <dv dv> spatial correlation
*----N=4 q1=du, q2=dv for the <du dv> spatial correlation
*----N=5 q1=q2=dT for the <dT dT> spatial correlation
*--CST(N,M) the sum of q1(NCO)*q2(M) where M is the time step
*----N=1 q1=q2=dN for the <dn dn> temporal correlation
*----N=2 q1=q2=du for the <du du> temporal correlation
*----N=3 q1=q2=dv for the <dv dv> temporal correlation
*----N=4 q1=q2=dw for the <dw dw> temporal correlation
*----N=5 q1=q2=dT for the <dT dT> temporal correlation
*----N=6 q1=du, q2=dv for the <du dv> temporal correlation
*--NSMPX the number of samples for the spatial correlation and dist.
*--NSMPT the number of samples for the temporal correlations
*--CNN the number in the density fluctuation interval
*--CSF(N,M) sum of the squares of the macroscopic quantities
*----N=1 density
*----N=2 u velocity component
*----N=3 v velocity component
*----N=4 w velocity component
*--NCO the cell on which the fluctuation sample is centred
*
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
*
*--variables as defined in DSMC0.FOR
*
      COMMON /GEOM  / CW,NSC,XF,XR
*
*--variables as defined in DSMC0.FOR
*
      COMMON /CONST / PI,SPI,BOLTZ
*
*--variables as defined in DSMC0.FOR
*
      WRITE (*,*) ' INPUT 0,1 FOR CONTINUING,NEW CALCULATION:- '
      READ (*,*) NQL
*
      IF (NQL.EQ.1) THEN
*
        CALL INIT0F
*
      ELSE
*
        WRITE (*,*) ' READ THE RESTART FILE'
        OPEN (4,FILE='DSMC0F.RES',STATUS='OLD',FORM='UNFORMATTED')
        READ (4) AVM,AVT,AVU,AVV,AVW,BOLTZ,CC,CCG,CG,CNN,COL,CS,CSF,CST,
     &           CSX,CVT,CW,DTM,FND,FNUM,FTMP,IC,IP,IR,ISC,ISCG,MOVT,
     &           NCO,NCOL,NIS,NM,NSC,NSMP,NSMPN,NSMPT,NSMPX,NPR,NPT,NSP,
     &           PI,PP,PV,SELT,SEPT,SP,SPI,SPM,TIME,VMP,XF,XR
        CLOSE (4)
*
      END IF
*
      IF (NQL.EQ.1) CALL SAMPI0F
*
      IF (NQL.EQ.1) CALL SAMPIF
*
100   NPR=NPR+1
*
      DO 200 JJJ=1,NSP
        DO 150 III=1,NIS
          TIME=TIME+DTM
*
          WRITE (*,99001) III,JJJ,NIS,NSP,NCOL
99001   FORMAT (' DSMC0F:- Move ',2I5,'   of ',2I5,F14.0,' Collisions')
*
          CALL MOVE0F
*
          CALL INDEXS
*
          CALL COLLS
*
          CALL SAMPFC
*
150     CONTINUE
*
        CALL SAMPLE0F
*
200   CONTINUE
*
      WRITE (*,*) ' WRITING RESTART AND OUTPUT FILES',NPR,'  OF ',NPT
      OPEN (4,FILE='DSMC0F.RES',FORM='UNFORMATTED')
      WRITE (4) AVM,AVT,AVU,AVV,AVW,BOLTZ,CC,CCG,CG,CNN,COL,CS,CSF,CST,
     &          CSX,CVT,CW,DTM,FND,FNUM,FTMP,IC,IP,IR,ISC,ISCG,MOVT,NCO,
     &          NCOL,NIS,NM,NSC,NSMP,NSMPN,NSMPT,NSMPX,NPR,NPT,NSP,PI,
     &          PP,PV,SELT,SEPT,SP,SPI,SPM,TIME,VMP,XF,XR
      CLOSE (4)
*
      CALL OUT0F
*
      IF (NPR.LT.NPT) GO TO 100
      STOP
      END
*   INIT0F.FOR
*
      SUBROUTINE INIT0F
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /GASS  / SP(2),SPM(5)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP,VMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
      COMMON /CONST / PI,SPI,BOLTZ
      COMMON /SAMPF / CVT(5,-MTC:MTC),CSX(5,-20:MXC),CST(6,-MTC:MTC),
     &                NSMPX,NSMPT,CNN(-MAXD:MAXD),NSMPN,AVM,AVT,AVU,AVV,
     &                AVW,CSF(4,-MXC:MXC),NCO
*
*--set constants
*
      PI=3.141592654
      SPI=SQRT(PI)
      BOLTZ=1.3806E-23
*
      CALL DATA0F
*
*--set additional data on the gas
*
      SPM(1)=PI*SP(2)**2
*--the collision cross section is given by eqn (1.8)
      SPM(5)=GAM(2.5-SPM(3))
*
*--Initialise variables
*
      TIME=0.
      NM=0
*
      CG(1,1)=XF
      CW=(XR-XF)/MNC
      DO 100 M=1,MNC
        IF (M.GT.1) CG(1,M)=CG(2,M-1)
        CG(2,M)=CG(1,M)+CW
        CG(3,M)=CW
        CC(M)=CW
        CCG(2,M)=RF(0)
        CCG(1,M)=SPM(1)*300.*SQRT(FTMP/300.)
*--the maximum value of the (rel. speed)*(cross-section) is set to a
*--reasonable, but low, initial value and will be increased as necessary
100   CONTINUE
*
*--set sub-cells
*
      DO 200 N=1,MNC
        DO 150 M=1,NSC
          L=(N-1)*NSC+M
          ISC(L)=N
150     CONTINUE
200   CONTINUE
*
*--generate initial gas in equilibrium at temperature FTMP
*
      SU=0.
      SV=0.
      SW=0.
      SQ=0.
      REM=0
      VMP=SQRT(2.*BOLTZ*FTMP/SP(1))
*--VMP is the most probable molecular speed, see eqns (4.1) and (4.7)
      DO 300 N=1,MNC
        A=FND*CG(3,N)/FNUM+REM
*--A is the number of simulated molecules in cell N
        IF (N.LT.MNC) THEN
          MM=A
          REM=(A-MM)
*--the remainder REM is carried forward to the next cell
        ELSE
          MM=NINT(A)
        END IF
        DO 250 M=1,MM
          IF (NM.LE.MNM) THEN
*--round-off error could have taken NM to MNM+1
            NM=NM+1
            PP(NM)=CG(1,N)+RF(0)*(CG(2,N)-CG(1,N))
            IP(NM)=(PP(NM)-CG(1,N))*(NSC-.001)/CG(3,N)+1+NSC*(N-1)
*--species, position, and sub-cell number have been set
            DO 210 K=1,3
              CALL RVELC(PV(K,NM),A,VMP)
210         CONTINUE
            SU=SU+PV(1,NM)
            SV=SV+PV(2,NM)
            SW=SW+PV(3,NM)
            SQ=SQ+PV(1,NM)**2+PV(2,NM)**2+PV(3,NM)**2
*--velocity components have been set
          END IF
250     CONTINUE
300   CONTINUE
      WRITE (*,99001) NM
99001 FORMAT (' ',I6,' MOLECULES')
      AVM=FLOAT(NM)/FLOAT(MNC)
      AVU=SU/FLOAT(NM)
      AVV=SV/FLOAT(NM)
      AVW=SW/FLOAT(NM)
      AVT=(0.33333*SP(1)/BOLTZ)*(SQ/FLOAT(NM)-AVU**2-AVV**2-AVW**2)
      WRITE (*,*) 'AV N,U,V,W,T',AVM,AVU,AVV,AVW,AVT
*
      RETURN
      END
*   SAMPI0F.FOR
*
      SUBROUTINE SAMPI0F
*
*--initialises all the sampling variables
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP,VMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
*
      NPR=0
      NCOL=0
      NSMP=0
      MOVT=0.
      SELT=0.
      SEPT=0.
      DO 100 N=1,MNC
        CS(1,N)=1.E-6
        DO 50 M=2,5
          CS(M,N)=0.
50      CONTINUE
100   CONTINUE
      RETURN
      END
*   SAMPIF.FOR
*
      SUBROUTINE SAMPIF
*
*--initialise the fluctuation and correlation sampling variables
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      COMMON /SAMPF / CVT(5,-MTC:MTC),CSX(5,-MXC:MXC),CST(6,-MTC:MTC),
     &                NSMPX,NSMPT,CNN(-MAXD:MAXD),NSMPN,AVM,AVT,AVU,AVV,
     &                AVW,CSF(4,-MXC:MXC),NCO
      NSMPX=0
      NSMPT=0
      DO 100 N=1,5
        DO 50 M=-MTC,MTC
          CVT(N,M)=0.
50      CONTINUE
100   CONTINUE
      DO 300 N=1,5
        DO 150 M=-MTC,MTC
          CST(N,M)=0.
150     CONTINUE
        DO 200 M=-MXC,MXC
          CSX(N,M)=0.
200     CONTINUE
300   CONTINUE
      DO 400 N=-MAXD,MAXD
        CNN(N)=0.
400   CONTINUE
      DO 500 N=-MXC,MXC
        DO 450 M=1,4
          CSF(M,N)=0.
450     CONTINUE
500   CONTINUE
      RETURN
      END
*   SAMPFC.FOR
*
      SUBROUTINE SAMPFC
*
*--sample the fluctuation and correlation information
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP,VMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
      COMMON /GASS  / SP(2),SPM(5)
      COMMON /CONST / PI,SPI,BOLTZ
*
      COMMON /SAMPF / CVT(5,-MTC:MTC),CSX(5,-MXC:MXC),CST(6,-MTC:MTC),
     &                NSMPX,NSMPT,CNN(-MAXD:MAXD),NSMPN,AVM,AVT,AVU,AVV,
     &                AVW,CSF(4,-MXC:MXC),NCO
*
      DIMENSION CVX(5,-MXC:MXC)
*
*--sample the distribution of number density
      NAVM=NINT(AVM)
      DO 100 M=1,MNC
        NMC=IC(2,M)
        N=NMC-NAVM
        IF (ABS(N).LE.MAXD) CNN(N)=CNN(N)+1
100   CONTINUE
      NSMPN=NSMPN+1
*--push time samples down the array
      DO 200 N=1,5
        DO 150 M=-MTC,MTC-1
          CVT(N,M)=CVT(N,M+1)
150     CONTINUE
200   CONTINUE
*
*--sample the current properties in cell NCO
      SU=0.
      SV=0.
      SW=0.
      SQ=0.
      NMC=IC(2,NCO)
      DO 300 N=1,NMC
        L=IC(1,NCO)+N
        M=IR(L)
        SU=SU+PV(1,M)/FLOAT(NMC)
        SV=SV+PV(2,M)/FLOAT(NMC)
        SW=SW+PV(3,M)/FLOAT(NMC)
        SQ=SQ+(PV(1,M)**2+PV(2,M)**2+PV(3,M)**2)/FLOAT(NMC)
300   CONTINUE
      CVT(1,MTC)=NMC
      CVT(2,MTC)=SU
      CVT(3,MTC)=SV
      CVT(4,MTC)=SW
      CVT(5,MTC)=(0.33333*SP(1)/BOLTZ)*(SQ-SU**2-SV**2-SW**2)
      IF (CVT(1,-MTC).GT.0.1) THEN
        NSMPT=NSMPT+1
        DO 350 N=-MTC,MTC
          CST(1,N)=CST(1,N)+(CVT(1,0)-AVM)*(CVT(1,N)-AVM)
          CST(2,N)=CST(2,N)+(CVT(2,0)-AVU)*(CVT(2,N)-AVU)
          CST(3,N)=CST(3,N)+(CVT(3,0)-AVV)*(CVT(3,N)-AVV)
          CST(4,N)=CST(4,N)+(CVT(4,0)-AVW)*(CVT(4,N)-AVW)
          CST(5,N)=CST(5,N)+(CVT(5,0)-AVT)*(CVT(5,N)-AVT)
          CST(6,N)=CST(6,N)+(CVT(2,0)-AVU)*(CVT(3,N)-AVV)
350     CONTINUE
      END IF
      DO 500 K=-MXC,MXC
        KC=NCO+K
        NMC=IC(2,KC)
        CVX(1,K)=NMC
        CVX(2,K)=0.
        CVX(3,K)=0.
        CVX(4,K)=0.
        CVX(5,K)=0.
        DO 400 N=1,NMC
          L=IC(1,KC)+N
          M=IR(L)
          CVX(2,K)=CVX(2,K)+PV(1,M)/FLOAT(NMC)
          CVX(3,K)=CVX(3,K)+PV(2,M)/FLOAT(NMC)
          CVX(4,K)=CVX(4,K)+PV(3,M)/FLOAT(NMC)
400     CONTINUE
        DO 450 N=1,NMC
          L=IC(1,KC)+N
          M=IR(L)
          CVX(5,K)=CVX(5,K)+((PV(1,M)-CVX(2,K))**2+(PV(2,M)-CVX(3,K))
     &             **2+(PV(3,M)-CVX(4,K))**2)/FLOAT(NMC)
450     CONTINUE
500   CONTINUE
      DO 600 K=-MXC,MXC
        CVX(5,K)=(0.33333*SP(1)/BOLTZ)*CVX(5,K)
        DO 550 N=1,4
          CSF(N,K)=CSF(N,K)+CVX(N,K)**2
550     CONTINUE
600   CONTINUE
      NSMPX=NSMPX+1
      DO 700 N=-MXC,MXC
        CSX(1,N)=CSX(1,N)+(CVX(1,0)-AVM)*(CVX(1,N)-AVM)
        CSX(2,N)=CSX(2,N)+(CVX(2,0)-AVU)*(CVX(2,N)-AVU)
        CSX(3,N)=CSX(3,N)+(CVX(3,0)-AVV)*(CVX(3,N)-AVV)
        CSX(4,N)=CSX(4,N)+(CVX(2,0)-AVU)*(CVX(3,N)-AVV)
        CSX(5,N)=CSX(5,N)+(CVX(5,0)-AVT)*(CVX(5,N)-AVT)
700   CONTINUE
      RETURN
      END
*   MOVE0F.FOR
*
*
      SUBROUTINE MOVE0F
*
*--the NM molecules are moved over the time interval DTM
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP,VMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
*
      DO 100 N=1,NM
        MOVT=MOVT+1
        MSC=IP(N)
        MC=ISC(MSC)
*--MC is the initial cell number
        XI=PP(N)
        DX=PV(1,N)*DTM
        X=XI+DX
*--molecule N at XI is moved by DX to X
        IF (X.LT.XF) THEN
*--specular reflection from the minimum x boundary at x=XF (eqn (11.7))
          X=2.*XF-X
          PV(1,N)=-PV(1,N)
        END IF
        IF (X.GT.XR) THEN
*--specular reflection from the maximum x boundary at x=XR (eqn (11.7))
          X=2.*XR-X
          PV(1,N)=-PV(1,N)
        END IF
        IF (X.LT.CG(1,MC).OR.X.GT.CG(2,MC)) THEN
*--the molecule has moved from the initial cell
          MC=(X-XF)/CW+0.99999
          IF (MC.EQ.0) MC=1
*--MC is the new cell number (note avoidance of round-off error)
        END IF
        MSC=((X-CG(1,MC))/CG(3,MC))*(NSC-.001)+1+NSC*(MC-1)
*--MSC is the new sub-cell number
        IP(N)=MSC
        PP(N)=X
100   CONTINUE
      RETURN
      END
*   INDEXS.FOR
*
      SUBROUTINE INDEXS
*
*--the NM molecule numbers are arranged in order of the cells and,
*--within the cells, in order of the sub-cells
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /GASS  / SP(2),SPM(5)
*
      DO 100 NN=1,MNC
        IC(2,NN)=0
100   CONTINUE
      DO 200 NN=1,MNSC
        ISCG(2,NN)=0
200   CONTINUE
      DO 300 N=1,NM
        MSC=IP(N)
        ISCG(2,MSC)=ISCG(2,MSC)+1
        MC=ISC(MSC)
        IC(2,MC)=IC(2,MC)+1
300   CONTINUE
*--numbers in the cells and sub-cells have been counted
      M=0
      DO 400 N=1,MNC
        IC(1,N)=M
        M=M+IC(2,N)
400   CONTINUE
*--the (start address -1) has been set for the cells
      M=0
      DO 500 N=1,MNSC
        ISCG(1,N)=M
        M=M+ISCG(2,N)
        ISCG(2,N)=0
500   CONTINUE
*--the (start address -1) has been set for the sub-cells
      DO 600 N=1,NM
        MSC=IP(N)
        ISCG(2,MSC)=ISCG(2,MSC)+1
        K=ISCG(1,MSC)+ISCG(2,MSC)
        IR(K)=N
*--the molecule number N has been set in the cross-reference array
600   CONTINUE
      RETURN
      END
*   COLLS.FOR
*
      SUBROUTINE COLLS
*
*--calculates collisions appropriate to DTM in a monatomic gas
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /GASS  / SP(2),SPM(5)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP,VMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
      COMMON /CONST / PI,SPI,BOLTZ
*
      DIMENSION VRC(3),VRCP(3),VCCM(3)
*--VRC(3) are the pre-collision components of the relative velocity
*--VRP(3) are the post-collision components of the relative velocity
*--VCCM(3) are the components of the centre of mass velocity
*
      DO 100 N=1,MNC
*--consider collisions in cell N
        SN=CS(1,N)
        IF (SN.GT.1.) THEN
          AVN=SN/FLOAT(NSMP)
        ELSE
          AVN=IC(2,N)
        END IF
*--AVN is the average number of group MM molecules in the cell
        ASEL=0.5*IC(2,N)*AVN*FNUM*CCG(1,N)*DTM/CC(N)+CCG(2,N)
*--ASEL is the number of pairs to be selected, see eqn (11.3)
        NSEL=ASEL
        CCG(2,N)=ASEL-NSEL
        IF (NSEL.GT.0) THEN
          IF (IC(2,N).LT.2) THEN
            CCG(2,N)=CCG(2,N)+NSEL
*--if there are insufficient molecules to calculate collisions,
*--the number NSEL is added to the remainer CCG(2,N)
          ELSE
            CVM=CCG(1,N)
            SELT=SELT+NSEL
            DO 20 ISEL=1,NSEL
              K=INT(RF(0)*(IC(2,N)-0.0001))+IC(1,N)+1
              L=IR(K)
*--the first mol. L has been chosen at random from group NN in cell N
5             MSC=IP(L)
              IF (ISCG(2,MSC).EQ.1) THEN
*--if MSC has only the chosen mol., find the nearest sub-cell with one
                NST=1
                NSG=1
6               INC=NSG*NST
                NSG=-NSG
                NST=NST+1
                MSC=MSC+INC
                IF (MSC.LT.1.OR.MSC.GT.MNSC) GO TO 6
                IF (ISC(MSC).NE.N.OR.ISCG(2,MSC).LT.1) GO TO 6
              END IF
*--the second molecule M is now chosen at random from the
*--molecules that are in the sub-cell MSC
              K=INT(RF(0)*(ISCG(2,MSC)-0.0001))+ISCG(1,MSC)+1
              M=IR(K)
              IF (L.EQ.M) GO TO 5
*--choose a new second molecule if the first is again chosen
*
              DO 10 K=1,3
                VRC(K)=PV(K,L)-PV(K,M)
10            CONTINUE
*--VRC(1 to 3) are the components of the relative velocity
              VRR=VRC(1)**2+VRC(2)**2+VRC(3)**2
              VR=SQRT(VRR)
*--VR is the relative speed
              CVR=VR*SPM(1)
     &            *((2.*BOLTZ*SPM(2)/(0.5*SP(1)*VRR))**(SPM(3)-0.5))
     &            /SPM(5)
*--the collision cross-section is based on eqn (4.63)
              IF (CVR.GT.CVM) CVM=CVR
*--if necessary, the maximum product in CVM is upgraded
              IF (RF(0).LT.CVR/CCG(1,N)) THEN
*--the collision is accepted with the probability of eqn (11.4)
                DO 12 K=1,3
                  VCCM(K)=0.5*(PV(K,L)+PV(K,M))
12              CONTINUE
*--VCCM defines the components of the centre-of-mass velocity (eqn 2.1)
                NCOL=NCOL+1
                SEPT=SEPT+ABS(PP(L)-PP(M))
                IF (ABS(SPM(4)-1.).LT.1.E-3) THEN
*--use the VHS logic
                  B=2.*RF(0)-1.
*--B is the cosine of a random elevation angle
                  A=SQRT(1.-B*B)
                  VRCP(1)=B*VR
                  C=2.*PI*RF(0)
*--C is a random azimuth angle
                  VRCP(2)=A*COS(C)*VR
                  VRCP(3)=A*SIN(C)*VR
                ELSE
*--use the VSS logic
                  B=2.*(RF(0)**SPM(4))-1.
*--B is the cosine of the deflection angle for the VSS model (eqn (11.8)
                  A=SQRT(1.-B*B)
                  C=2.*PI*RF(0)
                  OC=COS(C)
                  SC=SIN(C)
                  D=SQRT(VRC(2)**2+VRC(3)**2)
                  IF (D.GT.1.E-6) THEN
                    VRCP(1)=B*VRC(1)+A*SC*D
                    VRCP(2)=B*VRC(2)+A*(VR*VRC(3)*OC-VRC(1)*VRC(2)*SC)/D
                    VRCP(3)=B*VRC(3)-A*(VR*VRC(2)*OC+VRC(1)*VRC(3)*SC)/D
                  ELSE
                    VRCP(1)=B*VRC(1)
                    VRCP(2)=A*OC*VRC(1)
                    VRCP(3)=A*SC*VRC(1)
                  END IF
*--the post-collision rel. velocity components are based on eqn (2.22)
                END IF
*--VRCP(1 to 3) are the components of the post-collision relative vel.
                DO 14 K=1,3
                  PV(K,L)=VCCM(K)+0.5*VRCP(K)
                  PV(K,M)=VCCM(K)-0.5*VRCP(K)
14              CONTINUE
              END IF
20          CONTINUE
            CCG(1,N)=CVM
          END IF
        END IF
100   CONTINUE
      RETURN
      END
*   SAMPLE0F.FOR
*
*
      SUBROUTINE SAMPLE0F
*
*--sample the molecules in the flow.
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP,VMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
*
      NSMP=NSMP+1
      DO 100 N=1,MNC
        L=IC(2,N)
        IF (L.GT.0) THEN
          DO 20 J=1,L
            K=IC(1,N)+J
            M=IR(K)
            CS(1,N)=CS(1,N)+1
            DO 10 LL=1,3
              CS(LL+1,N)=CS(LL+1,N)+PV(LL,M)
              CS(5,N)=CS(5,N)+PV(LL,M)**2
10          CONTINUE
20        CONTINUE
        END IF
100   CONTINUE
      RETURN
      END
*   OUT0F.FOR
*
      SUBROUTINE OUT0F
*
*--output a progressive set of results to file DSMC0F.OUT.
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT,FND2
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /GEOM  / CW,NSC,XF,XR
      COMMON /GASS  / SP(2),SPM(5)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP,VMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
      COMMON /CONST / PI,SPI,BOLTZ
      COMMON /SAMPF / CVT(5,-MTC:MTC),CSX(5,-MXC:MXC),CST(6,-MTC:MTC),
     &                NSMPX,NSMPT,CNN(-MAXD:MAXD),NSMPN,AVM,AVT,AVU,AVV,
     &                AVW,CSF(4,-MXC:MXC),NCO
*
      DIMENSION VEL(3)
*
      OPEN (4,FILE='DSMC0F.OUT',FORM='FORMATTED')
*
      WRITE (4,*) ' FROM ZERO TIME TO TIME',TIME
      WRITE (4,*) ' COLLISIONS =',NCOL
      WRITE (4,*) ' TOTAL NUMBER OF SAMPLES ',NSMP
      WRITE (4,*) NM,' MOLECULES'
      WRITE (4,*) MOVT,' TOTAL MOLECULAR MOVES'
      IF (SELT.GT.0.1) WRITE (4,*) INT(SELT),' SELECTIONS ',INT(NCOL),
     &                             ' COLLISIONS, RATIO  ',
     &                             REAL(NCOL/SELT)
      IF (NCOL.GT.0) WRITE (4,*) ' MEAN COLLISION SEPARATION ',
     &                           REAL(SEPT/NCOL)
*
      WRITE (4,*) ' FLOWFIELD PROPERTIES'
      WRITE (4,*) 
     & '  CELL   X COORD   SAMPLE     N DENS.         U         V       
     & W     TEMP'
      TOT=0.
      DO 100 N=1,MNC
        A=FNUM/(CG(3,N)*NSMP)
        DENN=CS(1,N)*A
*--DENN is the number density
        IF (CS(1,N).GT.0.5) THEN
          DO 20 K=1,3
            VEL(K)=CS(K+1,N)/CS(1,N)
20        CONTINUE
*--VEL is the stream velocity components, see eqn (1.21)
          UU=VEL(1)**2+VEL(2)**2+VEL(3)**2
          TT=SP(1)*(CS(5,N)/CS(1,N)-UU)/(3.*BOLTZ)
*--TT is the temperature, see eqn (1.29a)
          TOT=TOT+TT
          XC=0.5*(CG(1,N)+CG(2,N))
*--XC is the x coordinate of the midpoint of the cell
          WRITE (4,99001) N,XC,INT(CS(1,N)),DENN,VEL(1),VEL(2),VEL(3),TT
        END IF
99001   FORMAT (' ',I5,F10.4,I9,1P,E12.4,0P,4F10.4)
100   CONTINUE
*
*
*--compare with theoretical collision number (actual temperarure)
      AVTMP=TOT/MNC
      WRITE (4,*) ' AVERAGE TEMPERATURE ',AVTMP
      WRITE (4,*)
      WRITE (4,*) ' RATIO OF COLLISION NUMBER TO THEORETICAL VALUE'
      WRITE (4,*)
      FND2=FND
      TCOL=2.*TIME*FND2*FND2*(XR-XF)*SPM(1)
     &     *((AVTMP/SPM(2))**(1.-SPM(3)))*SQRT(BOLTZ*SPM(2)/(PI*SP(1)))
     &     /FNUM
*--TCOL is the equilibrium collision rate, see eqn (4.64)
      WRITE (4,*) NCOL/TCOL
*
      WRITE (4,*) ' MEAN SQUARE FLUCTUATIONS '
      WRITE (4,*) ' CELL     <dn.dn>     <du.du>     <dv.dv>    <dw.dw>'
      DO 200 N=-MXC,MXC
        NC=N+NCO
        A=CS(1,NC)/FLOAT(NSMP)
        ANNC=(CSF(1,N)/FLOAT(NSMPX)-A*A)/A
        DO 150 K=1,3
          VEL(K)=CS(K+1,NC)/CS(1,NC)
150     CONTINUE
        AUUC=(CSF(2,N)/FLOAT(NSMPX)-VEL(1)**2)*A/VMP**2
        AVVC=(CSF(3,N)/FLOAT(NSMPX)-VEL(2)**2)*A/VMP**2
        AWWC=(CSF(4,N)/FLOAT(NSMPX)-VEL(3)**2)*A/VMP**2
        WRITE (4,99002) N,ANNC,AUUC,AVVC,AWWC
99002   FORMAT (I6,4E13.5)
200   CONTINUE
*
      WRITE (4,*) ' NUMBER DISTRIBUTION IN CELLS '
      DO 300 N=-MAXD,MAXD
        WRITE (4,*) AVM+N,CNN(N)/(NSMPN*MNC)
300   CONTINUE
*
      WRITE (4,*) ' TEMPORAL CORRELATION FUNCTION IN CELL ',NCO
 
      DO 400 N=-MTC,MTC
        WRITE (4,99003) N,CST(1,N)/(AVM*NSMPT),CST(2,N)
     &                  *AVM/(NSMPT*VMP**2),CST(3,N)*AVM/(NSMPT*VMP**2),
     &                  CST(4,N)*AVM/(NSMPT*VMP**2),CST(5,N)
     &                  *AVM/(NSMPT*AVT**2),CST(6,N)*AVM/(NSMPT*VMP**2)
400   CONTINUE
*
      WRITE (4,*) ' SPATIAL CORRELATION FUNCTION IN CELL ',NCO
      DO 500 N=-MXC,MXC
        WRITE (4,99003) N,CSX(1,N)/(AVM*NSMPX),CSX(2,N)
     &                  *AVM/(NSMPX*VMP**2),CSX(3,N)*AVM/(NSMPX*VMP**2),
     &                  CSX(4,N)*AVM/(NSMPX*VMP**2),CSX(5,N)
     &                  *AVM/(NSMPX*AVT**2)
500   CONTINUE
*
99003 FORMAT (I8,6E13.5)
      CLOSE (4)
*
      SU=0.
      SV=0.
      SW=0.
      SQ=0.
      DO 600 N=1,NM
        SU=SU+PV(1,N)
        SV=SV+PV(2,N)
        SW=SW+PV(3,N)
        SQ=SQ+PV(1,N)**2+PV(2,N)**2+PV(3,N)**2
600   CONTINUE
      AVU=SU/FLOAT(NM)
      AVV=SV/FLOAT(NM)
      AVW=SW/FLOAT(NM)
      AVT=(0.33333*SP(1)/BOLTZ)*(SQ/FLOAT(NM)-AVU**2-AVV**2-AVW**2)
      WRITE (*,*) 'AV N,U,V,W,T',AVM,AVU,AVV,AVW,AVT
      RETURN
      END
*   RVELC.FOR
*
*--end of listing
*
*
      SUBROUTINE RVELC(U,V,VMP)
*
*--generates two random velocity components U an V in an equilibrium
*--gas with most probable speed VMP  (based on eqns (C10) and (C12))
*
      A=SQRT(-LOG(RF(0)))
      B=6.283185308*RF(0)
      U=A*SIN(B)*VMP
      V=A*COS(B)*VMP
      RETURN
      END
*   GAM.FOR
*
      FUNCTION GAM(X)
*
*--calculates the Gamma function of X.
*
      A=1.
      Y=X
      IF (Y.LT.1.) THEN
        A=A/Y
      ELSE
50      Y=Y-1
        IF (Y.GE.1.) THEN
          A=A*Y
          GO TO 50
        END IF
      END IF
      GAM=A*(1.-0.5748646*Y+0.9512363*Y**2-0.6998588*Y**3+
     &    0.4245549*Y**4-0.1010678*Y**5)
      RETURN
      END
*   RF.FOR
*
      FUNCTION RF(IDUM)
*--generates a uniformly distributed random fraction between 0 and 1
*----IDUM will generally be 0, but negative values may be used to
*------re-initialize the seed
      SAVE MA,INEXT,INEXTP
      PARAMETER (MBIG=1000000000,MSEED=161803398,MZ=0,FAC=1.E-9)
      DIMENSION MA(55)
      DATA IFF/0/
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
        IFF=1
        MJ=MSEED-IABS(IDUM)
        MJ=MOD(MJ,MBIG)
        MA(55)=MJ
        MK=1
        DO 50 I=1,54
          II=MOD(21*I,55)
          MA(II)=MK
          MK=MJ-MK
          IF (MK.LT.MZ) MK=MK+MBIG
          MJ=MA(II)
50      CONTINUE
        DO 100 K=1,4
          DO 60 I=1,55
            MA(I)=MA(I)-MA(1+MOD(I+30,55))
            IF (MA(I).LT.MZ) MA(I)=MA(I)+MBIG
60        CONTINUE
100     CONTINUE
        INEXT=0
        INEXTP=31
      END IF
200   INEXT=INEXT+1
      IF (INEXT.EQ.56) INEXT=1
      INEXTP=INEXTP+1
      IF (INEXTP.EQ.56) INEXTP=1
      MJ=MA(INEXT)-MA(INEXTP)
      IF (MJ.LT.MZ) MJ=MJ+MBIG
      MA(INEXT)=MJ
      RF=MJ*FAC
      IF (RF.GT.1.E-8.AND.RF.LT.0.99999999) RETURN
      GO TO 200
      END
*   DATA0F.FOR
*
      SUBROUTINE DATA0F
*
*--defines the data for a particular run of DSMC0F.FOR
*
      PARAMETER (MNM=4100,MNC=41,MNSC=328,MTC=20,MXC=20,MAXD=50)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /GASS  / SP(2),SPM(5)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP,VMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
      COMMON /SAMPF / CVT(5,-MTC:MTC),CSX(5,-MXC:MXC),CST(6,-MTC:MTC),
     &                NSMPX,NSMPT,CNN(-MAXD:MAXD),NSMPN,AVM,AVT,AVU,AVV,
     &                AVW,CSF(4,-MXC:MXC),NCO
*
*--set data (must be consistent with PARAMETER variables)
*
      NCO=21
*--NCO is the central cell for the fluctuatuion sampling
      FND=1.E20
*--FND  is the number densty
      FTMP=300.
*--FTMP is the temperature
      FNUM=1.83738E15
*--FNUM  is the number of real molecules represented by a simulated mol.
      DTM=2.0E-6
*--DTM is the time step
      NSC=8
*--NSC is the number of sub-cells in each cell
      XF=0.
      XR=0.07533258
*--the simulated region is from x=XF to x=XR
      SP(1)=5.E-26
      SP(2)=3.5E-10
*--SP(1) is the molecular mass
*--SP(2) is the molecular diameter
      SPM(2)=273.
      SPM(3)=0.5
      SPM(4)=1.
*--SPM(2) is the reference temperature
*--SPM(3) is the viscosity-temperature power law
*--SPM(4) is the reciprocal of the VSS scattering parameter
      NIS=4
*--NIS is the number of time steps between samples
      NSP=40
*--NSP is the number of samples between restart and output file updates
      NPT=1000
*--NPT is the number of file updates to STOP
*
      RETURN
      END

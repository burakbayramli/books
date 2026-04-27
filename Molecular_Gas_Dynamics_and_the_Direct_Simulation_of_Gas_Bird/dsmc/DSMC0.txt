*   DSMC0.FOR
*
      PROGRAM DSMC0
*
*--test of collision procedures in a uniform gas
*
*--SI units are used throughout
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
*--MNM  is the maximum number of molecules
*--MNC  is the maximum number of sub
*--MNSC is the maximum number of sub-cells
*--MNSP is the maximum number of molecular species
*--MNSG is the number of species groups for collision sampling
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
*--COL(M,N) is the number of collisions between species N-M molecules
*--NCOL is the total number of collisions
*--MOVT the total number of molecular moves
*--SELT the total number of pair selections
*--SEPT the sum of collision pair separations
*--CS(N,M,L) sampled information on species L in cell M
*----N=1 number sum
*----N=2,3,4 sum of u,v,w
*----N=5,6,7 sum of u*u,v*v,w*w
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
*
*--NM is the number of molecules
*--PP(M)  is the x coordinate molecule M
*--PV(1 to 3,M)  u,v,w velocity components of molecule M
*--IPL(M) sub-cell number for molecule M
*--IPS(M) species code number
*--IR(M)  cross-reference array (molecule numbers in order of sub-cells)
*
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
*
*--CC(M) is the cell volume
*--CCG(N,M,L,K) is for collisions between species groups L-K in cell M
*----N=1 is the maximum value of (relative speed)*(coll. cross-section)
*----N=2 is the remainder when the selection number is rounded
*--CG(N,M) is the geometry related information on cell M
*----N=1 the minimum x coordinate
*----N=2 the maximum x coordinate
*----N=3 the cell width
*--IC(N,M,L) information on the molecules of species group L in cell M
*----N=1 (start address -1) of the molecule numbers in the array IR
*----N=2 the number of molecules in the cell
*--ISC(M) the cell in which the sub-cell lies
*--ISCG(N,M,L) is the information on species group L in sub-cell M
*----N=1 (start address -1) of the molecule numbers in the array IR
*----N=2 the number of molecules in the sub-cell
*--IG(2,M) information on group L molecules
*----N=1 (start address -1) of the molecule numbers in the array IR
*----N=2 the number of molecules in the cell
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
*
*--SP(N,M) information on species M
*----N=1 the reference cross-section (diameter in the data)
*----N=2 the reference temperature
*----N=3 the viscosity-temperature power law
*----N=4 the reciprocal of the VSS scattering parameter
*----N=5 the molecular mass
*--SPM(N,M,L) information on the interaction between L-M molecules
*----N=1  the reference cross-section (diameter in the data)
*----N=2  the reference temperature
*----N=3  the viscosity-temperature power law
*----N=4  the reciprocal of the VSS scattering parameter
*----N=5  the reduced mass
*----N=6  the Gamma function of (5/2 - viscosity-temperature power law)
*--ISP(M) the colision sampling group in which species M lies
*
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                FSP(MNSP),ISPD
*
*--TIME time
*--NPR the number of output/restart file update cycles
*--NSMP the total number of samples
*--FND the stream number density
*--FTMP the stream temperature
*--FSP(M) the fraction of species M in the stream
*--ISPD relates to the setting of data for colls. between unlike mols.
*----set to 0 if data is set automatically to the mean values
*----set to 1 if the values are set explicitly in the data
*
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
*
*--FNUM  is the number of real molecules represented by a simulated mol.
*--DTM is the time step
*--NIS is the number of time steps between samples
*--NSP is the number of samples between restart and output file updates
*--NPS is the estimated number of samples to steady flow
*--NPT is the number of file updates to STOP
*
      COMMON /GEOM  / CW,NSC,XF,XR
*
*--CW is the cell width
*--NSC is the number of sub-cells per cell
*--XF is the minimum x coordinate
*--XR is the maximum x coordinate
*
      COMMON /CONST / PI,SPI,BOLTZ
*
*--PI is pi and SPI is the square root of pi
*--BOLTZ is the Boltzmann constant
*
      WRITE (*,*) ' INPUT 0,1 FOR CONTINUING,NEW CALCULATION:- '
      READ (*,*) NQL
*
      IF (NQL.EQ.1) THEN
*
        CALL INIT0
*
      ELSE
*
        WRITE (*,*) ' READ THE RESTART FILE'
        OPEN (4,FILE='DSMC0.RES',STATUS='OLD',FORM='UNFORMATTED')
        READ (4) BOLTZ,CC,CCG,CG,COL,CS,CW,DTM,FNUM,FTMP,IC,IPL,IPS,IR,
     &           ISC,ISCG,ISP,MOVT,NCOL,NIS,NM,NSC,NSMP,NPR,NPT,NSP,PI,
     &           PP,PV,SELT,SEPT,SP,SPI,SPM,TIME,XF,XR
        CLOSE (4)
*
      END IF
*
      IF (NQL.EQ.1) CALL SAMPI0
*
100   NPR=NPR+1
*
      DO 200 JJJ=1,NSP
        DO 150 III=1,NIS
          TIME=TIME+DTM
*
          WRITE (*,99001) III,JJJ,NIS,NSP,NCOL
99001     FORMAT (' DSMC0:- Move ',2I5,'   of ',2I5,F14.0,' Collisions')
*
          CALL MOVE0
*
          CALL INDEXM
*
          CALL COLLM
*
150     CONTINUE
*
        CALL SAMPLE0
*
200   CONTINUE
*
      WRITE (*,*) ' WRITING RESTART AND OUTPUT FILES',NPR,'  OF ',NPT
      OPEN (4,FILE='DSMC0.RES',FORM='UNFORMATTED')
      WRITE (4) BOLTZ,CC,CCG,CG,COL,CS,CW,DTM,FNUM,FTMP,IC,IPL,IPS,IR,
     &          ISC,ISCG,ISP,MOVT,NCOL,NIS,NM,NSC,NSMP,NPR,NPT,NSP,PI,
     &          PP,PV,SELT,SEPT,SP,SPI,SPM,TIME,XF,XR
      CLOSE (4)
*
      CALL OUT0
*
      IF (NPR.LT.NPT) GO TO 100
      STOP
      END
*   INIT0.FOR
*
      SUBROUTINE INIT0
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                FSP(MNSP),ISPD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
      COMMON /CONST / PI,SPI,BOLTZ
*
*--set constants
*
      PI=3.141592654
      SPI=SQRT(PI)
      BOLTZ=1.3806E-23
*
      CALL DATA0
*
*--set information on the cross-species collisions
*
      IF (MNSP.EQ.1) ISPD=0
      DO 100 N=1,MNSP
        DO 50 M=1,MNSP
          IF ((ISPD.EQ.0).OR.(N.EQ.M)) THEN
            SPM(1,N,M)=0.25*PI*(SP(1,N)+SP(1,M))**2
*--the collision cross section is assumed to be given by eqn (1.35)
            SPM(2,N,M)=0.5*(SP(2,N)+SP(2,M))
            SPM(3,N,M)=0.5*(SP(3,N)+SP(3,M))
            SPM(4,N,M)=0.5*(SP(4,N)+SP(4,M))
*--mean values are used for ISPD=0
          ELSE
            SPM(1,N,M)=PI*SPM(1,N,M)**2
*--the cross-collision diameter is converted to the cross-section
          END IF
          SPM(5,N,M)=(SP(5,N)/(SP(5,N)+SP(5,M)))*SP(5,M)
*--the reduced mass is defined in eqn (2.7)
          SPM(6,N,M)=GAM(2.5-SPM(3,N,M))
50      CONTINUE
100   CONTINUE
*
*--initialise variables
*
      TIME=0.
      NM=0
*
      CG(1,1)=XF
      CW=(XR-XF)/MNC
      DO 200 M=1,MNC
        IF (M.GT.1) CG(1,M)=CG(2,M-1)
        CG(2,M)=CG(1,M)+CW
        CG(3,M)=CW
        CC(M)=CW
        DO 150 L=1,MNSG
          DO 120 K=1,MNSG
            CCG(2,M,L,K)=RF(0)
            CCG(1,M,L,K)=SPM(1,1,1)*300.*SQRT(FTMP/300.)
120       CONTINUE
150     CONTINUE
*--the maximum value of the (rel. speed)*(cross-section) is set to a
*--reasonable, but low, initial value and will be increased as necessary
200   CONTINUE
*
*--set sub-cells
*
      DO 300 N=1,MNC
        DO 250 M=1,NSC
          L=(N-1)*NSC+M
          ISC(L)=N
250     CONTINUE
300   CONTINUE
*
*--generate initial gas in equilibrium at temperature FTMP
*
      DO 400 L=1,MNSP
        REM=0
        VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
        DO 350 N=1,MNC
          A=FND*CG(3,N)*FSP(L)/FNUM+REM
*--A is the number of simulated molecules of species L in cell N to
*--simulate the required concentrations at a total number density of FND
          IF (N.LT.MNC) THEN
            MM=A
            REM=(A-MM)
*--the remainder REM is carried forward to the next cell
          ELSE
            MM=NINT(A)
          END IF
          DO 320 M=1,MM
            IF (NM.LE.MNM) THEN
*--round-off error could have taken NM to MNM+1
              NM=NM+1
              IPS(NM)=L
              PP(NM)=CG(1,N)+RF(0)*(CG(2,N)-CG(1,N))
              IPL(NM)=(PP(NM)-CG(1,N))*(NSC-.001)/CG(3,N)+1+NSC*(N-1)
*--species, position, and sub-cell number have been set
              DO 305 K=1,3
                CALL RVELC(PV(K,NM),A,VMP)
305           CONTINUE
*--velocity components have been set
            END IF
320       CONTINUE
350     CONTINUE
400   CONTINUE
      WRITE (*,99001) NM
99001 FORMAT (' ',I6,' MOLECULES')
*
      RETURN
      END
*   SAMPI0.FOR
*
      SUBROUTINE SAMPI0
*
*--initialises all the sampling variables
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                FSP(MNSP),ISPD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
*
      NPR=0
      NCOL=0
      NSMP=0
      MOVT=0.
      SELT=0.
      SEPT=0.
      DO 100 L=1,MNSP
        DO 50 N=1,MNC
          CS(1,N,L)=1.E-6
          DO 20 M=2,7
            CS(M,N,L)=0.
20        CONTINUE
50      CONTINUE
100   CONTINUE
      DO 200 M=1,MNSP
        DO 150 N=1,MNSP
          COL(M,N)=0.
150     CONTINUE
200   CONTINUE
      RETURN
      END
*   MOVE0.FOR
*
      SUBROUTINE MOVE0
*
*--the NM molecules are moved over the time interval DTM
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                FSP(MNSP),ISPD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
*
      DO 100 N=1,NM
        MOVT=MOVT+1
        MSC=IPL(N)
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
        IPL(N)=MSC
        PP(N)=X
100   CONTINUE
      RETURN
      END
*   INDEXM.FOR
*
      SUBROUTINE INDEXM
*
*--the NM molecule numbers are arranged in order of the molecule groups
*--and, within the groups, in order of the cells and, within the cells,
*--in order of the sub-cells
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
*
      DO 200 MM=1,MNSG
        IG(2,MM)=0
        DO 50 NN=1,MNC
          IC(2,NN,MM)=0
50      CONTINUE
        DO 100 NN=1,MNSC
          ISCG(2,NN,MM)=0
100     CONTINUE
200   CONTINUE
      DO 300 N=1,NM
        LS=IPS(N)
        MG=ISP(LS)
        IG(2,MG)=IG(2,MG)+1
        MSC=IPL(N)
        ISCG(2,MSC,MG)=ISCG(2,MSC,MG)+1
        MC=ISC(MSC)
        IC(2,MC,MG)=IC(2,MC,MG)+1
300   CONTINUE
*--number in molecule groups in the cells and sub-cells have been counte
      M=0
      DO 400 L=1,MNSG
        IG(1,L)=M
*--the (start address -1) has been set for the groups
        M=M+IG(2,L)
400   CONTINUE
      DO 600 L=1,MNSG
        M=IG(1,L)
        DO 450 N=1,MNC
          IC(1,N,L)=M
          M=M+IC(2,N,L)
450     CONTINUE
*--the (start address -1) has been set for the cells
        M=IG(1,L)
        DO 500 N=1,MNSC
          ISCG(1,N,L)=M
          M=M+ISCG(2,N,L)
          ISCG(2,N,L)=0
500     CONTINUE
600   CONTINUE
*--the (start address -1) has been set for the sub-cells
 
      DO 700 N=1,NM
        LS=IPS(N)
        MG=ISP(LS)
        MSC=IPL(N)
        ISCG(2,MSC,MG)=ISCG(2,MSC,MG)+1
        K=ISCG(1,MSC,MG)+ISCG(2,MSC,MG)
        IR(K)=N
*--the molecule number N has been set in the cross-reference array
700   CONTINUE
      RETURN
      END
*   COLLM.FOR
*
      SUBROUTINE COLLM
*
*--calculates collisions appropriate to DTM in a monatomic gas mixture
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                FSP(MNSP),ISPD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
      COMMON /CONST / PI,SPI,BOLTZ
      COMMON /ELAST / VRC(3),VRR,VR,L,M,LS,MS,CVR,MM,NN,N
*
*--VRC(3) are the pre-collision components of the relative velocity
*
      DO 100 N=1,MNC
*--consider collisions in cell N
        DO 50 NN=1,MNSG
          DO 20 MM=1,MNSG
            SN=0.
            DO 10 K=1,MNSP
              IF (ISP(K).EQ.MM) SN=SN+CS(1,N,K)
10          CONTINUE
            IF (SN.GT.1.) THEN
              AVN=SN/FLOAT(NSMP)
            ELSE
              AVN=IC(2,N,MM)
            END IF
*--AVN is the average number of group MM molecules in the cell
            ASEL=0.5*IC(2,N,NN)*AVN*FNUM*CCG(1,N,NN,MM)*DTM/CC(N)
     &           +CCG(2,N,NN,MM)
*--ASEL is the number of pairs to be selected, see eqn (11.5)
            NSEL=ASEL
            CCG(2,N,NN,MM)=ASEL-NSEL
            IF (NSEL.GT.0) THEN
              IF (((NN.NE.MM).AND.(IC(2,N,NN).LT.1.OR.IC(2,N,MM).LT.1))
     &            .OR.((NN.EQ.MM).AND.(IC(2,N,NN).LT.2))) THEN
                CCG(2,N,NN,MM)=CCG(2,N,NN,MM)+NSEL
*--if there are insufficient molecules to calculate collisions,
*--the number NSEL is added to the remainer CCG(2,N,NN,MM)
              ELSE
                CVM=CCG(1,N,NN,MM)
                SELT=SELT+NSEL
                DO 12 ISEL=1,NSEL
*
                  CALL SELECT
*
                  IF (CVR.GT.CVM) CVM=CVR
*--if necessary, the maximum product in CVM is upgraded
                  IF (RF(0).LT.CVR/CCG(1,N,NN,MM)) THEN
*--the collision is accepted with the probability of eqn (11.6)
                    NCOL=NCOL+1
                    SEPT=SEPT+ABS(PP(L)-PP(M))
                    COL(LS,MS)=COL(LS,MS)+1.D00
                    COL(MS,LS)=COL(MS,LS)+1.D00
*
                    CALL ELASTIC
*
                  END IF
12              CONTINUE
                CCG(1,N,NN,MM)=CVM
              END IF
            END IF
20        CONTINUE
50      CONTINUE
100   CONTINUE
      RETURN
      END
*   SELECT.FOR
*
      SUBROUTINE SELECT
*--selects a potential collision pair and calculates the product of the
*--collision cross-section and relative speed
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /CONST / PI,SPI,BOLTZ
      COMMON /ELAST / VRC(3),VRR,VR,L,M,LS,MS,CVR,MM,NN,N
*
      K=INT(RF(0)*(IC(2,N,NN)-0.0001))+IC(1,N,NN)+1
      L=IR(K)
*--the first molecule L has been chosen at random from group NN in cell
100   MSC=IPL(L)
      IF ((NN.EQ.MM.AND.ISCG(2,MSC,MM).EQ.1).OR.
     &    (NN.NE.MM.AND.ISCG(2,MSC,MM).EQ.0)) THEN
*--if MSC has no type MM molecule find the nearest sub-cell with one
        NST=1
        NSG=1
150     INC=NSG*NST
        NSG=-NSG
        NST=NST+1
        MSC=MSC+INC
        IF (MSC.LT.1.OR.MSC.GT.MNSC) GO TO 150
        IF (ISC(MSC).NE.N.OR.ISCG(2,MSC,MM).LT.1) GO TO 150
      END IF
*--the second molecule M is now chosen at random from the group MM
*--molecules that are in the sub-cell MSC
      K=INT(RF(0)*(ISCG(2,MSC,MM)-0.0001))+ISCG(1,MSC,MM)+1
      M=IR(K)
      IF (L.EQ.M) GO TO 100
*--choose a new second molecule if the first is again chosen
      DO 200 K=1,3
        VRC(K)=PV(K,L)-PV(K,M)
200   CONTINUE
*--VRC(1 to 3) are the components of the relative velocity
      VRR=VRC(1)**2+VRC(2)**2+VRC(3)**2
      VR=SQRT(VRR)
*--VR is the relative speed
      LS=IPS(L)
      MS=IPS(M)
      CVR=VR*SPM(1,LS,MS)*((2.*BOLTZ*SPM(2,LS,MS)/(SPM(5,LS,MS)*VRR))
     &    **(SPM(3,LS,MS)-0.5))/SPM(6,LS,MS)
*--the collision cross-section is based on eqn (4.63)
      RETURN
      END
*   ELASTIC.FOR
*
      SUBROUTINE ELASTIC
*
*--generates the post-collision velocity components
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /CONST / PI,SPI,BOLTZ
      COMMON /ELAST / VRC(3),VRR,VR,L,M,LS,MS,CVR,MM,NN,N
*
      DIMENSION VRCP(3),VCCM(3)
*--VRCP(3) are the post-collision components of the relative velocity
*--VCCM(3) are the components of the centre of mass velocity
*
      RML=SPM(5,LS,MS)/SP(5,MS)
      RMM=SPM(5,LS,MS)/SP(5,LS)
      DO 100 K=1,3
        VCCM(K)=RML*PV(K,L)+RMM*PV(K,M)
100   CONTINUE
*--VCCM defines the components of the centre of mass velocity (eqn 2.1)
      IF (ABS(SPM(4,LS,MS)-1.).LT.1.E-3) THEN
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
        B=2.*(RF(0)**SPM(4,LS,MS))-1.
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
      DO 200 K=1,3
        PV(K,L)=VCCM(K)+VRCP(K)*RMM
        PV(K,M)=VCCM(K)-VRCP(K)*RML
200   CONTINUE
      RETURN
      END
*   SAMPLE0.FOR
*
      SUBROUTINE SAMPLE0
*
*--samples the molecules in the flow
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                FSP(MNSP),ISPD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
*
      NSMP=NSMP+1
      DO 100 NN=1,MNSG
        DO 50 N=1,MNC
          L=IC(2,N,NN)
          IF (L.GT.0) THEN
            DO 10 J=1,L
              K=IC(1,N,NN)+J
              M=IR(K)
              I=IPS(M)
              CS(1,N,I)=CS(1,N,I)+1
              DO 5 LL=1,3
                CS(LL+1,N,I)=CS(LL+1,N,I)+PV(LL,M)
                CS(LL+4,N,I)=CS(LL+4,N,I)+PV(LL,M)**2
5             CONTINUE
10          CONTINUE
          END IF
50      CONTINUE
100   CONTINUE
      RETURN
      END
*   OUT0.FOR
*
      SUBROUTINE OUT0
*
*--output a progressive set of results to file DSMC0.OUT
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                FSP(MNSP),ISPD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /CONST / PI,SPI,BOLTZ
*
      DIMENSION VEL(3),SMU(3),TCOL(MNSP,MNSP),SVEL(3,MNC)
*
      OPEN (4,FILE='DSMC0.OUT',FORM='FORMATTED')
*
      WRITE (4,*) ' FROM ZERO TIME TO TIME',TIME
      WRITE (4,*) ' COLLISIONS:-'
      WRITE (4,99001) ((COL(M,L),M=1,MNSP),L=1,MNSP)
99001 FORMAT (5F12.0)
      WRITE (4,*) ' TOTAL NUMBER OF SAMPLES ',NSMP
      WRITE (4,*) NM,' MOLECULES'
      WRITE (4,*) MOVT,' TOTAL MOLECULAR MOVES'
      WRITE (4,*) INT(SELT),' SELECTIONS ',INT(NCOL),
     &            ' COLLISION EVENTS, RATIO  ',REAL(NCOL/SELT)
      IF (NCOL.GT.0) WRITE (4,*) ' MEAN COLLISION SEPARATION ',
     &                           REAL(SEPT/NCOL)
      WRITE (4,*) 'SAMPLES'
      WRITE (4,*) ' CELL     N SP 1    N SP 2     ETC '
      DO 100 N=1,MNC
        WRITE (4,99002) N,(CS(1,N,L),L=1,MNSP)
100   CONTINUE
99002 FORMAT (' ',I6,5F9.0)
*
      WRITE (4,*) ' FLOWFIELD PROPERTIES'
      WRITE (4,*) 
     &'  CELL   X COORD     DENSITY       U         V        W         T
     &EMP'
      TOT=0.
*--first the mixture properties
      DO 300 N=1,MNC
        A=FNUM/(CG(3,N)*NSMP)
        SN=0.
        SM=0.
        DO 150 K=1,3
          SMU(K)=0.
150     CONTINUE
        SMCC=0.
        DO 200 L=1,MNSP
          SN=SN+CS(1,N,L)
*--SN is the number sum
          SM=SM+SP(5,L)*CS(1,N,L)
*--SM is the sum of molecular masses
          DO 160 K=1,3
            SMU(K)=SMU(K)+SP(5,L)*CS(K+1,N,L)
*--SMU(1 to 3) are the sum of mu, mv, mw
160       CONTINUE
          SMCC=SMCC+(CS(5,N,L)+CS(6,N,L)+CS(7,N,L))*SP(5,L)
*--SMCC is the sum of m(u**2+v**2+w**2)
200     CONTINUE
        DENN=SN*A
*--DENN is the number density, see eqn (1.34)
        DEN=DENN*SM/SN
*--DEN is the density, see eqn (1.42)
        DO 250 K=1,3
          VEL(K)=SMU(K)/SM
          SVEL(K,N)=VEL(K)
250     CONTINUE
*--VEL and SVEL are the stream velocity components, see eqn (1.43)
        UU=VEL(1)**2+VEL(2)**2+VEL(3)**2
        TT=(SMCC-SM*UU)/(3.*BOLTZ*SN)
*--TT is the temperature, see eqn (1.51)
        TOT=TOT+TT
        XC=0.5*(CG(1,N)+CG(2,N))
*--XC is the x coordinate of the midpoint of the cell
        WRITE (4,99003) N,XC,DEN,VEL(1),VEL(2),VEL(3),TT
99003   FORMAT (' ',I5,F10.4,1P,E12.4,0P,4F10.4)
300   CONTINUE
*
      WRITE (4,*)
      DO 400 L=1,MNSP
*--now the properties of the separate species
        WRITE (4,*)
        WRITE (4,*) ' SPECIES ',L
        WRITE (4,*) 
     &' CELL   X COORD      N DENS     DENSITY U DIF VEL V DIF VEL W DIF
     & VEL      TEMP '
        DO 350 N=1,MNC
          A=FNUM/(CG(3,N)*NSMP)
          DENN=CS(1,N,L)*A
*--DENN is the partial number density
          DEN=SP(5,L)*DENN
*--DEN is the partial density, see eqn (1.13)
          DO 320 K=1,3
            VEL(K)=CS(K+1,N,L)/CS(1,N,L)
*--VEL defines the average velocity of the species L molecules
320       CONTINUE
          UU=VEL(1)**2+VEL(2)**2+VEL(3)**2
          TT=(SP(5,L)/(3.*BOLTZ))
     &       *((CS(5,N,L)+CS(6,N,L)+CS(7,N,L))/CS(1,N,L)-UU)
*--TT is the temperature, see eqn (1.29)
          DO 340 K=1,3
            VEL(K)=VEL(K)-SVEL(K,N)
*--VEL now defines the diffusion velocity of species L, see eqn (1,45)
340       CONTINUE
          XC=0.5*(CG(1,N)+CG(2,N))
          WRITE (4,99004) N,XC,DENN,DEN,VEL(1),VEL(2),VEL(3),TT
99004     FORMAT (' ',I5,F9.4,1P,2E12.4,0P,4F10.4)
350     CONTINUE
400   CONTINUE
*
*--compare with the theoretical collision number for actual temperarure
      AVTMP=TOT/MNC
      WRITE (4,*)
      WRITE (4,*) ' AVERAGE TEMPERATURE ',AVTMP
      WRITE (4,*)
      WRITE (4,*) ' COLLISIONS:-'
      WRITE (4,99001) ((COL(M,L),M=1,MNSP),L=1,MNSP)
      WRITE (4,*)
      WRITE (4,*) ' RATIO OF COLLISION NUMBER TO THEORETICAL VALUE'
      WRITE (4,*)
      DO 600 M=1,MNSP
        SML=0
        DO 450 K=1,MNC
          SML=SML+CS(1,K,M)/NSMP
450     CONTINUE
        DO 500 L=1,MNSP
          SLL=0
          DO 460 K=1,MNC
            SLL=SLL+CS(1,K,L)/NSMP
460       CONTINUE
          TCOL(M,L)=2.*TIME*FNUM*SML*SLL*(1./(CG(2,MNC)-CG(1,1)))
     &              *SPM(1,M,L)*(AVTMP/SPM(2,M,L))**(1.-SPM(3,M,L))
     &              *SQRT(2.*BOLTZ*SPM(2,L,M)/(PI*SPM(5,L,M)))
* TCOL is the equilibrium collision rate, see eqn (4.78)
500     CONTINUE
600   CONTINUE
      WRITE (4,99005) ((COL(M,L)/TCOL(M,L),M=1,MNSP),L=1,MNSP)
99005 FORMAT (5F10.6)
*
      CLOSE (4)
*
      RETURN
      END
*   RVELC.FOR
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
*--calculates the Gamma function of X
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
*   DATA0.FOR
*
      SUBROUTINE DATA0
*
*--defines the data for a particular run of DSMC0.FOR
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400,MNSP=5,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                FSP(MNSP),ISPD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
*
*--set data (must be consistent with PARAMETER variables)
*
      FND=1.E20
*--FND  is the number densty
      FTMP=300.
*--FTMP is the temperature
      FSP(1)=.6
      FSP(2)=.2
      FSP(3)=.1
      FSP(4)=.08
      FSP(5)=.02
*--FSP(N) is the number fraction of species N
      FNUM=1.0E17
*--FNUM  is the number of real molecules represented by a simulated mol.
      DTM=.25E-4
*--DTM is the time step
      NSC=8
*--NSC is the number of sub-cells in each cell
      XF=0.
      XR=1.
*--the simulated region is from x=XF to x=XR
      ISPD=0
*--the cross-collision data is set to the mean values
      SP(1,1)=3.5E-10
      SP(2,1)=273.
      SP(3,1)=0.75
      SP(4,1)=1.
      SP(5,1)=5.E-26
      ISP(1)=1
      SP(1,2)=4.E-10
      SP(2,2)=273.
      SP(3,2)=0.75
      SP(4,2)=1.
      SP(5,2)=4.5E-26
      ISP(2)=1
      SP(1,3)=3.E-10
      SP(2,3)=273.
      SP(3,3)=0.75
      SP(4,3)=1.
      SP(5,3)=2.5E-26
      ISP(3)=1
      SP(1,4)=3.E-10
      SP(2,4)=273.
      SP(3,4)=0.75
      SP(4,4)=1.
      SP(5,4)=2.E-26
      ISP(4)=1
      SP(1,5)=4.E-10
      SP(2,5)=273.
      SP(3,5)=0.75
      SP(4,5)=1.
      SP(5,5)=4.E-26
      ISP(5)=1
*--SP(1,N) is the molecular diameter of species N
*--SP(2,N) is the reference temperature
*--SP(3,N) is the viscosity-temperatire index
*--SP(4,N) is the reciprocal of the VSS scattering parameter
*--SP(5,N) is the molecular mass of species N
*--ISP(N) is the group for species N
      NIS=4
*--NIS is the number of time steps between samples
      NSP=40
*--NSP is the number of samples between restart and output file updates
      NPT=500
*--NPT is the number of file updates to STOP
*
      RETURN
      END

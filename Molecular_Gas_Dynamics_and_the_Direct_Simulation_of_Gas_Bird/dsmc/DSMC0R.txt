*   DSMC0R.FOR
*
      PROGRAM DSMC0R
*
*--test of rotational relaxation in a uniform gas.
*
*--SI units are used throughout
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
*--variables as defined in DSMC0.FOR
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
*--variables as defined in DSMC0.FOR
*
      DOUBLE PRECISION CSR(MNC,MNSP)
*
*  CSR(M,L) the sum of the rotational energy of species L in cell M
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
*
*--variables as defined in DSMC0.FOR
*
      COMMON /MOLSR / PR(MNM)
*
*--PR(M) is the rotational energy of molecule M
*
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
*
*--variables as defined in DSMC0.FOR
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
*
*--variables as defined in DSMC0.FOR
*
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
*
*--SPR(N,M,L) information on rotational relaxation properties of a
*----species M molecule in a collision with a species L molecule
*----N=1 the const. in the temperature polynomial for the collision numb
*----N=2 the coefficient of T in this polynomial
*----N=3 the coefficient of T**2 in this polynomial
*--ISPR(N,M) integer information on rotational properties of species M
*----N=1 the number of rotational degrees of freedom
*----N=2 0, 1 for constant, polynomial for relaxation collision number
*----N=3 0, 1 for a common, collision partner species dependent rate
*--CT(M) the macroscopic temperature in cell M
*
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
*
*--variables as defined in DSMC0.FOR
*
      COMMON /SAMPR / CSR
*
      COMMON /SAMPD / CSDV(MNSP,400),CSDE(MNSP,400)
*--CSDV samples the velocity distribution in 400 divisions
*--CSDE samples the rotational energy distribution in 400 divisions
*
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
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
      WRITE (*,*) ' INPUT 0,1 FOR CONTINUING,NEW SAMPLE:- '
      READ (*,*) NQLS
*
      IF (NQL.EQ.1) THEN
*
        CALL INIT0R
*
      ELSE
*
        WRITE (*,*) ' READ THE RESTART FILE'
        OPEN (4,FILE='DSMC0R.RES',STATUS='OLD',FORM='UNFORMATTED')
        READ (4) BOLTZ,CC,CCG,CG,COL,CS,CSDV,CSDE,CSR,CT,CW,DTM,FNUM,
     &           FTMP,IC,IPL,IPS,IR,ISC,ISCG,ISP,ISPR,MOVT,NCOL,NIS,NM,
     &           NPS,NSC,NSMP,NPR,NPT,NSP,PI,PP,PR,PV,SELT,SEPT,SP,SPI,
     &           SPM,SPR,TIME,TIMI,XF,XR
        CLOSE (4)
*
      END IF
*
      IF (NQLS.EQ.1) CALL SAMPI0R
*
100   NPR=NPR+1
*
      IF (NPR.LE.NPS) CALL SAMPI0R
*
      DO 200 JJJ=1,NSP
        DO 150 III=1,NIS
          TIME=TIME+DTM
*
          WRITE (*,99001) III,JJJ,NIS,NSP,NCOL
99001   FORMAT (' DSMC0R:- Move ',2I5,'   of ',2I5,F14.0,' Collisions')
*
          CALL MOVE0
*
          CALL INDEXM
*
          CALL COLLMR
*
150     CONTINUE
*
        CALL SAMPLE0R
*
200   CONTINUE
*
      WRITE (*,*) ' WRITING RESTART AND OUTPUT FILES',NPR,'  OF ',NPT
      OPEN (4,FILE='DSMC0R.RES',FORM='UNFORMATTED')
      WRITE (4) BOLTZ,CC,CCG,CG,COL,CS,CSDV,CSDE,CSR,CT,CW,DTM,FNUM,
     &          FTMP,IC,IPL,IPS,IR,ISC,ISCG,ISP,ISPR,MOVT,NCOL,NIS,NM,
     &          NPS,NSC,NSMP,NPR,NPT,NSP,PI,PP,PR,PV,SELT,SEPT,SP,SPI,
     &          SPM,SPR,TIME,TIMI,XF,XR
      CLOSE (4)
*
      CALL OUT0R
*
      IF (NPR.LT.NPT) GO TO 100
      STOP
      END
*   INIT0R.FOR
*
      SUBROUTINE INIT0R
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
 
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
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
      CALL DATA0R
*
*--set additional data on the gas
*
      IF (MNSP.EQ.1) ISPD=0
      DO 100 N=1,MNSP
        DO 50 M=1,MNSP
          IF ((ISPR(3,N).EQ.0).AND.(M.NE.N)) THEN
            SPR(1,N,M)=SPR(1,N,N)
            SPR(2,N,M)=SPR(2,N,N)
            SPR(3,N,M)=SPR(3,N,N)
          END IF
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
      NPR=0
      NCOL=0
      MOVT=0.
      SELT=0.
      SEPT=0.
*
      DO 200 M=1,MNSP
        DO 150 N=1,MNSP
          COL(M,N)=0.
150     CONTINUE
200   CONTINUE
*
      CG(1,1)=XF
      CW=(XR-XF)/MNC
      DO 300 M=1,MNC
        CT(M)=FTMP
*--the macroscopic temperature is set to the freestream temperature
        IF (M.GT.1) CG(1,M)=CG(2,M-1)
        CG(2,M)=CG(1,M)+CW
        CG(3,M)=CW
        CC(M)=CW
        DO 250 L=1,MNSG
          DO 220 K=1,MNSG
            CCG(2,M,L,K)=RF(0)
            CCG(1,M,L,K)=SPM(1,1,1)*300.*SQRT(FTMP/300.)
220       CONTINUE
250     CONTINUE
*--the maximum value of the (rel. speed)*(cross-section) is set to a
*--reasonable, but low, initial value and will be increased as necessary
300   CONTINUE
*
*--set sub-cells
*
      DO 400 N=1,MNC
        DO 350 M=1,NSC
          L=(N-1)*NSC+M
          ISC(L)=N
350     CONTINUE
400   CONTINUE
*
*--generate initial gas with translational temperature FTMP,
*--but with zero rotational temperature.
*
      DO 500 L=1,MNSP
        REM=0
        VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
        DO 450 N=1,MNC
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
          DO 420 M=1,MM
            IF (NM.LE.MNM) THEN
*--round-off error could have taken NM to MNM+1
              NM=NM+1
              IPS(NM)=L
              PP(NM)=CG(1,N)+RF(0)*(CG(2,N)-CG(1,N))
              IPL(NM)=(PP(NM)-CG(1,N))*(NSC-.001)/CG(3,N)+1+NSC*(N-1)
*--species, position, and sub-cell number have been set
              DO 405 K=1,3
                CALL RVELC(PV(K,NM),A,VMP)
405           CONTINUE
*--velocity components have been set
*--set the rotational energy
*--the initial rotational energy is zero for the relaxation test
              IF (ISPR(1,L).GT.0) PR(NM)=0.
            END IF
420       CONTINUE
450     CONTINUE
500   CONTINUE
      WRITE (*,99001) NM
99001 FORMAT (' ',I6,' MOLECULES')
*
      RETURN
      END
*   SAMPI0R.FOR
*
      SUBROUTINE SAMPI0R
*
*--initialises all the sampling variables
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
*
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
      COMMON /SAMPD / CSDV(MNSP,400),CSDE(MNSP,400)
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
*
      NSMP=0
      TIMI=TIME
      DO 100 L=1,MNSP
        DO 50 N=1,MNC
          CS(1,N,L)=1.E-6
          DO 20 M=2,7
            CS(M,N,L)=0.
20        CONTINUE
          CSR(N,L)=0.
50      CONTINUE
        IF (MNC.EQ.1) THEN
          DO 60 K=1,400
            CSDV(L,K)=0.
            CSDE(L,K)=0.
60        CONTINUE
        END IF
100   CONTINUE
      RETURN
      END
*   COLLMR.FOR
*
      SUBROUTINE COLLMR
*
*--calculates collisions appropriate to DTM in a gas mixture
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
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
                    IF (ISPR(1,LS).GT.0.OR.ISPR(1,MS).GT.0) CALL INELRS
*--bypass rotational redistribution if both molecules are monatomic
*--note the three options for the selection of rotational energy
*----INELR the hierarchical application of the Larsen-Borgnakke method
*----INELRS the serial application of the Larsen-Borgnakke method
*----INELRA the serial application of the alternative L-B method
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
*   INELR.FOR
*
      SUBROUTINE INELR
*
*--adjustment of rotational energy in a collision
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      COMMON /MOLSR / PR(MNM)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /ELAST / VRC(3),VRR,VR,L,M,LS,MS,CVR,MM,NN,N
*
      DIMENSION IR(2)
*--IR is the indicator for the rotational redistribution
      ETI=0.5*SPM(5,LS,MS)*VRR
*--ETI is the initial translational energy
      ECI=0.
*--ECI is the initial energy in the active rotational modes
      ECF=0.
*--ECF is the final energy in these modes
      ECC=ETI
*--ECC is the energy to be divided
      XIB=2.5-SPM(3,LS,MS)
*--XIB is th number of modes in the redistribution
      IRT=0
*--IRT is 0,1 if no,any redistribution is made
      DO 100 NSP=1,2
*--consider the molecules in turn
        IF (NSP.EQ.1) THEN
          K=L
          KS=LS
          JS=MS
        ELSE
          K=M
          KS=MS
          JS=LS
        END IF
        IR(NSP)=0
        IF (ISPR(1,KS).GT.0) THEN
          IF (ISPR(2,KS).EQ.0) THEN
            ATK=1./SPR(1,KS,JS)
          ELSE
            ATK=1./(SPR(1,KS,JS)+SPR(2,KS,JS)*CT(N)+SPR(3,KS,JS)*CT(N)
     &          **2)
          END IF
*--ATK is the probability that rotation is redistributed to molecule L
          IF (ATK.GT.RF(0)) THEN
            IRT=1
            IR(NSP)=1
            ECC=ECC+PR(K)
            ECI=ECI+PR(K)
            XIB=XIB+0.5*ISPR(1,KS)
          END IF
        END IF
100   CONTINUE
*--apply the general Larsen-Borgnakke distribution function
      IF (IRT.EQ.1) THEN
        DO 150 NSP=1,2
          IF (IR(NSP).EQ.1) THEN
            IF (NSP.EQ.1) THEN
              K=L
              KS=LS
            ELSE
              K=M
              KS=MS
            END IF
            XIB=XIB-0.5*ISPR(1,KS)
*--the current molecule is removed from the total modes
            IF (ISPR(1,KS).EQ.2) THEN
              ERM=1.-RF(0)**(1./XIB)
            ELSE
              XIA=0.5*ISPR(1,KS)
              CALL LBS(XIA-1.,XIB-1.,ERM)
            END IF
            PR(K)=ERM*ECC
            ECC=ECC-PR(K)
*--the available energy is reduced accordingly
            ECF=ECF+PR(K)
          END IF
150     CONTINUE
        ETF=ETI+ECI-ECF
*--ETF  is the post-collision translational energy
*--adjust VR and, for the VSS model, VRC for the change in energy
        A=SQRT(2.*ETF/SPM(5,LS,MS))
        IF (ABS(SPM(4,LS,MS)-1.).LT.1.E-3) THEN
          VR=A
        ELSE
          DO 160 K=1,3
            VRC(K)=VRC(K)*A/VR
160       CONTINUE
          VR=A
        END IF
      END IF
      RETURN
      END
*   INELRS.FOR
*
      SUBROUTINE INELRS
*
*--adjustment of rotational energy in a collision
*--alternative version with serial application of L-B method
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      COMMON /MOLSR / PR(MNM)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /ELAST / VRC(3),VRR,VR,L,M,LS,MS,CVR,MM,NN,N
*
      ETI=0.5*SPM(5,LS,MS)*VRR
*--ETI is the initial translational energy
      XIB=2.5-SPM(3,LS,MS)
*--XIB is the number of translational modes in the redistribution
      IRT=0
*--IRT remains zero if there is no redistribution
      DO 100 NSP=1,2
*--consider the molecules in turn for serial redistribution
        IF (NSP.EQ.1) THEN
          K=L
          KS=LS
          JS=MS
        ELSE
          K=M
          KS=MS
          JS=LS
        END IF
        IF (ISPR(1,KS).GT.0) THEN
          IF (ISPR(2,KS).EQ.0) THEN
            ATK=1./SPR(1,KS,JS)
          ELSE
            ATK=1./(SPR(1,KS,JS)+SPR(2,KS,JS)*CT(N)+SPR(3,KS,JS)*CT(N)
     &          **2)
          END IF
*--ATK is the probability that rotation is redistributed to molecule L
          IF (ATK.GT.RF(0)) THEN
            IRT=1
            ECC=ETI+PR(K)
            ECI=PR(K)
*--apply the general Larsen-Borgnakke distribution function
            IF (ISPR(1,KS).EQ.2) THEN
              ERM=1.-RF(0)**(1./XIB)
            ELSE
              XIA=0.5*ISPR(1,KS)
              CALL LBS(XIA-1.,XIB-1.,ERM)
            END IF
            PR(K)=ERM*ECC
            ETI=ETI+ECI-PR(K)
          END IF
        END IF
100   CONTINUE
      IF (IRT.EQ.1) THEN
*--adjust VR and, for the VSS model, VRC for the change in energy
        A=SQRT(2.*ETI/SPM(5,LS,MS))
        IF (ABS(SPM(4,LS,MS)-1.).LT.1.E-3) THEN
          VR=A
        ELSE
          DO 120 K=1,3
            VRC(K)=VRC(K)*A/VR
120       CONTINUE
          VR=A
        END IF
      END IF
      RETURN
      END
*   INELRA.FOR
*
      SUBROUTINE INELRA
*
*--adjustment of rotational energy in a collision
*--alternative version with serial application of L-B method
*--and with fractional adjustment (Larsen-Borgnakke second option)
*----this option is for demonstration only and it is not recommended
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      COMMON /MOLSR / PR(MNM)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /ELAST / VRC(3),VRR,VR,L,M,LS,MS,CVR,MM,NN,N
*
      ETI=0.5*SPM(5,LS,MS)*VRR
*--ETI is the initial translational energy
      XIB=2.5-SPM(3,LS,MS)
*--XIB is the number of translational modes in the redistribution
      IRT=0
*--IRT remains zero if there is no redistribution
      DO 100 NSP=1,2
*--consider the molecules in turn for serial redistribution
        IF (NSP.EQ.1) THEN
          K=L
          KS=LS
          JS=MS
        ELSE
          K=M
          KS=MS
          JS=LS
        END IF
        IF (ISPR(1,KS).GT.0) THEN
          IF (ISPR(2,KS).EQ.0) THEN
            ATK=1./SPR(1,KS,JS)
          ELSE
            ATK=1./(SPR(1,KS,JS)+SPR(2,KS,JS)*CT(N)+SPR(3,KS,JS)*CT(N)
     &          **2)
          END IF
*--ATK is the fraction of rotation that is redistributed to molecule L
          IRT=1
          ECC=ETI+PR(K)
          ECI=PR(K)
*--apply the general Larsen-Borgnakke distribution function
          IF (ISPR(1,KS).EQ.2) THEN
            ERM=1.-RF(0)**(1./XIB)
          ELSE
            XIA=0.5*ISPR(1,KS)
            CALL LBS(XIA-1.,XIB-1.,ERM)
          END IF
          PR(K)=PR(K)+(ERM*ECC-PR(K))*ATK
          ETI=ETI+ECI-PR(K)
        END IF
100   CONTINUE
      IF (IRT.EQ.1) THEN
*--adjust VR and, for the VSS model, VRC for the change in energy
        A=SQRT(2.*ETI/SPM(5,LS,MS))
        IF (ABS(SPM(4,LS,MS)-1.).LT.1.E-3) THEN
          VR=A
        ELSE
          DO 120 K=1,3
            VRC(K)=VRC(K)*A/VR
120       CONTINUE
          VR=A
        END IF
      END IF
      RETURN
      END
*   LBS.FOR
*
      SUBROUTINE LBS(XMA,XMB,ERM)
*--selects a Larsen-Borgnakke energy ratio using eqn (11.9)
100   ERM=RF(0)
      IF (XMA.LT.1.E-6.OR.XMB.LT.1.E-6) THEN
        IF (XMA.LT.1.E-6.AND.XMB.LT.1.E-6) RETURN
        IF (XMA.LT.1.E-6) P=(1.-ERM)**XMB
        IF (XMB.LT.1.E-6) P=(1.-ERM)**XMA
      ELSE
        P=(((XMA+XMB)*ERM/XMA)**XMA)*(((XMA+XMB)*(1.-ERM)/XMB)**XMB)
      END IF
      IF (P.LT.RF(0)) GO TO 100
      RETURN
      END
*   SAMPLE0R.FOR
*
      SUBROUTINE SAMPLE0R
*
*--sample the molecules in the flow.
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
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
              CSR(N,I)=CSR(N,I)+PR(M)
10          CONTINUE
          END IF
50      CONTINUE
100   CONTINUE
      RETURN
      END
*   OUT0R.FOR
*
      SUBROUTINE OUT0R
*
*--output a progressive set of results to file DSMC0R.OUT.
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
      COMMON /SAMPD / CSDV(MNSP,400),CSDE(MNSP,400)
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /CONST / PI,SPI,BOLTZ
*
      DOUBLE PRECISION VEL(3),SMU(3),SVEL(3,MNC),SN,SM,SMCC,SRDF,SRE,TT,
     &                 TROT,DBOLTZ
      DBOLTZ=BOLTZ
*
      OPEN (4,FILE='DSMC0R.OUT',FORM='FORMATTED')
      OPEN (3,FILE='RELAX.OUT',FORM='FORMATTED',ACCESS='DIRECT',RECL=80)
*
      WRITE (4,*) ' FLOW SAMPLED FROM TIME ',TIMI,' TO TIME ',TIME
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
     &'  CELL   X COORD     DENSITY   TR TEMP  ROT TEMP   OV TEMP  U V  
     &      W '
*--first the mixture properties
      DO 300 N=1,MNC
        A=FNUM/(CG(3,N)*NSMP)
        SN=0.
        SM=0.
        DO 150 K=1,3
          SMU(K)=0.
150     CONTINUE
        SMCC=0.
        SRE=0.
        SRDF=0.
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
          SRE=SRE+CSR(N,L)
*--SRE is the sum of rotational energy
          SRDF=SRDF+ISPR(1,L)*CS(1,N,L)
*--SRDF is the sum of the rotational degrees of freedom
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
        TT=(SMCC-SM*UU)/(3.D00*DBOLTZ*SN)
*--TT is the translational temperature, see eqn (1.51)
        TROT=(2.D00/DBOLTZ)*SRE/SRDF
*--TROT is the rotational temperature, see eqn (11.11)
        TEMP=(3.D00*TT+(SRDF/SN)*TROT)/(3.+SRDF/SN)
*--TEMP is the overall temperature, see eqn (11.12)
        CT(N)=TEMP
        XC=0.5*(CG(1,N)+CG(2,N))
*--XC is the x coordinate of the midpoint of the cell
        WRITE (4,99003) N,XC,DEN,TT,TROT,TEMP,VEL(1),VEL(2),VEL(3)
99003   FORMAT (' ',I5,F10.4,1P,E12.4,0P,6F10.4)
300   CONTINUE
*
      WRITE (4,*)
      DO 500 L=1,MNSP
*--now the properties of the separate species
        WRITE (4,*) ' SPECIES ',L
        WRITE (4,*) 
     &' CELL   X COORD      N DENS     DENSITY   TR TEMP  ROT TEMP U DIF
     & VEL V DIF VEL W DIF VEL '
        DO 400 N=1,MNC
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
          TT=(SP(5,L)/(3.D00*DBOLTZ))
     &       *((CS(5,N,L)+CS(6,N,L)+CS(7,N,L))/CS(1,N,L)-UU)
*--TT is the translational temperature, see eqn (1.29)
          IF (ISPR(1,L).GT.0) THEN
            TROT=2.D00*CSR(N,L)/(ISPR(1,L)*DBOLTZ*CS(1,N,L))
          ELSE
            TROT=0.
          END IF
*--TROT is the rotational temperature, see eqn (11.10)
          DO 340 K=1,3
            VEL(K)=VEL(K)-SVEL(K,N)
*--VEL now defines the diffusion velocity of species L, see eqn (1.45)
340       CONTINUE
          XC=0.5*(CG(1,N)+CG(2,N))
          WRITE (4,99004) N,XC,DENN,DEN,TT,TROT,VEL(1),VEL(2),VEL(3)
99004     FORMAT (' ',I5,F9.4,1P,2E12.4,0P,5F10.4)
*--output the relaxation information
          IF (NPR.LE.NPS) THEN
            CRATE=0.
            DO 350 M=1,MNSP
              CRATE=CRATE+COL(L,M)*NSMP/CS(1,N,L)
350         CONTINUE
            WRITE (3,99005,REC=MNSP*(NPR-1)+L) L,CRATE,TT,TROT
99005       FORMAT (' REC ',I3,3E14.5)
          END IF
400     CONTINUE
500   CONTINUE
*
      IF (MNC.EQ.1) THEN
*--output the molecular distribution function
        WRITE (4,*)
        WRITE (4,*) 
     &           '          V/VM      f(V/VM)        Er/kT     f(Er/kT)'
        DO 550 K=1,MNSP
          DKT=0.025*BOLTZ*CT(1)
          DVMP=0.01*SQRT(2.*BOLTZ*CT(1)/SP(5,K))
*--DKT and DVMP are the steps in kT and most probable speed
          DO 520 N=1,NM
            KK=IPS(N)
            IF (KK.EQ.K) THEN
              M=SQRT(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)/DVMP+1
              IF (M.LE.400) CSDV(K,M)=CSDV(K,M)+1.
              M=PR(N)/DKT+1
              IF (M.LE.400) CSDE(K,M)=CSDE(K,M)+1.
            END IF
520       CONTINUE
          DO 540 M=1,400
            WRITE (4,99006) M*.01,CSDV(K,M)*NSP/(CS(1,1,K)*.01),M*0.025,
     &                      CSDE(K,M)*NSP/(CS(1,1,K)*0.025)
540       CONTINUE
99006     FORMAT (' ',4E13.5)
550     CONTINUE
      END IF
*
      CLOSE (3)
      CLOSE (4)
*
*--check the total energy
      TOTE=0.
      DO 600 N=1,NM
        L=IPS(N)
        TOTE=TOTE+0.5*SP(5,L)*(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
        TOTE=TOTE+PR(N)
600   CONTINUE
      WRITE (*,*) ' TOTAL ENERGY = ',TOTE
*
      RETURN
      END
*   MOVE0.FOR
*
      SUBROUTINE MOVE0
*
*--the NM molecules are moved over the time interval DTM
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
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
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
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
*   SELECT.FOR
*
      SUBROUTINE SELECT
*--selects a potential collision pair and calculates the product of the
*--collision cross-section and relative speed
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
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
*
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
*--generate the post-collision velocity components.
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
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
*   DATA0R.FOR
*
      SUBROUTINE DATA0R
*
*--defines the data for a particular run of DSMC0R.FOR.
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
*
*--set data (must be consistent with PARAMETER variables)
*
      FND=1.E20
*--FND  is the number densty
      FTMP=500.
*--FTMP is the temperature
      FSP(1)=1.
*--FSP(N) is the number fraction of species N
      FNUM=1.0E15
*--FNUM  is the number of real molecules represented by a simulated mol.
      DTM=2.E-5
*--DTM is the time step
      NSC=1
*--NSC is the number of sub-cells in each cell
      XF=0.
      XR=1.
*--the simulated region is from x=XF to x=XR
      SP(1,1)=3.5E-10
      SP(2,1)=273.
      SP(3,1)=0.75
      SP(4,1)=1.
      SP(5,1)=5.E-26
*--SP(1,N) is the molecular diameter of species N
*--SP(2,N) is the reference temperature
*--SP(3,N) is the viscosity-temperatire index
*--SP(4,N) is the reciprocal of the VSS scattering parameter
*--SP(5,N) is the molecular mass of species N
      ISP(1)=1
*--ISP(N) is the group for species N
      ISPR(1,1)=2.
      SPR(1,1,1)=5.
      ISPR(2,1)=0
*--ISPR(1,N) is the number of degrees of freedom of species N
*--SPR(1,N,M) is the constant in the polynomial for the rotational
*--relaxation collision number of species N with species M
*--ISPR(2,N) is 0,1 for constant, polynomial for collision number
      NIS=1
*--NIS is the number of time steps between samples
      NSP=1
*--NSP is the number of samples between restart and output file updates
      NPS=100
*--NPS is the number of updates to reach assumed steady flow
      NPT=1000
*--NPT is the number of file updates to STOP
*
      RETURN
      END

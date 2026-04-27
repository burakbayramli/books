*   DSMC0V.FOR
*
      PROGRAM DSMC0V
*
*--test of vibrational excitation in a uniform gas
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
*--variable as defined in DSMC0R.FOR
*
      DOUBLE PRECISION CSV(MNC,MNSP),CSVS(2,MNC,MNSP)
*
*--CSV(M,L) the sum of the vibrational energies of species L in cell M
*--CSVS(N,M,L) the sum of molecules in ground (N=1) and level 1 (N=2)
*
      DOUBLE PRECISION CSDVV(MNSP,100)
*
*--CSDVV samples the numbers in up to 100 divisions
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
*
*--variables as defined in DSMC0.FOR
*
      COMMON /MOLSR / PR(MNM)
*
*--variable as defined in DSMC0R.FOR
*
      COMMON /MOLSV / IPV(MNM)
*
*--IPV(N) vibrational state of molecule N
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
*--variables as defined in DSMC0R.FOR
*
      COMMON /GASV  / SPV(MNSP),ISPV(MNSP),SPVM(2,MNSP,MNSP),
     &                SVIB(MNC,MNSP)
*--SPV(M) the characteristic temperature of species M
*--SPVM(N,L,K) information on species M in a coll. with species K
*--N=1 the constant C1 in eqn (6.53) for the relaxation collision number
*--or, if SPVM(2,M,L,K) is negative, the relaxation collision number
*--N=2 the constant C2 in eqn (6.53), or -1 to indicate a constant Zv
*--ISPV(M) the number of vibrational modes of species M (either 0 or 1)
*--SVIB(N,L) the eff. number of vib. d. of f. of species L in cell N
*
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
*
*--variables as defined in DSMC0.FOR
*
      COMMON /SAMPR / CSR
*
*--variables as defined in DSMC0R.FOR
*
      COMMON /SAMPV / CSV,CSVS
*
*--double precision variables defined above
*
      COMMON /SAMPDV/ CSDVV
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
        CALL INIT0V
*
      ELSE
*
        WRITE (*,*) ' READ THE RESTART FILE'
        OPEN (4,FILE='DSMC0V.RES',STATUS='OLD',FORM='UNFORMATTED')
        READ (4) BOLTZ,CC,CCG,CG,COL,CS,CSDVV,CSR,CSV,CSVS,CT,CW,DTM,
     &           FNUM,FTMP,IC,IPL,IPS,IPV,IR,ISC,ISCG,ISP,ISPR,ISPV,
     &           MOVT,NCOL,NIS,NM,NPS,NSC,NSMP,NPR,NPT,NSP,PI,PP,PR,PV,
     &           SELT,SEPT,SP,SPI,SPM,SPR,SPV,SPVM,TIME,TIMI,XF,XR
        CLOSE (4)
*
      END IF
*
      IF (NQLS.EQ.1) CALL SAMPI0V
*
100   NPR=NPR+1
*
      IF (NPR.LE.NPS) CALL SAMPI0V
*
      DO 200 JJJ=1,NSP
        DO 150 III=1,NIS
          TIME=TIME+DTM
*
          WRITE (*,99001) III,JJJ,NIS,NSP,NCOL
99001   FORMAT (' DSMC0V:- Move ',2I5,'   of ',2I5,F14.0,' Collisions')
*
          CALL MOVE1
*
          CALL INDEXM
*
          CALL COLLMV
*
150     CONTINUE
*
        CALL SAMPLE0V
*
200   CONTINUE
*
      WRITE (*,*) ' WRITING RESTART AND OUTPUT FILES',NPR,'  OF ',NPT
      OPEN (4,FILE='DSMC0V.RES',FORM='UNFORMATTED')
      WRITE (4) BOLTZ,CC,CCG,CG,COL,CS,CSDVV,CSR,CSV,CSVS,CT,CW,DTM,
     &          FNUM,FTMP,IC,IPL,IPS,IPV,IR,ISC,ISCG,ISP,ISPR,ISPV,MOVT,
     &          NCOL,NIS,NM,NPS,NSC,NSMP,NPR,NPT,NSP,PI,PP,PR,PV,SELT,
     &          SEPT,SP,SPI,SPM,SPR,SPV,SPVM,TIME,TIMI,XF,XR
      CLOSE (4)
*
      CALL OUT0V
*
      IF (NPR.LT.NPT) GO TO 100
      STOP
      END
*   INIT0V.FOR
*
      SUBROUTINE INIT0V
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSV(MNC,MNSP),CSVS(2,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /MOLSV / IPV(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /GASV  / SPV(MNSP),ISPV(MNSP),SPVM(2,MNSP,MNSP),
     &                SVIB(MNC,MNSP)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
      COMMON /SAMPV / CSV,CSVS
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
      CALL DATA0V
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
*--Set sub-cells
*
      DO 400 N=1,MNC
        DO 350 M=1,NSC
          L=(N-1)*NSC+M
          ISC(L)=N
350     CONTINUE
400   CONTINUE
*
*--generate initial gas with trans. and rotational temperature FTMP
*
      WRITE (*,*) 
     &' INPUT 0,1 TO SET THE INITIAL ROTATIONAL TEMPERATURETO ZERO,FREES
     &TREAM:- '
      READ (*,*) NQLR
*
      WRITE (*,*) 
     &' INPUT 0,1 TO SET THE INITIAL VIBRATIONAL TEMPERATURE TO ZERO,FRE
     &ESTREAM:- '
      READ (*,*) NQLV
*
      DO 500 L=1,MNSP
        REM=0
        VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
        DO 450 N=1,MNC
          SVIB(N,L)=0.
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
*--Species, position, and sub-cell number have been set
              DO 405 K=1,3
                CALL RVELC(PV(K,NM),A,VMP)
405           CONTINUE
*--velocity components have been set
*--set the rotational energy
              IF (ISPR(1,L).GT.0) THEN
                IF (NQLR.EQ.0) THEN
                  PR(NM)=0.
                ELSE IF (ISPR(1,L).EQ.2) THEN
                  PR(NM)=-LOG(RF(0))*BOLTZ*FTMP
*--for 2 degrees of freedom, the sampling is directly from eqn (11.22)
                ELSE
                  CALL SIE(ISPR(1,L),ERM)
*--otherwise apply the acceptance-rejection method to eqn (11.23)
                  PR(NM)=ERM*BOLTZ*FTMP
                END IF
              END IF
*--set the vibrational levels
              IF (ISPV(L).GT.0) THEN
                IF (NQLV.EQ.0) THEN
                  IPV(NM)=0
                ELSE
                  IPV(NM)=-LOG(RF(0))*FTMP/SPV(L)
*--eqn (11.24) is used to set the vibrational level
                END IF
              END IF
            END IF
420       CONTINUE
450     CONTINUE
500   CONTINUE
      WRITE (*,99001) NM
99001 FORMAT (' ',I6,' MOLECULES')
*
      RETURN
      END
*   SIE.FOR
*
      SUBROUTINE SIE(IDF,ERM)
*--selects a typical internal energy ratio for IDF degrees of freedom
*--applies the acceptance-rejection method to eqn (11.23)
*--IDF must be greater than 2
*
      A=0.5*IDF-1.
100   ERM=RF(0)*10.
*--the cut-off internal energy is 10 kT
      B=((ERM/A)**A)*EXP(A-ERM)
      IF (B.LT.RF(0)) GO TO 100
      RETURN
      END
*   SAMPI0V.FOR
*
*
      SUBROUTINE SAMPI0V
*
*--initialises all the sampling variables
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSV(MNC,MNSP),CSVS(2,MNC,MNSP)
      DOUBLE PRECISION CSDVV(MNSP,100)
*
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
      COMMON /SAMPV / CSV,CSVS
      COMMON /SAMPDV/ CSDVV
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
          CSV(N,L)=0.
          CSVS(1,N,L)=0.
          CSVS(2,N,L)=0.
50      CONTINUE
        IF (MNC.EQ.1) THEN
          DO 60 K=1,100
            CSDVV(L,K)=0.
60        CONTINUE
        END IF
100   CONTINUE
      RETURN
      END
*   COLLMV.FOR
*
      SUBROUTINE COLLMV
*
*--calculates collisions appropriate to DTM in gas mixture
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSV(MNC,MNSP),CSVS(2,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /MOLSV / IPV(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /GASV  / SPV(MNSP),ISPV(MNSP),SPVM(2,MNSP,MNSP),
     &                SVIB(MNC,MNSP)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
      COMMON /SAMPV / CSV,CSVS
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
                    IF (ISPR(1,LS).GT.0.OR.ISPR(1,MS).GT.0) CALL INELV
*--bypass inelastic redistribution if both molecules are monatomic
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
*   INELV.FOR
*
      SUBROUTINE INELV
*
*--adjustment of vibrational and rotational energy in a collision
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      COMMON /MOLSR / PR(MNM)
      COMMON /MOLSV / IPV(MNM)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /GASV  / SPV(MNSP),ISPV(MNSP),SPVM(2,MNSP,MNSP),
     &                SVIB(MNC,MNSP)
      COMMON /ELAST / VRC(3),VRR,VR,L,M,LS,MS,CVR,MM,NN,N
      COMMON /CONST / PI,SPI,BOLTZ
*
      ET=0.5*SPM(5,LS,MS)*VRR
*--ET is the initial translational energy
      DO 100 KSP=1,2
*--consider the molecules in turn
        IF (KSP.EQ.1) THEN
          K=L
          KS=LS
          JS=MS
        ELSE
          K=M
          KS=MS
          JS=LS
        END IF
        IF (ISPV(KS).GT.0) THEN
          IF (SPVM(2,KS,JS).GT.0.) THEN
            IF (SVIB(N,KS).GT..0001) THEN
              COLT=(ET+IPV(K)*BOLTZ*SPV(KS))
     &             /((2.5-SPM(3,KS,JS)+0.5*SVIB(N,KS))*BOLTZ)
*--the collision temperature COLT is calculated from eqn (11.29)
            ELSE
              COLT=CT(N)
            END IF
            A=SPVM(2,KS,JS)*(COLT**(-0.33333))
            IF (A.LT.50.) THEN
              ZV=(SPVM(1,KS,JS)/(COLT**SPM(3,LS,MS)))*EXP(A)
            ELSE
              ZV=1.E7
            END IF
*--the vibrational collision number has been calculated from eqn (6.59)
          ELSE
            ZV=SPVM(1,KS,JS)
*--the vibrational relaxation collision number is a constant
          END IF
          IF (1./ZV.GT.RF(0)) THEN
            EVIB=IPV(K)*BOLTZ*SPV(KS)
            ECC=ET+EVIB
            MAXLEV=ECC/(BOLTZ*SPV(KS))
*--MAXLEV is the maximum level within the available energy
10          IPV(K)=RF(0)*(MAXLEV+0.99999)
*--the above statement chooses a level uniformly from 0 to MAXLEV
            EVIB=IPV(K)*BOLTZ*SPV(KS)
            PROB=(1.-EVIB/ECC)**(1.5-SPM(3,KS,JS))
*--PROB is the probability ratio (eqn (5.61))
            IF (PROB.LT.RF(0)) GO TO 10
            ET=ECC-EVIB
          END IF
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
            ECC=ET+PR(K)
            IF (ISPR(1,KS).EQ.2) THEN
              ERM=1.-RF(0)**(1./(2.5-SPM(3,KS,JS)))
            ELSE
              XIA=0.5*ISPR(1,KS)
              CALL LBS(XIA-1.,1.5-SPM(3,KS,JS),ERM)
            END IF
            PR(K)=ERM*ECC
            ET=ECC-PR(K)
          END IF
        END IF
100   CONTINUE
*--ET is now the post-collision translational energy
*--adjust VR and, for the VSS model, VRC for the change in energy
      A=SQRT(2.*ET/SPM(5,LS,MS))
      IF (ABS(SPM(4,LS,MS)-1.).LT.1.E-3) THEN
        VR=A
      ELSE
        DO 150 K=1,3
          VRC(K)=VRC(K)*A/VR
150     CONTINUE
        VR=A
      END IF
      RETURN
      END
*   SAMPLE0V.FOR
*
      SUBROUTINE SAMPLE0V
*
*--sample the molecules in the flow.
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSV(MNC,MNSP),CSVS(2,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /MOLSV / IPV(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /GASV  / SPV(MNSP),ISPV(MNSP),SPVM(2,MNSP,MNSP),
     &                SVIB(MNC,MNSP)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
      COMMON /SAMPV / CSV,CSVS
      COMMON /CONST / PI,SPI,BOLTZ
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
*
      NSMP=NSMP+1
      DO 100 M=1,NM
        I=IPS(M)
        K=IPL(M)
        N=ISC(K)
        CS(1,N,I)=CS(1,N,I)+1
        DO 50 LL=1,3
          CS(LL+1,N,I)=CS(LL+1,N,I)+PV(LL,M)
          CS(LL+4,N,I)=CS(LL+4,N,I)+PV(LL,M)**2
50      CONTINUE
        IF (ISPR(1,I).GT.0) CSR(N,I)=CSR(N,I)+PR(M)
        IF (ISPV(I).GT.0) THEN
          CSV(N,I)=CSV(N,I)+IPV(M)*BOLTZ*SPV(I)
          IF (IPV(M).EQ.0) CSVS(1,N,I)=CSVS(1,N,I)+1.
          IF (IPV(M).EQ.1) CSVS(2,N,I)=CSVS(2,N,I)+1.
        END IF
100   CONTINUE
      RETURN
      END
*   OUT0V.FOR
*
      SUBROUTINE OUT0V
*
*--output a progressive set of results to file DSMC0V.OUT.
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSV(MNC,MNSP),CSVS(2,MNC,MNSP)
      DOUBLE PRECISION CSDVV(MNSP,100)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /MOLSV / IPV(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /GASV  / SPV(MNSP),ISPV(MNSP),SPVM(2,MNSP,MNSP),
     &                SVIB(MNC,MNSP)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /SAMPR / CSR
      COMMON /SAMPV / CSV,CSVS
      COMMON /SAMPDV/ CSDVV
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /CONST / PI,SPI,BOLTZ
*
      DOUBLE PRECISION VEL(3),SMU(3),SVEL(3,MNC),SN,SM,SMCC,SRDF,SRE,TT,
     &                 TROT,DBOLTZ,DTMP
      DIMENSION TVIB(MNC,MNSP)
*--TVIB(L,N) is the vibrational temperature of species L in cell N
      DBOLTZ=BOLTZ
*
      OPEN (4,FILE='DSMC0V.OUT',FORM='FORMATTED')
      OPEN (3,FILE='RELAX.OUT',FORM='FORMATTED',ACCESS='DIRECT',RECL=80)
*
      WRITE (4,*) ' FLOW SAMPLED FROM TIME ',TIMI,' TO TIME',TIME
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
     &'  CELL   X COORD     DENSITY   TR TEMP  ROT TEMP  VIB TEMP   OV T
     &EMP       U         V        W '
*--first the mixture properties
      DO 400 N=1,MNC
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
          IF (ISPV(L).GT.0) THEN
            IF (CSVS(2,N,L).GT.0) THEN
              TVIB(N,L)=SPV(L)/LOG(CSVS(1,N,L)/CSVS(2,N,L))
*--TVIB the vib. temp. of species L is calculated from eqn (11.27)
              SVIB(N,L)=2.*CSV(N,L)/(CS(1,N,L)*BOLTZ*TVIB(N,L))
*--SVIB is the effective number of vibrational degrees of freedom
*----calculated from eqn (11.28)
            ELSE
              TVIB(N,L)=1.E-6
              SVIB(N,L)=0.
            END IF
          END IF
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
        SN=0.
        SVT=0.
        SVDF=0.
*--SVT is the sum of vibrational temperatures
*--SVDF is the sum of effective vibrational degrees of freedom
        DO 300 L=1,MNSP
          SN=SN+CS(1,N,L)
          IF (ISPV(L).GT.0) THEN
            SVT=SVT+TVIB(N,L)*SVIB(N,L)*CS(1,N,L)
            SVDF=SVDF+SVIB(N,L)*CS(1,N,L)
          END IF
300     CONTINUE
        IF (SVDF.GT.1.E-6) THEN
          TV=SVT/SVDF
*--TV is the vibrational temperature
          AVDF=SVDF/SN
*--AVDF is the average number of vibrational degrees of freedom
        ELSE
          TV=0.
          AVDF=0.
        END IF
        TEMP=(3.D00*TT+(SRDF/SN)*TROT+AVDF*TV)/(3.+SRDF/SN+AVDF)
*--TEMP is the overall temperature, see eqn (11.30)
        CT(N)=TEMP
        XC=0.5*(CG(1,N)+CG(2,N))
*--XC is the x coordinate of the midpoint of the cell
        WRITE (4,99003) N,XC,DEN,TT,TROT,TV,TEMP,VEL(1),VEL(2),VEL(3)
99003   FORMAT (' ',I5,F10.4,1P,E12.4,0P,7F10.3)
400   CONTINUE
*
      WRITE (4,*)
      DO 600 L=1,MNSP
*--now the properties of the separate species
        WRITE (4,*) ' SPECIES ',L
        WRITE (4,*) 
     &' CELL   X COORD      N DENS    DENSITY U DIF  VEL V DIF VEL W DIF
     & VEL   TR TEMP  ROT TEMP  VIB TEMP, EFF D OF F  '
        DO 500 N=1,MNC
          A=FNUM/(CG(3,N)*NSMP)
          DENN=CS(1,N,L)*A
*--DENN is the partial number density
          DEN=SP(5,L)*DENN
*--DEN is the partial density, see eqn (1.13)
          DO 420 K=1,3
            VEL(K)=CS(K+1,N,L)/CS(1,N,L)
*--VEL defines the average`velocity of the species L molecules
420       CONTINUE
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
          DO 440 K=1,3
            VEL(K)=VEL(K)-SVEL(K,N)
*--VEL now defines the diffusion velocity of species L, see eqn (1.45)
440       CONTINUE
*
          XC=0.5*(CG(1,N)+CG(2,N))
          WRITE (4,99004) N,XC,DENN,DEN,VEL(1),VEL(2),VEL(3),TT,TROT,
     &                    TVIB(N,L),SVIB(N,L)
99004     FORMAT (' ',I5,F9.4,1P,2E12.4,0P,10F10.3)
*--output the relaxation information
          IF (NPR.LE.NPS) THEN
            CRATE=0.
            DO 450 M=1,MNSP
              CRATE=CRATE+COL(L,M)*NSMP/CS(1,N,L)
450         CONTINUE
            WRITE (3,99005,REC=MNSP*(NPR-1)+L) L,CRATE,TT,TROT,TV
99005       FORMAT (' REC ',I3,4E14.5)
          END IF
500     CONTINUE
600   CONTINUE
*
      IF (MNC.EQ.1) THEN
*--output the distribution function for the vibrational levels
        WRITE (4,*)
        WRITE (4,*) ' LEVEL     FRACTION       THEORY        SAMPLE'
        DO 650 N=1,NM
          L=IPS(N)
          IF (ISPV(L).GT.0) CSDVV(L,IPV(N)+1)=CSDVV(L,IPV(N)+1)+1.D00
650     CONTINUE
        DO 700 L=1,MNSP
          IF (ISPV(L).GT.0) THEN
            DO 660 M=1,100
              DTMP=-(M-1)*SPV(L)/CT(1)
              WRITE (4,99006) M-1,CSDVV(L,M)/CSDVV(L,1),DEXP(DTMP),
     &                        CSDVV(L,M)
660         CONTINUE
99006       FORMAT (' ',I5,2E13.5,F14.1)
          END IF
700     CONTINUE
      END IF
*
      CLOSE (3)
      CLOSE (4)
*
*--check the total energy
      TOTT=0.
      TOTR=0.
      TOTV=0.
      DO 800 N=1,NM
        L=IPS(N)
        TOTT=TOTT+0.5*SP(5,L)*(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
        IF (ISPR(1,L).GT.0) TOTR=TOTR+PR(N)
        IF (ISPV(L).GT.0) TOTV=TOTV+IPV(N)*BOLTZ*SPV(L)
800   CONTINUE
      TOTE=TOTT+TOTR+TOTV
      WRITE (*,*) ' TR, ROT, VIB, TOTAL ENERGY = ',TOTT,TOTR,TOTV,TOTE
*
      RETURN
      END
*   MOVE1.FOR
*
      SUBROUTINE MOVE1
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
*--VCCM defines the components of the centre-of-mass velocity (eqn 2.1)
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
*   DATA0V.FOR
*
*
      SUBROUTINE DATA0V
*
*--defines the data for a particular run of DSMC0V.FOR.
*
      PARAMETER (MNM=100000,MNC=1,MNSC=1,MNSP=1,MNSG=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /GASV  / SPV(MNSP),ISPV(MNSP),SPVM(2,MNSP,MNSP),
     &                SVIB(MNC,MNSP)
      COMMON /SAMP  / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
*
*--Set data (must be consistent with PARAMETER variables)
*
      FND=1.E20
*--FND  is the number densty
      FTMP=5000.
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
      ISPR(1,1)=2
      SPR(1,1,1)=5.
      ISPR(2,1)=0
*--ISPR(1,N) is the number of degrees of freedom of species N
*--SPR(1,N,M) is the constant in the polynomial for the rotational
*--relaxation collision number of species N
*--ISPR(2,N) is 0,1 for constant, polynomial for collision number
      ISPV(1)=1
      SPV(1)=2000.
*--ISPV(N) species N has a vibrational mode
*--SPV(N) is the characteristic vibrational temp.
*
*--one of the following pairs of statements must be commented out
*
*--the following two statements are for the constant vib. relax. rate
*--SPVM(1,N,K) is a constant vib. relax. collision number
*--SPVM(2,N,K) is negative to indicate the constant collision number
*
*--the following two statements specify a collision temperature
*----dependent vibrational relaxation rate
      SPVM(1,1,1)=10.
      SPVM(2,1,1)=100.
*--the constants C1 and C2 in eqn (6.53) are 10. and 100., respectively
      NIS=1
*--NIS is the number of time steps between samples
      NSP=1
*--NSP is the number of samples between restart and output file updates
      NPS=250
*--NPS is the number of updates to reach assumed steady flow
      NPT=1000
*--NPT is the number of file updates to STOP
*
      RETURN
      END

*   DSMC1S.FOR
*
      PROGRAM DSMC1S
*
*--program for the study of shock wave structure
*----flow gradients occur only in the direction of the x axis
*
*--SI units are used throughout
*
*-------------------------DESCRIPTION OF DATA---------------------------
*
*--the following is set in the PARAMETER statement
*--MNC the number of cells
*--(the other PARAMETER variables must be consistent with the data in
*----SUBROUTINE DATA1S, and MNSC can set a default if NSC is not set)
*
*--FVEL the upstream speed
*
*--FTMP the upstream temperature
*
*--FND the upstream number density
*
*--FSP(L) the fraction (by number) of species L in the upstream gas
*----a value is required for each species
*
*--FNUM the number of real mols. represented by each simulated molecule
*
*--DTM the time step over which the motion and collisions are uncoupled
*
*--FCU the fraction of the cells that are in the upstream region
*
*--NSC the number of sub-cells per cell (MNSC must be at least MNC*NSC)
*----this is optional because MNSC/MNC will be set as the default value
*
*--SXB the x coordinate of the upstream boundary (must be negative)
*--DXB the x coordinate of the downstream boundary (must be positive)
*----the `centre of mass' of the shock wave is at the origin
*
*--IFN > 1, the following are required for sampling of distn. function
*
*--NCDF(N) the cell numbers for the IFN dist. function samples
*--VPMIN the minimum parallel molecular velocity component
*--VPMAX the maximum parallel molecular velocity component
*--VNMAX the maximum normal molecular velocity component
*
*--ISPD (required only for gas mixtures) set to 0 if the diameter,
*----viscosity exponent, and VSS scattering parameter for the
*----cross-collisions are to be set to the mean values, or
*----set to 1 if these quantities are to be set as data
*
*--the following data must be repeated for each species (L=1 to MNSP)
*
*--SP(1,L) the reference diameter
*--SP(2,L) the reference temperature
*--SP(3,L) the viscosity temperature power law
*--SP(4,L) the reciprocal of the VSS scattering parameter (1. for VHS)
*--SP(5,L) the molecular mass
*
*--ISP(L) the collision sampling group in which the species lies
*----this must be LE.MNSC (not required if MNSG=1)
*
*--ISPR(1,L) the number of rotational degrees of freedom
*--ISPR(2,L) 0, 1 for constant, polynomial rotational relaxation number
*--ISPR(3,L) 0, 1 for common or collision partner species dependent
*----rotational relaxation rate
*
*--SPR(1,L,K) the constant value, or constant in the polynomial for Zr
*----in a collision of species L with species K
*--the following two items are required only if ISPR(2,L)=1
*--SPR(2,L,K) the coefficient of temperature in the polynomial
*--SPR(3,L,K) the coefficient of temperature squared in the polynomial
*
*--end of data for the individual species
*
*--the following data on the cross-collisions is required only if ISPD=1
*--then only for L.NE.M, but L,M data must be repeated for M,L
*
*--SPM(1,L,M) the reference diameter for species L-M collisions
*--SPM(2,L,M) the reference temperature for species L-M collisions
*--SPM(3,L,M) the viscosity temperature power law for species L-M colls.
*--SPM(4,L,M) the reciprocal of the VSS scattering parameter
*
*--end of species data
*
*--NIS the number of DTM time steps between samplings
*
*--NSP the number of samples between prints
*
*--NPS the number of prints to the assumed start of steady flow
*
*--NPT the number of prints to STOP
*
*-----------------------------------------------------------------------
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*--IFN the number of locations at which the distribution function is
*----sampled, (there is no sampling if IFN=1)
*
*--other variables as defined in DSMC0.FOR
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
*--variables as defined in DSMC0.FOR
*
      DOUBLE PRECISION CSR(MNC,MNSP)
*
*--CSR(M,L) the sum of the rotational energy of species L in cell M
*
      DOUBLE PRECISION CSH(3,MNC,MNSP)
*
*--(CSH(N,M,L) higher order sampling in cell M of species L
*----N=1 sum of u*v
*----n=2 sum of c**2*u
*----n=3 sum of rotl. energy*u
*
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
*--variables as defined in DSMC0R.FOR
*
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
*
*--variables as defined in DSMC0.FOR
*
      COMMON /SAMPR / CSR
*
      COMMON /SAMPH / CSH
*--double precision variables defined above
*
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
*
*--variables as defined in DSMC0.FOR
*
      COMMON /SAMPDF/ SDF(IFN,400,200,MNSP),VPMIN,VPMAX,VNMAX,NCDF(IFN)
*
*--SDF the distribution function sampling cells in the axially-symmetric
*----velocity space
*--VPMIN,VPMAX,VNMAX the velocity space boundaries
*--NCDF(N) the cell for the Nth distribution function sample
*
      COMMON /GEOM1S/ NSC,FCU,SXB,DXB,P(2,MNSP),BME(MNSP),BMR(MNSP),CWS,
     &                CWD,NCU,NCD
*
*--NSC the number of sub-cells per cell
*--FCU the fraction of cells upstream of the origin
*--SXB the location of the upstream boundary
*--DXB the location of the downstream boundary
*--CWS the upstream cell width
*--CWD the downstream cell width
*--BME(L) the number of molecules of species L that enter at each DTM
*--BMR(L) the remainder associated with entry number
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
        CALL INIT1S
*
      ELSE
*
        WRITE (*,*) ' READ THE RESTART FILE'
        OPEN (4,FILE='DSMC1S.RES',STATUS='OLD',FORM='UNFORMATTED')
        READ (4) BME,BMR,BOLTZ,BT,CC,CCG,CG,COL,CS,CSH,CSR,CT,CWD,CWS,
     &           UDN,DDN,DTM,DTMP,DVEL,DXB,FCU,FFP,FND,FNUM,FTMP,FVEL,
     &           IB,IC,IPL,IPS,IR,ISC,ISCG,ISP,ISPR,MOVT,NCDF,NCOL,NIS,
     &           NM,NPS,NSC,NSMP,NPR,NPT,NSP,PI,PP,PR,PV,SDF,SELT,SEPT,
     &           SMN,SP,SPI,SPM,SPR,SXB,TIME,TIMI,VNMAX,VPMAX,VPMIN
        CLOSE (4)
*
      END IF
*
      IF (NQLS.EQ.1) CALL SAMPI1S
*
100   NPR=NPR+1
*
      IF (NPR.LE.NPS) CALL SAMPI1S
*
      DO 200 JJJ=1,NSP
        DO 150 III=1,NIS
          TIME=TIME+DTM
*
          WRITE (*,99001) III,JJJ,NIS,NSP,NM,IDINT(NCOL)
99001     FORMAT (' DSMC1S:- Move',2I5,' of',2I5,I8,' Mols',I14,
     &            ' Colls')
*
          CALL MOVE1S
*
          IF (ABS(NM-NMI).GT.NMM.AND.ISTAB.EQ.1) THEN
            CALL STABIL
            ISTAB=0
*--there can be only one call of STABIL each output cycle
          END IF
*
          CALL INDEXM
*
          CALL COLLMR
*
150     CONTINUE
*
        CALL SAMPLE1S
*
200   CONTINUE
*
      WRITE (*,*) ' WRITING RESTART AND OUTPUT FILES',NPR,'  OF ',NPT
      OPEN (4,FILE='DSMC1S.RES',FORM='UNFORMATTED')
      WRITE (4) BME,BMR,BOLTZ,BT,CC,CCG,CG,COL,CS,CSH,CSR,CT,CWD,CWS,
     &          UDN,DDN,DTM,DTMP,DVEL,DXB,FCU,FFP,FND,FNUM,FTMP,FVEL,IB,
     &          IC,IPL,IPS,IR,ISC,ISCG,ISP,ISPR,MOVT,NCDF,NCOL,NIS,NM,
     &          NPS,NSC,NSMP,NPR,NPT,NSP,PI,PP,PR,PV,SDF,SELT,SEPT,SMN,
     &          SP,SPI,SPM,SPR,SXB,TIME,TIMI,VNMAX,VPMAX,VPMIN
      CLOSE (4)
*
      CALL OUT1S
*
      ISTAB=1
*--the subroutine STABIL is enabled
*
      IF (NPR.LT.NPT) GO TO 100
      STOP
      END
*   INIT1S.FOR
*
      SUBROUTINE INIT1S
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
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
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
      COMMON /SAMPR / CSR
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM1S/ NSC,FCU,SXB,DXB,P(2,MNSP),BME(MNSP),BMR(MNSP),CWS,
     &                CWD,NCU,NCD
      COMMON /CONST / PI,SPI,BOLTZ
      COMMON /SAMPDF/ SDF(IFN,400,200,MNSP),VPMIN,VPMAX,VNMAX,NCDF(IFN)
*
*--set constants
*
      PI=3.141592654
      SPI=SQRT(PI)
      BOLTZ=1.380622E-23
*--set data variables to defalt values that they retain if the data
*----does not reset them to specific values
      NSC=MNSC/MNC
*
      CALL DATA1S
*
*
*--calculate the shock wave parameters
*
      AVM=0.
      AVDF=0.
      DO 100 N=1,MNSP
        AVM=AVM+SP(5,N)*FSP(N)
        AVDF=AVDF+(3.+ISPR(1,N))*FSP(N)
100   CONTINUE
      G=(AVDF+2.)/AVDF
*--G is the specific heat ratio
      WRITE (*,*) ' SPECIFIC HEAT RATIO = ',G
      GM1=G-1.
      GP1=G+1.
      GR=GM1/GP1
      USS=SQRT(G*BOLTZ*FTMP/AVM)
*--USS is the speed of sound
      SMN=FVEL/USS
*--SMN is the shock MACH NUMBER
      WRITE (*,*) ' UPSTREAM SPEED OF SOUND = ',USS
      WRITE (*,*) ' SHOCK MACH NUMBER = ',SMN
*
      UDN=FND*AVM
      SM2=SMN*SMN
      A=GP1*SM2/(2.+GM1*SM2)
      WRITE (*,*) ' DENSITY RATIO = ',A
      DDN=UDN*A
      WRITE (*,*) ' DOWNSTREAM DENSITY = ',DDN
      DVEL=FVEL/A
      WRITE (*,*) ' DOWNSTREAM SPEED = ',DVEL
      B=2.*G*SM2/GP1-GR
      DTMP=FTMP*B/A
      WRITE (*,*) ' DOWNSTREAM TEMPERATURE = ',DTMP
*
*--set additional data on the gas
*
      IF (MNSP.EQ.1) ISPD=0
      DO 200 N=1,MNSP
        DO 150 M=1,MNSP
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
150     CONTINUE
200   CONTINUE
*
*--calculate the upstream mean free path from eqn (4.77)
      FFP=0.
      DO 300 NP=1,MNSP
        SS1=0.
        DO 250 NS=1,MNSP
          SS1=SS1+PI*SP(1,NS)**2*FSP(NS)*FND*(SPM(2,NS,NP)/FTMP)
     &        **(SPM(3,NS,NP)-0.5)*SQRT(1.+SP(5,NP)/SP(5,NS))
250     CONTINUE
        FFP=FFP+FSP(NP)/SS1
300   CONTINUE
      WRITE (*,*) ' UPSTREAM MEAN FREE PATH = ',FFP
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
      DO 400 M=1,MNSP
        DO 350 N=1,MNSP
          COL(M,N)=0.
350     CONTINUE
400   CONTINUE
*
      NCU=FCU*MNC
      NCD=MNC-NCU
      CG(1,1)=SXB
      CWS=(-SXB)/NCU
*--CWS is the uniform upstream cell width
      CWD=DXB/NCD
*
      DO 500 M=1,NCU
        CT(M)=FTMP
*--the macroscopic temperature is set to the freestream temperature
        IF (M.GT.1) CG(1,M)=CG(2,M-1)
        CG(2,M)=CG(1,M)+CWS
        CG(3,M)=CWS
        CC(M)=CWS
*--the flow has unit cross-sectional area
        DO 450 L=1,MNSG
          DO 420 K=1,MNSG
            CCG(2,M,L,K)=RF(0)
            CCG(1,M,L,K)=SPM(1,1,1)*300.*SQRT(FTMP/300.)
420       CONTINUE
450     CONTINUE
*--the maximum value of the (rel. speed)*(cross-section) is set to a
*--reasonable, but low, initial value and will be increased as necessary
500   CONTINUE
*
      DO 600 MM=1,NCD
        M=NCU+MM
        CT(M)=DTMP
*--the macroscopic temperature is set to the freestream temperature
        IF (M.GT.1) CG(1,M)=CG(2,M-1)
        CG(2,M)=CG(1,M)+CWD
        CG(3,M)=CWD
        CC(M)=CWD
*--the flow has unit cross-sectional area
        DO 550 L=1,MNSG
          DO 520 K=1,MNSG
            CCG(2,M,L,K)=0.
            CCG(1,M,L,K)=SPM(1,1,1)*300.*SQRT(DTMP/300.)
520       CONTINUE
550     CONTINUE
*--the maximum value of the (rel. speed)*(cross-section) is set to a
*--reasonable, but low, initial value and will be increased as necessary
600   CONTINUE
*
*--set sub-cells
*
      DO 700 N=1,MNC
        DO 650 M=1,NSC
          L=(N-1)*NSC+M
          ISC(L)=N
650     CONTINUE
700   CONTINUE
*
*
      DO 800 L=1,MNSP
        REM=0
        DO 750 N=1,MNC
          IF (N.LE.NCU) THEN
            FNDS=FND
            TMPS=FTMP
            VELS=FVEL
          ELSE
            FNDS=FND*DDN/UDN
            TMPS=DTMP
            VELS=DVEL
          END IF
          A=FNDS*CC(N)*FSP(L)/FNUM+REM
*--A is the number of simulated molecules of species L in cell N to
*--simulate the required concentrations at a total number density of FND
          VMP=SQRT(2.*BOLTZ*TMPS/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
          IF (N.LT.MNC) THEN
            MM=A
            REM=(A-MM)
*--the remainder REM is carried forward to the next cell
          ELSE
            MM=NINT(A)
          END IF
          IF (MM.GT.0) THEN
            DO 710 M=1,MM
              IF (NM.LT.MNM) THEN
*--round-off error could have taken NM to MNM+1
                NM=NM+1
                IPS(NM)=L
                PP(NM)=CG(1,N)+RF(0)*(CG(2,N)-CG(1,N))
                IPL(NM)=(PP(NM)-CG(1,N))*(NSC-.001)/CG(3,N)+1+NSC*(N-1)
*--species, position, and sub-cell number have been set
                DO 702 K=1,3
                  CALL RVELC(PV(K,NM),A,VMP)
702             CONTINUE
                PV(1,NM)=PV(1,NM)+VELS
*--velocity components have been set
*--set the rotational energy
                IF (ISPR(1,L).GT.0) CALL SROT(PR(NM),TMPS,ISPR(1,L))
              END IF
710         CONTINUE
          END IF
750     CONTINUE
800   CONTINUE
*
      WRITE (*,99001) NM
99001 FORMAT (' ',I6,' MOLECULES')
*
      NMI=NM
*--NMI is the initial number of molecules
      NMM=SQRT(FLOAT(NMI))/4.
*--NMM is the allowed tolerance on the fluctuations
      SDD=FLOAT(NMM)/((DDN-UDN)/(AVM*FNUM))
      WRITE (*,*) ' THE FLUCTUATION LIMIT IS ',NMM,' MOLECULES'
      WRITE (*,*) ' THE NECESSARY DISPLACEMENT IS ',SDD
*
*--calculate the number of molecules that enter at each time step
      DO 900 L=1,MNSP
        VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
        SC=FVEL/VMP
*--SC is the inward directed speed ratio
        IF (ABS(SC).LT.10.1) A=(EXP(-SC*SC)+SPI*SC*(1.+ERF(SC)))
     &                         /(2.*SPI)
        IF (SC.GT.10.) A=SC
        IF (SC.LT.-10.) A=0.
*--A is the non-dimensional flux of eqn (4.22)
        BME(L)=FND*FSP(L)*A*VMP*DTM/FNUM
        WRITE (*,*) ' ENTERING MOLS ',BME
900   CONTINUE
      RETURN
      END
*   MOVE1S.FOR
 
*
      SUBROUTINE MOVE1S
*
*--the NM molecules are moved over the time interval DTM
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM1S/ NSC,FCU,SXB,DXB,P(2,MNSP),BME(MNSP),BMR(MNSP),CWS,
     &                CWD,NCU,NCD
*
      IFT=-1
*--a negative IFT indicates that molecules have not entered at this step
      N=0
100   N=N+1
      IF (N.LE.NM) THEN
        IF (IFT.LT.0) AT=DTM
        IF (IFT.GT.0) AT=RF(0)*DTM
*--the time step is a random fraction of DTM for entering molecules
        MOVT=MOVT+1
        MSC=IPL(N)
        MC=ISC(MSC)
*--MC is the initial cell number
        XI=PP(N)
        IF ((XI+0.00001*CG(3,1)).LT.SXB.OR.
     &   (XI-0.00001*CG(3,MNC)).GT.DXB) THEN
          WRITE (*,*) ' MOL ',N,' OUTSIDE FLOW ',XI
          CALL REMOVE(N)
          GO TO 100
        END IF
        DX=PV(1,N)*AT
        X=XI+DX
*--molecule N at XI is moved by DX to X
        IF (X.LT.SXB) THEN
*--molecule leaves across the upstream boundary
          CALL REMOVE(N)
          GO TO 100
        END IF
        IF (X.GT.DXB) THEN
*--molecule reflects from the moving boundary
          PV(1,N)=2.*DVEL-PV(1,N)
          X=2.*DXB-XI+PV(1,N)*AT
          IF (X.GT.DXB) THEN
*--molecule leaves across the downstream boundary
            CALL REMOVE(N)
            GO TO 100
          END IF
        END IF
*
        IF (X.LT.CG(1,MC).OR.X.GT.CG(2,MC)) THEN
*--the molecule has moved from the initial cell
          IF (X.LT.0.) THEN
            MC=(X-SXB)/CWS+0.99999
          ELSE
            MC=NCU+X/CWD+0.99999
          END IF
          IF (MC.LT.1) MC=1
          IF (MC.GT.MNC) MC=MNC
*--MC is the new cell number (note avoidance of round-off error)
        END IF
        MSC=((X-CG(1,MC))/CG(3,MC))*(NSC-.001)+1+NSC*(MC-1)
*--MSC is the new sub-cell number
        IF (MSC.LT.1) MSC=1
        IF (MSC.GT.MNSC) MSC=MNSC
        IPL(N)=MSC
        PP(N)=X
        GO TO 100
      ELSE IF (IFT.LT.0) THEN
        IFT=1
*--new molecules enter
        CALL ENTER1S
        N=N-1
        GO TO 100
      END IF
*
      RETURN
      END
*   ENTER1S.FOR
*
      SUBROUTINE ENTER1S
*
*--new molecules enter at boundaries
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM1S/ NSC,FCU,SXB,DXB,P(2,MNSP),BME(MNSP),BMR(MNSP),CWS,
     &                CWD,NCU,NCD
      COMMON /CONST / PI,SPI,BOLTZ
*
      DO 100 L=1,MNSP
*--consider each species in turn
        A=BME(L)+BMR(L)
        M=A
        BMR(L)=A-M
*--M molecules enter, remainder has been reset
        IF (M.GT.0) THEN
          VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
          SC=FVEL/VMP
          FS1=SC+SQRT(SC*SC+2.)
          FS2=0.5*(1.+SC*(2.*SC-FS1))
* the above constants are required for the entering distn. of eqn (12.5)
          DO 20 K=1,M
            IF (NM.LT.MNM) THEN
              NM=NM+1
*--NM is now the number of the new molecule
              QA=3.
              IF (SC.LT.-3.) QA=ABS(SC)+1.
5             U=-QA+2.*QA*RF(0)
*--U is a potential normalized thermal velocity component
              UN=U+SC
*--UN is a potential inward velocity component
              IF (UN.LT.0.) GO TO 5
              A=(2.*UN/FS1)*EXP(FS2-U*U)
              IF (A.LT.RF(0)) GO TO 5
*--the inward normalized vel. component has been selected (eqn (12.3))
              PV(1,NM)=UN*VMP
              CALL RVELC(PV(2,NM),PV(3,NM),VMP)
*--a single call of RVELC generates the two normal velocity components
              IF (ISPR(1,L).GT.0) CALL SROT(PR(NM),FTMP,ISPR(1,L))
              PP(NM)=SXB+0.001*CG(3,1)
*--the molecule is moved just off the boundary
              IPS(NM)=L
              IPL(NM)=1
            ELSE
              WRITE (*,*) 
     &' WARNING: EXCESS MOLECULE LIMIT - RESTART WITH AN INCREASED FNUM'
            END IF
20        CONTINUE
        END IF
100   CONTINUE
      RETURN
      END
*   REMOVE.FOR
      SUBROUTINE REMOVE(N)
*
*--remove molecule N and replace it by molecule NM
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
*
      PP(N)=PP(NM)
      DO 100 M=1,3
        PV(M,N)=PV(M,NM)
100   CONTINUE
      PR(N)=PR(NM)
      IPL(N)=IPL(NM)
      IPS(N)=IPS(NM)
      NM=NM-1
      N=N-1
      RETURN
      END
*   STABIL.FOR
 
*
      SUBROUTINE STABIL
*
*--the shock location is stabilised against possible random walk effects
*----by not allowing the fluctuation in NM to exceed NMM
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM1S/ NSC,FCU,SXB,DXB,P(2,MNSP),BME(MNSP),BMR(MNSP),CWS,
     &                CWD,NCU,NCD
*
      IF (NM.GT.NMI+NMM) THEN
        WRITE (*,*) ' POSITIVE DISPLACEMENT'
        N=0
50      N=N+1
        IF (N.LE.NM) THEN
          IF (IPL(N).LT.0) GO TO 50
          X=PP(N)+SDD
*--the molecules are displaced through +SDD
          IF (X.GT.DXB) THEN
*--those now beyond the downstream boundary are removed
            CALL REMOVE(N)
            GO TO 50
          END IF
          IF (PP(N).LT.SXB+SDD.AND.NM.LT.MNM) THEN
*--those that were within SDD of the upstream boundary are duplicated
            NM=NM+1
            PP(NM)=PP(N)
            DO 60 M=1,3
              PV(M,NM)=PV(M,N)
60          CONTINUE
            PR(NM)=PR(N)
            IPL(NM)=-IPL(N)
            IPS(NM)=IPS(N)
          END IF
          PP(N)=X
          GO TO 50
        END IF
      ELSE
        WRITE (*,*) ' NEGATIVE DISPLACEMENT'
        N=0
100     N=N+1
        IF (N.LE.NM) THEN
          IF (IPL(N).LT.0) GO TO 100
          X=PP(N)-SDD
*--the molecules are displaced through -SDD
          IF (X.LT.SXB) THEN
*--those now beyond the downstream boundary are removed
            CALL REMOVE(N)
            GO TO 100
          END IF
          IF (PP(N).GT.DXB-SDD.AND.NM.LT.MNM) THEN
*--those that were within SDD of the downstream boundary are duplicated
            NM=NM+1
            PP(NM)=PP(N)
            DO 110 M=1,3
              PV(M,NM)=PV(M,N)
110         CONTINUE
            PR(NM)=PR(N)
            IPL(NM)=-IPL(N)
            IPS(NM)=IPS(N)
          END IF
          PP(N)=X
          GO TO 100
        END IF
      END IF
      DO 200 N=1,NM
        IF (IPL(N).LT.0) IPL(N)=-IPL(N)
200   CONTINUE
      RETURN
      END
*   SAMPI1S.FOR
*
      SUBROUTINE SAMPI1S
*
*--initialises all the sampling variables
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSH(3,MNC,MNSP)
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
      COMMON /SAMPR / CSR
      COMMON /SAMPH / CSH
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /SAMPDF/ SDF(IFN,400,200,MNSP),VPMIN,VPMAX,VNMAX,NCDF(IFN)
*
      NSMP=0
      TIMI=TIME
      DO 100 L=1,MNSP
        DO 50 N=1,MNC
          CS(1,N,L)=1.E-6
          DO 20 M=2,7
            CS(M,N,L)=0.
20        CONTINUE
          DO 40 M=1,3
            CSH(M,N,L)=0.
40        CONTINUE
          CSR(N,L)=0.
50      CONTINUE
100   CONTINUE
      DO 200 K=1,IFN
        DO 150 L=1,MNSP
          DO 120 N=1,400
            DO 110 M=1,200
              SDF(K,N,M,L)=0.
110         CONTINUE
120       CONTINUE
150     CONTINUE
200   CONTINUE
      RETURN
      END
*   SAMPLE1S.FOR
*
      SUBROUTINE SAMPLE1S
*
*--sample the molecules in the flow.
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSH(3,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
      COMMON /SAMPR / CSR
      COMMON /SAMPH / CSH
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /SAMPDF/ SDF(IFN,400,200,MNSP),VPMIN,VPMAX,VNMAX,NCDF(IFN)
*
*--sample the flow properties in the cells
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
              CSQ=0.
              DO 5 LL=1,3
                CS(LL+1,N,I)=CS(LL+1,N,I)+PV(LL,M)
                CS(LL+4,N,I)=CS(LL+4,N,I)+PV(LL,M)**2
                CSQ=CSQ+PV(LL,M)**2
5             CONTINUE
              CSR(N,I)=CSR(N,I)+PR(M)
              CSH(1,N,I)=CSH(1,N,I)+PV(1,M)*PV(2,M)
              CSH(2,N,I)=CSH(2,N,I)+CSQ*PV(1,M)
              CSH(3,N,I)=CSH(3,N,I)+PR(M)*PV(1,M)
10          CONTINUE
          END IF
50      CONTINUE
100   CONTINUE
      IF (IFN.GT.1) THEN
*--sample the distribution function
        DO 150 KK=1,IFN
          N=NCDF(KK)
          DO 120 NN=1,MNSG
            L=IC(2,N,NN)
            IF (L.GT.0) THEN
              DO 105 J=1,L
                K=IC(1,N,NN)+J
                M=IR(K)
                I=IPS(M)
                VP=PV(1,M)
                VN=SQRT(PV(2,M)**2+PV(3,M)**2)
                IF (VP.LE.VPMIN) VP=VPMIN+0.01
                IF (VP.GE.VPMAX) VP=VPMAX-0.01
                IF (VN.GE.VPMAX) VN=VNMAX-0.01
                NVP=400.*(VP-VPMIN)/(VPMAX-VPMIN)+1.
                NVN=200.*VN/VNMAX+1.
                SDF(KK,NVP,NVN,I)=SDF(KK,NVP,NVN,I)+1.
105           CONTINUE
            END IF
120       CONTINUE
150     CONTINUE
      END IF
      RETURN
      END
*   OUT1S.FOR
*
*
      SUBROUTINE OUT1S
*
*--output a progressive set of results to file DSMC1S.OUT.
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSH(3,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
      COMMON /SAMPR / CSR
      COMMON /SAMPH / CSH
      COMMON /GEOM1S/ NSC,FCU,SXB,DXB,P(2,MNSP),BME(MNSP),BMR(MNSP),CWS,
     &                CWD,NCU,NCD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /CONST / PI,SPI,BOLTZ
      COMMON /SAMPDF/ SDF(IFN,400,200,MNSP),VPMIN,VPMAX,VNMAX,NCDF(IFN)
 
*
      DIMENSION CP(8,MNC)
*--CP(N,M) the non-dimensional properties in each cell
*----N=1 the x coordinate
*----N=2 the density
*----N=3 the speed
*----N=4 the temperature
*----N=5 the translational temperature
*----N=6 the rotational temperature
      DOUBLE PRECISION VEL(3),SMU(3),SVEL(3,MNC),SN,SM,SMCC,SRDF,SRE,TT,
     &                 TROT,DBOLTZ,SUV,SCCU,SRU,SUU
      DBOLTZ=BOLTZ
*
      OPEN (4,FILE='DSMC1S.OUT',FORM='FORMATTED')
*
      WRITE (4,*) ' FLOW SAMPLED FROM TIME ',TIMI,' TO TIME ',TIME
      WRITE (4,*) ' COLLISIONS:-'
      WRITE (4,99001) ((IDINT(COL(M,L)),M=1,MNSP),L=1,MNSP)
99001 FORMAT (5I12)
      WRITE (4,*) ' TOTAL NUMBER OF SAMPLES ',NSMP
      WRITE (4,*) NM,' MOLECULES'
      WRITE (4,*) MOVT,' TOTAL MOLECULAR MOVES'
      IF (NCOL.GT.0) THEN
        WRITE (4,*) INT(SELT),' SELECTIONS ',INT(NCOL),
     &              ' COLLISION EVENTS, RATIO  ',REAL(NCOL/SELT)
        IF (NCOL.GT.0) WRITE (4,*) ' MEAN COLLISION SEPARATION ',
     &                             REAL(SEPT/NCOL)
      END IF
*
*
      WRITE (4,*) 'SAMPLES'
      WRITE (4,*) ' CELL     N SP 1    N SP 2     ETC '
      DO 100 N=1,MNC
        WRITE (4,99002) N,(IDINT(CS(1,N,L)),L=1,MNSP)
100   CONTINUE
99002 FORMAT (' ',I6,5I9)
*
      WRITE (4,*) ' FLOWFIELD PROPERTIES'
      WRITE (4,*) 
     &'  CELL   X COORD     DENSITY   TR TEMP  ROT TEMP   OV TEMP      U
     &         V           W   SHEAR STRESS    HEAT FLUX <nn> <uu> etc '
*--first the mixture properties
      DO 300 N=1,MNC
        A=FNUM/(CC(N)*NSMP)
        SN=0.
        SM=0.
        DO 150 K=1,3
          SMU(K)=0.
150     CONTINUE
        SMCC=0.
        SRE=0.
        SRDF=0.
        SUV=0.
        SCCU=0.
        SRU=0.
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
          SUU=SUU+SP(5,L)*CS(5,N,L)
*--SUU is the sum of m*u*u
          SUV=SUV+SP(5,L)*CSH(1,N,L)
*--SUV is the sum of m*u*v
          SCCU=SCCU+SP(5,L)*CSH(2,N,L)
*--SCCU is the sum of m*c**2*u
          SRU=SRU+CSH(3,N,L)
*--SRU is the sum of rotl. energy * u
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
        IF (SRDF.GT.1.E-6) TROT=(2.D00/DBOLTZ)*SRE/SRDF
*--TROT is the rotational temperature, see eqn (11.11)
        TEMP=(3.D00*TT+(SRDF/SN)*TROT)/(3.+SRDF/SN)
*--TEMP is the overall temperature, see eqn (11.12)
        CT(N)=TEMP
        TXY=-DENN*SUV/SN
*--TXY is the xy component of shear stress (see eqn (12.13))
        QX=DENN*(0.5*SCCU-SUV*VEL(2)-(SUU-SM*VEL(1)**2+0.5*SMCC+SRE)
     &     *VEL(1)+SRU)/SN
*--QX is the x component of the heat flux vector (see eqn (12.14))
        XC=0.5*(CG(1,N)+CG(2,N))
*--XC is the x coordinate of the midpoint of the cell
*--set the normalized values
        CP(1,N)=XC/FFP
        CP(2,N)=(DEN-UDN)/(DDN-UDN)
        CP(3,N)=(VEL(1)-FVEL)/(DVEL-FVEL)
        CP(4,N)=(TEMP-FTMP)/(DTMP-FTMP)
        CP(5,N)=(TT-FTMP)/(DTMP-FTMP)
        CP(6,N)=(TROT-FTMP)/(DTMP-FTMP)
        WRITE (4,99003) N,XC,DEN,TT,TROT,TEMP,VEL(1),VEL(2),VEL(3),TXY,
     &                  QX
99003   FORMAT (' ',I5,F10.4,1P,E12.4,0P,6F10.4,2E12.4)
300   CONTINUE
*
      WRITE (4,*)
      DO 400 L=1,MNSP
*--now the properties of the separate species
        WRITE (4,*) ' SPECIES ',L
        WRITE (4,*) 
     &' CELL   X COORD      N DENS     DENSITY     TTX       TTY       T
     &TZ    TR TEMP   ROT TEMP    TEMP   U DIF VEL V DIF VEL W DIF VEL '
        DO 350 N=1,MNC
          A=FNUM/(CC(N)*NSMP)
          DENN=CS(1,N,L)*A
*--DENN is the partial number density
          DEN=SP(5,L)*DENN
*--DEN is the partial density, see eqn (1.13)
          DO 320 K=1,3
            VEL(K)=CS(K+1,N,L)/CS(1,N,L)
*--VEL defines the average velocity of the species L molecules
320       CONTINUE
          UU=VEL(1)**2+VEL(2)**2+VEL(3)**2
          TTX=(SP(5,L)/DBOLTZ)*(CS(5,N,L)/CS(1,N,L)-VEL(1)**2)
          TTY=(SP(5,L)/DBOLTZ)*(CS(6,N,L)/CS(1,N,L)-VEL(2)**2)
          TTZ=(SP(5,L)/DBOLTZ)*(CS(7,N,L)/CS(1,N,L)-VEL(3)**2)
*--the component temperatures are based on eqn (1.30)
          TT=(SP(5,L)/(3.D00*DBOLTZ))
     &       *((CS(5,N,L)+CS(6,N,L)+CS(7,N,L))/CS(1,N,L)-UU)
*--TT is the translational temperature, see eqn (1.29)
          IF (ISPR(1,L).GT.0) THEN
            TROT=2.D00*CSR(N,L)/(ISPR(1,L)*DBOLTZ*CS(1,N,L))
          ELSE
            TROT=0.
          END IF
*--TROT is the rotational temperature, see eqn (11.10)
          TEMP=(3.D00*TT+ISPR(1,L)*TROT)/(3.+ISPR(1,L))
          DO 340 K=1,3
            VEL(K)=VEL(K)-SVEL(K,N)
*--VEL now defines the diffusion velocity of species L, see eqn (1,45)
340       CONTINUE
          XC=0.5*(CG(1,N)+CG(2,N))
          WRITE (4,99004) N,XC,DENN,DEN,TTX,TTY,TTZ,TT,TROT,TEMP,VEL(1),
     &                    VEL(2),VEL(3)
99004     FORMAT (' ',I5,F9.4,1P,2E12.4,0P,9F10.4)
350     CONTINUE
400   CONTINUE
*
      WRITE (4,*) ' NORMALIZED VALUES '
      WRITE (4,*) 
     &' CELL   X/MFP        DENSITY      VELOCITY     TEMP         TRANS
     &TEMP    ROT TEMP '
      DO 500 N=1,MNC
        WRITE (4,99005) N,(CP(M,N),M=1,6)
99005   FORMAT (' ',I5,1P,6E13.4)
500   CONTINUE
*
      CLOSE (4)
*
      IF (IFN.GT.1) THEN
        OPEN (3,FILE='DSMCDF.OUT',FORM='UNFORMATTED')
        WRITE (3) SDF
        CLOSE (3)
      END IF
 
*
      RETURN
      END
*   SROT.FOR
*
      SUBROUTINE SROT(PR,TEMP,IDF)
*--selects a typical equuilibrium value of the rotational energy PR at
*----the temperature TEMP in a gas with IDF rotl. deg. of f.
*
      COMMON /CONST / PI,SPI,BOLTZ
 
      IF (IDF.EQ.2) THEN
        PR=-LOG(RF(0))*BOLTZ*TEMP
*--for 2 degrees of freedom, the sampling is directly from eqn (11.22)
      ELSE
*--otherwise apply the acceptance-rejection method to eqn (11.23)
        A=0.5*IDF-1.
50      ERM=RF(0)*10.
*--the cut-off internal energy is 10 kT
        B=((ERM/A)**A)*EXP(A-ERM)
        IF (B.LT.RF(0)) GO TO 50
        PR=ERM*BOLTZ*TEMP
      END IF
      RETURN
      END
*   ERF.FOR
*
      FUNCTION ERF(S)
*
*--calculates the error function of S
*
      B=ABS(S)
      IF (B.GT.4.) THEN
        D=1.
      ELSE
        C=EXP(-B*B)
        T=1./(1.+0.3275911*B)
        D=1.-(0.254829592*T-0.284496736*T*T+1.421413741*T*T*T-
     &    1.453152027*T*T*T*T+1.061405429*T*T*T*T*T)*C
      END IF
      IF (S.LT.0.) D=-D
      ERF=D
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
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
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
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS / CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /CONST / PI,SPI,BOLTZ
      COMMON /ELAST / VRC(3),VRR,VR,L,M,LS,MS,CVR,MM,NN,N
*
      K=INT(RF(0)*(IC(2,N,NN)-0.001))+IC(1,N,NN)+1
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
      K=INT(RF(0)*(ISCG(2,MSC,MM)-0.001))+ISCG(1,MSC,MM)+1
      M=IR(K)
      IF (L.EQ.M) GO TO 100
*--choose a new second molecule if the first is again chosen
*
      DO 200 K=1,3
        VRC(K)=PV(K,L)-PV(K,M)
200   CONTINUE
*--VRC(1 to 3) are the components of the relative velocity
      VRR=VRC(1)**2+VRC(2)**2+VRC(3)**2
      IF (VRR.LT.1.E-8) VRR=1.E-8
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
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
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
*--VCCM defines the components of the centre-of-mass velocity, eqn (2.1)
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
*--B is the cosine of the deflection angle for the VSS model, eqn (11.8)
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
*   COLLMR.FOR
*
      SUBROUTINE COLLMR
*
*--calculates collisions appropriate to DTM in a gas mixture
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
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
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
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
                  IF (RF(0).LT.CVR/CCG(1,N,NN,MM).AND.VR.GT.1.E-3) THEN
*--the collision is accepted with the probability of eqn (11.6)
                    NCOL=NCOL+1
                    SEPT=SEPT+ABS(PP(L)-PP(M))
                    COL(LS,MS)=COL(LS,MS)+1.D00
                    COL(MS,LS)=COL(MS,LS)+1.D00
*
                    IF (ISPR(1,LS).GT.0.OR.ISPR(1,MS).GT.0) CALL INELR
*--bypass rotational redistribution if both molecules are monatomic
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
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
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
*   DATA1S.FOR
*
      SUBROUTINE DATA1S
*
*--defines the data for a particular run of DSMC1S.FOR.
*
      PARAMETER (MNM=20000,MNC=300,MNSC=1800,MNSP=1,MNSG=1,IFN=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1S/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,TIMI,NPR,NSMP,FFP,
     &                FND,FTMP,FVEL,SMN,DVEL,DTMP,UDN,DDN,FSP(MNSP),
     &                ISPD,NMM,NMI,SDD
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /SAMPDF/ SDF(IFN,400,200,MNSP),VPMIN,VPMAX,VNMAX,NCDF(IFN)
      COMMON /GEOM1S/ NSC,FCU,SXB,DXB,P(2,MNSP),BME(MNSP),BMR(MNSP),CWS,
     &                CWD,NCU,NCD
*
*--set data (must be consistent with PARAMETER variables)
*
      FTMP=293.
*--FTMP is the temperature
      FND=1.E20
*--FND is the number densty
      FVEL=446.11
*--FVEL is the upstream speed
      FSP(1)=1.
*--FSP(N) is the number fraction of species N
      FNUM=0.4E16
*--FNUM  is the number of real molecules represented by a simulated mol.
      DTM=0.75E-6
*--DTM is the time step
      SXB=-0.3
      DXB=0.3
*--the flowfield is from SXB to DXB
      FCU=0.5001
*--FCU is the fraction of cells in the upstream region
*
      SP(1,1)=4.092E-10
      SP(2,1)=293.
      SP(3,1)=0.81
      SP(4,1)=0.6015
      SP(5,1)=6.64E-26
*--SP(1,N) is the molecular diameter of species N
*--SP(2,N) is the reference temperature
*--SP(3,N) is the viscosity-temperatire index
*--SP(4,N) is the reciprocal of the VSS scattering parameter
*--SP(5,N) is the molecular mass of species N
      ISPR(1,1)=0
      SPR(1,1,1)=0.
      ISPR(2,1)=0
*--ISPR(1,N) is the number of degrees of freedom of species N
*--SPR(1,N,L) is the constant in the polynomial for the rotational
*--relaxation collision number of species N
*--ISPR(2,N) is 0,1 for constant, polynomial for collision number
      ISP(1)=1
      NSC=6
      NIS=4
*--NIS is the number of time steps between samples
      NSP=20
*--NSP is the number of samples between restart and output file updates
      NPS=100
*--NPS is the number of updates to reach assumed steady flow
      NPT=10000
*--NPT is the number of file updates to STOP
*
      RETURN
      END

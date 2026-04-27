*   DSMC1U.FOR
*
      PROGRAM DSMC1U
*
*--general one-dimensional unsteady flow program
*---an ensemble average is made over a number of runs
*----includes options for cylindrical and spherical flows
*----flow gradients occur only in the direction of the x axis
*----the x axis becomes the radius in cylindrical and spherical cases
*------the origin is then at x=0
*----the axis of a cylindrical flow is along the z axis
*----in plane flows, there may be a velocity in the y direction
*----in cylindrical flow, there may be a circumferential velocity
*----the is an `inner' (smaller x) and `outer' (larger x) boundary
*----each boundary is one of five types
*-----1: an axis or centre (it must then be at x=0)
*-----2: a plane of symmetry or a specularly reflecting surface
*-----3: a solid surface
*-----4: a stream boundary
*-----5: a vacuum
*-----6: a specularly reflecting piston
*----the cell widths may be either uniform or in geometric progression
*
*--SI units are used throughout
*
*-------------------------DESCRIPTION OF DATA---------------------------
*
*--the following is set in the PARAMETER statement
*--MNC the number of cells
*--(the other PARAMETER variables must be consistent with the data in
*----SUBROUTINE DATA1U, and MNSC can set a default if NSC is not set)
*
*--IFX set to 0, 1, or 2 for plane, cylindrical, or spherical flow
*
*--IIS 0 if the initial state is a vacuum, 1 if it is a uniform stream,
*----2 for a uniform gradient between two surfaces, or
*----3 if the initial state is part uniform gas and part vacuum
*
*--if IIS=3 the following variable must be set
*--IVS if positive, the gas is initially in cells 1 to IVS, or
*----if negative, the gas is initially in cells IVS to MNC
*
*--IFC set to 0 or 1 for uniform or non-uniform cell widths
*---if IFC=1, set CWR as the ratio of the cell width at the outer
*----boundary to that at the inner boundary (default 0)
*
*--FTMP the stream temperature if IIS=1 or 3, or a temp. characteristic
*----of the flow otherwise (of FTMP is not set for IIS= 0 or 2, the
*----default value of 273 is used to set the initial value of CCG(1
*
*--FND the initial number density for IIS=1 or 3,
*----the mean value for IIS=2,or need not be set for IIS=0
*
*--FSP(L) the fraction (by number) of species L in the initial stream
*----a value is requred for each species, but need not be set for IIS=0
*
*--FNUM the number of real mols. represented by each simulated molecule
*
*--DTM the time step over which the motion and collisions are uncoupled
*
*--NSC the number of sub-cells per cell (MNSC must be at least MNC*NSC)
*----this is optional because MNSC/MNC will be set as the default value
*
*--the following data is required for each boundary
*----K=1 for the inner boundary (lower value of x)
*----K=2 for the outer boundary (higher value of x)
*
*--XBI(K) the initial x coordinate of the boundary
*----(must be positive if IFX>1)
*
*--IB(K) the type code of the boundary
*
*--no further data on the boundary is required if:-
*----IB(K)=1 for an axis or centre (valid for IFX= 1 or 2, and XBI(K)=0)
*----IB(K)=2 for a plane of symmetry (if IFX=1) or a specularly
*------reflecting surface (valid for all IFX values)
*----IB(K)=5 for an interface with a vacuum
*
*--if IB(K)=3 (a solid surface) the following are required:-
*--BT(K) the temperature of the surface (diffuse reflection)
*--BVY(K) the velovity in the y direction (not valid for IFX=2)
*
*--if IB(K)=4 (an interface with an external stream) the reqd. data is:-
*--BFND(K) the number density of the stream
*--BFTMP(K) the temperature
*--BVFX(K) the x (and only) component of the stream velocity
*--BFSP(K,L) the number fraction of species L in the stream
*----a value of BFSP is required for each species
*
*--if IB(K)=6 (a specularly reflecting piston) the reqd. data is:-
*--PVEL(K) the speed of the piston
*
*--end of the boundary data
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
*--NRT the number of ensemble averages to STOP
*
*-----------------------------------------------------------------------
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
*--MNOI the number of output intervals in a run
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
      DOUBLE PRECISION CSS(8,2,MNSP)
*
*--CSS(N,M,L) sampled info. on the molecules striking the boundaries
*----M=1, 2 for the inner, outer boundaries; L is the species
*----N=1 the number sum
*----N=2 the sum of the normal momentum of the incident molecules
*----N=3 the sum of the normal momentum for the reflected molecules
*----N=4 the sum of the incident parallel momentum in the y direction
*----N=5 the sum of the incident translational energy
*----N=6 the sum of the reflected translational energy
*----N=7 the sum of the incident rotational energy
*----N=8 the sum of the reflected rotational energy
*
      DOUBLE PRECISION CSP(7,MNC,MNSP),CSRP(MNC,MNSP),CSSP(8,2,MNSP)
*
      COMMON /PRSAM / CSP,CSRP,CSSP
*
*--above variables refer to the previous samples
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
*
*--variables as defined in DSMC0.FOR
*
      COMMON /MOLSR / PR(MNM)
*
*--PR(M) is the rotational energy of molecule M
*
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
*
*--variables as defined in DSMC1.FOR
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
*
*--variables as defined in DSMC0.FOR
*
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
*
*--variables as defined in DSMC0R.FOR
*
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
*
*--ISPD 0,1 for default, full data on cross-collisions
*--LRCS the length in bytes of a single record in the file DSMC1U.DAT
*--NSAI the code number of the sampling interval
*--other variables as defined in DSMC0.FOR
*
      COMMON /SAMPR / CSR
*
      COMMON /SAMPS / CSS
*
*--double precision variables defined above
*
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
*
*--NRT the maximum number of runs for the ensemble average
*--other variables as defined in DSMC0.FOR
*
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
*
*--IFX 0, 1, or 2 for plane, cylindrical, or spherical flow
*--IIS 0, 1, 2, or 3 if the initial flow is a vacuum, uniform stream,
*----a uniform gradient between the values at two solid surfaces, or
*----if the flow is part gas and part vacuum
*--NSC the number of sub-cells per cell
*--XBI(N) N=1, 2 the initial location of the inner, outer boundary
*--XB(N) N=1, 2 the location of the inner, outer boundary
*--IB(N) N=1, 2 the type code for the inner, outer boundary
*--no further data is needed if IB=1, 2, or 5
*--if IB=3 (solid surface), the following info. is needed (N as above)
*--BT(N) the temperature of the surface
*--BVY(N) the y velocity component (valid for IFX= 0 or 1)
*--if IB=4 (external gas stream), the following info. (N as above)
*--BFND(N) the number density of the external stream
*--BFTMP(N) the temperature
*--BVFX(N) the x component of the velocity
*--BFSP(N,L) the fraction of species L in the stream
*--the following are non-data variables that can apply for IB=3, or 4
*--BME(N,L) the number of molecules of species L that enter at each DTM
*--BMR(N,L) the remainder associated with entry number
*--if IB=6 (piston) the following is required as data
*--PVEL(N) the piston speed
*--CCO(MNC) coordinate of cell
*
      COMMON /CONST / PI,SPI,BOLTZ
*
*--variables as defined in DSMC0.FOR
*
      LRCS=4+12*MNC+64*(MNC*MNSP+2*MNSP)
*--LRCS is the number of bytes in one record of the sampling file
*
      WRITE (*,*) ' INPUT 1 FOR COMPLETELY NEW CALCULATION,'
      WRITE (*,*) ' OR 0 TO INCREASE AN EXISTING ENSEMBLE AVERAGE :- '
      READ (*,*) NQLS
*
      IF (NQLS.EQ.1) THEN
        CALL SAMPIU
      ELSE
        OPEN (3,FILE='DSMC1U.DAT',ACCESS='DIRECT',RECL=LRCS)
        READ (3,REC=1) NRUN(1),(((CSP(LL,I,J),LL=1,7),I=1,MNC),J=1,MNSP)
     &                 ,((CSRP(I,J),I=1,MNC),J=1,MNSP),(CCD(I),I=1,MNC),
     &                 (CD(I),I=1,MNC),
     &                 (((CSSP(LL,I,J),LL=1,8),I=1,2),J=1,MNSP)
        CLOSE (3)
      END IF
*
100   WRITE (*,*) 'COMMENCING RUN ',NRUN(1)+1
*
      CALL INIT1U
 
*
      DO 200 NSAI=1,MNOI
        WRITE (*,*) ' SAMPLING INTERVAL ',NSAI
*
        OPEN (3,FILE='DSMC1U.DAT',ACCESS='DIRECT',RECL=LRCS)
        READ (3,REC=NSAI) NRUN(NSAI),
     &                    (((CSP(LL,I,J),LL=1,7),I=1,MNC),J=1,MNSP),
     &                    ((CSRP(I,J),I=1,MNC),J=1,MNSP),
     &                    (CCD(I),I=1,MNC),(CD(I),I=1,MNC),
     &                    (((CSSP(LL,I,J),LL=1,8),I=1,2),J=1,MNSP)
        CLOSE (3)
*
        CALL SAMPI1U
*
        DO 150 III=1,NIS
          TIME=TIME+DTM
*
          WRITE (*,99001) III,NIS,NM,IDINT(NCOL)
99001     FORMAT (' DSMC1U:- Move',I5,' of',I5,I8,' Mols',I14,' Colls')
*
          IF (IB(1).EQ.6.OR.IB(2).EQ.6) CALL MOVP
*
          CALL MOVE1U
*
          CALL INDEXM
*
          CALL COLLMR
*
150     CONTINUE
*
        CALL SAMPLE1U
*
200   CONTINUE
*
      CALL OUT1U
*
      IF (NRUN(1).LT.NRT) GO TO 100
      STOP
      END
*   INIT1U.FOR
*
      SUBROUTINE INIT1U
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
 
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /SAMPR / CSR
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
      COMMON /CONST / PI,SPI,BOLTZ
*
*--set constants
*
      PI=3.141592654
      SPI=SQRT(PI)
      BOLTZ=1.380622E-23
*
      N=NRUN(1)
*--generate random numbers to ensure that restarts do not repeat runs
      DO 100 M=1,N
        FND=RF(0)
100   CONTINUE
 
*--set data variables to defalt values that they retain if the data
*----does not reset them to specific values
      PVEL(1)=0.
      PVEL(2)=0.
      NSC=MNSC/MNC
      FND=0.
      FTMP=273.
      IFC=0
      DO 200 N=1,2
        IB(N)=5
        BT(N)=0.
        BVY(N)=0.
        BFND(N)=0.
        BFTMP(N)=0.
        BVFX(N)=0.
        DO 150 L=1,MNSP
          ISP(L)=1
          FSP(L)=0.
          BFSP(N,L)=0.
          BME(N,L)=0.
          BMR(N,L)=0.
150     CONTINUE
200   CONTINUE
*
      CALL DATA1U
*
*--set additional data on the gas
*
      XB(1)=XBI(1)
      XB(2)=XBI(2)
      IF (MNSP.EQ.1) ISPD=0
      DO 300 N=1,MNSP
        DO 250 M=1,MNSP
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
250     CONTINUE
300   CONTINUE
*
*--initialise variables
*
      TIME=0.
      NM=0
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
      FW=XB(2)-XB(1)
      CG(1,1)=XB(1)
      IF (IFC.EQ.0) THEN
        CW=FW/MNC
*--CW is the uniform cell width
      ELSE
        RP=CWR**(1./(MNC-1.))
*--RP is the ratio in the geometric progression
        AP=(1.-RP)/(1.-RP**MNC)
*--AP is the first term of the progression
      END IF
      DO 500 M=1,MNC
        CT(M)=FTMP
*--the macroscopic temperature is set to the freestream temperature
        IF (M.GT.1) CG(1,M)=CG(2,M-1)
        IF (IFC.EQ.0) THEN
          CG(2,M)=CG(1,M)+CW
        ELSE
          CG(2,M)=CG(1,M)+FW*AP*RP**(M-1)
        END IF
        CG(3,M)=CG(2,M)-CG(1,M)
        CCO(M)=0.5*(CG(2,M)+CG(1,M))
        IF (IFX.EQ.0) CC(M)=CG(3,M)
*--a plane flow has unit cross-sectional area
        IF (IFX.EQ.1) CC(M)=PI*(CG(2,M)**2-CG(1,M)**2)
*--a cylindrical flow has unit length in the axial direction
        IF (IFX.EQ.2) CC(M)=(4./3.)*PI*(CG(2,M)**3-CG(1,M)**3)
*--a spherical flow occupies the full sphere
        DO 450 L=1,MNSG
          DO 420 K=1,MNSG
            CCG(2,M,L,K)=RF(0)
            CCG(1,M,L,K)=SPM(1,1,1)*300.*SQRT(FTMP/300.)
420       CONTINUE
450     CONTINUE
*--the maximum value of the (rel. speed)*(cross-section) is set to a
*--reasonable, but low, initial value and will be increased as necessary
500   CONTINUE
      IF (IFC.EQ.1) THEN
        AP=(1.-RP)/AP
        RP=LOG(RP)
*--AP and RP are now convenient terms in eqn (12.1)
      END IF
*
*--set sub-cells
*
      DO 600 N=1,MNC
        DO 550 M=1,NSC
          L=(N-1)*NSC+M
          ISC(L)=N
550     CONTINUE
600   CONTINUE
*
      IF (IIS.GT.0) THEN
*--if IIS=1 generate initial gas with temperature FTMP,
*--if IIS=2 generate initial gas as a uniform gradient between two, or
*--if IIS=3 generate part gas and part vacuum
*----surfaces (valid only if IB(1)=3 and IB(2)=3)
*
        IF (IIS.EQ.2.AND.(IB(1).NE.3.OR.IB(2).NE.3)) THEN
          WRITE (*,*) ' IIS=2 IS AN ILLEGAL OPTION IN THIS CASE '
          STOP
        END IF
        DO 650 L=1,MNSP
          REM=0
          IF (IIS.EQ.1.OR.IIS.EQ.3) VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
          DO 620 N=1,MNC
            IF (IIS.EQ.2) THEN
              PROP=(N-0.5)/FLOAT(MNC)
              VELS=BVY(1)+PROP*(BVY(2)-BVY(1))
              TMPS=BT(1)+PROP*(BT(2)-BT(1))
              FNDS=FND*0.5*(BT(1)+BT(2))/TMPS
              VMP=SQRT(2.*BOLTZ*TMPS/SP(5,L))
            ELSE
              FNDS=FND
              TMPS=FTMP
            END IF
            A=FNDS*CC(N)*FSP(L)/FNUM+REM
            IF (IIS.EQ.3) THEN
              IF (IVS.GT.0.AND.N.GT.IVS) A=0.
              IF (IVS.LT.0.AND.N.LT.ABS(IVS)) A=0.
            END IF
*--A is the number of simulated molecules of species L in cell N to
*--simulate the required concentrations at a total number density of FND
            IF (IFX.GT.0) A=0.5*A
*--For cylindrical and spherical flows, molecules are generated in pairs
*----such that there is zero initial momentum in the axial direction
            IF (N.LT.MNC) THEN
              MM=A
              REM=(A-MM)
*--the remainder REM is carried forward to the next cell
              IF (IFX.GT.0) REM=2.*REM
            ELSE
              MM=NINT(A)
            END IF
            IF (MM.GT.0) THEN
              DO 605 M=1,MM
                IF (NM.LT.MNM) THEN
*--round-off error could have taken NM to MNM+1
                  NM=NM+1
                  IPS(NM)=L
                  IF (IFX.EQ.0) PP(NM)=CG(1,N)+RF(0)*(CG(2,N)-CG(1,N))
                  IF (IFX.EQ.1) PP(NM)=SQRT(CG(1,N)**2+RF(0)*(CG(2,N)**2
     &                                 -CG(1,N)**2))
                  IF (IFX.EQ.2) PP(NM)=(CG(1,N)**3+RF(0)*(CG(2,N)**3-CG(
     &                                 1,N)**3))**0.3333333
                  IPL(NM)=(PP(NM)-CG(1,N))*(NSC-.001)/CG(3,N)
     &                    +1+NSC*(N-1)
*--species, position, and sub-cell number have been set
                  DO 602 K=1,3
                    CALL RVELC(PV(K,NM),A,VMP)
602               CONTINUE
                  IF (IIS.EQ.2) PV(2,NM)=PV(2,NM)+VELS
*--velocity components have been set
*--set the rotational energy
                  IF (ISPR(1,L).GT.0) CALL SROT(PR(NM),TMPS,ISPR(1,L))
*--now generate the paired molecule
                  IF (IFX.GT.0) THEN
                    NM=NM+1
                    IPS(NM)=IPS(NM-1)
                    PP(NM)=PP(NM-1)
                    PV(1,NM)=-PV(1,NM-1)
                    PV(2,NM)=PV(2,NM-1)
                    PV(3,NM)=PV(3,NM-1)
                    IF (ISPR(1,L).GT.0) PR(NM)=PR(NM-1)
                  END IF
                END IF
605           CONTINUE
            END IF
620       CONTINUE
650     CONTINUE
      END IF
*
      WRITE (*,99001) NM
99001 FORMAT (' ',I6,' MOLECULES')
*
*--calculate the number of molecules that enter at each time step
      DO 700 N=1,2
        IF (IB(N).EQ.4) THEN
*--the entry molecules are from an external stream
          DO 660 L=1,MNSP
            VMP=SQRT(2.*BOLTZ*BFTMP(N)/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
            IF (N.EQ.1) SC=BVFX(N)/VMP
            IF (N.EQ.2) SC=-BVFX(N)/VMP
*--SC is the inward directed speed ratio
            IF (ABS(SC).LT.10.1) A=(EXP(-SC*SC)+SPI*SC*(1.+ERF(SC)))
     &                             /(2.*SPI)
            IF (SC.GT.10.) A=SC
            IF (SC.LT.-10.) A=0.
*--A is the non-dimensional flux of eqn (4.22)
            IF (IFX.EQ.1) A=A*2.*PI*XB(N)
            IF (IFX.EQ.2) A=A*4.*PI*XB(N)**2
            BME(N,L)=BFND(N)*BFSP(N,L)*A*VMP*DTM/FNUM
            WRITE (*,*) ' entering mols ',BME(N,L)
660       CONTINUE
        END IF
700   CONTINUE
      RETURN
      END
*   MOVP.FOR
 
*
      SUBROUTINE MOVP
*
*--move any pistion type boundaries through DTM and recalculate cells
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSS(8,2,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /SAMPS / CSS
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
      COMMON /CONST / PI,SPI,BOLTZ
*
      DO 100 K=1,2
        IF (IB(K).EQ.6) XB(K)=XB(K)+DTM*PVEL(K)
100   CONTINUE
      FW=XB(2)-XB(1)
      CG(1,1)=XB(1)
      IF (IFC.EQ.0) THEN
        CW=FW/MNC
*--CW is the uniform cell width
      ELSE
        RP=CWR**(1./(MNC-1.))
*--RP is the ratio in the geometric progression
        AP=(1.-RP)/(1.-RP**MNC)
*--AP is the first term of the progression
      END IF
      DO 200 M=1,MNC
        IF (M.GT.1) CG(1,M)=CG(2,M-1)
        IF (IFC.EQ.0) THEN
          CG(2,M)=CG(1,M)+CW
        ELSE
          CG(2,M)=CG(1,M)+FW*AP*RP**(M-1)
        END IF
        CG(3,M)=CG(2,M)-CG(1,M)
        CCO(M)=0.5*(CG(2,M)+CG(1,M))
        IF (IFX.EQ.0) CC(M)=CG(3,M)
*--a plane flow has unit cross-sectional area
        IF (IFX.EQ.1) CC(M)=PI*(CG(2,M)**2-CG(1,M)**2)
*--a cylindrical flow has unit length in the axial direction
        IF (IFX.EQ.2) CC(M)=(4./3.)*PI*(CG(2,M)**3-CG(1,M)**3)
*--a spherical flow occupies the full sphere
200   CONTINUE
      IF (IFC.EQ.1) THEN
        AP=(1.-RP)/AP
        RP=LOG(RP)
*--AP and RP are now convenient terms in eqn (12.1)
      END IF
*--the cells have now been reset for the new boundary locations
*
      RETURN
      END
*   MOVE1U.FOR
 
*
      SUBROUTINE MOVE1U
*
*--the NM molecules are moved over the time interval DTM
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSS(8,2,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /SAMPS / CSS
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
      COMMON /CONST / PI,SPI,BOLTZ
*
      IFT=-1
*--a negative IFT indicates that molecules have not entered at this step
      N=0
100   N=N+1
      IF (N.LE.NM) THEN
        IF (IFT.LT.0) AT=DTM
        IF (IFT.GT.0) AT=RF(0)*DTM
*--the time step is a random fraction of DTM for entering molecules
150     MOVT=MOVT+1
        MSC=IPL(N)
        MC=ISC(MSC)
*--MC is the initial cell number
        XI=PP(N)
        DX=PV(1,N)*AT
        IF (IFX.GT.0) DY=PV(2,N)*AT
        IF (IFX.EQ.2) DZ=PV(3,N)*AT
        X=XI+DX
        IF (IFX.NE.0) THEN
*--cylindrical or spherical flow
*--first check for inner boundary interactions
          IF (IB(1).NE.1.AND.IB(1).NE.6) THEN
*--there can be no interaction with an axis or centre,
*----and a moving piston boundary is treated separately
            IF (X.LT.XB(1)) THEN
              CALL RBC(IFX,XI,DX,DY,DZ,XB(1),S1)
              IF (S1.LT.1..OR.XI.LT.XB(1)) THEN
*--collision with inner boundary
                IF (IB(1).GT.3) THEN
*--molecule leaves flow
                  CALL REMOVE(N)
                  GO TO 100
                END IF
                DX=S1*DX
                DY=S1*DY
                DZ=S1*DZ
                CALL AIFX(IFX,XI,DX,DY,DZ,XC,PV(1,N),PV(2,N),PV(3,N))
*--the frame of reference has been rotated with regard to the point of
*----intersection with the inner surface
                IF (IB(1).EQ.2) THEN
*--specular reflection from the boundary
                  PV(1,N)=-PV(1,N)
                  PP(N)=XB(1)+0.001*CG(3,1)
                  AT=AT*(1.-S1)
                  GO TO 150
                END IF
                IF (IB(1).EQ.3) THEN
*--molecule reflects from the surface
                  CALL REFLECT1(N,1)
*--AT is the remaining in the time step for this molecule
                  AT=AT*(1.-S1)
                  GO TO 150
                END IF
              END IF
            END IF
          END IF
          IF (IB(1).EQ.6.AND.X.LT.XB(1)) THEN
*--calculate possible specular reflection from a moving piston
            RI=XB(1)-PVEL(1)*AT
*--RI is the piston location at the beginning of the time step
            CALL RBCP(IFX,XI,DX,DY,DZ,XB(1),S1,RI)
            IF (S1.LT.1.) THEN
*--there is a collision with the piston
              DX=S1*DX
              DY=S1*DY
              DZ=S1*DZ
              CALL AIFX(IFX,XI,DX,DY,DZ,XC,PV(1,N),PV(2,N),PV(3,N))
*--the frame of reference has been rotated with regard to the point of
*----intersection with the inner surface
              PV(1,N)=2.*PVEL(1)-PV(1,N)
              RC=RI+S1*PVEL(1)*AT
              PP(N)=RC+0.001*CG(3,1)
              AT=AT*(1.-S1)
              GO TO 150
            END IF
          END IF
          RR=X*X+DY*DY+(IFX-1)*DZ*DZ
          IF (RR.GT.XB(2)*XB(2)) THEN
*--interaction with the outer boundary
            CALL RBC(IFX,XI,DX,DY,DZ,XB(2),S1)
            IF (S1.LT.1..OR.XI.GT.XB(2)) THEN
*--collision with outer boundary
              IF (IB(2).GT.3.AND.IB(2).NE.6) THEN
*--molecule leaves flow
                CALL REMOVE(N)
                GO TO 100
              END IF
              DX=S1*DX
              DY=S1*DY
              DZ=S1*DZ
              CALL AIFX(IFX,XI,DX,DY,DZ,XC,PV(1,N),PV(2,N),PV(3,N))
*--the frame of reference has been rotated with regard to the point of
*----intersection with the outer surface
              IF (IB(2).EQ.2) THEN
*--specular reflection from the boundary
                PV(1,N)=-PV(1,N)
                PP(N)=XB(2)-0.001*CG(3,MNC)
                AT=AT*(1.-S1)
                GO TO 150
              END IF
              IF (IB(2).EQ.3) THEN
*--molecule reflects from the surface
                CALL REFLECT1(N,2)
*--AT is the remaining in the time step for this molecule
                AT=AT*(1.-S1)
                GO TO 150
              END IF
*
              IF (IB(2).EQ.6) THEN
*--calculate possible specular reflection from a moving piston
                RI=XB(2)-PVEL(2)*AT
*--RI is the piston location at the beginning of the time step
                CALL RBCP(IFX,XI,DX,DY,DZ,XB(2),S1,RI)
*
                IF (S1.LT.1.) THEN
                  DX=S1*DX
                  DY=S1*DY
                  DZ=S1*DZ
                  CALL AIFX(IFX,XI,DX,DY,DZ,XC,PV(1,N),PV(2,N),PV(3,N))
*--the frame of reference has been rotated with regard to the point of
*----intersection with the inner surface
*--there is a collision with the piston
                  PV(1,N)=2.*PVEL(2)-PV(1,N)
                  RC=RI+S1*PVEL(2)*AT
                  PP(N)=RC-0.005*CG(3,MNC)
                  AT=AT*(1.-S1)
                  GO TO 150
                END IF
              END IF
            END IF
          END IF
*--calculate the end of the trajectory
          CALL AIFX(IFX,XI,DX,DY,DZ,X,PV(1,N),PV(2,N),PV(3,N))
          IF (X-XB(1).LT.0.001*CG(3,1)) X=XB(1)+0.001*CG(3,1)
          IF (XB(2)-X.LT.0.001*CG(3,MNC)) X=XB(2)-0.001*CG(3,MNC)
        END IF
        IF (IFX.EQ.0) THEN
*  plane flow
*--molecule N at XI is moved by DX to X
          IF (X.LT.XB(1).OR.X.GT.XB(2)) THEN
            IF (X.LT.XB(1)) K=1
            IF (X.GT.XB(2)) K=2
*--intersection with inner, outer boundary for K=1, 2
            IF (IB(K).EQ.2) THEN
*--specular reflection from the boundary (eqn (11.7))
              X=2.*XB(K)-X
              PV(1,N)=-PV(1,N)
            END IF
            IF (IB(K).GT.3) THEN
              IF (IB(K).NE.6) THEN
*--molecule leaves flow
                CALL REMOVE(N)
                GO TO 100
              ELSE
*--collision with piston
                RI=XB(K)-PVEL(K)*AT
*--RI is the piston position at the start of the time interval
                S=(RI-XI)/((PV(1,N)-PVEL(K))*AT)
*--S is the fraction of trajectory element to the intersection
                XC=XI+S*AT*PV(1,N)
*--XC is the point of collision
                PV(1,N)=2.*PVEL(K)-PV(1,N)
                X=XC+(1.-S)*AT*PV(1,N)
              END IF
            END IF
            IF (IB(K).EQ.3) THEN
*--AT is the remaining in the time step for this molecule
              AT=AT-(XB(K)-XI)/PV(1,N)
*--molecule reflects from the surface
              CALL REFLECT1(N,K)
              GO TO 150
            END IF
*--no boundary interactions
          END IF
        END IF
*
        IF (X.LT.CG(1,MC).OR.X.GT.CG(2,MC)) THEN
*--the molecule has moved from the initial cell
          IF (IFC.EQ.0) THEN
            MC=(X-XB(1))/CW+0.99999
          ELSE
            XD=(X-XB(1))/FW+1.E-6
            MC=1.+(LOG(1.-XD*AP))/RP
*--the cell number is calculated from eqn (12.1)
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
        IF (X.LT.XB(1).OR.X.GT.XB(2)) THEN
          WRITE (*,*) ' MOL ',N,' OUTSIDE FLOW ',X
          CALL REMOVE(N)
          GO TO 100
        END IF
        PP(N)=X
        GO TO 100
      ELSE IF (IFT.LT.0) THEN
        IFT=1
*--new molecules enter
        CALL ENTER6
        N=N-1
        GO TO 100
      END IF
      RETURN
      END
*   ENTER6.FOR
*
      SUBROUTINE ENTER6
*
*--new molecules enter at boundaries
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
      COMMON /CONST / PI,SPI,BOLTZ
*
      DO 100 N=1,2
*--consider each boundary in turn
        DO 50 L=1,MNSP
*--consider each species in turn
          A=BME(N,L)+BMR(N,L)
          M=A
          BMR(N,L)=A-M
*--M molecules enter, remainder has been reset
          IF (M.GT.0) THEN
            VMP=SQRT(2.*BOLTZ*BFTMP(N)/SP(5,L))
            IF (ABS(BVFX(N)).GT.1.E-6) THEN
              IF (N.EQ.1) SC=BVFX(N)/VMP
              IF (N.EQ.2) SC=-BVFX(N)/VMP
              FS1=SC+SQRT(SC*SC+2.)
              FS2=0.5*(1.+SC*(2.*SC-FS1))
            END IF
* the above constants are required for the entering distn. of eqn (12.5)
            DO 10 K=1,M
              IF (NM.LT.MNM) THEN
                NM=NM+1
*--NM is now the number of the new molecule
                IF (ABS(BVFX(N)).GT.1.E-6) THEN
                  QA=3.
                  IF (SC.LT.-3.) QA=ABS(SC)+1.
2                 U=-QA+2.*QA*RF(0)
*--U is a potential normalised thermal velocity component
                  UN=U+SC
*--UN is a potential inward velocity component
                  IF (UN.LT.0.) GO TO 2
                  A=(2.*UN/FS1)*EXP(FS2-U*U)
                  IF (A.LT.RF(0)) GO TO 2
*--the inward normalised vel. component has been selected (eqn (12.5))
                  IF (N.EQ.1) PV(1,NM)=UN*VMP
                  IF (N.EQ.2) PV(1,NM)=-UN*VMP
                ELSE
                  IF (N.EQ.1) PV(1,NM)=SQRT(-LOG(RF(0)))*VMP
                  IF (N.EQ.2) PV(1,NM)=-SQRT(-LOG(RF(0)))*VMP
*--for a stationary external gas, use eqn (12.3)
                END IF
                CALL RVELC(PV(2,NM),PV(3,NM),VMP)
*--a single call of RVELC generates the two normal velocity components
                IF (ISPR(1,L).GT.0) CALL SROT(PR(NM),BFTMP(N),ISPR(1,L))
                IF (N.EQ.1) PP(NM)=XB(1)+0.001*CG(3,1)
                IF (N.EQ.2) PP(NM)=XB(2)-0.001*CG(3,MNC)
*--the molecule is moved just off the boundary
                IPS(NM)=L
                IF (N.EQ.1) IPL(NM)=1
                IF (N.EQ.2) IPL(NM)=MNSC
              ELSE
                WRITE (*,*) 
     &' WARNING: EXCESS MOLECULE LIMIT - RESTART WITH AN INCREASED FNUM'
              END IF
10          CONTINUE
          END IF
50      CONTINUE
100   CONTINUE
      RETURN
      END
*   REFLECT1.FOR
      SUBROUTINE REFLECT1(N,K)
*
*--diffuse reflection of molecule N from surface K
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSS(8,2,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /SAMPS / CSS
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
      COMMON /CONST / PI,SPI,BOLTZ
*
      L=IPS(N)
*--sample the surface properies due to the incident molecules
      CSS(1,K,L)=CSS(1,K,L)+1.
      IF (K.EQ.1) CSS(2,K,L)=CSS(2,K,L)-SP(5,L)*PV(1,N)
      IF (K.EQ.2) CSS(2,K,L)=CSS(2,K,L)+SP(5,L)*PV(1,N)
      CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*(PV(2,N)-BVY(K))
      CSS(5,K,L)=CSS(5,K,L)+0.5*SP(5,L)
     &           *(PV(1,N)**2+(PV(2,N)-BVY(K))**2+PV(3,N)**2)
      CSS(7,K,L)=CSS(7,K,L)+PR(N)
*
      VMP=SQRT(2.*BOLTZ*BT(K)/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
      IF (K.EQ.1) PV(1,N)=SQRT(-LOG(RF(0)))*VMP
      IF (K.EQ.2) PV(1,N)=-SQRT(-LOG(RF(0)))*VMP
*--the normal velocity component has been generated (eqn(12.3))
      CALL RVELC(PV(2,N),PV(3,N),VMP)
*--a single call of RVELC generates the two parallel velocity components
      PV(2,N)=PV(2,N)+BVY(K)
      IF (ISPR(1,L).GT.0) CALL SROT(PR(N),BT(K),ISPR(1,L))
      IF (K.EQ.1) PP(N)=XB(1)+0.001*CG(3,1)
      IF (K.EQ.2) PP(N)=XB(2)-0.001*CG(3,MNC)
*--the molecule is moved just off the boundary
      IF (K.EQ.1) IPL(N)=1
      IF (K.EQ.2) IPL(N)=MNSC
*--sample the surface properties due to the reflected molecules
      IF (K.EQ.1) CSS(3,K,L)=CSS(3,K,L)+SP(5,L)*PV(1,N)
      IF (K.EQ.2) CSS(3,K,L)=CSS(3,K,L)-SP(5,L)*PV(1,N)
      CSS(6,K,L)=CSS(6,K,L)-0.5*SP(5,L)
     &           *(PV(1,N)**2+(PV(2,N)-BVY(K))**2+PV(3,N)**2)
      CSS(8,K,L)=CSS(8,K,L)-PR(N)
      RETURN
      END
*   REMOVE.FOR
      SUBROUTINE REMOVE(N)
*
*--remove molecule N and replace it by molecule NM
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
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
*   SAMPIU.FOR
*
      SUBROUTINE SAMPIU
*
*--initialises the ensemble averaged sampling variables
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSS(8,2,MNSP)
      DOUBLE PRECISION CSP(7,MNC,MNSP),CSRP(MNC,MNSP),CSSP(8,2,MNSP)
*
      COMMON /PRSAM / CSP,CSRP,CSSP
*
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /SAMPR / CSR
      COMMON /SAMPS / CSS
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
*
      OPEN (3,FILE='DSMC1U.DAT',ACCESS='DIRECT',RECL=LRCS)
      DO 100 K=1,MNOI
        NRUN(K)=0
        DO 50 L=1,MNSP
          DO 20 N=1,MNC
            CS(1,N,L)=1.E-6
            CSP(1,N,L)=1.E-6
            DO 10 M=2,7
              CS(M,N,L)=0.
              CSP(M,N,L)=0.
10          CONTINUE
            CSR(N,L)=0.
            CSRP(N,L)=0.
20        CONTINUE
          DO 40 N=1,2
            CSS(1,N,L)=1.E-6
            CSSP(1,N,L)=1.E-6
            DO 30 M=2,8
              CSS(M,N,L)=0.
              CSSP(M,N,L)=0.
30          CONTINUE
40        CONTINUE
50      CONTINUE
        WRITE (3,REC=K) NRUN(K),(((CS(LL,I,J),LL=1,7),I=1,MNC),J=1,MNSP)
     &                  ,((CSR(I,J),I=1,MNC),J=1,MNSP),(CCO(I),I=1,MNC),
     &                  (CC(I),I=1,MNC),
     &                  (((CSS(LL,I,J),LL=1,8),I=1,2),J=1,MNSP)
100   CONTINUE
      CLOSE (3)
      RETURN
      END
*   SAMPI1U.FOR
*
      SUBROUTINE SAMPI1U
*
*--initialises the sampling variables for a samplling interval
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSS(8,2,MNSP)
*
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /SAMPR / CSR
      COMMON /SAMPS / CSS
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
*
      DO 200 L=1,MNSP
        DO 50 N=1,MNC
          CS(1,N,L)=1.E-6
          DO 20 M=2,7
            CS(M,N,L)=0.
20        CONTINUE
          CSR(N,L)=0.
50      CONTINUE
        DO 100 N=1,2
          CSS(1,N,L)=1.E-6
          DO 60 M=2,8
            CSS(M,N,L)=0.
60        CONTINUE
100     CONTINUE
200   CONTINUE
      RETURN
      END
*   SAMPLE1U.FOR
*
      SUBROUTINE SAMPLE1U
*
*--sample the molecules in the flow.
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSS(8,2,MNSP)
      DOUBLE PRECISION CSP(7,MNC,MNSP),CSRP(MNC,MNSP),CSSP(8,2,MNSP)
*
      COMMON /PRSAM / CSP,CSRP,CSSP
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /SAMPR / CSR
      COMMON /SAMPS / CSS
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
*
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
*
      OPEN (3,FILE='DSMC1U.DAT',ACCESS='DIRECT',RECL=LRCS)
      READ (3,REC=NSAI) NRUN(NSAI),
     &                  (((CSP(LL,I,J),LL=1,7),I=1,MNC),J=1,MNSP),
     &                  ((CSRP(I,J),I=1,MNC),J=1,MNSP),(CCD(I),I=1,MNC),
     &                  (CD(I),I=1,MNC),
     &                  (((CSSP(LL,I,J),LL=1,8),I=1,2),J=1,MNSP)
*
      NRUN(NSAI)=NRUN(NSAI)+1
      DO 300 J=1,MNSP
        DO 150 I=1,MNC
          CSRP(I,J)=CSRP(I,J)+CSR(I,J)
          DO 120 LL=1,7
            CSP(LL,I,J)=CSP(LL,I,J)+CS(LL,I,J)
120       CONTINUE
150     CONTINUE
        DO 200 I=1,2
          DO 160 LL=1,8
            CSSP(LL,I,J)=CSSP(LL,I,J)+CSS(LL,I,J)
160       CONTINUE
200     CONTINUE
300   CONTINUE
*
      WRITE (3,REC=NSAI) NRUN(NSAI),
     &                   (((CSP(LL,I,J),LL=1,7),I=1,MNC),J=1,MNSP),
     &                   ((CSRP(I,J),I=1,MNC),J=1,MNSP),(CCO(I),I=1,MNC)
     &                   ,(CC(I),I=1,MNC),
     &                   (((CSSP(LL,I,J),LL=1,8),I=1,2),J=1,MNSP)
      CLOSE (3)
      RETURN
      END
*   OUT1U.FOR
*
*
      SUBROUTINE OUT1U
*
*--output a progressive set of results to file DSMC1U.OUT.
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSS(8,2,MNSP)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /SAMPR / CSR
      COMMON /SAMPS / CSS
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
      COMMON /CONST / PI,SPI,BOLTZ
*
      DOUBLE PRECISION VEL(3),SMU(3),SVEL(3,MNC),SN,SM,SMCC,SRDF,SRE,TT,
     &                 TROT,DBOLTZ,SS(8),SUU
      DBOLTZ=BOLTZ
*
      OPEN (4,FILE='DSMC1U.OUT',FORM='FORMATTED')
*
      WRITE (4,*) ' COLLISIONS:-'
      WRITE (4,99001) ((IDINT(COL(M,L)),M=1,MNSP),L=1,MNSP)
99001 FORMAT (5I12)
      WRITE (4,*) NM,' MOLECULES'
      WRITE (4,*) MOVT,' TOTAL MOLECULAR MOVES'
      IF (NCOL.GT.0) THEN
        WRITE (4,*) INT(SELT),' SELECTIONS ',INT(NCOL),
     &              ' COLLISION EVENTS, RATIO  ',REAL(NCOL/SELT)
        IF (NCOL.GT.0) WRITE (4,*) ' MEAN COLLISION SEPARATION ',
     &                             REAL(SEPT/NCOL)
      END IF
*
      DO 300 NSAI=1,MNOI
*
        TIMI=(NSAI-1)*NIS*DTM
        TIME=NSAI*NIS*DTM
*
        OPEN (3,FILE='DSMC1U.DAT',ACCESS='DIRECT',RECL=LRCS)
*
        READ (3,REC=NSAI) NRUN(NSAI),
     &                    (((CS(LL,I,J),LL=1,7),I=1,MNC),J=1,MNSP),
     &                    ((CSR(I,J),I=1,MNC),J=1,MNSP),(CCO(I),I=1,MNC)
     &                    ,(CC(I),I=1,MNC),
     &                    (((CSS(LL,I,J),LL=1,8),I=1,2),J=1,MNSP)
*
        NSMP=NRUN(NSAI)
*
        DO 50 K=1,2
          IF (IB(K).EQ.3) THEN
            IF (K.EQ.1) WRITE (4,*) ' INNER SURFACE PROPERTIES '
            IF (K.EQ.2) WRITE (4,*) ' OUTER SURFACE PROPERTIES '
            WRITE (4,*) ' FROM TIME ',TIMI,' TO TIME ',TIME
            IF (IFX.EQ.0) A=FNUM/(TIME-TIMI)
            IF (IFX.EQ.1) A=FNUM/((TIME-TIMI)*2.*PI*XB(K))
            IF (IFX.EQ.2) A=FNUM/((TIME-TIMI)*4.*PI*XB(K)*XB(K))
            DO 10 N=1,8
              SS(N)=0.
              DO 5 L=1,MNSP
                SS(N)=SS(N)+CSS(N,K,L)
5             CONTINUE
10          CONTINUE
            WRITE (4,*) ' SAMPLE  FRACTION SPECIES 1,  SPECIES 2....'
            WRITE (4,99002) SS(1),(CSS(1,K,L)/SS(1),L=1,MNSP)
99002       FORMAT (F12.1,6F12.6)
            DO 20 N=1,8
              SS(N)=SS(N)*A
20          CONTINUE
            WRITE (4,*) ' NUM FLUX INC PRESS REFL PRESS SHEAR STR '
            WRITE (4,99003) (SS(N),N=1,4)
99003       FORMAT (6E12.5)
            WRITE (4,*) 
     & ' INC TR EN  REFL TR EN  INC ROT EN  REFL ROT EN NET HEAT FLUX  '
            WRITE (4,99003) (SS(N),N=5,8),SS(5)+SS(6)+SS(7)+SS(8)
          END IF
50      CONTINUE
*
        WRITE (4,*) 'SAMPLES'
        WRITE (4,*) ' CELL     N SP 1    N SP 2     ETC '
        DO 100 N=1,MNC
          WRITE (4,99004) N,(IDINT(CS(1,N,L)),L=1,MNSP)
100     CONTINUE
99004   FORMAT (' ',I6,5I9)
*
        WRITE (4,*) ' FLOWFIELD PROPERTIES AT TIME ',TIME
        WRITE (4,*) 
     &'  CELL   X COORD     DENSITY   TR TEMP  ROT TEMP   OV TEMP      U
     &         V           W   SHEAR STRESS    HEAT FLUX'
*--first the mixture properties
        DO 200 N=1,MNC
          A=FNUM/(CC(N)*NSMP)
          SN=0.
          SM=0.
          DO 120 K=1,3
            SMU(K)=0.
120       CONTINUE
          SMCC=0.
          SRE=0.
          SRDF=0.
          DO 140 L=1,MNSP
            SN=SN+CS(1,N,L)
*--SN is the number sum
            SM=SM+SP(5,L)*CS(1,N,L)
*--SM is the sum of molecular masses
            DO 130 K=1,3
              SMU(K)=SMU(K)+SP(5,L)*CS(K+1,N,L)
*--SMU(1 to 3) are the sum of mu, mv, mw
130         CONTINUE
            SMCC=SMCC+(CS(5,N,L)+CS(6,N,L)+CS(7,N,L))*SP(5,L)
*--SMCC is the sum of m(u**2+v**2+w**2)
            SRE=SRE+CSR(N,L)
*--SRE is the sum of rotational energy
            SRDF=SRDF+ISPR(1,L)*CS(1,N,L)
*--SRDF is the sum of the rotational degrees of freedom
            SUU=SUU+SP(5,L)*CS(5,N,L)
*--SUU is the sum of m*u*u
140       CONTINUE
          DENN=SN*A
*--DENN is the number density, see eqn (1.34)
          DEN=DENN*SM/SN
*--DEN is the density, see eqn (1.42)
          DO 160 K=1,3
            VEL(K)=SMU(K)/SM
            SVEL(K,N)=VEL(K)
160       CONTINUE
*--VEL and SVEL are the stream velocity components, see eqn (1.43)
          UU=VEL(1)**2+VEL(2)**2+VEL(3)**2
          TT=(SMCC-SM*UU)/(3.D00*DBOLTZ*SN)
*--TT is the translational temperature, see eqn (1.51)
          IF (SRDF.GT.1.E-6) TROT=(2.D00/DBOLTZ)*SRE/SRDF
*--TROT is the rotational temperature, see eqn (11.11)
          TEMP=(3.D00*TT+(SRDF/SN)*TROT)/(3.+SRDF/SN)
*--TEMP is the overall temperature, see eqn (11.12)
          CT(N)=TEMP
          WRITE (4,99005) N,CCO(N),DEN,TT,TROT,TEMP,(VEL(K),K=1,3)
99005     FORMAT (' ',I5,F10.4,1P,E12.4,0P,6F10.4)
200     CONTINUE
*
        WRITE (4,*)
        DO 250 L=1,MNSP
*--now the properties of the separate species
          WRITE (4,*) ' SPECIES ',L
          WRITE (4,*) 
     &' CELL   X COORD      N DENS     DENSITY     TTX       TTY       T
     &TZ    TR TEMP   ROT TEMP    TEMP   U DIF VEL V DIF VEL W DIF VEL '
          DO 240 N=1,MNC
            A=FNUM/(CC(N)*NSMP)
            DENN=CS(1,N,L)*A
*--DENN is the partial number density
            DEN=SP(5,L)*DENN
*--DEN is the partial density, see eqn (1.13)
            DO 210 K=1,3
              VEL(K)=CS(K+1,N,L)/CS(1,N,L)
*--VEL defines the average velocity of the species L molecules
210         CONTINUE
            UU=VEL(1)**2+VEL(2)**2+VEL(3)**2
            TTX=(SP(5,L)/DBOLTZ)*(CS(5,N,L)/CS(1,N,L)-VEL(1)**2)
            TTY=(SP(5,L)/DBOLTZ)*(CS(6,N,L)/CS(1,N,L)-VEL(2)**2)
            TTZ=(SP(5,L)/DBOLTZ)*(CS(7,N,L)/CS(1,N,L)-VEL(3)**2)
*--the component temperatures are based on eqn (1.30)
            TT=(SP(5,L)/(3.D00*DBOLTZ))
     &         *((CS(5,N,L)+CS(6,N,L)+CS(7,N,L))/CS(1,N,L)-UU)
*--TT is the translational temperature, see eqn (1.29)
            IF (ISPR(1,L).GT.0) THEN
              TROT=2.D00*CSR(N,L)/(ISPR(1,L)*DBOLTZ*CS(1,N,L))
            ELSE
              TROT=0.
            END IF
*--TROT is the rotational temperature, see eqn (11.10)
            TEMP=(3.D00*TT+ISPR(1,L)*TROT)/(3.+ISPR(1,L))
            DO 220 K=1,3
              VEL(K)=VEL(K)-SVEL(K,N)
*--VEL now defines the diffusion velocity of species L, see eqn (1.45)
220         CONTINUE
            WRITE (4,99006) N,CCO(N),DENN,DEN,TTX,TTY,TTZ,TT,TROT,TEMP,
     &                      VEL(1),VEL(2),VEL(3)
99006       FORMAT (' ',I5,F9.4,1P,2E12.4,0P,9F10.4)
240       CONTINUE
250     CONTINUE
*
300   CONTINUE
*
      CLOSE (3)
      CLOSE (4)
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
*   RBC.FOR
*
      SUBROUTINE RBC(IFX,XI,DX,DY,DZ,R,S)
*--calculates the trajectory fraction S from a point at radius XI with
*----displacements DX, DY, and DZ to a possible intersection with a
*----surface of radius R, IFX=1, 2 for cylindrical, spherical geometry
      DD=DX*DX+DY*DY
      IF (IFX.EQ.2) DD=DD+DZ*DZ
      B=XI*DX/DD
      C=(XI*XI-R*R)/DD
      A=B*B-C
      IF (A.GE.0.) THEN
*--find the least positive solution to the quadratic
        A=SQRT(A)
        S1=-B+A
        S2=-B-A
        IF (S2.LT.0.) THEN
          IF (S1.GT.0.) THEN
            S=S1
          ELSE
            S=2.
          END IF
        ELSE IF (S1.LT.S2) THEN
          S=S1
        ELSE
          S=S2
        END IF
      ELSE
        S=2.
*--setting S to 2 indicates that there is no intersection
      END IF
      RETURN
      END
*   RBCP.FOR
*
      SUBROUTINE RBCP(IFX,XI,DX,DY,DZ,R,S,RI)
*--calculates the trajectory fraction S from a point at radius XI with
*----displacements DX, DY, and DZ to a possible intersection with a
*----moving piston, IFX=1, 2 for cylindrical, spherical geometry
*----VP is the piston velocity and it moves from RI to R in time AT
      DD=DX*DX+DY*DY
      IF (IFX.EQ.2) DD=DD+DZ*DZ
      DR=R-RI
      B=(XI*DX-RI*DR)/DD
      AD=1.-DR*DR/DD
      C=(XI*XI-RI*RI)/DD
      A=B*B-C*AD
      IF (A.GE.0.) THEN
*--find the least positive solution to the quadratic
        A=SQRT(A)
        S1=(-B+A)/AD
        S2=(-B-A)/AD
        IF (S2.LT.0.) THEN
          IF (S1.GT.0.) THEN
            S=S1
          ELSE
            S=2.
          END IF
        ELSE IF (S1.LT.S2) THEN
          S=S1
        ELSE
          S=S2
        END IF
      ELSE
        S=2.
*--setting S to 2 indicates that there is no intersection
      END IF
      RETURN
      END
*   AIFX.FOR
*
      SUBROUTINE AIFX(IFX,XI,DX,DY,DZ,X,U,V,W)
*--calculates the new radius and realigns the velocity components in
*----cylindrical (IFX=1) and spherical (IFX=2) flows
      IF (IFX.EQ.1) THEN
        DR=DY
        VR=V
      ELSE IF (IFX.EQ.2) THEN
        DR=SQRT(DY*DY+DZ*DZ)
        VR=SQRT(V*V+W*W)
      END IF
      A=XI+DX
      X=SQRT(A*A+DR*DR)
      S=DR/X
      C=A/X
      B=U
      U=B*C+VR*S
      V=-B*S+VR*C
      IF (IFX.EQ.2) THEN
        VR=V
        A=6.2831853*RF(0)
        V=VR*SIN(A)
        W=VR*COS(A)
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
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
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
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
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
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
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
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSP(7,MNC,MNSP),CSRP(MNC,MNSP),CSSP(8,2,MNSP)
*
      COMMON /PRSAM / CSP,CSRP,CSSP
*
      COMMON /MOLS  / NM,PP(MNM),PV(3,MNM),IPL(MNM),IPS(MNM),IR(MNM)
      COMMON /MOLSR / PR(MNM)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /SAMPR / CSR
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
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
              IF (ISP(K).EQ.MM) SN=SN+CSP(1,N,K)
10          CONTINUE
            IF (SN.GT.1.) THEN
              AVN=SN/FLOAT(NRUN(NSAI))
            ELSE
              AVN=IC(2,N,MM)
            END IF
*
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
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
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
*   DATA1U.FOR
*
      SUBROUTINE DATA1U
*
*--defines the data for a particular run of DSMC1U.FOR.
*
      PARAMETER (MNM=10000,MNC=100,MNSC=1000,MNSP=1,MNSG=1,MNOI=50)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP1U/ COL,NCOL,MOVT,SELT,SEPT,CS,TIME,FND,FTMP,TIMI,
     &                FSP(MNSP),ISPD,NRUN(MNOI),LRCS,NSAI
      COMMON /COMP1U/ FNUM,DTM,NIS,NRT
      COMMON /GEOM1U/ IFX,NSC,XB(2),IB(2),BT(2),BVY(2),BFND(2),BFTMP(2),
     &                BVFX(2),BFSP(2,MNSP),BME(2,MNSP),BMR(2,MNSP),IIS,
     &                IVS,PVEL(2),CW,FW,XBI(2),CCO(MNC),CCD(MNC)
      COMMON /CELLS1U/ CC(MNC),CG(3,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                 CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG)
     &                 ,IFC,CWR,AP,RP,CD(MNC)
*
*--set data (must be consistent with PARAMETER variables)
*
      IFX=0
      IFC=0
      IIS=1
      FTMP=273.
*--FTMP is the temperature
      FND=1.E20
*--FND is the number densty
      FSP(1)=1.
*--FSP(N) is the number fraction of species N
      FNUM=1.0001E16
*--FNUM  is the number of real molecules represented by a simulated mol.
      DTM=1.E-6
*--DTM is the time step
      XBI(1)=0.0
      XBI(2)=1.0
      IB(1)=6
      PVEL(1)=2285.5
      IB(2)=2
      SP(1,1)=4.17E-10
      SP(2,1)=273.
      SP(3,1)=0.81
      SP(4,1)=1.0
      SP(5,1)=6.63E-26
*--SP(1,N) is the molecular diameter of species N
*--SP(2,N) is the reference temperature
*--SP(3,N) is the viscosity-temperatire index
*--SP(4,N) is the reciprocal of the VSS scattering parameter
*--SP(5,N) is the molecular mass of species N
      ISPR(1,1)=0
      SPR(1,1,1)=0.
      ISPR(2,1)=0
*--ISPR(1,N) is the number of degrees of freedom of species N
*--SPR(1,N,K) is the constant in the polynomial for the rotational
*--relaxation collision number of species N
*--ISPR(2,N) is 0,1 for constant, polynomial for collision number
      NIS=4
*--NIS is the number of time steps between samples
      NRT=10000
*--NRT is the number of ensemble averages to STOP
*
      RETURN
      END

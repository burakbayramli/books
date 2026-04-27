*   DSMC0S.FOR
*
      PROGRAM DSMC0S
*
*--test of collision procedures in a uniform simple gas
*
*--SI units are used throughout
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400)
*
*--MNM  is the maximum number of molecules
*--MNC  is the maximum number of sub-cells
*--MNSC is the maximum number of sub-cells
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
*--NCOL is the total number of collisions
*--MOVT the total number of molecular moves
*--SELT the total number of pair selections
*--SEPT the sum of collision pair separations
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
*
*--NM is the number of molecules
*--PP(M)  is the x coordinate molecule M
*--PV(1 to 3,M)  u,v,w velocity components of molecule M
*--IP(M)  sub-cell number of molecule M
*--IR(M)  cross-reference array (molecule numbers in order of sub-cells)
*
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
*
*--CC(M) is the cell volume
*--CCG(N,M) is for collisions in cell M
*----N=1 is the maximum value of (relative speed)*(coll. cross-section)
*----N=2 is the remainder when the selection number is rounded
*--CG(N,M) is the geometry related information on cell M
*----N=1 the minimum x coordinate
*----N=2 the maximum x coordinate
*----N=3 the cell width
*--IC(N,M) information on the molecules in cell M
*----N=1 (start address -1) of the molecule numbers in the array IR
*----N=2 the number of molecules in the cell
*--ISC(M) the cell in which the sub-cell lies
*--ISCG(N,M) is the indexing information on sub-cell M
*----N=1 (start address -1) of the molecule numbers in the array IR
*----N=2 the number of molecules in the sub-cell
*
      COMMON /GASS  / SP(2),SPM(5)
*
*--SP(N) information on species
*----N=1 the molecular mass
*----N=2 the reference diameter of the molecule
*--SPM(N) information on the interaction
*----N=1  the reference value of the collision cross-section
*----N=2  the reference temperature
*----N=3  the viscosity-temperature power law
*----N=4  the reciprocal of the VSS scattering parameter
*----N=5  the Gamma function of (5/2 - viscosity-temperature power law)
*
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP
*
*--CS(N,M) sampled information on cell M
*----N=1 the number in the sample
*----N=2,3,4 the sum of u,v,w
*----N=5 the sum of u*u+v*v+w*w
*--TIME time
*--NPR the number of output/restart file update cycles
*--NSMP the total number of samples
*--FND the stream number density
*--FTMP the stream temperature
*
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
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
        CALL INIT0S
*
      ELSE
*
        WRITE (*,*) ' READ THE RESTART FILE'
        OPEN (4,FILE='DSMC0S.RES',STATUS='OLD',FORM='UNFORMATTED')
        READ (4) BOLTZ,CC,CCG,CG,COL,CS,CW,DTM,FND,FNUM,FTMP,IC,IP,IR,
     &           ISC,ISCG,MOVT,NCOL,NIS,NM,NSC,NSMP,NPR,NPT,NSP,PI,PP,
     &           PV,SELT,SEPT,SP,SPI,SPM,TIME,XF,XR
        CLOSE (4)
*
      END IF
*
      IF (NQL.EQ.1) CALL SAMPI0S
*
100   NPR=NPR+1
*
      DO 200 JJJ=1,NSP
        DO 150 III=1,NIS
          TIME=TIME+DTM
*
          WRITE (*,99001) III,JJJ,NIS,NSP,NCOL
99001   FORMAT (' DSMC0S:- Move ',2I5,'   of ',2I5,F14.0,' Collisions')
*
          CALL MOVE0S
*
          CALL INDEXS
*
          CALL COLLS
*
150     CONTINUE
*
        CALL SAMPLE0S
*
200   CONTINUE
*
      WRITE (*,*) ' WRITING RESTART AND OUTPUT FILES',NPR,'  OF ',NPT
      OPEN (4,FILE='DSMC0S.RES',FORM='UNFORMATTED')
      WRITE (4) BOLTZ,CC,CCG,CG,COL,CS,CW,DTM,FND,FNUM,FTMP,IC,IP,IR,
     &          ISC,ISCG,MOVT,NCOL,NIS,NM,NSC,NSMP,NPR,NPT,NSP,PI,PP,PV,
     &          SELT,SEPT,SP,SPI,SPM,TIME,XF,XR
      CLOSE (4)
*
      CALL OUT0S
*
      IF (NPR.LT.NPT) GO TO 100
      STOP
      END
*   INIT0S.FOR
*
*--initialize the variables and the flow at zero time
*
      SUBROUTINE INIT0S
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /GASS  / SP(2),SPM(5)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
      COMMON /CONST / PI,SPI,BOLTZ
*
*--set constants
*
      PI=3.141592654
      SPI=SQRT(PI)
      BOLTZ=1.3806E-23
*
      CALL DATA0S
*
*--set additional data on the gas
*
      SPM(1)=PI*SP(2)**2
*--the collision cross section is given by eqn (1.8)
      SPM(5)=GAM(2.5-SPM(3))
*
*--initialise variables
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
*--velocity components have been set
          END IF
250     CONTINUE
300   CONTINUE
      WRITE (*,99001) NM
99001 FORMAT (' ',I6,' MOLECULES')
*
      RETURN
      END
*   SAMPI0S.FOR
*
      SUBROUTINE SAMPI0S
*
*--initialises all the sampling variables
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP
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
*   MOVE0S.FOR
*
      SUBROUTINE MOVE0S
*
*--the NM molecules are moved over the time interval DTM
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP
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
      PARAMETER (MNM=1000,MNC=50,MNSC=400)
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
      PARAMETER (MNM=1000,MNC=50,MNSC=400)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /GASS  / SP(2),SPM(5)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP
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
*   SAMPLE0S.FOR
*
*
      SUBROUTINE SAMPLE0S
*
*--sample the molecules in the flow.
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP
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
*   OUT0S.FOR
*
      SUBROUTINE OUT0S
*
*--output a progressive set of results to file DSMC0S.OUT.
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT,FND2
*
      COMMON /MOLSS / NM,PP(MNM),PV(3,MNM),IP(MNM),IR(MNM)
      COMMON /CELLSS/ CC(MNC),CG(3,MNC),IC(2,MNC),ISC(MNSC),CCG(2,MNC),
     &                ISCG(2,MNSC)
      COMMON /GASS  / SP(2),SPM(5)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP
      COMMON /GEOM  / CW,NSC,XF,XR
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
      COMMON /CONST / PI,SPI,BOLTZ
      DIMENSION VEL(3)
*
      OPEN (4,FILE='DSMC0S.OUT',FORM='FORMATTED')
*
      WRITE (4,*) ' FROM ZERO TIME TO TIME',TIME
      WRITE (4,*) ' COLLISIONS =',NCOL
      WRITE (4,*) ' TOTAL NUMBER OF SAMPLES ',NSMP
      WRITE (4,*) NM,' MOLECULES'
      WRITE (4,*) MOVT,' TOTAL MOLECULAR MOVES'
      WRITE (4,*) INT(SELT),' SELECTIONS ',INT(NCOL),
     &            ' COLLISIONS, RATIO  ',REAL(NCOL/SELT)
      IF (NCOL.GT.0) WRITE (4,*) ' MEAN COLLISION SEPARATION ',
     &                           REAL(SEPT/NCOL)
*
      WRITE (4,*) ' FLOWFIELD PROPERTIES'
      WRITE (4,*) 
     & '  CELL   X COORD   SAMPLE     N DENS.         U         V WTEMP'
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
C--compare with theoretical collision number (actual temperarure)
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
*   DATA0S.FOR
*
      SUBROUTINE DATA0S
*
*--defines the data for a particular run of DSMC0S.FOR
*
      PARAMETER (MNM=1000,MNC=50,MNSC=400)
*
      DOUBLE PRECISION MOVT,NCOL,SELT,SEPT
*
      COMMON /GASS  / SP(2),SPM(5)
      COMMON /SAMPS / NCOL,MOVT,SELT,SEPT,CS(5,MNC),TIME,NPR,NSMP,FND,
     &                FTMP
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPT
      COMMON /GEOM  / CW,NSC,XF,XR
*
*--set data (must be consistent with PARAMETER variables)
*
      FND=1.E20
*--FND  is the number densty
      FTMP=300.
*--FTMP is the temperature
      FNUM=1.0E17
*--FNUM  is the number of real molecules represented by a simulated mol.
      DTM=.25E-4
*--DTM is the time step
      NSC=8
*--NSC is the number of sub-cells in each cell
      XF=0.
      XR=1.
*--the simulated region is from x=XF to x=XR
      SP(1)=5.E-26
      SP(2)=3.5E-10
*--SP(1) is the molecular mass
*--SP(2) is the molecular diameter
      SPM(2)=273.
      SPM(3)=0.75
      SPM(4)=1.
*--SPM(2) is the reference temperature
*--SPM(3) is the viscosity-temperature power law
*--SPM(4) is the recoprocal of the VSS scattering parameter
      NIS=4
*--NIS is the number of time steps between samples
      NSP=40
*--NSP is the number of samples between restart and output file updates
      NPT=500
*--NPT is the number of file updates to STOP
*
      RETURN
      END

*   DSMC3.FOR
*
      PROGRAM DSMC3
*
*--three-dimensional steady flow program
*----the flowfield is a rectangular region in the x,y,z space
*----the cells and sub-cells are rectangular and of uniform size
*----each of the six boundaries may be either
*------a uniform stream,
*------a plane of symmetry, or
*------a vacuum
*----there may be one or two flat one-sided surfaces
*------they must lie along a cell boundary
*------they may be back-to-back to form one double-sided surface
*------the surface reflection may be diffuse or specular
*----there may be a uniform circular jet in the y direction and
*------with the efflux in the y=0 plane
*
*--SI units are used throughout
*
*-------------------------DESCRIPTION OF DATA---------------------------
*
*--PARAMETER variables must be consistent with data in subroutine DATA3
*
*--NCX the number of cells in the x direction (cell columns)
*--NCY the number of cells in the y direction (cell rows)
*--NCZ the number of cells in the z direction (cell slices)
*----(MNC must be equal to or greater than NCX*NCY*NCZ)
*
*--NSCX the number of sub-cells per cell in the x direction
*--NSCY the number of sub-cells per cell in the y direction
*--NSCZ the number of sub-cells per cell in the z direction
*----(MNSC must be at least MNC*NSCX*NSCZ)
*
*--IIS 0 if there is no stream, 1 if there is a uniform stream
*--ISG 0 if there is a stream and the initial state is a vacuum, or
*----1 if the initial state is a uniform stream, or
*
*--FTMP the stream temperature if IIS=1, or a temperature characteristic
*----of the flow otherwise (if FTMP is not set for IIS= 0, the
*----default value of 273 is used to set the initial value of CCG(1
*
*--FND the initial number density for IIS=1
*----or need not be set for IIS=0
*
*--VFX, VFY, VFZ the stream velocity in the x, y, z direction
*--need not be set for IIS=0
*
*--FSP(L) the fraction (by number) of species L in the initial stream
*----a value is requred for each species, but need not be set for IIS=0
*
*--FNUM the number of real mols. represented by each simulated molecule
*
*--DTM the time step over which the motion and collisions are uncoupled
*
*--the following data is required for each boundary
*----K=1 for the lower value of x
*----K=2 for the higher value of x
*----K=3 for the lower value of y
*----K=4 for the higher value of y
*----K=5 for the lower value of z
*----K=6 for the higher value of Z
*
*--CB(K) the coordinate of the boundary (x for K=1 or 2, y for K=3 or 4,
*----z for K=4 OR 5)
*
*--IB(K) the type code of the boundary
*----1 for stream, 2 for plane of symmetry, or 3 for vacuum
*
*--ISURF(K) with K=1, 2 for the two surfaces
*----0 if there is no surface
*----1 if the surface normal is in the positive y direction
*----2 if the surface normal is in the negative y direction
*----3 if the surface normal is in the positive x direction
*----4 if the surface normal is in the negative x direction
*----5 if the surface normal is in the positive z direction
*----6 if the surface normal is in the negative z direction
*
*--the following surface data is required if ISURF(K) > 0
*
*--LIMS(K,L) with K as before, and the surface lies along the
*----lower boundary of the cell row, column, or slice given by L=1
*----one edge of the surface is at the lower edge of the row, column,
*----or slice given by L=2 & L=4, and the other is at the upper edge
*----of the row, column or slice given by L=3 & L=5
*----if ISURF(K)=1 or 2, x limits are in L=2 & 3, z limits in L=4 & 5
*----if ISURF(K)=3 or 4, y limits are in L=2 & 3, z limits in L=4 & 5
*----if ISURF(K)=5 or 6, x limits are in L=2 & 3, y limits in L=4 & 5
*
*--if a surface lies along boundary K, then IB(K) must be 2
*--TSURF(K) the temperature of the surface
*----if TSURF is negative, the reflection is specular
*
*--end of surface data
*
*--IJET  0 if there is no jet, 1
*----1 if there is full circular jet in the plane y=0
*----2 if the centre of the jet is also on a plane of symmetry at z=0
*----3 if the centre is at a corner of symmetry at x=0, z=0
*
*--if IJET > 0, the following data is required
*
*--XJ, ZJ the x,z coordinate of the centre of the jet
*--RJ the radius of the jet
*
*--TMPJ the jet temperature
*
*--FNDJ the number density of the jet
*
*--FVJ the jet velocity
*
*--FSPJ(L) the fraction (by number) of species L in the jet
*----a value is requred for each species
*
*--end of jet data
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
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
*--MNSE the maximum number of surface elements
*--MNMR 1 if all molecules are monatomic, MNM otherwise
*--MNMS 1 if there is only one species, MNM for a gas mixture
*--other variables as defined in DSMC0.FOR
*
*-----------------------------------------------------------------------
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
*--variables as defined in DSMC0.FOR
*
      DOUBLE PRECISION CSR(MNC,MNSP)
*
*--CSR(M,L) the sum of the rotational energy of species L in cell M
*
      DOUBLE PRECISION CSS(11,MNSE,MNSP)
*
*--CSS(N,M,L) sampled info. on the molecules striking the boundaries
*----M is the code number of the element; L is the species
*----N=1 the number sum
*----N=2 the sum of the normal momentum of the incident molecules
*----N=3 the sum of the normal momentum for the reflected molecules
*----N=4 the sum of the incident tangential momentum in direction 1
*----N=5 the sum of the incident translational energy
*----N=6 the sum of the reflected translational energy
*----N=7 the sum of the incident rotational energy
*----N=8 the sum of the reflected rotational energy
*----N=9 the sum of the reflected tangential momentum in direction 1
*----N=10 the sum of the incident tangential momentum in direction 2
*----N=11 the sum of the reflected tangential momentum in direction 2
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
*
*--variables as defined in DSMC0.FOR except that PP( is three-diml.
*
      COMMON /MOLSR / PR(MNMR)
*
*--PR(M) is the rotational energy of molecule M
*
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
*
*--other variables as defined in DSMC0.FOR except that
*----CG(1,L),CG(2,L) are the minimim and maximum x coordinates of cell L
*----CG(3,L),CG(4,L) are the minimim and maximum y coordinates of cell L
*----CG(5,L),CG(6,L) are the minimim and maximum z coordinates of cell L
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
*
*--variables as defined in DSMC0.FOR
*
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
*
*--variables as defined in DSMC0R.FOR
*
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
*
*--VFX stream velocity component in X direction
*--VFY stream velocity component in Y direction
*--VFZ stream velocity component in Z direction
*--other variables as defined in DSMC0.FOR
*
      COMMON /SAMPR / CSR
*
      COMMON /SAMPS / CSS,ICS(MNSE)
*--ICS(N) the number of the cell adjacent to surface element N
*
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
*
*--variables as defined in DSMC0.FOR
*
      COMMON /GEOM3 / NSCX,NSCY,NSCZ,CB(6),IB(6),ISURF(2),LIMS(2,5),IIS,
     &                ISG,TSURF(2),BME(6,MNSP),BMR(6,MNSP),CW,FW,CH,FH,
     &                FD,CD,IJET,XJ,ZJ,RJ,TMPJ,FNDJ,FVJ,FSPJ(MNSP),
     &                BMEJ(MNSP),BMRJ(MNSP)
*
*--IIS 0, 1 if the initial flow is a vacuum, uniform stream
*--NSCX,NSCY,NSCZ the number of sub-cells per cell in x,y,z directions
*--CB(N) the location of the boundary
*----N=1,2 for lower, higher value of x
*----N=3,4 for lower, higher value of y
*----N=5,6 for lower, higher value of z
*--IB(N) N=1 to 4 as above,  the type code for the boundary
*--ISURF(K) 0 for no surface, otherwise direction of normal to surface K
*--LIMS(K,5) defines the location of the surface
*--TSURF(K) the temperature of the surface (negative for specular refl.)
*--BME(N,L) number of molecules of species L that enter side N each DTM
*--BMR(N,L) the remainder associated with BME
*--CW the cell width for uniform cells (X dirn)
*--FW the flow width
*--CH the cell height for uniform cells (Y dirn)
*--FH the flow height
*--CD the cell depth for uniform cells (Z dirn)
*--FD the flow depth
*--IJET 0 for no jet, otherwise the jet direction
*--XJ, YJ the location of the centre of the jet
*--RJ the radius of the jet
*--TMPJ the jet temperature
*--FNDJ the number density of the jet
*--FVJ the jet velocity
*--FSPJ(L) the fraction of species L in the jet
*--BME(N,L) number of molecules of species L that enter side N each DTM
*--BMR(N,L) the remainder associated with BME
*--BMEJ(L) number of molecules of species L that enter each cell in jet
*--BMRJ(L) the remainder associated with BMEJ
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
        CALL INIT3
*
      ELSE
*
        WRITE (*,*) ' READ THE RESTART FILE'
        OPEN (4,FILE='DSMC3.RES',STATUS='OLD',FORM='UNFORMATTED')
        READ (4) BFND,BME,BMEJ,BMR,BMRJ,BOLTZ,CB,CC,CCG,CD,CG,CH,COL,CS,
     &           CSR,CSS,CT,CW,DTM,FNDJ,FNUM,FSPJ,FTMP,FD,FH,FVJ,FW,IB,
     &           IC,ICS,IIS,IJET,IPL,IPS,IR,ISC,ISCG,ISG,ISP,ISPR,ISURF,
     &           LIMS,MOVT,NCOL,NCX,NCY,NCZ,NIS,NM,NPS,NSCX,NSCY,NSCZ,
     &           NSMP,NPR,NPT,NSP,PI,PP,PR,PV,RJ,SELT,SEPT,SP,SPI,SPM,
     &           SPR,TIME,TIMI,TMPJ,TSURF,VFX,VFY,VFZ,XJ,ZJ
        CLOSE (4)
*
      END IF
*
      IF (NQLS.EQ.1) CALL SAMPI3
*
100   NPR=NPR+1
*
      IF (NPR.LE.NPS) CALL SAMPI3
*
      DO 200 JJJ=1,NSP
        DO 150 III=1,NIS
          TIME=TIME+DTM
*
          WRITE (*,99001) III,JJJ,NIS,NSP,NM,NCOL
99001  FORMAT (' DSMC3:- Move',2I5,' of',2I5,I8,' Mols',F14.0,' Colls')
*
          CALL MOVE3
*
          CALL INDEXM
*
          CALL COLLMR
*
150     CONTINUE
*
        CALL SAMPLE3
*
200   CONTINUE
*
      WRITE (*,*) ' WRITING RESTART AND OUTPUT FILES',NPR,'  OF ',NPT
      OPEN (4,FILE='DSMC3.RES',FORM='UNFORMATTED')
      WRITE (4) BFND,BME,BMEJ,BMR,BMRJ,BOLTZ,CB,CC,CCG,CD,CG,CH,COL,CS,
     &          CSR,CSS,CT,CW,DTM,FNDJ,FNUM,FSPJ,FTMP,FD,FH,FVJ,FW,IB,
     &          IC,ICS,IIS,IJET,IPL,IPS,IR,ISC,ISCG,ISG,ISP,ISPR,ISURF,
     &          LIMS,MOVT,NCOL,NCX,NCY,NCZ,NIS,NM,NPS,NSCX,NSCY,NSCZ,
     &          NSMP,NPR,NPT,NSP,PI,PP,PR,PV,RJ,SELT,SEPT,SP,SPI,SPM,
     &          SPR,TIME,TIMI,TMPJ,TSURF,VFX,VFY,VFZ,XJ,ZJ
      CLOSE (4)
*
      CALL OUT3
*
      IF (NPR.LT.NPT) GO TO 100
      STOP
      END
*   INIT3.FOR
*
      SUBROUTINE INIT3
*
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
 
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /MOLSR / PR(MNMR)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
      COMMON /SAMPR / CSR
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM3 / NSCX,NSCY,NSCZ,CB(6),IB(6),ISURF(2),LIMS(2,5),IIS,
     &                ISG,TSURF(2),BME(6,MNSP),BMR(6,MNSP),CW,FW,CH,FH,
     &                FD,CD,IJET,XJ,ZJ,RJ,TMPJ,FNDJ,FVJ,FSPJ(MNSP),
     &                BMEJ(MNSP),BMRJ(MNSP)
      COMMON /CONST / PI,SPI,BOLTZ
*
*--set constants
*
      PI=3.141592654
      SPI=SQRT(PI)
      BOLTZ=1.380622E-23
*
*--set data variables to default values that they retain if the data
*----does not reset them to specific values
      FND=0.
      FTMP=273.
      VFX=0.
      VFY=0.
      VFZ=0.
      DO 100 N=1,6
        IB(N)=3
        DO 50 L=1,MNSP
          ISP(L)=1
          FSP(L)=0.
          BME(N,L)=0.
          BMR(N,L)=0.
50      CONTINUE
100   CONTINUE
      IJET=0
      DO 200 L=1,MNSP
        BMEJ(L)=0.
        BMRJ(L)=0.
200   CONTINUE
*
      CALL DATA3
*
*--set additional data on the gas
*
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
      FW=CB(2)-CB(1)
      FH=CB(4)-CB(3)
      FD=CB(6)-CB(5)
      CG(1,1)=CB(1)
      CW=FW/NCX
*--CW is the uniform cell width
      CG(3,1)=CB(3)
      CH=FH/NCY
*--CH is the uniform cell height
      CG(5,1)=CB(5)
      CD=FD/NCZ
*--CD is the uniform cell depth (in the z direction)
      CC=CW*CH*CD
*--CC is the uniform cell volume
      DO 500 MZ=1,NCZ
        DO 450 MY=1,NCY
          DO 420 MX=1,NCX
            M=(MZ-1)*NCX*NCY+(MY-1)*NCX+MX
*--M is the cell number
            CT(M)=FTMP
*--the macroscopic temperature is set to the freestream temperature
*--set the x coordinates
            IF (MX.EQ.1) CG(1,M)=CG(1,1)
            IF (MX.GT.1) CG(1,M)=CG(2,M-1)
            CG(2,M)=CG(1,M)+CW
*--set the y coordinates
            IF (MY.EQ.1) CG(3,M)=CG(3,1)
            IF (MY.GT.1.AND.MX.EQ.1) CG(3,M)=CG(4,M-1)
            IF (MY.GT.1.AND.MX.GT.1) CG(3,M)=CG(3,M-1)
            CG(4,M)=CG(3,M)+CH
*--set the z coordinates
            IF (MZ.EQ.1) CG(5,M)=CG(5,1)
            IF (MZ.GT.1.AND.MX.EQ.1.AND.MY.EQ.1) CG(5,M)=CG(6,M-1)
            IF (MZ.GT.1.AND.(MX.GT.1.OR.MY.GT.1)) CG(5,M)=CG(5,M-1)
            CG(6,M)=CG(5,M)+CD
            DO 410 L=1,MNSG
              DO 405 K=1,MNSG
                CCG(2,M,L,K)=RF(0)
                CCG(1,M,L,K)=SPM(1,1,1)*300.*SQRT(FTMP/300.)
405           CONTINUE
410         CONTINUE
*--the maximum value of the (rel. speed)*(cross-section) is set to a
*--reasonable, but low, initial value and will be increased as necessary
420       CONTINUE
450     CONTINUE
500   CONTINUE
*
*--set sub-cells
*
      DO 600 N=1,MNC
        DO 550 J=1,NSCZ
          DO 520 M=1,NSCY
            DO 510 K=1,NSCX
              L=(N-1)*NSCX*NSCY*NSCZ+(J-1)*NSCX*NSCY+(M-1)*NSCX+K
              ISC(L)=N
510         CONTINUE
520       CONTINUE
550     CONTINUE
600   CONTINUE
*
      IF (IIS.GT.0.AND.ISG.GT.0) THEN
*--if IIS=1 generate initial gas with temperature FTMP
*
        DO 650 L=1,MNSP
          REM=0
          IF (IIS.EQ.1) VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
          DO 620 N=1,MNC
            A=FND*CC*FSP(L)/FNUM+REM
*--A is the number of simulated molecules of species L in cell N to
*--simulate the required concentrations at a total number density of FND
            IF (N.LT.MNC) THEN
              MM=A
              REM=(A-MM)
*--the remainder REM is carried forward to the next cell
            ELSE
              MM=NINT(A)
            END IF
            IF (MM.GT.0) THEN
              DO 605 M=1,MM
                IF (NM.LT.MNM) THEN
*--round-off error could have taken NM to MNM+1
                  NM=NM+1
                  IF (MNSP.GT.1) IPS(NM)=L
                  PP(1,NM)=CG(1,N)+RF(0)*(CG(2,N)-CG(1,N))
                  NCOLM=(PP(1,NM)-CG(1,N))*(NSCX-.001)/CW+1
                  PP(2,NM)=CG(3,N)+RF(0)*(CG(4,N)-CG(3,N))
                  NROW=(PP(2,NM)-CG(3,N))*(NSCY-.001)/CH+1
                  PP(3,NM)=CG(5,N)+RF(0)*(CG(6,N)-CG(5,N))
                  NSLC=(PP(3,NM)-CG(5,N))*(NSCZ-.001)/CD+1
                  IPL(NM)=(N-1)*NSCX*NSCY*NSCZ+(NSLC-1)*NSCX*NSCY+
     &                    (NROW-1)*NSCX+NCOLM
*--species, position, and sub-cell number have been set
                  DO 602 K=1,3
                    CALL RVELC(PV(K,NM),A,VMP)
602               CONTINUE
                  PV(1,NM)=PV(1,NM)+VFX
                  PV(2,NM)=PV(2,NM)+VFY
                  PV(3,NM)=PV(3,NM)+VFZ
*--velocity components have been set
*--set the rotational energy
                  IF (ISPR(1,L).GT.0) CALL SROT(PR(NM),FTMP,ISPR(1,L))
                END IF
605           CONTINUE
            END IF
620       CONTINUE
650     CONTINUE
*
        WRITE (*,99001) NM
99001   FORMAT (' ',I6,' MOLECULES')
      END IF
      IF (IIS.GT.0) THEN
*
*--calculate the number of molecules that enter at each time step
*--across the six sides of the simulated region
        DO 700 N=1,6
          IF (IB(N).EQ.1) THEN
            WRITE (*,*) 'side',N
*--molecules enter from an external stream
            DO 660 L=1,MNSP
              VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
              IF (N.EQ.1) SC=VFX/VMP
              IF (N.EQ.2) SC=-VFX/VMP
              IF (N.EQ.3) SC=VFY/VMP
              IF (N.EQ.4) SC=-VFY/VMP
              IF (N.EQ.5) SC=VFZ/VMP
              IF (N.EQ.6) SC=-VFZ/VMP
*--SC is the inward directed speed ratio
              IF (ABS(SC).LT.10.1) A=(EXP(-SC*SC)+SPI*SC*(1.+ERF(SC)))
     &                               /(2.*SPI)
              IF (SC.GT.10.) A=SC
              IF (SC.LT.-10.) A=0.
*--A is the non-dimensional flux of eqn (4.22)
              IF (N.EQ.1.OR.N.EQ.2) BME(N,L)=FND*FSP(L)
     &            *A*VMP*DTM*FH*FD/FNUM
              IF (N.EQ.3.OR.N.EQ.4) BME(N,L)=FND*FSP(L)
     &            *A*VMP*DTM*FW*FD/FNUM
              IF (N.EQ.5.OR.N.EQ.6) BME(N,L)=FND*FSP(L)
     &            *A*VMP*DTM*FH*FW/FNUM
              WRITE (*,*) ' species ',L,' entering mols ',BME(N,L)
660         CONTINUE
          END IF
700     CONTINUE
      END IF
*--now calculate the number that enter in jet
      IF (IJET.GT.0) THEN
*--molecules enter from a jet
        DO 750 L=1,MNSP
          VMP=SQRT(2.*BOLTZ*TMPJ/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
          SC=FVJ/VMP
*--SC is the inward directed speed ratio
          IF (ABS(SC).LT.10.1) A=(EXP(-SC*SC)+SPI*SC*(1.+ERF(SC)))
     &                           /(2.*SPI)
          IF (SC.GT.10.) A=SC
          IF (SC.LT.-10.) A=0.
*--A is the non-dimensional flux of eqn (4.22)
          AJ=PI*RJ*RJ
          IF (IJET.EQ.2) AJ=0.5*AJ
          IF (IJET.EQ.3) AJ=0.25*AJ
          BMEJ(L)=FNDJ*FSPJ(L)*A*VMP*DTM*AJ/FNUM
          WRITE (*,*) ' entering mols in jet ',BMEJ(L)
750     CONTINUE
      END IF
*
      RETURN
      END
*   MOVE3.FOR
 
*
      SUBROUTINE MOVE3
*
*--the NM molecules are moved over the time interval DTM
*
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSS(11,MNSE,MNSP)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /MOLSR / PR(MNMR)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
      COMMON /SAMPS / CSS,ICS(MNSE)
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM3 / NSCX,NSCY,NSCZ,CB(6),IB(6),ISURF(2),LIMS(2,5),IIS,
     &                ISG,TSURF(2),BME(6,MNSP),BMR(6,MNSP),CW,FW,CH,FH,
     &                FD,CD,IJET,XJ,ZJ,RJ,TMPJ,FNDJ,FVJ,FSPJ(MNSP),
     &                BMEJ(MNSP),BMRJ(MNSP)
*
      DIMENSION DT(3)
*--DT is the time step to doundary interactions
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
        XI=PP(1,N)
        IF ((XI+0.00001*CW).LT.CB(1).OR.(XI-0.00001*CW).GT.CB(2)) THEN
          CALL REMOVE(N)
          GO TO 100
        END IF
        YI=PP(2,N)
        IF ((YI+0.00001*CH).LT.CB(3).OR.(YI-0.00001*CH).GT.CB(4)) THEN
          CALL REMOVE(N)
          GO TO 100
        END IF
        ZI=PP(3,N)
        IF ((ZI+0.00001*CD).LT.CB(5).OR.(ZI-0.00001*CD).GT.CB(6)) THEN
          CALL REMOVE(N)
          GO TO 100
        END IF
        DX=PV(1,N)*AT
        DY=PV(2,N)*AT
        DZ=PV(3,N)*AT
        X=XI+DX
        Y=YI+DY
        Z=ZI+DZ
        DO 200 KS=1,2
*--check the surfaces
          IF (ISURF(KS).GT.0) THEN
            IF (ISURF(KS).EQ.1.OR.ISURF(KS).EQ.2) THEN
              L1=LIMS(KS,1)
              YS=(L1-1)*CH+CB(3)
              IF ((ISURF(KS).EQ.1.AND.(YI.GT.YS.AND.Y.LT.YS)).OR.
     &            (ISURF(KS).EQ.2.AND.(YI.LT.YS.AND.Y.GT.YS))) THEN
                XC=XI+(YS-YI)*DX/DY
                ZC=ZI+(YS-YI)*DZ/DY
                IF ((XC.GT.CB(1)).AND.(XC.LT.CB(2)).AND.(ZC.GT.CB(5))
     &              .AND.(ZC.LT.CB(6))) THEN
                  L2=LIMS(KS,2)
                  L3=LIMS(KS,3)
                  XSU=(L2-1)*CW+CB(1)
                  XSD=L3*CW+CB(1)
                  L4=LIMS(KS,4)
                  L5=LIMS(KS,5)
                  ZSU=(L4-1)*CD+CB(5)
                  ZSD=L5*CD+CB(5)
                  IF ((XC.GT.XSU.AND.XC.LT.XSD).AND.
     &                (ZC.GT.ZSU.AND.ZC.LT.ZSD)) THEN
*--molecule collides with surface at XC,ZC
                    MCX=(XC-CB(1))/CW+0.99999
                    IF (MCX.LT.1) MCX=1
                    IF (MCX.GT.NCX) MCX=NCX
                    MCZ=(ZC-CB(5))/CD+0.99999
                    IF (MCZ.LT.1) MCZ=1
                    IF (MCZ.GT.NCZ) MCZ=NCZ
                    MCSX=MCX-(L2-1)
                    MCSZ=MCZ-(L4-1)
                    MCS=(MCSZ-1)*(L3-L2+1)+MCSX
                    IF (KS.EQ.2) MCS=MCS+(LIMS(1,3)-LIMS(1,2)+1)
     &                               *(LIMS(1,5)-LIMS(1,4)+1)
*--MCS is the code number of the surface element
                    MC=(MCZ-1)*NCX*NCY+MCX
                    IF (ISURF(KS).EQ.1) MC=MC+(L1-1)*NCX
                    IF (ISURF(KS).EQ.2) MC=MC+(L1-2)*NCX
*--MC is the cell number for the reflected molecule
                    AT=AT*(Y-YS)/DY
                    CALL REFLECT3(N,KS,MCS,XC,YS,ZC,MC)
                    GO TO 150
                  END IF
                END IF
              END IF
            END IF
            IF (ISURF(KS).EQ.3.OR.ISURF(KS).EQ.4) THEN
              L1=LIMS(KS,1)
              XS=(L1-1)*CW+CB(1)
              IF ((ISURF(KS).EQ.3.AND.(XI.GT.XS.AND.X.LT.XS)).OR.
     &            (ISURF(KS).EQ.4.AND.(XI.LT.XS.AND.X.GT.XS))) THEN
                YC=YI+(XS-XI)*DY/DX
                ZC=ZI+(XS-XI)*DZ/DX
                IF ((YC.GT.CB(3)).AND.(YC.LT.CB(4)).AND.(ZC.GT.CB(5))
     &              .AND.(ZC.LT.CB(6))) THEN
                  L2=LIMS(KS,2)
                  L3=LIMS(KS,3)
                  YSU=(L2-1)*CH+CB(3)
                  YSD=L3*CH+CB(3)
                  L4=LIMS(KS,4)
                  L5=LIMS(KS,5)
                  ZSU=(L4-1)*CD+CB(5)
                  ZSD=L5*CD+CB(5)
                  IF ((YC.GT.YSU.AND.YC.LT.YSD).AND.
     &                (ZC.GT.ZSU.AND.ZC.LT.ZSD)) THEN
*--molecule collides with surface at YC, ZC
                    MCY=(YC-CB(3))/CH+0.99999
                    IF (MCY.LT.1) MCY=1
                    IF (MCY.GT.NCY) MCY=NCY
                    MCZ=(ZC-CB(5))/CD+0.99999
                    IF (MCZ.LT.1) MCZ=1
                    IF (MCZ.GT.NCZ) MCZ=NCZ
                    MCSY=MCY-(L2-1)
                    MCSZ=MCZ-(L4-1)
                    MCS=(MCSZ-1)*(L3-L2+1)+MCSY
                    IF (KS.EQ.2) MCS=MCS+(LIMS(1,3)-LIMS(1,2)+1)
     &                               *(LIMS(1,5)-LIMS(1,4)+1)
*--MCS is the code number of the surface element
                    MC=(MCZ-1)*NCX*NCY+(MCY-1)*NCX
                    IF (ISURF(KS).EQ.3) MC=MC+L1
                    IF (ISURF(KS).EQ.4) MC=MC+L1-1
*--MC is the cell number for the reflected molecule
                    AT=AT*(X-XS)/DX
                    CALL REFLECT3(N,KS,MCS,XS,YC,ZC,MC)
                    GO TO 150
                  END IF
                END IF
              END IF
            END IF
            IF (ISURF(KS).EQ.5.OR.ISURF(KS).EQ.6) THEN
              L1=LIMS(KS,1)
              IF (L1.LE.NCZ) THEN
                ZS=(L1-1)*CD+CB(5)
              ELSE
                ZS=CB(6)
                L1=L1-1
              END IF
              IF ((ISURF(KS).EQ.5.AND.(ZI.GT.ZS.AND.Z.LT.ZS)).OR.
     &            (ISURF(KS).EQ.6.AND.(ZI.LT.ZS.AND.Z.GT.ZS))) THEN
                YC=YI+(ZS-ZI)*DY/DZ
                XC=XI+(ZS-ZI)*DX/DZ
                IF ((XC.GT.CB(1)).AND.(XC.LT.CB(2)).AND.(YC.GT.CB(3))
     &              .AND.(YC.LT.CB(4))) THEN
                  L2=LIMS(KS,2)
                  L3=LIMS(KS,3)
                  XSU=(L2-1)*CW+CB(1)
                  XSD=L3*CW+CB(1)
                  L4=LIMS(KS,4)
                  L5=LIMS(KS,5)
                  YSU=(L4-1)*CH+CB(3)
                  YSD=L5*CH+CB(3)
                  IF ((XC.GT.XSU.AND.XC.LT.XSD).AND.
     &                (YC.GT.YSU.AND.YC.LT.YSD)) THEN
*--molecule collides with surface at YC, XC
                    MCY=(YC-CB(3))/CH+0.99999
                    IF (MCY.LT.1) MCY=1
                    IF (MCY.GT.NCY) MCY=NCY
                    MCX=(XC-CB(1))/CW+0.99999
                    IF (MCX.LT.1) MCX=1
                    IF (MCX.GT.NCX) MCX=NCX
                    MCSX=MCX-(L2-1)
                    MCSY=MCY-(L4-1)
                    MCS=(MCSY-1)*(L3-L2+1)+MCSX
                    IF (KS.EQ.2) MCS=MCS+(LIMS(1,3)-LIMS(1,2)+1)
     &                               *(LIMS(1,5)-LIMS(1,4)+1)
*--MCS is the code number of the surface element
                    MC=(MCY-1)*NCX+MCX
                    IF (ISURF(KS).EQ.5) MC=MC+(L1-1)*NCX*NCY
                    IF (ISURF(KS).EQ.6) MC=MC+(L1-2)*NCX*NCY
*--MC is the cell number for the reflected molecule
                    AT=AT*(Z-ZS)/DZ
                    CALL REFLECT3(N,KS,MCS,XC,YC,ZS,MC)
                    GO TO 150
                  END IF
                END IF
              END IF
            END IF
          END IF
200     CONTINUE
*
*--new coding based on Rob Nance's modifications 
        IBOUND=0
        ICOUNT=0
        IF (X.LT.CB(1).OR.X.GT.CB(2)) THEN
          IBOUND=1
          ICOUNT=ICOUNT+1
        END IF
        IF (Y.LT.CB(3).OR.Y.GT.CB(4)) THEN
          IBOUND=2
          ICOUNT=ICOUNT+1
        END IF
        IF (Z.LT.CB(5).OR.Z.GT.CB(6)) THEN
          IBOUND=3
          ICOUNT=ICOUNT+1
        END IF
*--determine whether more than one boundary was crossed
        IF (ICOUNT.GT.1) THEN
*
          DO KK=1,3
            DT(KK)=5.*DTM
          END DO
*
          IF (ABS(PV(1,N)).GT.1.E-6) THEN
            DT1=(CB(1)-PP(1,N))/PV(1,N)
            DT2=(CB(2)-PP(1,N))/PV(1,N)
            DT(1)=DT1
            IF (DT2.GT.DT1) DT(1)=DT2
          END IF
          IF (ABS(PV(2,N)).GT.1.E-6) THEN
            DT1=(CB(3)-PP(2,N))/PV(2,N)
            DT2=(CB(4)-PP(2,N))/PV(2,N)
            DT(2)=DT1
            IF (DT2.GT.DT1) DT(2)=DT2
          END IF
          IF (ABS(PV(3,N)).GT.1.E-6) THEN
            DT1=(CB(5)-PP(3,N))/PV(3,N)
            DT2=(CB(6)-PP(3,N))/PV(3,N)
            DT(3)=DT1
            IF (DT2.GT.DT1) DT(3)=DT2
          END IF
*--determine which boundary is crossed first
          DTMIN=5.*DTM
          DO KK=1,3
            IF (DT(KK).LT.DTMIN) THEN
              DTMIN=DT(KK)
              IBOUND=KK
*--IBOUND now indicates the first side that is crossed
            END IF
          END DO
        ENDIF
*
        IF (IBOUND.EQ.1) THEN
          IF (X.LT.CB(1)) K=1
          IF (X.GT.CB(2)) K=2
*--intersection with boundary K
          IF (IB(K).EQ.2) THEN
*--specular reflection from the boundary (eqn (11.7))
            AT=AT*(X-CB(K))/DX
            PP(1,N)=X-AT*PV(1,N)+0.001*CW*(3.-2.*K)
            PP(2,N)=Y-AT*PV(2,N)
            PP(3,N)=Z-AT*PV(3,N)
            PV(1,N)=-PV(1,N)
            GO TO 150
          ELSE
*--molecule leaves flow
            CALL REMOVE(N)
            GO TO 100
          END IF
        END IF
*
        IF (IBOUND.EQ.2) THEN
          IF (Y.LT.CB(3)) K=3
          IF (Y.GT.CB(4)) K=4
*--intersection with boundary K
          IF (IB(K).EQ.2) THEN
*--specular reflection from the boundary (eqn (11.7))
            AT=AT*(Y-CB(K))/DY
            PP(1,N)=X-AT*PV(1,N)
            PP(2,N)=Y-AT*PV(2,N)+0.001*CH*(7.-2.*K)
            PP(3,N)=Z-AT*PV(3,N)
            PV(2,N)=-PV(2,N)
            GO TO 150
          ELSE
*--molecule leaves flow
            CALL REMOVE(N)
            GO TO 100
          END IF
        END IF
*
        IF (IBOUND.EQ.3) THEN
          IF (Z.LT.CB(5)) K=5
          IF (Z.GT.CB(6)) K=6
*--intersection with boundary K
          IF (IB(K).EQ.2) THEN
*--specular reflection from the boundary (eqn (11.7))
            AT=AT*(Z-CB(K))/DZ
            PP(1,N)=X-AT*PV(1,N)
            PP(2,N)=Y-AT*PV(2,N)
            PP(3,N)=Z-AT*PV(3,N)+0.001*CD*(11.-2.*K)
            PV(3,N)=-PV(3,N)
            GO TO 150
          ELSE
*--molecule leaves flow
            CALL REMOVE(N)
            GO TO 100
          END IF
        END IF
*
        IF (X.LT.CG(1,MC).OR.X.GT.CG(2,MC).OR.Y.LT.CG(3,MC).OR.
     &      Y.GT.CG(4,MC).OR.Z.LT.CG(5,MC).OR.Z.GT.CG(6,MC)) THEN
*--the molecule has moved from the initial cell
          MCX=(X-CB(1))/CW+0.99999
          IF (MCX.LT.1) MCX=1
          IF (MCX.GT.NCX) MCX=NCX
*--MCX is the new cell column (note avoidance of round-off error)
          MCY=(Y-CB(3))/CH+0.99999
          IF (MCY.LT.1) MCY=1
          IF (MCY.GT.NCY) MCY=NCY
*--MCY is the new cell row (note avoidance of round-off error)
          MCZ=(Z-CB(5))/CD+0.99999
          IF (MCZ.LT.1) MCZ=1
          IF (MCZ.GT.NCZ) MCZ=NCZ
*--MCZ is the new cell slice (note avoidance of round-off error)
          MC=(MCZ-1)*NCX*NCY+(MCY-1)*NCX+MCX
        END IF
        MSCX=((X-CG(1,MC))/CW)*(NSCX-.001)+1
        MSCY=((Y-CG(3,MC))/CH)*(NSCY-.001)+1
        MSCZ=((Z-CG(5,MC))/CD)*(NSCZ-.001)+1
        MSC=(MSCZ-1)*NSCX*NSCY+(MSCY-1)*NSCX+MSCX+NSCX*NSCY*NSCZ*(MC-1)
*--MSC is the new sub-cell number
        IF (MSC.LT.1) MSC=1
        IF (MSC.GT.MNSC) MSC=MNSC
        IPL(N)=MSC
        PP(1,N)=X
        PP(2,N)=Y
        PP(3,N)=Z
        GO TO 100
      ELSE IF (IFT.LT.0) THEN
        IFT=1
*--new molecules enter
        CALL ENTER3
        N=N-1
        GO TO 100
      END IF
      RETURN
      END
*   ENTER3.FOR
*
      SUBROUTINE ENTER3
*
*--new molecules enter at boundaries
*
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /MOLSR / PR(MNMR)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM3 / NSCX,NSCY,NSCZ,CB(6),IB(6),ISURF(2),LIMS(2,5),IIS,
     &                ISG,TSURF(2),BME(6,MNSP),BMR(6,MNSP),CW,FW,CH,FH,
     &                FD,CD,IJET,XJ,ZJ,RJ,TMPJ,FNDJ,FVJ,FSPJ(MNSP),
     &                BMEJ(MNSP),BMRJ(MNSP)
      COMMON /CONST / PI,SPI,BOLTZ
*
      DO 100 N=1,6
*--consider each boundary in turn
        IF (IB(N).EQ.1) THEN
          IF (N.LT.3) THEN
            NCS1=NCY
            NCS2=NCZ
          END IF
          IF (N.GT.2.AND.N.LT.5) THEN
            NCS1=NCX
            NCS2=NCZ
          END IF
          IF (N.GT.4) THEN
            NCS1=NCX
            NCS2=NCY
          END IF
          DO 20 NC1=1,NCS1
            DO 10 NC2=1,NCS2
              DO 5 L=1,MNSP
*--consider each species in turn
                VMP=SQRT(2.*BOLTZ*FTMP/SP(5,L))
                IF (N.LT.3) A=BME(N,L)*CH*CD/(FH*FD)+BMR(N,L)
                IF (N.GT.2.AND.N.LT.5) A=BME(N,L)*CW*CD/(FW*FD)+BMR(N,L)
                IF (N.GT.4) A=BME(N,L)*CW*CH/(FW*FH)+BMR(N,L)
                M=A
                BMR(N,L)=A-M
*--M molecules enter, remainder has been reset
                IF (M.GT.0) THEN
                  IF (N.EQ.1.OR.N.EQ.2) THEN
                    IF (ABS(VFX).GT.1.E-6) THEN
                      IF (N.EQ.1) SC=VFX/VMP
                      IF (N.EQ.2) SC=-VFX/VMP
                    END IF
                  END IF
                  IF (N.EQ.3.OR.N.EQ.4) THEN
                    IF (ABS(VFY).GT.1.E-6) THEN
                      IF (N.EQ.3) SC=VFY/VMP
                      IF (N.EQ.4) SC=-VFY/VMP
                    END IF
                  END IF
                  IF (N.EQ.5.OR.N.EQ.6) THEN
                    IF (ABS(VFZ).GT.1.E-6) THEN
                      IF (N.EQ.5) SC=VFZ/VMP
                      IF (N.EQ.6) SC=-VFZ/VMP
                    END IF
                  END IF
                  FS1=SC+SQRT(SC*SC+2.)
                  FS2=0.5*(1.+SC*(2.*SC-FS1))
* the above constants are required for the entering distn. of eqn (12.5)
                  DO 4 K=1,M
                    IF (NM.LT.MNM) THEN
                      NM=NM+1
*--NM is now the number of the new molecule
                      IF ((N.LT.3.AND.ABS(VFX).GT.1.E-6).OR.
     &                    (N.GT.2.AND.N.LT.5.AND.ABS(VFY).GT.1.E-6).OR.
     &                    (N.GT.4.AND.ABS(VFZ).GT.1.E-6)) THEN
                        QA=3.
                        IF (SC.LT.-3.) QA=ABS(SC)+1.
2                       U=-QA+2.*QA*RF(0)
*--U is a potential normalised thermal velocity component
                        UN=U+SC
*--UN is a potential inward velocity component
                        IF (UN.LT.0.) GO TO 2
                        A=(2.*UN/FS1)*EXP(FS2-U*U)
                        IF (A.LT.RF(0)) GO TO 2
*--the inward normalised vel. component has been selected (eqn (12.5))
                        IF (N.EQ.1) PV(1,NM)=UN*VMP
                        IF (N.EQ.2) PV(1,NM)=-UN*VMP
                        IF (N.EQ.3) PV(2,NM)=UN*VMP
                        IF (N.EQ.4) PV(2,NM)=-UN*VMP
                        IF (N.EQ.5) PV(3,NM)=UN*VMP
                        IF (N.EQ.6) PV(3,NM)=-UN*VMP
                      ELSE
                        IF (N.EQ.1) PV(1,NM)=SQRT(-LOG(RF(0)))*VMP
                        IF (N.EQ.2) PV(1,NM)=-SQRT(-LOG(RF(0)))*VMP
                        IF (N.EQ.3) PV(2,NM)=SQRT(-LOG(RF(0)))*VMP
                        IF (N.EQ.4) PV(2,NM)=-SQRT(-LOG(RF(0)))*VMP
                        IF (N.EQ.5) PV(3,NM)=SQRT(-LOG(RF(0)))*VMP
                        IF (N.EQ.6) PV(3,NM)=-SQRT(-LOG(RF(0)))*VMP
*--for a stationary external gas, use eqn (12.3)
                      END IF
                      IF (N.LT.3) THEN
                        CALL RVELC(PV(2,NM),PV(3,NM),VMP)
                        PV(2,NM)=PV(2,NM)+VFY
                        PV(3,NM)=PV(3,NM)+VFZ
                      END IF
                      IF (N.GT.2.AND.N.LT.5) THEN
                        CALL RVELC(PV(1,NM),PV(3,NM),VMP)
                        PV(1,NM)=PV(1,NM)+VFX
                        PV(3,NM)=PV(3,NM)+VFZ
                      END IF
                      IF (N.GT.4) THEN
                        CALL RVELC(PV(1,NM),PV(2,NM),VMP)
                        PV(1,NM)=PV(1,NM)+VFX
                        PV(2,NM)=PV(2,NM)+VFY
                      END IF
*--a single call of RVELC generates the two normal velocity components
                      IF (ISPR(1,L).GT.0)
     &                     CALL SROT(PR(NM),FTMP,ISPR(1,L))
                      IF (N.EQ.1) PP(1,NM)=CB(1)+0.001*CW
                      IF (N.EQ.2) PP(1,NM)=CB(2)-0.001*CW
                      IF (N.EQ.3) PP(2,NM)=CB(3)+0.001*CH
                      IF (N.EQ.4) PP(2,NM)=CB(4)-0.001*CH
                      IF (N.EQ.5) PP(3,NM)=CB(5)+0.001*CD
                      IF (N.EQ.6) PP(3,NM)=CB(6)-0.001*CD
*--the molecule is moved just off the boundary
                      IF (MNSP.GT.1) IPS(NM)=L
                      IF (N.LT.3) THEN
                        IF (N.EQ.1) MC=(NC2-1)*NCX*NCY+(NC1-1)*NCX+1
                        IF (N.EQ.2) MC=(NC2-1)*NCX*NCY+NC1*NCX
                        PP(2,NM)=CG(3,MC)+RF(0)*CH
                        PP(3,NM)=CG(5,MC)+RF(0)*CD
                      END IF
                      IF (N.GT.2.AND.N.LT.5) THEN
                        IF (N.EQ.3) MC=(NC2-1)*NCX*NCY+NC1
                        IF (N.EQ.4) MC=(NC2-1)*NCX*NCY+(NCY-1)*NCX+NC1
                        PP(1,NM)=CG(1,MC)+RF(0)*CW
                        PP(3,NM)=CG(5,MC)+RF(0)*CD
                      END IF
                      IF (N.GT.4) THEN
                        IF (N.EQ.5) MC=(NC2-1)*NCX+NC1
                        IF (N.EQ.6) MC=(NCZ-1)*NCX*NCY+(NC2-1)*NCX+NC1
                        PP(1,NM)=CG(1,MC)+RF(0)*CW
                        PP(2,NM)=CG(3,MC)+RF(0)*CH
                      END IF
                      MSCX=((PP(1,NM)-CG(1,MC))/CW)*(NSCX-.001)+1
                      MSCY=((PP(2,NM)-CG(3,MC))/CH)*(NSCY-.001)+1
                      MSCZ=((PP(3,NM)-CG(5,MC))/CD)*(NSCZ-.001)+1
                      MSC=(MSCZ-1)*NSCX*NSCY+(MSCY-1)*NSCX+MSCX+
     &                    NSCX*NSCY*(MC-1)
*--MSC is the new sub-cell number
                      IF (MSC.LT.1) MSC=1
                      IF (MSC.GT.MNSC) MSC=MNSC
                      IPL(NM)=MSC
                    ELSE
                      WRITE (*,*) 
     &' WARNING: EXCESS MOLECULE LIMIT - RESTART WITH AN INCREASED FNUM'
                    END IF
4                 CONTINUE
                END IF
5             CONTINUE
10          CONTINUE
20        CONTINUE
        END IF
100   CONTINUE
*--now the jet molecules
      IF (IJET.GT.0) THEN
        DO 150 L=1,MNSP
*--consider each species in turn
          VMP=SQRT(2.*BOLTZ*TMPJ/SP(5,L))
          A=BMEJ(L)+BMRJ(L)
          M=SQRT(A)
          BMRJ(L)=A-M*M
*--M molecules enter, remainder has been reset
          IF (M.GT.0) THEN
            IF (ABS(FVJ).GT.1.E-6) SC=FVJ/VMP
            FS1=SC+SQRT(SC*SC+2.)
            FS2=0.5*(1.+SC*(2.*SC-FS1))
* the above constants are required for the entering distn. of eqn (12.5)
            DO 110 K=1,M
              DO 105 KK=1,M
                IF (NM.LT.MNM) THEN
                  NM=NM+1
*--NM is now the number of the new molecule
                  IF (ABS(FVJ).GT.1.E-6) THEN
                    QA=3.
                    IF (SC.LT.-3.) QA=ABS(SC)+1.
102                 U=-QA+2.*QA*RF(0)
*--U is a potential normalised thermal velocity component
                    UN=U+SC
*--UN is a potential inward velocity component
                    IF (UN.LT.0.) GO TO 102
                    A=(2.*UN/FS1)*EXP(FS2-U*U)
                    IF (A.LT.RF(0)) GO TO 102
*--the inward normalised vel. component has been selected (eqn (12.5))
                    PV(2,NM)=UN*VMP
                  ELSE
                    PV(2,NM)=SQRT(-LOG(RF(0)))*VMP
*--for a stationary external gas, use eqn (12.3)
                  END IF
                  CALL RVELC(PV(1,NM),PV(3,NM),VMP)
*--a single call of RVELC generates the two normal velocity components
                  IF (ISPR(1,L).GT.0) CALL SROT(PR(NM),TMPJ,ISPR(1,L))
*
                  PP(2,NM)=0.001*CH
*--the molecule is moved just off the boundary
                  IPS(NM)=L
                  R=SQRT((K-RF(0))/FLOAT(M))*RJ
                  A=2.*PI*(KK-RF(0))/FLOAT(M)
*--the above statements implement eqns (C21) and (C22)
                  IF (IJET.EQ.2) A=0.5*A
                  IF (IJET.EQ.3) A=0.25*A
*--R and A are the random radius and azimuth relative to the jet centre
                  PP(1,NM)=XJ+R*COS(A)
                  PP(3,NM)=ZJ+R*SIN(A)
*
                  MCX=(PP(1,NM)-CB(1))/CW+0.99999
                  IF (MCX.LT.1) MCX=1
                  IF (MCX.GT.NCX) MCX=NCX
*--MCX is the new cell column (note avoidance of round-off error)
                  MCZ=(PP(3,NM)-CB(5))/CD+0.99999
                  IF (MCZ.LT.1) MCZ=1
                  IF (MCZ.GT.NCZ) MCZ=NCZ
*--MCZ is the new cell slice (note avoidance of round-off error)
                  MC=(MCZ-1)*NCX*NCY+MCX
                  MSCX=((PP(1,NM)-CG(1,MC))/CW)*(NSCX-.001)+1
                  MSCZ=((PP(3,NM)-CG(5,MC))/CD)*(NSCZ-.001)+1
                  MSC=(MSCZ-1)*NSCX*NSCY+MSCX+NSCX*NSCY*NSCZ*(MC-1)
*--MSC is the new sub-cell number
                  IF (MSC.LT.1) MSC=1
                  IF (MSC.GT.MNSC) MSC=MNSC
                  IPL(NM)=MSC
                ELSE
                  WRITE (*,*) 
     &' WARNING: EXCESS MOLECULE LIMIT - RESTART WITH AN INCREASED FNUM'
                END IF
105           CONTINUE
110         CONTINUE
          END IF
150     CONTINUE
      END IF
 
      RETURN
      END
*   REFLECT3.FOR
      SUBROUTINE REFLECT3(N,KS,K,XC,YC,ZC,MC)
*
*--reflection of molecule N from surface KS, element K,
*----location XC,YC,ZC cell MC
*
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSS(11,MNSE,MNSP)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /MOLSR / PR(MNMR)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
      COMMON /SAMPS / CSS,ICS(MNSE)
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM3 / NSCX,NSCY,NSCZ,CB(6),IB(6),ISURF(2),LIMS(2,5),IIS,
     &                ISG,TSURF(2),BME(6,MNSP),BMR(6,MNSP),CW,FW,CH,FH,
     &                FD,CD,IJET,XJ,ZJ,RJ,TMPJ,FNDJ,FVJ,FSPJ(MNSP),
     &                BMEJ(MNSP),BMRJ(MNSP)
      COMMON /CONST / PI,SPI,BOLTZ
*
      IF (ICS(K).EQ.0) ICS(K)=MC
      IF (MNSP.GT.1) THEN
        L=IPS(N)
      ELSE
        L=1
      END IF
*--sample the surface properies due to the incident molecules
      CSS(1,K,L)=CSS(1,K,L)+1.
      IF (ISURF(KS).EQ.1) THEN
        CSS(2,K,L)=CSS(2,K,L)-SP(5,L)*PV(2,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*PV(1,N)
        CSS(10,K,L)=CSS(10,K,L)+SP(5,L)*PV(3,N)
      END IF
      IF (ISURF(KS).EQ.2) THEN
        CSS(2,K,L)=CSS(2,K,L)+SP(5,L)*PV(2,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*PV(1,N)
        CSS(10,K,L)=CSS(10,K,L)+SP(5,L)*PV(3,N)
      END IF
      IF (ISURF(KS).EQ.3) THEN
        CSS(2,K,L)=CSS(2,K,L)-SP(5,L)*PV(1,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*PV(2,N)
        CSS(10,K,L)=CSS(10,K,L)+SP(5,L)*PV(3,N)
      END IF
      IF (ISURF(KS).EQ.4) THEN
        CSS(2,K,L)=CSS(2,K,L)+SP(5,L)*PV(1,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*PV(2,N)
        CSS(10,K,L)=CSS(10,K,L)+SP(5,L)*PV(3,N)
      END IF
      IF (ISURF(KS).EQ.5) THEN
        CSS(2,K,L)=CSS(2,K,L)-SP(5,L)*PV(3,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*PV(1,N)
        CSS(10,K,L)=CSS(10,K,L)+SP(5,L)*PV(2,N)
      END IF
      IF (ISURF(KS).EQ.6) THEN
        CSS(2,K,L)=CSS(2,K,L)+SP(5,L)*PV(3,N)
        CSS(4,K,L)=CSS(4,K,L)+SP(5,L)*PV(1,N)
        CSS(10,K,L)=CSS(10,K,L)+SP(5,L)*PV(2,N)
      END IF
      CSS(5,K,L)=CSS(5,K,L)+0.5*SP(5,L)
     &           *(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
      IF (MNMR.GT.1) CSS(7,K,L)=CSS(7,K,L)+PR(N)
*
      IF (TSURF(KS).LT.0.) THEN
*--specular reflection
        IF (ISURF(KS).EQ.1.OR.ISURF(KS).EQ.2) PV(2,N)=-PV(2,N)
        IF (ISURF(KS).EQ.3.OR.ISURF(KS).EQ.4) PV(1,N)=-PV(1,N)
        IF (ISURF(KS).EQ.5.OR.ISURF(KS).EQ.6) PV(3,N)=-PV(3,N)
      ELSE
*--diffuse reflection
        VMP=SQRT(2.*BOLTZ*TSURF(KS)/SP(5,L))
*--VMP is the most probable speed in species L, see eqns (4.1) and (4.7)
        IF (ISURF(KS).EQ.1) THEN
          PV(2,N)=SQRT(-LOG(RF(0)))*VMP
          CALL RVELC(PV(1,N),PV(3,N),VMP)
        END IF
        IF (ISURF(KS).EQ.2) THEN
          PV(2,N)=-SQRT(-LOG(RF(0)))*VMP
          CALL RVELC(PV(1,N),PV(3,N),VMP)
        END IF
        IF (ISURF(KS).EQ.3) THEN
          PV(1,N)=SQRT(-LOG(RF(0)))*VMP
          CALL RVELC(PV(2,N),PV(3,N),VMP)
        END IF
        IF (ISURF(KS).EQ.4) THEN
          PV(1,N)=-SQRT(-LOG(RF(0)))*VMP
          CALL RVELC(PV(2,N),PV(3,N),VMP)
        END IF
        IF (ISURF(KS).EQ.5) THEN
          PV(3,N)=SQRT(-LOG(RF(0)))*VMP
          CALL RVELC(PV(2,N),PV(1,N),VMP)
        END IF
        IF (ISURF(KS).EQ.6) THEN
          PV(3,N)=-SQRT(-LOG(RF(0)))*VMP
          CALL RVELC(PV(2,N),PV(1,N),VMP)
        END IF
*--the normal velocity component has been generated
*--a single call of RVELC generates the two tangential vel. components
        IF (ISPR(1,L).GT.0) CALL SROT(PR(N),TSURF(KS),ISPR(1,L))
      END IF
      IF (ISURF(KS).EQ.1) THEN
        PP(1,N)=XC
        PP(2,N)=YC+0.001*CH
        PP(3,N)=ZC
      END IF
      IF (ISURF(KS).EQ.2) THEN
        PP(1,N)=XC
        PP(2,N)=YC-0.001*CH
        PP(3,N)=ZC
      END IF
      IF (ISURF(KS).EQ.3) THEN
        PP(1,N)=XC+0.001*CW
        PP(2,N)=YC
        PP(3,N)=ZC
      END IF
      IF (ISURF(KS).EQ.4) THEN
        PP(1,N)=XC-0.001*CW
        PP(2,N)=YC
        PP(3,N)=ZC
      END IF
      IF (ISURF(KS).EQ.5) THEN
        PP(3,N)=ZC+0.001*CD
        PP(2,N)=YC
        PP(1,N)=XC
      END IF
      IF (ISURF(KS).EQ.6) THEN
        PP(3,N)=ZC-0.001*CD
        PP(2,N)=YC
        PP(1,N)=XC
      END IF
      IPL(N)=(MC-1)*NSCX*NSCY*NSCZ+1
*--sample the surface properties due to the reflected molecules
      IF (ISURF(KS).EQ.1) CSS(3,K,L)=CSS(3,K,L)+SP(5,L)*PV(2,N)
      IF (ISURF(KS).EQ.2) CSS(3,K,L)=CSS(3,K,L)-SP(5,L)*PV(2,N)
      IF (ISURF(KS).EQ.3) CSS(3,K,L)=CSS(3,K,L)+SP(5,L)*PV(1,N)
      IF (ISURF(KS).EQ.4) CSS(3,K,L)=CSS(3,K,L)-SP(5,L)*PV(1,N)
      IF (ISURF(KS).EQ.5) CSS(3,K,L)=CSS(3,K,L)+SP(5,L)*PV(3,N)
      IF (ISURF(KS).EQ.6) CSS(3,K,L)=CSS(3,K,L)-SP(5,L)*PV(3,N)
      IF (ISURF(KS).LT.3) THEN
        CSS(9,K,L)=CSS(9,K,L)-SP(5,L)*PV(1,N)
        CSS(11,K,L)=CSS(11,K,L)-SP(5,L)*PV(3,N)
      END IF
      IF (ISURF(KS).GT.2.AND.ISURF(KS).LT.5) THEN
        CSS(9,K,L)=CSS(9,K,L)-SP(5,L)*PV(2,N)
        CSS(11,K,L)=CSS(11,K,L)-SP(5,L)*PV(3,N)
      END IF
      IF (ISURF(KS).GT.4) THEN
        CSS(9,K,L)=CSS(9,K,L)-SP(5,L)*PV(1,N)
        CSS(11,K,L)=CSS(11,K,L)-SP(5,L)*PV(2,N)
      END IF
      CSS(6,K,L)=CSS(6,K,L)-0.5*SP(5,L)
     &           *(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
      IF (MNMR.GT.1) CSS(8,K,L)=CSS(8,K,L)-PR(N)
      RETURN
      END
*   REMOVE.FOR
      SUBROUTINE REMOVE(N)
*
*--remove molecule N and replace it by molecule NM
*
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /MOLSR / PR(MNMR)
*
      DO 100 M=1,3
        PP(M,N)=PP(M,NM)
        PV(M,N)=PV(M,NM)
100   CONTINUE
      IF (MNMR.GT.1) PR(N)=PR(NM)
      IPL(N)=IPL(NM)
      IF (MNSP.GT.1) IPS(N)=IPS(NM)
      NM=NM-1
      N=N-1
      RETURN
      END
*   SAMPI3.FOR
*
      SUBROUTINE SAMPI3
*
*--initialises all the sampling variables
*
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSS(11,MNSE,MNSP)
*
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
      COMMON /SAMPR / CSR
      COMMON /SAMPS / CSS,ICS(MNSE)
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
*
      NSMP=0
      TIMI=TIME
      DO 200 L=1,MNSP
        DO 50 N=1,MNC
          CS(1,N,L)=1.E-6
          DO 20 M=2,7
            CS(M,N,L)=0.
20        CONTINUE
          CSR(N,L)=0.
50      CONTINUE
        DO 100 N=1,MNSE
          CSS(1,N,L)=1.E-6
          ICS(N)=0
*--ICS remains zero until the element is struck by a molecule
          DO 60 M=2,11
            CSS(M,N,L)=0.
60        CONTINUE
100     CONTINUE
200   CONTINUE
      RETURN
      END
*   SAMPLE3.FOR
*
      SUBROUTINE SAMPLE3
*
*--sample the molecules in the flow.
*
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /MOLSR / PR(MNMR)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
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
              IF (MNSP.GT.1) THEN
                I=IPS(M)
              ELSE
                I=1
              END IF
              CS(1,N,I)=CS(1,N,I)+1
              DO 5 LL=1,3
                CS(LL+1,N,I)=CS(LL+1,N,I)+PV(LL,M)
                CS(LL+4,N,I)=CS(LL+4,N,I)+PV(LL,M)**2
5             CONTINUE
              IF (MNMR.GT.1) CSR(N,I)=CSR(N,I)+PR(M)
10          CONTINUE
          END IF
50      CONTINUE
100   CONTINUE
      RETURN
      END
*   OUT3.FOR
*
*
      SUBROUTINE OUT3
*
*--output a progressive set of results to file DSMC3.OUT.
*
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
      DOUBLE PRECISION CSS(11,MNSE,MNSP)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /MOLSR / PR(MNMR)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
      COMMON /SAMPR / CSR
      COMMON /SAMPS / CSS,ICS(MNSE)
      COMMON /GEOM3 / NSCX,NSCY,NSCZ,CB(6),IB(6),ISURF(2),LIMS(2,5),IIS,
     &                ISG,TSURF(2),BME(6,MNSP),BMR(6,MNSP),CW,FW,CH,FH,
     &                FD,CD,IJET,XJ,ZJ,RJ,TMPJ,FNDJ,FVJ,FSPJ(MNSP),
     &                BMEJ(MNSP),BMRJ(MNSP)
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /CONST / PI,SPI,BOLTZ
*
      DOUBLE PRECISION VEL(3),SMU(3),SVEL(3,MNC),SN,SM,SMCC,SRDF,SRE,TT,
     &                 TROT,DBOLTZ,SS(11)
      DBOLTZ=BOLTZ
*
      OPEN (4,FILE='DSMC3.OUT',FORM='FORMATTED')
*
      WRITE (4,*) ' FLOW SAMPLED FROM TIME ',TIMI,' TO TIME ',TIME
      WRITE (4,*) ' COLLISIONS:-'
      WRITE (4,99001) ((COL(M,L),M=1,MNSP),L=1,MNSP)
99001 FORMAT (5F14.0)
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
      WRITE (4,*)
      DO 100 KS=1,2
        IF (ISURF(KS).GT.0) THEN
          WRITE (4,*) ' SURFACE ',KS
          WRITE (4,*)
          NEL=(LIMS(KS,3)-LIMS(KS,2)+1)*(LIMS(KS,5)-LIMS(KS,4)+1)
          IF (KS.EQ.1) THEN
            NEL1=1
            NEL2=NEL
          ELSE
            NEL1=(LIMS(1,3)-LIMS(1,2)+1)*(LIMS(1,5)-LIMS(1,4)+1)+1
            NEL2=NEL1+NEL-1
          END IF
          A=FNUM/(TIME-TIMI)
          WRITE (4,*) 
     &'    X COORD     Y COORD      Z COORD     SAMPLE     FRACTIONSPECI
     &ES 1,    SPECIES2....'
          DO 20 K=NEL1,NEL2
            NC=ICS(K)
            IF (ICS(K).GT.0) THEN
              IF (ISURF(KS).LT.3) THEN
                Y=(LIMS(KS,1)-1)*CH+CB(3)
                X=0.5*(CG(1,NC)+CG(2,NC))
                Z=0.5*(CG(5,NC)+CG(6,NC))
              END IF
              IF (ISURF(KS).GT.2.AND.ISURF(KS).LT.5) THEN
                X=(LIMS(KS,1)-1)*CW+CB(1)
                Y=0.5*(CG(3,NC)+CG(4,NC))
                Z=0.5*(CG(5,NC)+CG(6,NC))
              END IF
              IF (ISURF(KS).GT.4) THEN
                Z=(LIMS(KS,1)-1)*CD+CB(5)
                Y=0.5*(CG(3,NC)+CG(4,NC))
                X=0.5*(CG(1,NC)+CG(2,NC))
              END IF
              SS(1)=0.
              DO 5 L=1,MNSP
                SS(1)=SS(1)+CSS(1,K,L)
5             CONTINUE
              WRITE (4,99002) X,Y,Z,SS(1),(CSS(1,K,L)/SS(1),L=1,MNSP)
99002         FORMAT (3F12.5,F12.1,7F13.6)
            END IF
20        CONTINUE
*
          WRITE (4,*) 
     &'     X COORD      Y COORD     Z COORD     NUM FLUX    INC PRESSRE
     &FL PRESS   INC SH ST1    REFL SH ST1   INC SH ST2   REFL SH ST2INC
     & TR EN   REF TR EN   INC ROT EN   REFL ROTEN  NET HEAT FLUX'
          DO 40 K=NEL1,NEL2
            NC=ICS(K)
            IF (ICS(K).GT.0) THEN
              IF (ISURF(KS).LT.3) THEN
                Y=(LIMS(KS,1)-1)*CH+CB(3)
                X=0.5*(CG(1,NC)+CG(2,NC))
                Z=0.5*(CG(5,NC)+CG(6,NC))
                AR=CW*CD
              END IF
              IF (ISURF(KS).GT.2.AND.ISURF(KS).LT.5) THEN
                X=(LIMS(KS,1)-1)*CW+CB(1)
                Y=0.5*(CG(3,NC)+CG(4,NC))
                Z=0.5*(CG(5,NC)+CG(6,NC))
                AR=CH*CD
              END IF
              IF (ISURF(KS).GT.4) THEN
                Z=(LIMS(KS,1)-1)*CD+CB(5)
                Y=0.5*(CG(3,NC)+CG(4,NC))
                X=0.5*(CG(1,NC)+CG(2,NC))
                AR=CW*CH
              END IF
              DO 25 N=1,11
                SS(N)=0.
                DO 22 L=1,MNSP
                  SS(N)=SS(N)+CSS(N,K,L)
22              CONTINUE
25            CONTINUE
              DO 30 N=1,11
                SS(N)=SS(N)*A/AR
30            CONTINUE
              WRITE (4,99003) X,Y,Z,(SS(N),N=1,4),SS(9),SS(10),SS(11),
     &                        SS(5),SS(6),SS(7),SS(8),SS(5)+SS(6)+SS(7)
     &                        +SS(8)
99003         FORMAT (15E13.5)
            END IF
40        CONTINUE
        END IF
100   CONTINUE
*
      WRITE (4,*) ' FLOWFIELD PROPERTIES '
      WRITE (4,*) 'SAMPLES'
      WRITE (4,*) ' CELL     N SP 1    N SP 2     ETC '
      DO 200 N=1,MNC
        WRITE (4,99004) N,(CS(1,N,L),L=1,MNSP)
200   CONTINUE
99004 FORMAT (' ',6D12.0)
*
      WRITE (4,*) ' FLOWFIELD PROPERTIES'
      WRITE (4,*) 
     &'  CELL   X COORD   Y COORD   Z COORD   DENSITY   TR TEMP   ROTL T
     &EMP    OV. TEMP      U         V         W'
*--first the mixture properties
      DO 400 N=1,MNC
        A=FNUM/(CC*NSMP)
        SN=0.
        SM=0.
        DO 250 K=1,3
          SMU(K)=0.
250     CONTINUE
        SMCC=0.
        SRE=0.
        SRDF=0.
        DO 300 L=1,MNSP
          SN=SN+CS(1,N,L)
*--SN is the number sum
          SM=SM+SP(5,L)*CS(1,N,L)
*--SM is the sum of molecular masses
          DO 260 K=1,3
            SMU(K)=SMU(K)+SP(5,L)*CS(K+1,N,L)
*--SMU(1 to 3) are the sum of mu, mv, mw
260       CONTINUE
          SMCC=SMCC+(CS(5,N,L)+CS(6,N,L)+CS(7,N,L))*SP(5,L)
*--SMCC is the sum of m(u**2+v**2+w**2)
          SRE=SRE+CSR(N,L)
*--SRE is the sum of rotational energy
          SRDF=SRDF+ISPR(1,L)*CS(1,N,L)
*--SRDF is the sum of the rotational degrees of freedom
          SUU=SUU+SP(5,L)*CS(5,N,L)
*--SUU is the sum of m*u*u
300     CONTINUE
        DENN=SN*A
*--DENN is the number density, see eqn (1.34)
        DEN=DENN*SM/SN
*--DEN is the density, see eqn (1.42)
        DO 350 K=1,3
          VEL(K)=SMU(K)/SM
          SVEL(K,N)=VEL(K)
350     CONTINUE
*--VEL and SVEL are the stream velocity components, see eqn (1.43)
        UU=VEL(1)**2+VEL(2)**2+VEL(3)**2
        TT=(SMCC-SM*UU)/(3.D00*DBOLTZ*SN)
*--TT is the translational temperature, see eqn (1.51)
        IF (SRDF.GT.1.E-6) TROT=(2.D00/DBOLTZ)*SRE/SRDF
*--TROT is the rotational temperature, see eqn (11.11)
        TEMP=(3.D00*TT+(SRDF/SN)*TROT)/(3.+SRDF/SN)
*--TEMP is the overall temperature, see eqn (11.12)
        CT(N)=TEMP
        XC=0.5*(CG(1,N)+CG(2,N))
        YC=0.5*(CG(3,N)+CG(4,N))
        ZC=0.5*(CG(5,N)+CG(6,N))
*--XC,YC are the x,y coordinates of the midpoint of the cell
        WRITE (4,99005) N,XC,YC,ZC,DEN,TT,TROT,TEMP,VEL(1),VEL(2),VEL(3)
99005   FORMAT (' ',I5,3F11.5,1P,E12.4,0P,6F10.4,2E12.4)
400   CONTINUE
*
      WRITE (4,*)
      DO 500 L=1,MNSP
*--now the properties of the separate species
        WRITE (4,*) ' SPECIES ',L
        WRITE (4,*) 
     &' CELL   X COORD    Y COORD    Z COORD   N DENS    DENSITY    TTX 
     &   TTY       TTZ     TR TEMP    ROT TEMP     TEMP    U DIF VEL VDI
     &F VEL   W DIF VEL '
        DO 450 N=1,MNC
          A=FNUM/(CC*NSMP)
          DENN=CS(1,N,L)*A
*--DENN is the partial number density
          DEN=SP(5,L)*DENN
*--DEN is the partial density, see eqn (1.13)
          DO 420 K=1,3
            VEL(K)=CS(K+1,N,L)/CS(1,N,L)
*--VEL defines the average velocity of the species L molecules
420       CONTINUE
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
          DO 440 K=1,3
            VEL(K)=VEL(K)-SVEL(K,N)
*--VEL now defines the diffusion velocity of species L, see eqn (1.45)
440       CONTINUE
          XC=0.5*(CG(1,N)+CG(2,N))
          YC=0.5*(CG(3,N)+CG(4,N))
          ZC=0.5*(CG(5,N)+CG(6,N))
          WRITE (4,99006) N,XC,YC,ZC,DENN,DEN,TTX,TTY,TTZ,TT,TROT,TEMP,
     &                    VEL(1),VEL(2),VEL(3)
99006     FORMAT (' ',I5,3F10.5,1P,2E12.4,0P,9F10.4)
450     CONTINUE
500   CONTINUE
*
*
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
*   ERF.FOR
*
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
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
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
        IF (MNSP.GT.1) THEN
          LS=IPS(N)
        ELSE
          LS=1
        END IF
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
        IF (MNSP.GT.1) THEN
          LS=IPS(N)
        ELSE
          LS=1
        END IF
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
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
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
      IF (MNSP.GT.1) THEN
        LS=IPS(L)
        MS=IPS(M)
      ELSE
        LS=1
        MS=1
      END IF
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
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
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
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
      DOUBLE PRECISION CSR(MNC,MNSP)
*
      COMMON /MOLS3 / NM,PP(3,MNM),PV(3,MNM),IPL(MNM),IPS(MNMS),IR(MNM)
      COMMON /MOLSR / PR(MNMR)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
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
            ASEL=0.5*IC(2,N,NN)*AVN*FNUM*CCG(1,N,NN,MM)
     &           *DTM/CC+CCG(2,N,NN,MM)
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
                    SEPT=SEPT+
     &                   SQRT((PP(1,L)-PP(1,M))**2+(PP(2,L)-PP(2,M))
     &                   **2+(PP(3,L)-PP(3,M))**2)
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
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      COMMON /MOLSR / PR(MNMR)
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
*   DATA3.FOR
*
      SUBROUTINE DATA3
*
*--defines the data for a particular run of DSMC3.FOR.
*
      PARAMETER (MNM=70000,MNC=9720,MNSC=9720,MNSP=1,MNSG=1,MNSE=900,
     &           MNMR=1,MNMS=1)
*
      DOUBLE PRECISION COL(MNSP,MNSP),MOVT,NCOL,SELT,SEPT,CS(7,MNC,MNSP)
*
      COMMON /GAS   / SP(5,MNSP),SPM(6,MNSP,MNSP),ISP(MNSP)
      COMMON /GASR  / SPR(3,MNSP,MNSP),ISPR(3,MNSP),CT(MNC)
      COMMON /CELL3 / CC,CG(6,MNC),IC(2,MNC,MNSG),ISC(MNSC),
     &                CCG(2,MNC,MNSG,MNSG),ISCG(2,MNSC,MNSG),IG(2,MNSG),
     &                NCX,NCY,NCZ
      COMMON /SAMP3 / COL,NCOL,MOVT,SELT,SEPT,CS,TIME,NPR,NSMP,FND,FTMP,
     &                TIMI,FSP(MNSP),ISPD,VFX,VFY,VFZ
      COMMON /COMP  / FNUM,DTM,NIS,NSP,NPS,NPT
      COMMON /GEOM3 / NSCX,NSCY,NSCZ,CB(6),IB(6),ISURF(2),LIMS(2,5),IIS,
     &                ISG,TSURF(2),BME(6,MNSP),BMR(6,MNSP),CW,FW,CH,FH,
     &                FD,CD,IJET,XJ,ZJ,RJ,TMPJ,FNDJ,FVJ,FSPJ(MNSP),
     &                BMEJ(MNSP),BMRJ(MNSP)
*
*--set data (must be consistent with PARAMETER variables)
*
      NCX=30
      NCY=18
      NCZ=18
      NSCX=1
      NSCY=1
      NSCZ=1
      IIS=1
      ISG=1
      FTMP=300.
*--FTMP is the temperature
      FND=1.E20
*--FND is the number densty
      VFX=1936.
      VFY=0.
      VFZ=0.
      FSP(1)=1.
*--FSP(N) is the number fraction of species N
      FNUM=0.2E14
*--FNUM  is the number of real molecules represented by a simulated mol.
      DTM=2.E-6
*--DTM is the time step
      CB(1)=0.0
      CB(2)=0.3
      CB(3)=0.
      CB(4)=0.18
      CB(5)=0.
      CB(6)=0.18
*--the simulated region is from x=XB(1) to x=XB(2)
      IB(1)=1
      IB(2)=1
      IB(3)=2
      IB(4)=1
      IB(5)=2
      IB(6)=1
      ISURF(1)=1
      LIMS(1,1)=1
      LIMS(1,2)=6
      LIMS(1,3)=30
      LIMS(1,4)=1
      LIMS(1,5)=18
      TSURF(1)=1000.
      ISURF(2)=5
      LIMS(2,1)=1
      LIMS(2,2)=6
      LIMS(2,3)=30
      LIMS(2,4)=1
      LIMS(2,5)=18
      TSURF(2)=1000.
      IJET=0
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
      SPR(1,1,1)=1.
      ISPR(2,1)=0
      ISP(1)=1
*--ISPR(1,N) is the number of degrees of freedom of species N
*--SPR(1,N,K) is the constant in the polynomial for the rotational
*--relaxation collision number of species N in collision with species K
*--ISPR(2,N) is 0,1 for constant, polynomial for collision number
      NIS=2
*--NIS is the number of time steps between samples
      NSP=10
*--NSP is the number of samples between restart and output file updates
      NPS=15
*--NPS is the number of updates to reach assumed steady flow
      NPT=10000
*--NPT is the number of file updates to STOP
*
      RETURN
      END

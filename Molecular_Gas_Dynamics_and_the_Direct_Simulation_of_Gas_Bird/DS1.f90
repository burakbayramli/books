!******************************************************************************
!****DSMC program for one-dimensional plane, cylindrical or spherical flows****
!******************************************************************************
!
!--Version 1.10
!
!******************************************************************************
!
!--Development History 
!
!--Version 1.1  writing commenced 16 November 2007, and was used for the RGD26 chemistry work
!--Version 1.2  added the full coding for cylindrical and spherical flows (from April 2009)
!--Version 1.3  added the moving wall and enhanced unsteady sampling options
!--Version 1.4  added radial weighting factors
!--Version 1.5  extended the new chemistry model to cope with O2-H2 sysyem 
!--Version 1.6  modified model for exothermic reactions
!--Version 1.7  added velocity in y direction
!--Version 1.8  removed diagnostic subroutines - first version for web(POSTED Nov 25, 2010)
!--Version 1.9  revised volume for ternary collisions, only the relevant vibrational modes are considered in reactions,
!               an improved procedure when there are multiple reactions possible for the collision pair
!--Version 1.10 new Q-K procedure for exothermic exchange and chain reactions
!
!******************************************************************************
!
!--Base SI units are employed throughout the program
!
!--Variables in data file DS1VD.DAT
!
!--NVER the n in version n.m that generated the data file
!--MVER the m in version n.m that generated the data file
!
!--m must change whenever the data file is altered
!
!--AMEG the approximate number of megabytes that are initially available to the calculation
!
!--IFX 0 for plane flows, 1 for cylindrical flows or 2 for spherical flows (the axis for cyl. flows and the center of spherical flows is at x=0)
!
!--the type of the boundaries of the flowfield is set by ITYPE, the number code is
!    0 for a boundary with the stream
!    1 plane of symmetry for IFX=0, or center for IFX>0 and XB(1)=0, or mandatory for outer boundary if IVB=1
!    2 for solid surface
!    3 for vacuum
!    
!--XB(1) the minimum x coordinate of the flow (must be zero or positive if IFX>0)
!--ITYPE(1) sets type of XB(1)
! 
!--XB(2) the maximum x coordinate of the flow 
!--ITYPE(2) sets type of XB(2)
!
!--if IFX>0
!----IWF 0 for no weighting factors, 1 for weighting factors based on radius
!------the weighting factors are 1+WFM*(r/r_max)**IFX 
!----if (IWF=1) WFM the (maximum weighting factor -1) 
!--end if
!
!--GASCODE the code number of the gas
!    1 for a hard sphere gas
!    2 for argon
!    3 for nitrogen
!    4 for real oxygen
!    5 for ideal air
!    6 for real air @ 7.5 km/s
!    7 for helium-xenon mixture with argon molecular weight
!    8 for combustible hydrogen-oxygen
!
!--IGS 0 if the initial state is a vacuum, 1 if it is the stream or reference gas and (if present) the secondary stream 
!--ISECS=0 if there is no secondary stream, 1 if there is a secondary stream that applies when x>0. for IFX=0
!          (XB(1) must then be negative and XB(2) must be positive),or x>XS for IFX>0.
!--if (IFX>0 and ISECS=1) 
!----XS the initial boundary between the initialand secondary streams
!----ISECS must be zero if there are radial weighting factors       
!--in a loop over K=1,2 (1 for XB(1), 2 for XB(2)
!----if K=1 or (K=2 and ISECS=1) 
!------FND(K)  the number density of the stream or reference gas  
!------FTMP(K) temperature of stream or reference gas
!------FVTMP(K) vibrational temperature of stream or reference gas (if any species have vibrational modes)
!------VFX(K) the velocity component in the x direction
!------VFY(K) the velocity component in the y direction
!------in a loop over the number of species (if there is more than one species)
!--------FSP(N,K)) the fraction of species N in the stream or reference gas
!------end if
!----end if
!----if ITYPE(K)=2
!------TSURF(K) the surface temperature
!------FSPEC(K) the fraction of specular reflection at this surface
!------VSURF(K) the y velocity component
!----end if
!--end loop over ends
!
!--if ITYPE(1)=0 (stream) and IFX=0
!----IREM 0 for no molecule removal (based on "stagnation streamline" theory)
!         1 for steady removal (FREM=1) set as data (e.g. for shock wave problem)
!         2 for removal set as a restart option
!----if IREM=1
!------XREM the coordinate at which the removal starts (uniform from XREM to XB(2))
!----end if
!--end if
!
!--if ITYPE(2)=1
!----IVB 0 if the maximum x boundary is stationary, 1 if it is a moving specularly reflecting piston
!------if IVB=1, ITYPE(2)=1 
!----if (IVB = 1) VELOB  the velocity of the boundary
!--end if
!
!--MOLSC target number of molecules per sampling cell
!--NCIS number of collision cells in a sampling cell 
!--CPDTM collisions in time step
!--collisions appropriate to CPDTM*(local mean collision time)are calculated in a collision cell
!  when its time falls half this time interval behind the flow time 
!--TPDTM maximum flow transits of a sampling cell in a time step
!--a molecule is moved appropriate to the minimum of CPDTM*(local mean coll time) and TPDTM*(local transit time)
!  when its time falls half this time interval behind the flow time 
!--NNC 0 to select collision partner randomly from collision cell, 1 for nearest neighbor collisions
!--IMTS 0 for move time steps that are the same for all cells, 1 if they are allowed to vary over the cells
!--SAMPRAT the number of time steps between samplings
!--OUTRAT the number of sampling steps bewteen the output of results
!--ISF 0 if the sample is reset only as a user restart option, 1 if it is reset at the end of each output interval (unsteady flow sampling)
!--FRACSAM fraction (at the end) of the output interval in which samples are made in a flow with unsteady sampling 
!--JCD 0 if any reactions are based on rate equations (GASCODE=6 or 8 only), 1 if they are based on the Q-K reaction model
!--IRECOM 0 if recombination reactions can be neglected, 1 if they must be included
!
!************************************************************************************
!
!--Restart options in Version 1.1
!
!--if IREM=2
!----FREM fraction of molecule removal 
!----XREM the coordinate at which removal commences
!--end if 
!
!************************************************************************************
!
MODULE MOLECS
!
!--declares the variables associated with the molecules
!
IMPLICIT NONE
!
INTEGER, ALLOCATABLE, DIMENSION(:) :: IPCELL,IPSP,ICREF,IPCP
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: IPVIB
INTEGER :: NM,MNM
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: PV
REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: PX,PTIM,PROT
!
!--PX(N) position (x coordinate) of molecule N
!--PTIM(N) molecule time
!--PROT(N) rotational energy
!--PV(1-3,N) u,v,w velocity components
!--IPSP(N) the molecular species
!--IPCELL(N) the collision cell number
!--ICREF the cross-reference array (molecule numbers in order of collision cells)
!--IPCP(N) the code number of the last collision partner of molecule 
!--IPVIB(K,N) level of vibrational mode K of molecule N
!--NM number of molecules
!--MNM the maximum number of molecules
!
END MODULE MOLECS
!
!*************************************************************************************
!
MODULE GEOM
!
!--declares the variables associated with the flowfield geometry and cell structure
!
IMPLICIT NONE
!
INTEGER :: NCELLS,NCCELLS,NCIS,NDIV,MDIV,ILEVEL,IFX,JFX,IVB,IWF    
INTEGER, DIMENSION(2) :: ITYPE            
INTEGER, ALLOCATABLE, DIMENSION(:) :: ICELL
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ICCELL,JDIV
REAL(KIND=8) :: DDIV,XS,VELOB,WFM,AWF
REAL(KIND=8), DIMENSION(2) :: XB
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: CELL,CCELL
!
!--XB(1), XB(2) the minimum, maximum x coordinate
!--DDIV the width of a division
!--ITYPE(K) the tpe of boundary at the minimum x (K=1) and maximum x (K=2) boundaries
!          0 for a stream boundary
!          1 for a plane of symmetry
!          2 for a solid surface
!          3 for a vacuum
!--NCELLS the number of sampling cells
!--NCCELLS the number of collision cells
!--NCIS the number of collision cells in a sampling cell
!  MDIV the maximum number of sampling cell divisions at any level of subdivision
!--IVB 0,1 for stationary, moving outer boundary
!--IWF 0 for no radial weighting factors, 1 for radial weighting factors
!--WFM, set in data as the maximum weighting factor -1, then divided by the maximum radius
!--AWF overall ratio of real to weighted molecules 
!--VELOB the speed of the outer boundary
!--JDIV(N,M) (-cell number) or (start address -1 in JDIV(N+1,M), where M is MDIV
!--JFX  IFX+1
!--CELL(M,N) information on sampling cell N
!    M=1 x coordinate
!    M=2 minimum x coordinate
!    M=3 maximum x cooedinate 
!    M=4 volume
!--ICELL(N) number of collision cells preceding those in sampling cell N
!--CCELL(M,N) information on collision cell N
!    M=1 volume
!    M=2 remainder in collision counting
!    M=3 half the allowed time step
!    M=4 maximum value of product of cross-section and relative velocity 
!    M=5 collision cell time
!--ICCELL(M,N) integer information on collision cell N
!    M=1 the (start address -1) in ICREF of molecules in collision cell N
!    M=2 the number of molecules in collision cell N
!    M=3 the sampling cell in which the collision cell lies
!
END MODULE GEOM
!
!********************************************************************
!
MODULE GAS
!
!--declares the variables associated with the molecular species and the stream definition
!
IMPLICIT NONE
!
REAL(KIND=8) :: RMAS,CXSS,RGFS,VMPM,FDEN,FPR,FMA,FP,CTM
REAL(KIND=8), DIMENSION(2) :: FND,FTMP,FVTMP,VFX,VFY,TSURF,FSPEC,VSURF
REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: CI,AE,AC,BC,ER,ERS,CR,TNEX,psf
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: FSP,SP,SPR,SPV,REA,THBP,VMP
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:) :: SPM,SPVM,ENTR
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:,:) :: SPEX
INTEGER :: MSP,MMVM,MMRM,MNRE,MNSR,MTBP,GASCODE,MMEX,MEX
INTEGER, ALLOCATABLE, DIMENSION(:) :: ISP,ISPV,LE,ME,KP,LP,MP
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ISPR,IREA,NREA,NRSP,LIS,LRS,ISRCD,ISPRC,TREACG,TREACL,NSPEX
INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: ISPVM,NEX,IRCD,JREA
INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: ISPEX
!
!--MSP the number of molecular species
!--MMVM the maximum number of vibrational modes of any species
!--MEX number of exchange or chain reactions
!--MMEX the maximum number of exchange reactions involving the same precollision pair of molecules
!--MMRM 0 if gass is completely monatomic, 1 if some species have rotation
!--MNRE the number of gas phase chemical reactions (old scheme)
!--MNSR the number oF surface reactions
!--MTBP the number of third body tables
!--SP(1,L) the reference diameter of species L       
!--SP(2,L) the reference temperature of species L
!--SP(3,L) the viscosity-temperature power law of species L
!--SP(4,L) the reciprocal of the VSS scattering parameter
!--SP(5,L) molecular mass of species L
!--ISPR(1,L) number of rotational degrees of freedom of species L
!--ISPR(2,L) 0,1 for constant, polynomial rotational relaxation collision number
!--SPR(1,L) constant rotational relaxation collision number of species L
!           or the constant in a second order polynomial in temperature
!--SPR(2,L) the coefficient of temperature in the polynomial
!--SPR(3,L) the coefficient of temperature squared in the polynomial
!--SPM(1,L,M) the reduced mass for species L,M
!--SPM(2,L,M) the reference collision cross-section for species L,M
!--SPM(3,L,M) the mean value of the viscosity-temperature power law
!--SPM(4,L,M) the reference diameter for L,M collisions
!--SPM(5,L,M) the reference temperature for species L,M
!--SPM(6,L,M) reciprocal of the gamma function of (5/2-w) for species L,M
!--SPM(7,L,M) rotational relaxation collision number for species L,M, or const in polynomial
!--SPM(8,L,M) reciprocal of VSS scattering parameter
!--ISPV(L) the number of vibrational modes
!--SPVM(1,K,L) the characteristic vibrational temperature
!--SPVM(2,K,L) constant Zv, or reference Zv for mode K
!--SPVM(3,K,L) -1. for constant Zv, or reference temperature
!--SPVM(4,K,L) the characteristic dissociation temperature
!--ISPVM(1,K,L) the species code of the first dissociation product
!--ISPVM(2,K,L) the species code of the second dissociation product
!--ISPRC(L,M) the species of the recombined molecule from species L and M
!--NSPEX(L,M) the number of exchange reactios with L,M as the pre-collision species
!--in the following variables, J is the reaction number (1 to NSPEX(L,M))
!--ISPEX(J,0,L,M) the species that splits in an exchange reaction
!--ISPEX(J,1,L,M) the post collision species of the molecule that splits in an L,M exchange reaction (all ISPEX are set to 0 if no exchange reaction)
!--ISPEX(J,2,L,M) the post collision species of either the atom or the molecule that does not split in an L,M exchange reaction
!--ISPEX(J,3 to 2+MMVM)1,0 if vibrational mode does, does not contribute to this reaction
!--SPEX(I,J,L,M) I=1 for the activation energy, I=2 for the heat of reaction
!--TNEX(N) total number of exchange reaction N 
!--NEX(N,L,M) the number of exchange or chain reactions in L,M collisions
!
!--The reaction information for the Q-K reaction model (JCD=1) is in the above
!--The reaction information for the traditional reaction model (JCD =0) follows 
!
!--IRCD(N,M,L) the code number of the Nth reaction between species M,L 
!--IREA(1,N),IREA(2,N) the codes of the pre-reaction species
!--JREA(1,N,K) the code of the Kth post reaction species from first mol.
!--JREA(2,N,K) similarly for second, but -ve indicates 3rd body table
!--NREA(1,N),NREA(2,N) the number of post-reaction molecules from each
!--NRSP(L,M) the number of chemical reactions for L-M precollision
!--REA(1,N) number of contributing internal degrees of freedom
!--REA(2,N) the activation energy for reaction N
!--REA(3,N) the constant in the reaction rate
!--REA(4,N) temperature exponent in the rate equation
!--REA(5,N) heat of reaction (+,- for exothermic,endothermic)
!--REA(6,N) non-energy terms in the steric factor expressions (6.10,14)
!--LE(N) the species code of the first molecule
!--ME(N) the species code of the second molecule
!--for three post-collision particles
!----KP(N) the first post-reaction species code
!----LP(N) the second post-reaction species code
!----MP(N) the third post-reaction species code
!--for one post-collision particle
!----KP(N)=-1
!----LP(N) the species code of the molecule
!----MP(N) if positive the code of the third body in a recombination
!          or, if negative, the code of the third-body table
!--for two post-collision particles
!----KP(N)=0
!----LP(N) the first post-reaction species code
!----MP(N) the second post-reaction species code
!----CI(N) the number of internal degrees of freedom that contribute
!----AE(N) the activation energy for the reaction
!----AC(N) the pre-exponential parameter
!----BC(N) the temperature exponent in the rate equation
!----ER(N) the energy of the reaction
!--THBP(N,L) third body efficiency in table N of species L 
!--RMAS reduced mass for single species case
!--CXSS reference cross-section for single species case
!--RGFS reciprocal of gamma function for single species case
!--for the following, J=1 for the reference gas and/or the minimum x boundary, J=2 for the secondary sream at maximum x boundary
!--FND(J) stream or reference gas number density
!--FTMP(J) stream temperature
!--FVTMP(J) the vibrational temperature in the freestream
!--VFX(J)  the x velocity components of the stream
!--VFY(J) the y velocity component in the stream
!--FSP(N,J)) fraction of species N in the stream
!--FMA stream Mach number
!--VMP(N,J) most probable molecular velocity of species N at FTMP(J)
!--VMPM the maximum value of VMP in stream 1
!--ENTR(M,L,K) entry/removal information for species L at K=1 for 1, K=2 for XB(2)
!    M=1 number per unut time
!    M=2 remainder
!    M=3 speed ratio
!    M=4 first constant
!    M=5 second constant
!    M=6 the maxinum normal velocity component in the removal zone (> XREM)
!--LIS(1,N) the species code of the first incident molecule
!--LIS(2,N) the species code of the second incident molecule (0 if none)
!--LRS(1,N) the species code of the first reflected molecule
!--LRS(2,N) the species code of the second reflected molecule (0 if none)
!--LRS(3,N) the species code of the third reflected molecule (0 if none)
!--LRS(4,N) the species code of the fourth reflected molecule (0 if none)
!--LRS(5,N) the species code of the fifth reflected molecule (0 if none)
!--LRS(6,N) the species code of the sixth reflected molecule (0 if none)
!--ERS(N) the energy of the reaction (+ve for recombination, -ve for dissociation) 
!--NSRSP(L) number of surface reactions that involve species L as incident molecule
!--ISRCD(N,L) code number of Nth surface reaction with species L as incident molecule  
!--CTM approximate mean collision time in stream (exact for simple gas)
!--FP approximate mean free path
!--FDEN stream 1 density  
!--FPR stream 1 pressure
!--FMA stream 1 Mach number
!--RMAS reduced mass for single species case
!--CXSS reference cross-section for single species case
!--RGFS reciprocal of gamma function for single species case
!--CR(L) collision rate of species L
!--TREACG(N,L) the total number of species L gained from reaction type N=1 for dissociation, 2 for recombination, 3 for forward exchange, 4 for reverse exchange
!--TREACL(N,L) the total number of species L lost from reaction type N=1 for dissociation, 2 for recombination, 3 for forward exchange, 4 for reverse exchange
!       
 END MODULE GAS
!
!********************************************************************
!
MODULE CALC
!
!--declares the variables associated with the calculation
!
IMPLICIT NONE
!
INTEGER :: NVER,MVER,MOLSC,JCD,ISF,ISECS,IGS,IREM,IRECOM,NNC,IMTS,ERROR
REAL (KIND=8) :: FREM,XREM,FTIME,TLIM,PI,SPI,DPI,BOLTZ,FNUM,DTM,TREF,TSAMP,TOUT,AMEG,SAMPRAT,OUTRAT,RANF,TOTCOLI,TOTMOVI,    &
                 DTSAMP,DTOUT,TPOUT,FRACSAM,TOTMOV,TOTCOL,ENTMASS,CPDTM,TPDTM,TDISS,TRECOMB,TFOREX,TREVEX,TOTDUP,AVOG
REAL (KIND=8), ALLOCATABLE, DIMENSION(:) :: VNMAX
REAL (KIND=8), ALLOCATABLE, DIMENSION(:,:) :: TCOL
!
!--NVER.MVER the version number
!--AMEG the initial number of megabytes to be used by the program
!--MOLSC the target number of molecules per sampling cell
!--FREM fraction of molecule removal 
!--XREM the coordinate at which removal commences 
!--FTIME the flow time
!--TLIM the time at which the calculation stops
!--FNUM the number of real molecules represented by each simulated molecule
!--CPDTM the maximum number of collisions per time step (standard 0.2)
!--TPDTM the maximum number of sampling cell transit times of the flow per time step 
!--TOTMOV total molecule moves
!--TOTCOL total collisions
!--TOTDUP total duplicated collisions
!--TDISS dissociations since sample reset
!--TFOREX forward exchange reactions since sample reset
!--TREVEX backward exchange reactions since sample reset
!--TRECOMB recombinations since sample reset
!--ENTMASS the current entry mass of which a fraction FREM is to be removed 
!--VNMAX(L) the maximum normal velocity component of species L
!--JCD  0 if chemical reactions are based on rate equations (GASCODE=6 only), 1 if they based on the Q-K model 
!--TCOL species dependent collision counter
!--ISF 0,1 for steady, unsteady flow sampling
!--IRECOM 0 to neglect recombination reactions, 1 to include them
!--ISECS 0,1 for no secondary stream,a secondary stream that applies for positive values of x
!--IREM data item to set type of molecule removal
!--NNC 0 for normal collisions, 1 for nearest neighbor collisions
!--IMTS 0 for uniform move time steps, 1 for time steps that vary over the cells
!--IGS 0 for initial gas, 1 for stream(s) or reference gas
!
END MODULE CALC
!
!********************************************************************
!
MODULE OUTPUT
!
!--declares the variables associated with the sampling and output
!
IMPLICIT NONE
!
INTEGER :: NSAMP,NMISAMP,NOUT
REAL (KIND=8):: TISAMP,XVELS,YVELS,AVDTM
REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: COLLS,WCOLLS,CLSEP,REAC,SREAC
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: VAR,VARS,CSSS,SUMVIB
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:) :: CS,VARSP,VIBFRAC
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:,:) :: CSS
!
!--NSAMP the number of samples
!--TISAMP the time at which the sampling was last reset
!--MNISAMP the number of molecules at the last reset
!--AVDTM the average value of DTM in the cells
!--NOUT the number of output intervals
!--COLLS(N) total number of collisions in sampling cell N
!--WCOLLS(N) total weighted collisins in N
!--CLSEP(N) sum of collision pair separation in cell N
!--CS(0,N,L) sampled number of species L in cell N
!--CS(1,N,L) sampled weighted number of species L in cell N
!----all the following CS are weighted sums 
!--CS(2,N,L), CS(3,N,L), CS(4,N,L) sampled sum of u, v, w
!--CS(5,N,L), CS(6,N,L), CS(7,N,L) sampled sum of u*u, v*v, w*w
!--CS(8,N,L) sampled sum of rotational energy of species L in cell N
!--CS(8+K,N,L) sampled sum of vibrational energy of species L in cell N
!              K is the mode
!
!--in CSS, M=1 for incident molecules and M=2 for reflected molecules 
!--J=1 for surface at x=XB(1), 2 for surface at x=XB(2)
!
!--CSS(0,J,L,M) number sum of molecules of species L
!--CSS(1,J,L,M) weighted number sum of molecules of species L
!----all the following CSS are weighted  
!--CSS(2,J,L,M) normal momentum sum to surface 
!--CSS(3,J,L,M) y momentum sum to surface 
!--CSS(4,J,L,M) z momentum sum to surface 
!--CSS(5,J,L,M) tranlational energy sum to surface 
!--CSS(6,J,L,M) rotational energy sum to surface
!--CSS(7,J,L,M) vibrational energy sum to the surface 
!--CSS(8,J,L,M) reaction energy sum to the surface 
!
!--CSSS(1,J) weighted sum (over incident AND reflected molecules) of 1/normal vel. component
!----all the following CSSS are weighted  
!--CSSS(2,J) similar sum of molecular mass / normal vel. component
!--CSSS(3,J) similar sum of molecular mass * parallel vel. component / normal vel. component
!--CSSS(4,J) similar sum of molecular mass * speed squared / normal vel. component
!--CSSS(5,J) similar sum of rotational energy / normal vel. component
!--CSSS(6,J) similar sum of rotational degrees of freedom /normal velocity component
!
!--REAC(N) the number of type N reactions
!--SREAC(N) the number of type N surface reactions
!
!--VAR(M,N) the flowfield properties in cell N
!--M=1 the x coordinate 
!--M=2 sample size
!--M=3 number density
!--M=4 density
!--M=5 u velocity component
!--M=6 v velocity component
!--M=7 w velocity component
!--M=8 translational temperature
!--M=9 rotational temperature
!--M=10 vibrational temperature
!--M=11 temperature
!--M=12 Mach number
!--M=13 molecules per cell
!--M=14 mean collision time / rate
!--M=15 mean free path
!--M=16 ratio (mean collisional separation) / (mean free path) 
!--M=17 flow speed
!--M=18 scalar pressure nkT
!--M=19 x component of translational temperature TTX
!--M=20 y component of translational temperature TTY
!--M=21 z component of translational temperature TTZ
!
!--VARSP(M,N,L) the flowfield properties for species L in cell N
!--M=0 the sample size
!--M=1 the fraction
!--M=2 the temperature component in the x direction
!--M=3 the temperature component in the y direction
!--M=4 the temperature component in the z direction
!--M=5 the translational temperature
!--M=6 the rotational temperature
!--M=7 the vibrational temperature
!--M=8 the temperature
!--M=9 the x component of the diffusion velocity
!--M=10 the y component of the diffusion velocity
!--M=11 the z component of the diffusion velocity
! 
!--VARS(N,M) surface property N on interval L of surface M
!
!--N=1 the incident sample
!--N=2 the reflected sample
!--N=3 the incident number flux
!--N=4 the reflected number flux
!--N=5 the incident pressure
!--N=6 the reflected pressure
!--N=7 the incident parallel shear tress
!--N=8 the reflected parallel shear stress
!--N=9 the incident normal-to-plane shear stress
!--N=10 the reflected normal shear stress
!--N=11 the incident translational heat flux
!--N=12 the reflected translational heat fluc
!--N=13 the incident rotational heat flux
!--N=14 the reflected rotational heat flux
!--N=15 the incident vibrational heat flux
!--N=16 the reflected vibrational heat flux
!--N=17 the incident heat flux from surface reactions
!--N=18 the reflected heat flux from surface reactions
!--N=19 slip velocity    
!--N=20 temperature slip   
!--N=21 rotational temperature slip   
!--N=22 the net pressure
!--N=23 the net parallel in-plane shear
!--N=24 the net parallel normal-to-plane shear
!--N=25 the net translational energy flux
!--N=26 the net rotational heat flux
!--N=27 the net vibrational heat flux
!--N=28 the heat flux from reactions
!--N=29 total incident heat transfer
!--N=30 total reflected heat transfer
!--N=31 net heat transfer 
!--N=32 surface temperature   --not implemented
!--N=32+K the percentage of species K
!
!--COLLS(N) the number of collisions in sampling cell N
!--WCOLLS(N) weighted number
!--CLSEP(N) the total collision partner separation distance in sampling cell N
!
!--VIBFRAC(L,K,M) the sum of species L mode K in level M
!--SUMVIB(L,K) the total sample in VIBFRAC
!
END MODULE OUTPUT
!
!***********************************************************************
!*****************************MAIN PROGRAM******************************
!***********************************************************************
!
!--Version 1.10.xx (must be set in NVER,MVER)
!
!--the version changes when there are major enhancements to the program
!--the first subscript must change if there is any alteration to the data
!--the second subscript is the release number 
!
PROGRAM DS1
!
USE MOLECS
USE GEOM
USE GAS
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: IRUN,N,M,IADAPT,IRETREM
REAL(KIND=8) :: A 
!
!--IRUN run options
!
NVER=1          !--for changes to basic architecture
MVER=10          !--must change whenever the data in DS1VD.DAT changes
!
PI=3.1415926535897932D00
DPI=6.283185307179586D00
SPI=1.772453850905516D00
BOLTZ=1.380658D-23
AVOG=6.022169D26
!
OPEN (9,FILE='DIAG.TXT',FORM='FORMATTED',STATUS='REPLACE')
WRITE (9,*,IOSTAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'Stop the DS1.EXE that is already running and try again'
  STOP
ELSE
  WRITE (9,*) 'File DIAG.TXT has been opened'
END IF
!
IRUN=0
!
DO WHILE ((IRUN < 1).OR.(IRUN > 3))
  WRITE (*,*) 'DS1 Version',NVER,'.',MVER
  IF (ISF == 0) THEN  
    WRITE (*,*) 'Enter 1 to continue the current sample or'
    WRITE (*,*) 'enter 2 to continue with a new sample, or'
    WRITE (*,*) 'enter 3 to start a new run :-'
  ELSE
    WRITE (*,*) 'Enter 1 to continue the current sample or'
    WRITE (*,*) 'enter 3 to start a new run :-'
  END IF  
!
  READ (*,*)  IRUN
END DO
IF (IRUN == 1) WRITE (9,*) 'Continuing the current run and sample'
IF (IRUN == 2) WRITE (9,*) 'Continuing the current run with a new sample'
IF (IRUN == 3) WRITE (9,*) 'Starting a new run'
!
IF (IRUN < 3) CALL READ_RESTART
!
IF ((IRUN == 2).AND.(IVB == 0)) THEN
  WRITE (*,*) 'Enter 1 to adapt the cells, or 0 to continue with current cells:-'
  READ(*,*) IADAPT
  IF (IADAPT == 1) THEN 
    WRITE (*,*) 'Adapting cells'
    CALL ADAPT_CELLS  
    CALL INDEX_MOLS
    CALL WRITE_RESTART   
  ELSE
    WRITE (*,*) 'Continuing with existing cells'
  END IF  
  IF (IREM == 2) THEN
    WRITE (*,*) 'Enter 1 to reset the removal details, or 0 to continue with current details:-'
    READ(*,*) IRETREM
    IF (IRETREM == 1) THEN
      FREM=-1.D00
      DO WHILE ((FREM < -0.0001).OR.(FREM > 2.))
        WRITE (*,*) 'Enter the fraction of entering molecules that are removed:-'
        READ (*,*) FREM
        WRITE (*,999) FREM
      END DO
      WRITE (9,999) FREM
999   FORMAT (' The ratio of removed to entering molecules is ',G15.5)
      IF (FREM > 1.D-10) THEN
      XREM=XB(1)-1.
      DO WHILE ((XREM < XB(1)-0.0001).OR.(XREM > XB(2)+0.0001))
        WRITE (*,*) 'Enter x coordinate of the upstream removal limit:-'
        READ (*,*) XREM
        WRITE (*,998) XREM,XB(2)
      END DO
      WRITE (9,998) XREM,XB(2)
998   FORMAT (' The molecules are removed from ',G15.5,' to',G15.5)
      END IF
    END IF
  END IF
!
  CALL INITIALISE_SAMPLES    
!
END IF
!
IF (IRUN == 3) THEN
!
  CALL READ_DATA 
!
  CALL SET_INITIAL_STATE 
  IF ((GASCODE == 6).OR.(GASCODE == 4).OR.(GASCODE >= 8))  CALL ENERGY(GASCODE,0,A) 
!
  CALL WRITE_RESTART
!
END IF
!
TOTCOLI=TOTCOL
TOTMOVI=TOTMOV
!
DO WHILE (FTIME < TLIM)
!
!
  FTIME=FTIME+DTM
!
  WRITE (9,*) 'TIME',FTIME,' NM',NM,' COLLS',TOTCOL
  WRITE (*,*) 'TIME',FTIME,' NM',NM,' COLLS',TOTCOL
! 
!  WRITE (*,*) 'MOVE'
  CALL MOLECULES_MOVE
!  
  IF ((ITYPE(1) == 0).OR.(ITYPE(2) == 0)) CALL MOLECULES_ENTER
! 
!  WRITE (*,*) 'INDEX'
  CALL INDEX_MOLS
!
!  WRITE (*,*) 'COLLISIONS'
  CALL COLLISIONS
!
  IF ((MMVM > 0).AND.(JCD == 1)) CALL DISSOCIATION 
!
  IF (FTIME > TSAMP) THEN 
!    WRITE (*,*) 'SAMPLE'
    IF (ISF == 0) CALL SAMPLE_FLOW
    IF ((ISF == 1).AND.(FTIME < TPOUT+(1.D00-FRACSAM)*DTOUT)) THEN
      TSAMP=TSAMP+DTSAMP
      CALL INITIALISE_SAMPLES
    END IF    
    IF ((ISF == 1).AND.(FTIME >= TPOUT+(1.D00-FRACSAM)*DTOUT)) CALL SAMPLE_FLOW
  END IF
!
  IF (FTIME > TOUT) THEN
!    WRITE (*,*) 'OUTPUT'
    CALL WRITE_RESTART
    IF ((GASCODE == 6).OR.(GASCODE == 4).OR.(GASCODE >= 8))THEN
      CALL ENERGY(GASCODE,0,A)
    END IF
    IF (JCD == 1) THEN
      WRITE (9,*) 'DISSOCIATIONS',TDISS,' RECOMBINATIONS',TRECOMB
      WRITE (*,*) 'DISSOCIATIONS',TDISS,' RECOMBINATIONS',TRECOMB
      WRITE (9,*) 'FORWARD EXCH.',TFOREX,' REVERSE EXCH.',TREVEX
      WRITE (*,*) 'FORWARD EXCH.',TFOREX,' REVERSE EXCH.',TREVEX
      DO N=1,MEX
        WRITE (*,*) 'EX,C reaction',N,' number',TNEX(N)
        WRITE (9,*) 'EX,C reaction',N,' number',TNEX(N)        
      END DO  
    ELSE
      DO M=1,MNRE
        WRITE (*,*) 'Reaction',M,' number',REAC(M)
        WRITE (9,*) 'Reaction',M,' number',REAC(M)        
      END DO 
    END IF
    CALL OUTPUT_RESULTS
    TPOUT=FTIME
  END IF  
!
END DO
!
STOP
END PROGRAM DS1
!
!***************************************************************************
!
SUBROUTINE READ_DATA
!
USE MOLECS
USE GEOM
USE GAS
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: NVERD,MVERD,N,K
!
WRITE (*,*) 'Reading the data file DS1VD.DAT'
OPEN (4,FILE='DS1VD.DAT')
OPEN (3,FILE='DS1VD.TXT')
!
WRITE (3,*) 'Data summary for program DS1'
!
READ (4,*) NVERD
WRITE (3,*) 'The n in version number n.m is',NVERD
READ (4,*) MVERD
WRITE (3,*) 'The m in version number n.m is',MVERD
!
READ (4,*) AMEG
WRITE (3,*) 'The approximate number of megabytes for the calculation is',AMEG
!
READ (4,*) IFX
IF (IFX == 0) WRITE (3,*) 'Plane flow'
IF (IFX == 1) WRITE (3,*) 'Cylindrical flow'
IF (IFX == 2) WRITE (3,*) 'Spherical flow'
JFX=IFX+1
!
READ (4,*) XB(1)
WRITE (3,*) 'The minimum x coordinate is',XB(1)
!
READ (4,*) ITYPE(1)
IF (ITYPE(1) == 0) WRITE (3,*) 'The minimum x coordinate is a stream boundary'
IF (ITYPE(1) == 1) WRITE (3,*) 'The minimum x coordinate is a plane of symmetry'
IF (ITYPE(1) == 2) WRITE (3,*) 'The minimum x coordinate is a solid surface'
IF (ITYPE(1) == 3) WRITE (3,*) 'The minimum x coordinate is a vacuum'
!
READ (4,*) XB(2)
WRITE (3,*) 'The maximum x coordinate is',XB(2)
!
READ (4,*) ITYPE(2)
IF (ITYPE(2) == 0) WRITE (3,*) 'The maximum x coordinate is a stream boundary'
IF (ITYPE(2) == 1) WRITE (3,*) 'The maximum x coordinate is a plane of symmetry'
IF (ITYPE(2) == 2) WRITE (3,*) 'The maximum x coordinate is a solid surface'
IF (ITYPE(2) == 3) WRITE (3,*) 'The maximum x coordinate is a vacuum'
!
IF (IFX > 0) THEN
  READ (4,*) IWF
  IF (IWF == 0) WRITE (3,*) 'There are no radial weighting factors'
  IF (IWF == 1) WRITE (3,*) 'There are radial weighting factors'
  IF (IWF == 1) THEN
    READ (4,*) WFM
    WRITE (3,*) 'The maximum value of the weighting factor is ',WFM+1.D00
    WFM=WFM/XB(2)
  END IF
END IF  
!
READ (4,*) GASCODE
WRITE (3,*) GASCODE 
IF (GASCODE == 1) THEN
  WRITE (3,*) 'Hard sphere gas'
  CALL HARD_SPHERE
END IF  
IF (GASCODE == 2) THEN
  WRITE (3,*) 'Argon'
  CALL ARGON
END IF  
IF (GASCODE == 3) THEN
  WRITE (3,*) 'Nitrogen'
  CALL IDEAL_NITROGEN
END IF  
IF (GASCODE == 4) THEN
  WRITE (3,*) 'Real oxygen'
  CALL REAL_OXYGEN
END IF  
IF (GASCODE == 5) THEN
  WRITE (3,*) 'Ideal air'
  CALL IDEAL_AIR
END IF  
IF (GASCODE == 6) THEN
  WRITE (3,*) 'Real air @ 7.5 km'
  JCD=-1  !--do not read reaction data until JCD IS SET
  CALL REAL_AIR
END IF 
IF (GASCODE == 7) THEN
  WRITE (3,*) 'Helium-xenon mixture with mol. wt. of argon'
  CALL HELIUM_XENON
END IF
IF (GASCODE == 8) THEN
  WRITE (3,*) 'Oxygen-hydrogen'
  JCD=-1
  CALL OXYGEN_HYDROGEN
END IF 
! 
READ (4,*) IGS
IF (IGS == 0) WRITE (3,*) 'The flowfield is initially a vacuum'
IF (IGS == 1) WRITE (3,*) 'The flowfield is initially the stream(s) or reference gas'
READ (4,*) ISECS
IF (ISECS == 0) WRITE (3,*) 'There is no secondary stream initially at x > 0'
IF (ISECS == 1) WRITE (3,*) 'There is a secondary stream applied initially at x > 0 (XB(2) must be > 0)'
IF ((ISECS == 1).AND.(IFX > 0)) THEN
  IF (IWF == 1) THEN
    WRITE (3,*) 'There cannot be a secondary stream when weighting factors are present'
    STOP
  END IF  
  READ (4,*) XS
  WRITE (3,*) 'The secondary stream boundary is at x=',XS
END IF  
! 
DO K=1,2
  IF ((K == 1).OR.(ISECS == 1)) THEN
    IF (K == 1) THEN
      WRITE (3,*) 'The stream or reference gas properties are:-'
    ELSE
      WRITE (3,*) 'The secondary stream (at x>0) properties are:-'
    END IF
    READ (4,*) FND(K)
    WRITE (3,*) '    The stream number density is',FND(K)
!
    READ (4,*) FTMP(K)
    WRITE (3,*) '    The stream temperature is',FTMP(K)
!
    IF (MMVM > 0) THEN
      READ (4,*) FVTMP(K)
      WRITE (3,*) '    The stream vibrational temperature is',FVTMP(K)
    END IF
!
    READ (4,*) VFX(K)
    WRITE (3,*) '    The stream velocity in the x direction is',VFX(K)
    IF ((NVERD > 1).OR.((NVERD == 1).AND.(MVERD > 6))) THEN
      READ (4,*) VFY(K)
      WRITE (3,*) '    The stream velocity in the y direction is',VFY(K)    
    END IF   
!
    IF (MSP > 1) THEN
      DO N=1,MSP
        READ (4,*) FSP(N,K)
        WRITE (3,*) '    The fraction of species',N,' is',FSP(N,K)
      END DO
    ELSE
      FSP(1,K)=1.
    END IF
  END IF
  IF (ITYPE(K) == 2) THEN
    IF (K == 1) WRITE (3,*) 'The minimum x boundary is a surface with the following properties'
    IF (K == 2) WRITE (3,*) 'The maximum x boundary is a surface with the following properties'
!
    READ (4,*) TSURF(K)
    WRITE (3,*) '     The temperature of the surface is',TSURF(K)
!
    READ (4,*) FSPEC(K)
    WRITE (3,*) '     The fraction of specular reflection at this surface is',FSPEC(K)
!
    READ (4,*) VSURF(K)
    WRITE (3,*) '     The velocity in the y direction of this surface is',VSURF(K)
  END IF
END DO
! 
IF (ITYPE(1) == 0) THEN
  READ (4,*) IREM
  IF (IREM == 0) THEN
    WRITE (3,*) 'There is no molecule removal'
    XREM=XB(1)-1.D00
    FREM=0.D00
  ELSE IF (IREM == 1) THEN
    READ (4,*) XREM
    WRITE (3,*) ' There is full removal of the entering (at XB(1)) molecules between',XREM,' and',XB(2)  
    FREM=1.D00
  ELSE IF (IREM == 2) THEN
    WRITE (3,*) ' Molecule removal is specified whenever the program is restarted'
    XREM=XB(1)-1.D00
    FREM=0.D00
  END IF 
ELSE
  XREM=XB(1)-1.D00
  FREM=0.D00
END IF
!
!--set the speed of the outer boundary
!
IVB=0
VELOB=0.D00
IF (ITYPE(2) == 1) THEN
  READ (4,*) IVB
  IF (IVB == 0) WRITE (3,*) ' The outer boundary is stationary'
  IF (IVB == 1) THEN
    WRITE (3,*) ' The outer boundary moves with a constant speed'
    READ (4,*) VELOB
    WRITE (3,*) ' The speed of the outer boundary is',VELOB
  END IF
END IF
!
WRITE (3,*) 'Computational Parameters'   
READ (4,*) MOLSC
WRITE (3,*) ' The target number of molecules per sampling cell is',MOLSC
READ (4,*) NCIS
WRITE (3,*) ' The number of collision cells per sampling cell is ',NCIS
READ (4,*) CPDTM
WRITE (3,*) ' Maximum collisions in a time step is',CPDTM
READ (4,*) TPDTM
WRITE (3,*) ' Maximum sampling cell transits in a time step is',TPDTM
READ (4,*) NNC
IF (NNC == 0) WRITE (3,*) ' Collision partners are selected randomly from the collision cell  '
IF (NNC == 1) WRITE (3,*) ' Nearest neighbor collisions are employed '
READ (4,*) IMTS
IF (IMTS == 0) WRITE (3,*) ' The move time step is uniform over the cells  '
IF (IMTS == 1) WRITE (3,*) ' The move time step can vary over the cells '
READ (4,*) SAMPRAT
WRITE (3,*) ' The number if time steps in a sampling interval is ',SAMPRAT
READ (4,*) OUTRAT
WRITE (3,*) ' The number of smpling intervals in an output interval is ',OUTRAT
READ (4,*) ISF
IF (ISF == 0) WRITE (3,*) ' The sampling is for an eventual steady flow '
IF (ISF == 1) WRITE (3,*) ' The sampling is for a continuing unsteady flow '
READ (4,*) FRACSAM
WRITE (3,*) ' Any unsteady sample is over the final ',FRACSAM,' of the output interval'
READ (4,*) JCD
IF (JCD == 0) WRITE (3,*) ' Any chemical reactions are based on the continuum rate equations (Real air only)'
IF (JCD == 1) WRITE (3,*) ' Any chemical reactions are based on quantum kinetic theory'
IF ((GASCODE == 4).OR.(GASCODE == 6).OR.(GASCODE == 8)) THEN
!--deallocate EARLIER ALLOCATION
DEALLOCATE (NEX,NSPEX,SPEX,ISPEX,TREACG,PSF,TREACL,TNEX,STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT DEALLOCATE INITIAL GAS VARIABLES',ERROR
END IF
!--set the reaction data
  IF (GASCODE == 4) CALL REAL_OXYGEN
  IF (GASCODE == 6) CALL REAL_AIR
  IF (GASCODE == 8) CALL OXYGEN_HYDROGEN
END IF  
READ (4,*) IRECOM
IF (IRECOM == 0) WRITE (3,*) ' Any recombination reactions are not included '
IF (IRECOM == 1) WRITE (3,*) ' Any recombination reactions are included'
!
CLOSE (3)
CLOSE (4)
!
!--set the stream at the maximum x boundary if there is no secondary stream
!
IF ((ISECS == 0).AND.(ITYPE(2) == 0)) THEN
  FND(2)=FND(1)
  FTMP(2)=FTMP(1)
  IF (MMVM > 0) FVTMP(2)=FVTMP(1)
  VFX(2)=VFX(1)
  IF (MSP > 1) THEN
    DO N=1,MSP
      FSP(N,2)=FSP(N,1)
    END DO
  ELSE
    FSP(1,2)=1.
  END IF
END IF
!
RETURN
END SUBROUTINE READ_DATA
!
!***************************************************************************
!
SUBROUTINE SET_INITIAL_STATE
!
USE MOLECS
USE GEOM
USE GAS
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: J,L,K,KK,KN,NMI,II,III,INC,NSET,NSC
INTEGER(KIND=8) :: N,M
REAL(KIND=8) :: A,B,BB,SN,XMIN,XMAX,WFMIN,DENG
REAL(KIND=8), DIMENSION(3) :: DMOM
REAL(KIND=8), DIMENSION(3,2) :: VB
REAL(KIND=8), DIMENSION(2) :: ROTE
REAL :: GAM,AA,ERF
!
!--NSET the alternative set numbers in the setting of exact initial state
!--DMOM(N) N=1,2,3 for x,y and z momentum sums of initial molecules
!--DENG the energy sum of the initial molecules
!--VB alternative sets of velocity components
!--ROTE alternative sets of rotational energy
!--NMI the initial number of molecules
!--INC counting increment
!
DMOM=0.D00
DENG=0.D00
!--set the number of molecules, divisions etc. based on stream 1
!
NMI=10000*AMEG+0.999999999D00
NDIV=NMI/MOLSC !--MOLSC molecules per division
WRITE (9,*) 'The number of divisions is',NDIV
!
MDIV=NDIV
ILEVEL=0
!
ALLOCATE (JDIV(0:ILEVEL,MDIV),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR JDIV ARRAY',ERROR
ENDIF
!
DDIV=(XB(2)-XB(1))/DFLOAT(NDIV)
NCELLS=NDIV
WRITE (9,*) 'The number of sampling cells is',NCELLS
NCCELLS=NCIS*NDIV
WRITE (9,*) 'The number of collision cells is',NCCELLS
!
IF (IFX == 0) XS=0.D00
!
IF (ISECS == 0) THEN
  IF (IFX == 0) FNUM=(XB(2)-XB(1))*FND(1)/DFLOAT(NMI)
  IF (IFX == 1) FNUM=PI*(XB(2)**2-XB(1)**2)*FND(1)/DFLOAT(NMI)
  IF (IFX == 2) FNUM=1.3333333333333333333333D00*PI*(XB(2)**3-XB(1)**3)*FND(1)/DFLOAT(NMI)
ELSE
  IF (IFX == 0) FNUM=((XS-XB(1))*FND(1)+(XB(2)-XS)*FND(2))/DFLOAT(NMI)
  IF (IFX == 1) FNUM=PI*((XS**2-XB(1)**2)*FND(1)+(XB(2)**2-XS**2)*FND(2))/DFLOAT(NMI)
  IF (IFX == 2) FNUM=1.3333333333333333333333D00*PI*((XS**3-XB(1)**3)*FND(1)-(XB(2)**3-XS**3)*FND(2))/DFLOAT(NMI)
END IF
!
FTIME=0.D00
TLIM=1.D20
!
TOTMOV=0.D00
TOTCOL=0.D00
TOTDUP=0.D00
TCOL=0.D00
TDISS=0.D00
TRECOMB=0.D00
TFOREX=0.D00
TREVEX=0.D00
TREACG=0
TREACL=0
TNEX=0.D00
!
DO N=1,NDIV
  JDIV(0,N)=-N
END DO
!
ALLOCATE (CELL(4,NCELLS),ICELL(NCELLS),CCELL(5,NCCELLS),ICCELL(3,NCCELLS),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR CELL ARRAYS',ERROR
ENDIF
!
ALLOCATE (COLLS(NCELLS),WCOLLS(NCELLS),CLSEP(NCELLS),REAC(MNRE),SREAC(MNSR),VAR(21,NCELLS),VARSP(0:11,NCELLS,MSP),    &
          VARS(0:32+MSP,2),CS(0:8+MSP,NCELLS,MSP),CSS(0:8,2,MSP,2),CSSS(6,2),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR SAMPLING ARRAYS',ERROR
ENDIF
!
IF (MMVM > 0) THEN
  ALLOCATE (VIBFRAC(MSP,MMVM,0:150),SUMVIB(MSP,MMVM),STAT=ERROR)
  IF (ERROR /= 0) THEN
    WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR RECOMBINATION ARRAYS',ERROR
  ENDIF
END IF
!
CALL INITIALISE_SAMPLES
!
!--Set the initial cells

DO N=1,NCELLS
  CELL(2,N)=XB(1)+DFLOAT(N-1)*DDIV
  CELL(3,N)=CELL(2,N)+DDIV
  CELL(1,N)=CELL(2,N)+0.5D00*DDIV
  IF (IFX == 0) CELL(4,N)=CELL(3,N)-CELL(2,N)    !--calculation assumes unit cross-section
  IF (IFX == 1) CELL(4,N)=PI*(CELL(3,N)**2-CELL(2,N)**2)  !--assumes unit length of full cylinder
  IF (IFX == 2) CELL(4,N)=1.33333333333333333333D00*PI*(CELL(3,N)**3-CELL(2,N)**3)    !--flow is in the full sphere
  ICELL(N)=NCIS*(N-1)
  DO M=1,NCIS
    L=ICELL(N)+M
    XMIN=CELL(2,N)+DFLOAT(M-1)*DDIV/DFLOAT(NCIS)
    XMAX=XMIN+DDIV/DFLOAT(NCIS)
    IF (IFX == 0) CCELL(1,L)=XMAX-XMIN
    IF (IFX == 1) CCELL(1,L)=PI*(XMAX**2-XMIN**2)  !--assumes unit length of full cylinder
    IF (IFX == 2) CCELL(1,L)=1.33333333333333333333D00*PI*(XMAX**3-XMIN**3)    !--flow is in the full sphere
    CCELL(2,L)=0.D00
    ICCELL(3,L)=N
  END DO
END DO
!
IF (IWF == 0) AWF=1.D00
IF (IWF == 1) THEN
!--FNUM must be reduced to allow for the weighting factors
  A=0.D00
  B=0.D00 
  DO N=1,NCELLS
    A=A+CELL(4,N)
    B=B+CELL(4,N)/(1.+WFM*CELL(1,N)**IFX)
  END DO
  AWF=A/B
  FNUM=FNUM*B/A     
END IF
!
WRITE (9,*) 'FNUM is',FNUM
!
!--set the information on the molecular species
!
A=0.D00
B=0.D00
DO L=1,MSP
  A=A+SP(5,L)*FSP(L,1)
  B=B+(3.+ISPR(1,L))*FSP(L,1)
  VMP(L,1)=SQRT(2.D00*BOLTZ*FTMP(1)/SP(5,L))
  IF ((ITYPE(2)== 0).OR.(ISECS == 1)) VMP(L,2)=SQRT(2.D00*BOLTZ*FTMP(2)/SP(5,L))
  VNMAX(L)=3.*VMP(L,1)
  IF (L == 1) THEN
    VMPM=VMP(L,1)
  ELSE
    IF (VMP(L,1) > VMPM) VMPM=VMP(L,1)
  END IF
END DO
WRITE (9,*) 'VMPM =',VMPM 
FDEN=A*FND(1)
FPR=FND(1)*BOLTZ*FTMP(1)
FMA=VFX(1)/DSQRT((B/(B+2.D00))*BOLTZ*FTMP(1)/A) 
DO L=1,MSP
  DO M=1,MSP
    SPM(4,L,M)=0.5D00*(SP(1,L)+SP(1,M))
    SPM(3,L,M)=0.5D00*(SP(3,L)+SP(3,M))
    SPM(5,L,M)=0.5D00*(SP(2,L)+SP(2,M))
    SPM(1,L,M)=SP(5,L)*(SP(5,M)/(SP(5,L)+SP(5,M)))
    SPM(2,L,M)=0.25D00*PI*(SP(1,L)+SP(1,M))**2
    AA=2.5D00-SPM(3,L,M)
    A=GAM(AA)
    SPM(6,L,M)=1.D00/A
	SPM(8,L,M)=0.5D00*(SP(4,L)+SP(4,M))
    IF ((ISPR(1,L) > 0).AND.(ISPR(1,M) > 0)) THEN
      SPM(7,L,M)=(SPR(1,L)+SPR(1,M))*0.5D00
    END IF
    IF ((ISPR(1,L) > 0).AND.(ISPR(1,M) == 0)) THEN
      SPM(7,L,M)=SPR(1,L)
    END IF   
    IF ((ISPR(1,M) > 0).AND.(ISPR(1,L) == 0)) THEN
      SPM(7,L,M)=SPR(1,M)
    END IF     
  END DO
END DO
IF (MNRE > 0) CALL INIT_REAC
IF (MSP == 1) THEN
  RMAS=SPM(1,1,1)
  CXSS=SPM(2,1,1)
  RGFS=SPM(6,1,1)
END IF
!
DO L=1,MSP
  CR(L)=0.
  DO M=1,MSP
    CR(L)=CR(L)+2.D00*SPI*SPM(4,L,M)**2*FND(1)*FSP(M,1)*(FTMP(1)/SPM(5,L,M))** &
        (1.-SPM(3,L,M))*DSQRT(2.*BOLTZ*SPM(5,L,M)/SPM(1,L,M))
  END DO
END DO
A=0.D00
FP=0.D00
DO L=1,MSP
  A=A+FSP(L,1)*CR(L)
  FP=FP+(2./SPI)*FSP(L,1)*VMP(L,1)/CR(L)
END DO
CTM=1.D00/A
WRITE (9,*) 'Approximate collision time in the stream is',CTM
!
WRITE (9,*) 'Approximate mean free path in the stream is',FP 
!
!--set the initial time step
DTM=CTM*CPDTM
IF (DABS(VFX(1)) > 1.D-6) THEN
  A=(0.5D00*DDIV/VFX(1))*TPDTM
ELSE
  A=0.5D00*DDIV/VMPM  
END IF
IF (IVB == 1) THEN
  B=0.25D00*DDIV/(ABS(VELOB)+VMPM)
  IF (B < A) A=B
END IF
IF (DTM > A) DTM=A  
DTSAMP=SAMPRAT*DTM
DTOUT=OUTRAT*DTSAMP
TSAMP=DTSAMP
TOUT=DTOUT
ENTMASS=0.
!
WRITE (9,*) 'The initial value of the overall time step is',DTM
!
!--initialise cell quantities associated with collisions
!
DO N=1,NCCELLS
  CCELL(3,N)=DTM/2.D00
  CCELL(4,N)=2.D00*VMPM*SPM(2,1,1)
  CALL RANDOM_NUMBER(RANF)
  CCELL(2,N)=RANF
  CCELL(5,N)=0.D00
END DO
!
!--set the entry quantities
!
DO K=1,2
  IF (ITYPE(K) == 0) THEN
    DO L=1,MSP    
      IF (K == 1) SN=VFX(1)/VMP(L,1)
      IF (K == 2) SN=-VFX(2)/VMP(L,2)
      AA=SN
      A=1.D00+ERF(AA)
      BB=DEXP(-SN**2)
      ENTR(3,L,K)=SN
      ENTR(4,L,K)=SN+SQRT(SN**2+2.D00)
      ENTR(5,L,K)=0.5D00*(1.D00+SN*(2.D00*SN-ENTR(4,L,K)))
      ENTR(6,L,K)=3.D00*VMP(L,K)
      B=BB+SPI*SN*A
      ENTR(1,L,K)=(FND(K)*FSP(L,K)*VMP(L,K))*B/(FNUM*2.D00*SPI)
      ENTR(2,L,K)=0.D00
    END DO
  END IF
END DO
!
!--Set the uniform stream
!
MNM=1.1D00*NMI
!
IF (MMVM > 0) THEN
  ALLOCATE (PX(MNM),PTIM(MNM),PROT(MNM),IPCELL(MNM),IPSP(MNM),ICREF(MNM),IPCP(MNM),PV(3,MNM),     &
       IPVIB(MMVM,MNM),STAT=ERROR)

ELSE
  IF (MMRM > 0) THEN
    ALLOCATE (PX(MNM),PTIM(MNM),PROT(MNM),IPCELL(MNM),IPSP(MNM),ICREF(MNM),IPCP(MNM),PV(3,MNM),STAT=ERROR)
  ELSE
    ALLOCATE (PX(MNM),PTIM(MNM),IPCELL(MNM),IPSP(MNM),ICREF(MNM),IPCP(MNM),PV(3,MNM),STAT=ERROR)
  END IF  
END IF
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR MOLECULE ARRAYS',ERROR
ENDIF
!
NM=0
IF (IGS == 1) THEN
  WRITE (*,*) 'Setting the initial gas'
  DO L=1,MSP
    ROTE=0.
    DO K=1,ISECS+1
      IF (ISECS == 0) THEN         !--no secondary stream
        M=(DFLOAT(NMI)*FSP(L,1)*AWF)
        XMIN=XB(1)
        XMAX=XB(2)
      ELSE 
        A=(XS**JFX-XB(1)**JFX)*FND(1)+(XB(2)**JFX-XS**JFX)*FND(2)
        IF (K == 1) THEN
          M=IDINT(DFLOAT(NMI)*((XS**JFX-XB(1)**JFX)*FND(1)/A)*FSP(L,1))
          XMIN=XB(1)
          XMAX=XS
        ELSE
          M=IDINT(DFLOAT(NMI)*((XB(2)**JFX-XS**JFX)*FND(2)/A)*FSP(L,2))
          XMIN=XS
          XMAX=XB(2)   
        END IF
      END IF 
      IF ((K == 1).OR.(ISECS == 1)) THEN
        III=0 
        WFMIN=1.D00+WFM*XB(1)**IFX
        N=1
        INC=1
        DO WHILE (N < M)
          A=(XMIN**JFX+((DFLOAT(N)-0.5D00)/DFLOAT(M))*(XMAX-XMIN)**JFX)**(1.D00/DFLOAT(JFX))
          IF (IWF == 0) THEN
            B=1.D00
          ELSE  
            B=WFMIN/(1.D00+WFM*A**IFX)
            IF ((B < 0.1D00).AND.(INC == 1)) INC=10
            IF ((B < 0.01D00).AND.(INC == 10)) INC=100
            IF ((B < 0.001D00).AND.(INC == 100)) INC=1000
            IF ((B < 0.0001D00).AND.(INC == 1000)) INC=10000
          END IF  
          CALL RANDOM_NUMBER(RANF)
          IF (B*DFLOAT(INC) > RANF) THEN
            NM=NM+1
            PX(NM)=A
            IPSP(NM)=L
            PTIM(NM)=0.
            IF (IVB == 0) CALL FIND_CELL(PX(NM),IPCELL(NM),KK)
            IF (IVB == 1) CALL FIND_CELL_MB(PX(NM),IPCELL(NM),KK,PTIM(NM))
 !           
             DO NSET=1,2
              DO KK=1,3
                CALL RVELC(A,B,VMP(L,K))
                IF (A < B) THEN
                  IF (DMOM(KK) < 0.D00) THEN
                    BB=B
                  ELSE
                    BB=A
                  END IF    
                ELSE
                  IF (DMOM(KK) < 0.D00) THEN
                    BB=A
                  ELSE
                    BB=B
                  END IF  
                END IF
                VB(KK,NSET)=BB              
              END DO
              IF (ISPR(1,L) > 0) CALL SROT(L,FTMP(K),ROTE(NSET))
            END DO
            A=(0.5D00*SP(5,L)*(VB(1,1)**2+VB(2,1)**2+VB(3,1)**2)+ROTE(1))/(0.5D00*BOLTZ*FTMP(K))-3.D00-DFLOAT(ISPR(1,L))
            B=(0.5D00*SP(5,L)*(VB(1,2)**2+VB(2,2)**2+VB(3,2)**2)+ROTE(2))/(0.5D00*BOLTZ*FTMP(K))-3.D00-DFLOAT(ISPR(1,L))
            IF (A < B) THEN
              IF (DENG < 0.D00) THEN
                KN=2
              ELSE
                KN=1
              END IF    
            ELSE
              IF (DENG < 0.D00) THEN
                KN=1
              ELSE
                KN=2
              END IF  
            END IF
            DO KK=1,3
              PV(KK,NM)=VB(KK,KN)
              DMOM(KK)=DMOM(KK)+VB(KK,KN)
            END DO
            PV(1,NM)=PV(1,NM)+VFX(K)
            PV(2,NM)=PV(2,NM)+VFY(K)
            IF (ISPR(1,L) > 0) PROT(NM)=ROTE(KN)
            IF (KN == 1) DENG=DENG+A  
            IF (KN == 2) DENG=DENG+B
            IF (MMVM > 0) THEN
              IF (ISPV(L) > 0) THEN
                DO J=1,ISPV(L)
                  CALL SVIB(L,FVTMP(K),IPVIB(J,NM),J)
                END DO
              END IF
            END IF
          END IF
          N=N+INC  
        END DO   
      END IF
    END DO
  END DO
!
  WRITE (9,*) 'DMOM',DMOM
  WRITE (9,*) 'DENG',DENG
END IF
!

!--SPECIAL Q-K CODING FOR INITIATION OF EXPLOSION IN H2-02 MIXTURE (FORCED IGNITION CASES)
!--set the vibrational levels of A% random molecules to 5    
!IF ((GASCODE == 8).AND.(JCD == 1)) THEN
!  A=2.D00
!  M=0.01D00*A*NM
!  DO N=1,M
!    CALL RANDOM_NUMBER(RANF)
!    K=INT(RANF*DFLOAT(NM))+1
!    IPVIB(1,K)=5
!  END DO
!END IF
    
!
!CALL SAMPLE_FLOW
!CALL OUTPUT_RESULTS
!
RETURN
END SUBROUTINE SET_INITIAL_STATE
!
!***************************************************************************
!******************************GAS DATABASE*********************************
!***************************************************************************
!
SUBROUTINE HARD_SPHERE
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
MSP=1
MMRM=0
MMVM=0
MNRE=0
MTBP=0
MNSR=0
MEX=0
MMEX=0
!
CALL ALLOCATE_GAS
!
SP(1,1)=4.0D-10    !reference diameter
SP(2,1)=273.       !reference temperature
SP(3,1)=0.5        !viscosity-temperature index
SP(4,1)=1.         !reciprocal of VSS scattering parameter (1 for VHS)
SP(5,1)=5.D-26     !mass
ISPR(1,1)=0        !number of rotational degrees of freedom
!
RETURN
END SUBROUTINE HARD_SPHERE
!
!***************************************************************************
!
SUBROUTINE ARGON
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
MSP=1
MMRM=0
MMVM=0
MNRE=0
MTBP=0
MNSR=0
MEX=0
MMEX=0
!
CALL ALLOCATE_GAS
SP(1,1)=4.17D-10
SP(2,1)=273.15
SP(3,1)=0.81    
SP(4,1)=1.
SP(5,1)=6.63D-26
ISPR(1,1)=0
ISPR(2,1)=0
!
RETURN
END SUBROUTINE ARGON
!
!***************************************************************************
!
SUBROUTINE IDEAL_NITROGEN
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
MSP=1
MMRM=1
MMVM=0
MNRE=0
MTBP=0
MNSR=0
MEX=0
MMEX=0
!
CALL ALLOCATE_GAS
!
SP(1,1)=4.17D-10
SP(2,1)=273.
SP(3,1)=0.74
SP(4,1)=1.
SP(5,1)=4.65D-26
ISPR(1,1)=2
ISPR(2,1)=0
SPR(1,1)=5.
!
RETURN
END SUBROUTINE IDEAL_NITROGEN
!
!***************************************************************************
!
SUBROUTINE REAL_OXYGEN
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
MSP=2
MMRM=1
MMVM=1
MNRE=0
MTBP=0
MNSR=0
MEX=0
MMEX=0
!
CALL ALLOCATE_GAS
!
SP(1,1)=4.07D-10
SP(2,1)=273.D00
SP(3,1)=0.77D00
SP(4,1)=1.D00
SP(5,1)=5.312D-26
ISPR(1,1)=2
ISPR(2,1)=0             ! 0,1 for constant,polynomial rotational relaxation collision number 
SPR(1,1)=5.             ! the collision number or the coefficient of temperature in the polynomial (if a polynomial, the coeff. of T^2 is in spr_db(3  )
ISPV(1)=1               ! the number of vibrational modes
SPVM(1,1,1)=2256.D00          ! the characteristic vibrational temperature
SPVM(2,1,1)=90000.D00        ! a constant Zv, or the reference Zv
SPVM(3,1,1)=2256.D00        ! -1 for a constant Zv, or the reference temperature
SPVM(4,1,1)=59500.D00
ISPVM(1,1,1)=2
ISPVM(2,1,1)=2
!
!--species 2 is atomic oxygen
SP(1,2)=3.D-10
SP(2,2)=273.D00
SP(3,2)=0.8D00
SP(4,2)=1.D00
SP(5,2)=2.656D-26
ISPR(1,2)=0
ISPV(2)=0     !--must be set!
!--set data needed for recombination
!
ISPRC=0
ISPRC(2,2)=1    !--O+O -> O2  recombined species code for an O+O recombination
!
NSPEX=0
SPEX=0.D00
ISPEX=0
!
RETURN
END SUBROUTINE REAL_OXYGEN
!
!***************************************************************************
!
SUBROUTINE IDEAL_AIR
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
MSP=2
MMRM=1
MMVM=0
MNRE=0
MTBP=0
MNSR=0
MEX=0
MMEX=0
!
CALL ALLOCATE_GAS
!
SP(1,1)=4.07D-10
SP(2,1)=273.
SP(3,1)=0.77
SP(4,1)=1.
SP(5,1)=5.312D-26
ISPR(1,1)=2
ISPR(2,1)=0
SPR(1,1)=5.
SP(1,2)=4.17D-10
SP(2,2)=273.
SP(3,2)=0.74
SP(4,2)=1.
SP(5,2)=4.65D-26
ISPR(1,2)=2
ISPR(2,2)=0
SPR(1,2)=5.
RETURN
END SUBROUTINE IDEAL_AIR
!
!***************************************************************************
!
SUBROUTINE REAL_AIR
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
MSP=5
MMRM=1
MMVM=1

!
IF (JCD == 0) THEN
  MNRE=23 
  MTBP=4
  MEX=0
  MMEX=0
END IF
IF (JCD == 1) THEN
  MNRE=0       
  MTBP=0
  MEX=4
  MMEX=1 
END IF  
! 
MNSR=0
CALL ALLOCATE_GAS
!--species 1 is oxygen
SP(1,1)=4.07D-10
SP(2,1)=273.D00
SP(3,1)=0.77D00
SP(4,1)=1.d00
SP(5,1)=5.312D-26
ISPR(1,1)=2
ISPR(2,1)=0
SPR(1,1)=5.D00
ISPV(1)=1               ! the number of vibrational modes
SPVM(1,1,1)=2256.D00          ! the characteristic vibrational temperature
SPVM(2,1,1)=18000.D00  !90000.D00        ! a constant Zv, or the reference Zv
SPVM(3,1,1)=2256.D00        ! -1 for a constant Zv, or the reference temperature
SPVM(4,1,1)=59500.D00
ISPVM(1,1,1)=3
ISPVM(2,1,1)=3
!--species 2 is nitrogen
SP(1,2)=4.17D-10
SP(2,2)=273.D00
SP(3,2)=0.74D00
SP(4,2)=1.D00
SP(5,2)=4.65D-26
ISPR(1,2)=2
ISPR(2,2)=0
SPR(1,2)=5.D00
ISPV(2)=1
SPVM(1,1,2)=3371.D00
SPVM(2,1,2)=52000.D00     !260000.D00    
SPVM(3,1,2)=3371.D00
SPVM(4,1,2)=113500.D00  
ISPVM(1,1,2)=4
ISPVM(2,1,2)=4
!--species 3 is atomic oxygen
SP(1,3)=3.D-10
SP(2,3)=273.D00
SP(3,3)=0.8D00
SP(4,3)=1.D00
SP(5,3)=2.656D-26
ISPR(1,3)=0
ISPV(3)=0
!--species 4 is atomic nitrogen
SP(1,4)=3.D-10
SP(2,4)=273.D00
SP(3,4)=0.8D00
SP(4,4)=1.0D00
SP(5,4)=2.325D-26
ISPR(1,4)=0
ISPV(4)=0
!--species 5 is NO
SP(1,5)=4.2D-10
SP(2,5)=273.D00
SP(3,5)=0.79D00
SP(4,5)=1.0D00
SP(5,5)=4.98D-26
ISPR(1,5)=2
ISPR(2,5)=0
SPR(1,5)=5.D00
ISPV(5)=1
SPVM(1,1,5)=2719.D00
SPVM(2,1,5)=14000.D00   !70000.D00
SPVM(3,1,5)=2719.D00
SPVM(4,1,5)=75500.D00
ISPVM(1,1,5)=3
ISPVM(2,1,5)=4
!--following data is required if JCD=1 (new reaction model)
IF (JCD == 1) THEN
!--set the recombination data for the molecule pairs
  ISPRC=0    !--data os zero unless explicitly set5
  ISPRC(3,3)=1    !--O+O -> O2  recombined species code for an O+O recombination
  ISPRC(4,4)=2
  ISPRC(3,4)=5
  ISPRC(4,3)=5
!--set the exchange reaction data
  SPEX=0.D00
  ISPEX=0
  NSPEX=0
  NSPEX(2,3)=1
  NSPEX(3,2)=1
  NSPEX(5,4)=1
  NSPEX(4,5)=1
  NSPEX(5,3)=1
  NSPEX(3,5)=1
  NSPEX(1,4)=1
  NSPEX(4,1)=1
!--N2+O->NO+N
  ISPEX(1,0,2,3)=2
  ISPEX(1,1,2,3)=5
  ISPEX(1,2,2,3)=4
  ISPEX(1,3,2,3)=0
  ISPEX(1,4,2,3)=1
  SPEX(1,1,2,3)=5.175D-19
  SPEX(2,1,2,3)=-5.175D-19
  NEX(1,2,3)=1
  ISPEX(1,0,3,2)=2
  ISPEX(1,1,3,2)=5
  ISPEX(1,2,3,2)=4
  ISPEX(1,3,3,2)=1
  SPEX(1,1,3,2)=5.175D-19
  SPEX(2,1,3,2)=-5.175D-19
  NEX(1,3,2)=1
!--NO+N->N2+0
  ISPEX(1,0,5,4)=5
  ISPEX(1,1,5,4)=2
  ISPEX(1,2,5,4)=3
  ISPEX(1,3,5,4)=1
  SPEX(1,1,5,4)=0.D00
  SPEX(2,1,5,4)=5.175D-19
  NEX(1,5,4)=2
  ISPEX(1,0,4,5)=5
  ISPEX(1,1,4,5)=2
  ISPEX(1,2,4,5)=3
  ISPEX(1,3,4,5)=0
  ISPEX(1,4,4,5)=1
  SPEX(1,1,4,5)=0.
  SPEX(2,1,4,5)=5.175D-19
  NEX(1,4,5)=2
!--NO+0->O2+N
  ISPEX(1,0,5,3)=5
  ISPEX(1,1,5,3)=1
  ISPEX(1,2,5,3)=4
  ISPEX(1,3,5,3)=1
  SPEX(1,1,5,3)=2.719D-19
  SPEX(2,1,5,3)=-2.719D-19
  NEX(1,5,3)=3
  ISPEX(1,0,3,5)=5
  ISPEX(1,1,3,5)=1
  ISPEX(1,2,3,5)=4
  ISPEX(1,3,3,5)=1
  SPEX(1,1,3,5)=2.719D-19
  SPEX(2,1,3,5)=-2.719D-19
  NEX(1,3,5)=3
!--O2+N->NO+O
  ISPEX(1,0,1,4)=1
  ISPEX(1,1,1,4)=5
  ISPEX(1,2,1,4)=3
  ISPEX(1,3,1,4)=1
  SPEX(1,1,1,4)=0.D00
  SPEX(2,1,1,4)=2.719D-19
  NEX(1,1,4)=4
  ISPEX(1,0,4,1)=1
  ISPEX(1,1,4,1)=5
  ISPEX(1,2,4,1)=3
  ISPEX(1,3,4,1)=1
  SPEX(1,1,4,1)=0.D00
  SPEX(2,1,4,1)=2.719D-19
  NEX(1,4,1)=4
!
END IF
IF (JCD == 0) THEN
!--the following data is required if JCD=0 (old reaction model)
! REACTION 1 IS O2+N->2O+N
  LE(1)=1 
  ME(1)=4 
  KP(1)=3 
  LP(1)=3 
  MP(1)=4 
  CI(1)=1.
  AE(1)=8.197D-19
  AC(1)=5.993D-12
  BC(1)=-1.
  ER(1)=-8.197D-19 
!--REACTION 2 IS O2+NO>2O+NO
  LE(2)=1 
  ME(2)=5 
  KP(2)=3 
  LP(2)=3
  MP(2)=5 
  CI(2)=1. 
  AE(2)=8.197D-19
  AC(2)=5.993D-12
  BC(2)=-1.
  ER(2)=-8.197D-19 
!--REACTION 3 IS O2+N2>2O+N2
LE(3)=1 
ME(3)=2 
KP(3)=3 
LP(3)=3 
MP(3)=2 
CI(3)=1.5
AE(3)=8.197D-19
AC(3)=1.198D-11
BC(3)=-1.
ER(3)=-8.197D-19 
!--REACTION 4 IS 2O2>2O+O2
LE(4)=1 
ME(4)=1 
KP(4)=3
LP(4)=3 
MP(4)=1 
CI(4)=1.5
AE(4)=8.197D-19
AC(4)=5.393D-11
BC(4)=-1.
ER(4)=-8.197D-19 
!--REACTION 5 IS O2+O>3O
LE(5)=1 
ME(5)=3 
KP(5)=3 
LP(5)=3 
MP(5)=3 
CI(5)=1. 
AE(5)=8.197D-19
AC(5)=1.498D-10
BC(5)=-1.
ER(5)=-8.197D-19 
!--REACTION 6 IS N2+O>2N+O
LE(6)=2
ME(6)=3 
KP(6)=4 
LP(6)=4 
MP(6)=3 
CI(6)=0.5
AE(6)=1.561D-18
AC(6)=3.187D-13
BC(6)=-0.5 
ER(6)=-1.561D-18 
!--REACTION 7 IS N2+O2>2N+O2
LE(7)=2 
ME(7)=1 
KP(7)=4 
LP(7)=4 
MP(7)=1 
CI(7)=0.5
AE(7)=1.561D-18
AC(7)=3.187D-13
BC(7)=-0.5  
ER(7)=-1.561D-18 
!--REACTION 8 IS N2+NO>2N+NO
LE(8)=2
ME(8)=5 
KP(8)=4 
LP(8)=4 
MP(8)=5 
CI(8)=0.5
AE(8)=1.561D-18                                                                                                                                                                            
AC(8)=3.187D-13
BC(8)=-0.5 
ER(8)=-1.561D-18 
!--REACTION 9 IS 2N2>2N+N2
LE(9)=2 
ME(9)=2 
KP(9)=4 
LP(9)=4 
MP(9)=2 
CI(9)=1. 
AE(9)=1.561D-18
AC(9)=7.968D-13
BC(9)=-0.5 
ER(9)=-1.561D-18 
!--REACTION 10 IS N2+N>3N 
LE(10)=2
ME(10)=4 
KP(10)=4 
LP(10)=4 
MP(10)=4 
CI(10)=1. 
AE(10)=1.561D-18
AC(10)=6.9E12
BC(10)=-1.5 
ER(10)=-1.561D-18 
!--REACTION 11 IS NO+N2>N+O+N2
LE(11)=5 
ME(11)=2 
KP(11)=4 
LP(11)=3 
MP(11)=2 
CI(11)=1. 
AE(11)=1.043D-18
AC(11)=6.59D-10 
BC(11)=-1.5 
ER(11)=-1.043D-18 
!--REACTION 12 IS NO+O2>N+O+O2
LE(12)=5 
ME(12)=1 
KP(12)=4 
LP(12)=3 
MP(12)=1 
CI(12)=1. 
AE(12)=1.043D-18
AC(12)=6.59D-10 
BC(12)=-1.5 
ER(12)=-1.043D-18 
!--REACTION 13 IS NO+NO>N+O+NO
LE(13)=5 
ME(13)=5 
KP(13)=4
LP(13)=3 
MP(13)=5 
CI(13)=1. 
AE(13)=1.043D-18
AC(13)=1.318D-8 
BC(13)=-1.5 
ER(13)=-1.043D-18 
!--REACTION 14 IS NO+O>N+O+O
LE(14)=5 
ME(14)=3 
KP(14)=4 
LP(14)=3 
MP(14)=3 
CI(14)=1. 
AE(14)=1.043D-18
AC(14)=1.318D-8 
BC(14)=-1.5 
ER(14)=-1.043D-18 
!--REACTION 15 IS NO+N>2N+O 
LE(15)=5 
ME(15)=4 
KP(15)=4 
LP(15)=3 
MP(15)=4 
CI(15)=1. 
AE(15)=1.043D-18
AC(15)=1.318D-8 
BC(15)=-1.5 
ER(15)=-1.043D-18 
!--REACTION 16 IS NO+O>O2+N 
LE(16)=5 
ME(16)=3 
KP(16)=0 
LP(16)=1
MP(16)=4 
CI(16)=0. 
AE(16)=2.719D-19
AC(16)=5.279D-21
BC(16)=1. 
ER(16)=-2.719D-19 
!--REACTION 17 IS N2+O>NO+N 
LE(17)=2 
ME(17)=3 
KP(17)=0 
LP(17)=5 
MP(17)=4 
CI(17)=0. 
AE(17)=5.175D-19
AC(17)=1.120D-16
BC(17)=0. 
ER(17)=-5.175D-19 
!--REACTION 18 IS O2+N>NO+O 
LE(18)=1 
ME(18)=4 
KP(18)=0 
LP(18)=5 
MP(18)=3 
CI(18)=0. 
AE(18)=4.968D-20
AC(18)=1.598D-18
BC(18)=0.5
ER(18)=2.719D-19
!--REACTION 19 IS NO+N>N2+O 
LE(19)=5 
ME(19)=4 
KP(19)=0
LP(19)=2 
MP(19)=3 
CI(19)=0.
AE(19)=0. 
AC(19)=2.49D-17 
BC(19)=0. 
ER(19)=5.175D-19
!--REACTION 20 IS O+O+M1>O2+M1
LE(20)=3 
ME(20)=3 
KP(20)=-1 
LP(20)=1 
MP(20)=-1
CI(20)=0. 
AE(20)=0. 
AC(20)=8.297D-45
BC(20)=-0.5 
ER(20)=8.197D-19
!--REACTION 21 IS N+N+M2>N2+M2
LE(21)=4 
ME(21)=4 
KP(21)=-1
LP(21)=2 
MP(21)=-2
CI(21)=0. 
AE(21)=0. 
AC(21)=3.0051D-44 
BC(21)=-0.5 
ER(21)=1.561D-18
!--REACTION 22 IS N+N+N>N2+N
LE(22)=4
ME(22)=4 
KP(22)=-1 
LP(22)=2 
MP(22)=-3
CI(22)=0. 
AE(22)=0. 
AC(22)=6.3962D-40 
BC(22)=-1.5 
ER(22)=1.5637D-18 
!--REACTION 23 IS N+O+M3>NO+M3
LE(23)=4 
ME(23)=3  
KP(23)=-1 
LP(23)=5 
MP(23)=-4
CI(23)=0. 
AE(23)=0. 
AC(23)=2.7846D-40 
BC(23)=-1.5 
ER(23)=1.043D-18
! 
  THBP(1,1)=9.
  THBP(1,2)=2.
  THBP(1,3)=25. 
  THBP(1,4)=1. 
  THBP(1,5)=1.
  THBP(2,1)=1.
  THBP(2,2)=2.5 
  THBP(2,3)=1.
  THBP(2,4)=0.
  THBP(2,5)=1.
  THBP(3,1)=0.
  THBP(3,2)=0.
  THBP(3,3)=0.
  THBP(3,4)=1.
  THBP(3,5)=0.
  THBP(4,1)=1.
  THBP(4,2)=1.
  THBP(4,3)=20. 
  THBP(4,4)=20. 
  THBP(4,5)=20.
END IF  
RETURN
END SUBROUTINE REAL_AIR
!
!******************************************************************************************************************************************
!
SUBROUTINE HELIUM_XENON
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
MSP=2
MMRM=0
MMVM=0
MNRE=0
MTBP=0
MNSR=0
MEX=0
MMEX=0
!
CALL ALLOCATE_GAS
!
SP(1,1)=2.33D-10
SP(2,1)=273.
SP(3,1)=0.66
SP(4,1)=1.
SP(5,1)=6.65D-27
ISPR(1,1)=0
ISPR(2,1)=0
SP(1,2)=5.74D-10
SP(2,2)=273.
SP(3,2)=0.85
SP(4,2)=1.
SP(5,2)=21.8D-26
ISPR(1,2)=0
ISPR(2,2)=0
RETURN
END SUBROUTINE HELIUM_XENON
!
!***************************************************************************
!
SUBROUTINE OXYGEN_HYDROGEN
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
MSP=8
MMRM=3
MMVM=3
!
IF (JCD == 1) THEN 
  MNRE=0       
  MTBP=0
  MEX=16
  MMEX=3
END IF
IF (JCD == 0) THEN
  MNRE=37 
  MTBP=0
  MEX=0
  MMEX=0
END IF
! 
MNSR=0
!
CALL ALLOCATE_GAS
!
!--species 1 is hydrogen H2
SP(1,1)=2.92D-10
SP(2,1)=273.D00
SP(3,1)=0.67D00
SP(4,1)=1.d00
SP(5,1)=3.34D-27
ISPR(1,1)=2
ISPR(2,1)=0
SPR(1,1)=5.D00
ISPV(1)=1               ! the number of vibrational modes
SPVM(1,1,1)=6159.D00          ! the characteristic vibrational temperature
SPVM(2,1,1)=20000.D00  !--estimate
SPVM(3,1,1)=2000.D00   !--estimate
SPVM(4,1,1)=52438.76D00
ISPVM(1,1,1)=2
ISPVM(2,1,1)=2
!--species 2 is atomic hydrogen H
SP(1,2)=2.5D-10      !--estimate
SP(2,2)=273.D00
SP(3,2)=0.8D00
SP(4,2)=1.D00
SP(5,2)=1.67D-27
ISPR(1,2)=0
ISPV(2)=0
!--species 3 is oxygen O2
SP(1,3)=4.07D-10
SP(2,3)=273.D00
SP(3,3)=0.77D00
SP(4,3)=1.d00
SP(5,3)=5.312D-26
ISPR(1,3)=2
ISPR(2,3)=0
SPR(1,3)=5.d00 
ISPV(3)=1               ! the number of vibrational modes
SPVM(1,1,3)=2256.D00          ! the characteristic vibrational temperature
SPVM(2,1,3)=18000.D00  !90000.D00        ! a constant Zv, or the reference Zv
SPVM(3,1,3)=2256.D00        ! -1 for a constant Zv, or the reference temperature
SPVM(4,1,3)=59971.4D00
ISPVM(1,1,3)=4
ISPVM(2,1,3)=4
!--species 4 is atomic oxygen O
SP(1,4)=3.D-10     !--estimate
SP(2,4)=273.D00
SP(3,4)=0.8D00
SP(4,4)=1.D00
SP(5,4)=2.656D-26
ISPR(1,4)=0
ISPV(4)=0
!--species 5 is hydroxy OH
SP(1,5)=4.D-10       !--estimate
SP(2,5)=273.D00
SP(3,5)=0.75D00      !-estimate
SP(4,5)=1.0D00
SP(5,5)=2.823D-26
ISPR(1,5)=2
ISPR(2,5)=0
SPR(1,5)=5.D00
ISPV(5)=1
SPVM(1,1,5)=5360.D00
SPVM(2,1,5)=20000.D00   !--estimate
SPVM(3,1,5)=2500.D00    !--estimate
SPVM(4,1,5)=51497.18D00
ISPVM(1,1,5)=2
ISPVM(2,1,5)=4
!--species 6 is water vapor H2O
SP(1,6)=4.5D-10       !--estimate
SP(2,6)=273.D00
SP(3,6)=0.75D00      !-estimate
SP(4,6)=1.0D00
SP(5,6)=2.99D-26
ISPR(1,6)=3
ISPR(2,6)=0
SPR(1,6)=5.D00
ISPV(6)=3
SPVM(1,1,6)=5261.D00  !--symmetric stretch mode  
SPVM(2,1,6)=20000.D00   !--estimate
SPVM(3,1,6)=2500.D00    !--estimate
SPVM(4,1,6)=60043.83D00
SPVM(1,2,6)=2294.D00  !--bend mode  
SPVM(2,2,6)=20000.D00   !--estimate
SPVM(3,2,6)=2500.D00    !--estimate
SPVM(4,2,6)=60043.83D00
SPVM(1,3,6)=5432.D00  !--asymmetric stretch mode  
SPVM(2,3,6)=20000.D00   !--estimate
SPVM(3,3,6)=2500.D00    !--estimate
SPVM(4,3,6)=60043.83D00
ISPVM(1,1,6)=2
ISPVM(2,1,6)=5
ISPVM(1,2,6)=2
ISPVM(2,2,6)=5
ISPVM(1,3,6)=2
ISPVM(2,3,6)=5
!--species 7 is hydroperoxy HO2
SP(1,7)=5.5D-10       !--estimate
SP(2,7)=273.D00
SP(3,7)=0.75D00      !-estimate
SP(4,7)=1.0D00
SP(5,7)=5.479D-26
ISPR(1,7)=2    !--assumes that HO2 is linear
ISPR(2,7)=0
SPR(1,7)=5.D00
ISPV(7)=3
SPVM(1,1,7)=4950.D00    
SPVM(2,1,7)=20000.D00   !--estimate
SPVM(3,1,7)=2500.D00    !--estimate
SPVM(4,1,7)=24988.08D00
SPVM(1,2,7)=2000.D00 
SPVM(2,2,7)=20000.D00   !--estimate
SPVM(3,2,7)=2500.D00    !--estimate
SPVM(4,2,7)=24988.08D00
SPVM(1,3,7)=1580.D00 
SPVM(2,3,7)=20000.D00   !--estimate
SPVM(3,3,7)=2500.D00    !--estimate
SPVM(4,3,7)=24988.08D00
ISPVM(1,1,7)=2
ISPVM(2,1,7)=3
ISPVM(1,2,7)=2
ISPVM(2,2,7)=3
ISPVM(1,3,7)=2
ISPVM(2,3,7)=3
!--Species 8 is argon
SP(1,8)=4.17D-10
SP(2,8)=273.15
SP(3,8)=0.81    
SP(4,8)=1.
SP(5,8)=6.63D-26
ISPR(1,8)=0
ISPV(8)=0
!
IF (JCD ==1) THEN    !--Q-K model
ISPRC=0    !--data is zero unless explicitly set
ISPRC(4,4)=3    !--O+O+M -> O2+M  recombined species code for an O+O recombination
ISPRC(2,2)=1    !--H+H+M -> H2+M
ISPRC(2,4)=5    !--H+0+M -> OH+M
ISPRC(4,2)=5    !--0+H+M -> OH+M
ISPRC(2,5)=6    !--H+OH+M -> H2O+M
ISPRC(5,2)=6    !--OH+H+M -> H2O+M
ISPRC(2,3)=7    !--H+O2+M -> H02+M
ISPRC(3,2)=7    !--O2+H+M -> HO2+M
!--set the exchange reaction data
SPEX=0.D00    !--all activation energies and heats of reaction are zero unless set otherwise
ISPEX=0        !-- ISPEX is also zero unless set otherwise
NSPEX=0
!--set the number of exchange reactions for each species pair
NSPEX(1,3)=1
NSPEX(3,1)=1
NSPEX(2,7)=3
NSPEX(7,2)=3
NSPEX(3,2)=1
NSPEX(2,3)=1
NSPEX(5,4)=1
NSPEX(4,5)=1
NSPEX(1,4)=1
NSPEX(4,1)=1
NSPEX(5,2)=1
NSPEX(2,5)=1
NSPEX(5,1)=1
NSPEX(1,5)=1
NSPEX(6,2)=1
NSPEX(2,6)=1
NSPEX(6,4)=2
NSPEX(4,6)=2
NSPEX(5,5)=2
NSPEX(7,4)=1
NSPEX(4,7)=1
NSPEX(5,3)=1
NSPEX(3,5)=1
!--set the information on the chain reactions
!--H2+O2 -> HO2+H
ISPEX(1,0,1,3)=1
ISPEX(1,1,1,3)=7
ISPEX(1,2,1,3)=2
ISPEX(1,3,1,3)=1
SPEX(1,1,1,3)=3.79D-19
SPEX(2,1,1,3)=-3.79D-19
NEX(1,1,3)=1
ISPEX(1,0,3,1)=1
ISPEX(1,1,3,1)=7
ISPEX(1,2,3,1)=2
ISPEX(1,3,3,1)=1
SPEX(1,1,3,1)=3.79D-19
SPEX(2,1,3,1)=-3.79D-19
NEX(1,3,1)=1
!--HO2+H -> H2+02
ISPEX(1,0,2,7)=7
ISPEX(1,1,2,7)=1
ISPEX(1,2,2,7)=3
ISPEX(1,3,2,7)=1
ISPEX(1,4,2,7)=0
ISPEX(1,5,2,7)=0
!--H02 is H-O-O so that not all vibrational modes contribute to this reaction, but the numbers here are guesses!
SPEX(1,1,2,7)=0.D00
SPEX(2,1,2,7)=3.79D-19
NEX(1,2,7)=2
ISPEX(1,0,7,2)=7
ISPEX(1,1,7,2)=1
ISPEX(1,2,7,2)=3
ISPEX(1,3,7,2)=0
ISPEX(1,4,7,2)=0
ISPEX(1,5,7,2)=1
ISPEX(1,6,7,2)=1
SPEX(1,1,7,2)=0.D00
SPEX(2,1,7,2)=3.79D-19
NEX(1,7,2)=2
!
!--O2+H -> OH+O
ISPEX(1,0,3,2)=3
ISPEX(1,1,3,2)=5
ISPEX(1,2,3,2)=4
ISPEX(1,3,3,2)=1
SPEX(1,1,3,2)=1.17D-19
SPEX(2,1,3,2)=-1.17D-19
NEX(1,3,2)=3
ISPEX(1,0,2,3)=3
ISPEX(1,1,2,3)=5
ISPEX(1,2,2,3)=4
ISPEX(1,3,2,3)=1
SPEX(1,1,2,3)=1.17D-19
SPEX(2,1,2,3)=-1.17D-19
NEX(1,2,3)=3
!--OH+O -> O2+H
ISPEX(1,0,5,4)=5
ISPEX(1,1,5,4)=3
ISPEX(1,2,5,4)=2
ISPEX(1,3,5,4)=1
SPEX(1,1,5,4)=0.D00
SPEX(2,1,5,4)=1.17D-19
NEX(1,5,4)=4
ISPEX(1,0,4,5)=5
ISPEX(1,1,4,5)=3
ISPEX(1,2,4,5)=2
ISPEX(1,3,4,5)=1
SPEX(1,1,4,5)=0.D00
SPEX(2,1,4,5)=1.17D-19
NEX(1,4,5)=4
!
!--H2+O -> OH+H
ISPEX(1,0,1,4)=1
ISPEX(1,1,1,4)=5
ISPEX(1,2,1,4)=2
ISPEX(1,3,1,4)=1
SPEX(1,1,1,4)=0.13D-19
SPEX(2,1,1,4)=-0.13D-19
NEX(1,1,4)=5
ISPEX(1,0,4,1)=1
ISPEX(1,1,4,1)=5
ISPEX(1,2,4,1)=2
ISPEX(1,3,4,1)=1
SPEX(1,1,4,1)=0.13D-19
SPEX(2,1,4,1)=-0.13D-19
NEX(1,4,1)=5
!--OH+H -> H2+O
ISPEX(1,0,5,2)=5
ISPEX(1,1,5,2)=1
ISPEX(1,2,5,2)=4
ISPEX(1,3,5,2)=1
SPEX(1,1,5,2)=0.D00
SPEX(2,1,5,2)=0.13D-19
NEX(1,5,2)=6
ISPEX(1,0,2,5)=5
ISPEX(1,1,2,5)=1
ISPEX(1,2,2,5)=4
ISPEX(1,3,2,5)=1
SPEX(1,1,2,5)=0.D00
SPEX(2,1,2,5)=0.13D-19
NEX(1,2,5)=6
!
!--OH+H2 -> H2O+H
ISPEX(1,0,5,1)=5
ISPEX(1,1,5,1)=6
ISPEX(1,2,5,1)=2
ISPEX(1,3,5,1)=1
SPEX(1,1,5,1)=-1.D-19  !--a negative activation energy enables reactions from the ground state      
SPEX(2,1,5,1)=1.05D-19
NEX(1,5,1)=7
ISPEX(1,0,1,5)=5
ISPEX(1,1,1,5)=6
ISPEX(1,2,1,5)=2
ISPEX(1,3,1,5)=1
SPEX(1,1,1,5)=-1.D-19
SPEX(2,1,1,5)=1.05D-19
NEX(1,1,5)=7
!
!--H20+H -> OH+H2
ISPEX(1,0,6,2)=6
ISPEX(1,1,6,2)=5
ISPEX(1,2,6,2)=1
ISPEX(1,3,6,2)=1
ISPEX(1,4,6,2)=1
ISPEX(1,5,6,2)=1
SPEX(1,1,6,2)=1.05D-19*2.D00    !--activation energy set above heat of reaction
SPEX(2,1,6,2)=-1.05D-19   !--heat of reaction
NEX(1,6,2)=8
ISPEX(1,0,2,6)=6
ISPEX(1,1,2,6)=5
ISPEX(1,2,2,6)=1
ISPEX(1,3,2,6)=1
ISPEX(1,4,2,6)=1
ISPEX(1,5,2,6)=1
SPEX(1,1,2,6)=1.05D-19*2.D00
SPEX(2,1,2,6)=-1.05D-19
NEX(1,2,6)=8
!
!--H2O+O -> OH+OH
ISPEX(1,0,6,4)=6
ISPEX(1,1,6,4)=5
ISPEX(1,2,6,4)=5
ISPEX(1,3,6,4)=1
ISPEX(1,4,6,4)=1
ISPEX(1,5,6,4)=1
SPEX(1,1,6,4)=1.18D-19     
SPEX(2,1,6,4)=-1.18D-19
NEX(1,6,4)=9
ISPEX(1,0,4,6)=6
ISPEX(1,1,4,6)=5
ISPEX(1,2,4,6)=5
ISPEX(1,3,4,6)=1
ISPEX(1,4,4,6)=1
ISPEX(1,5,4,6)=1
SPEX(1,1,4,6)=1.18D-19    
SPEX(2,1,4,6)=-1.18D-19
NEX(1,4,6)=9
!--0H+OH -> H2O+O
ISPEX(1,0,5,5)=5
ISPEX(1,1,5,5)=6
ISPEX(1,2,5,5)=4
ISPEX(1,3,5,5)=1
SPEX(1,1,5,5)=-1.D-19    
SPEX(2,1,5,5)=1.18D-19
NEX(1,5,5)=10
!
!--H02+H -> 0H+OH
ISPEX(2,0,7,2)=7
ISPEX(2,1,7,2)=5
ISPEX(2,2,7,2)=5
ISPEX(2,3,7,2)=1
ISPEX(2,4,7,2)=1
ISPEX(2,5,7,2)=0
SPEX(1,2,7,2)=0.D00
SPEX(2,2,7,2)=2.49D-19
NEX(2,7,2)=11
ISPEX(2,0,2,7)=7
ISPEX(2,1,2,7)=5
ISPEX(2,2,2,7)=5
ISPEX(2,3,2,7)=1
ISPEX(2,4,2,7)=0
ISPEX(2,5,2,7)=1
SPEX(1,2,2,7)=0.D00
SPEX(2,2,2,7)=2.49D-19
NEX(2,2,7)=11
!--OH+OH  -> HO2+H
ISPEX(2,0,5,5)=5
ISPEX(2,1,5,5)=7
ISPEX(2,2,5,5)=2
ISPEX(2,3,5,5)=1
SPEX(1,2,5,5)=2.49D-19
SPEX(2,2,5,5)=-2.49D-19
NEX(2,5,5)=12
!
!--H02+H -> H2O+O
ISPEX(3,0,7,2)=7
ISPEX(3,1,7,2)=6
ISPEX(3,2,7,2)=4
ISPEX(3,3,7,2)=1
ISPEX(3,4,7,2)=1
ISPEX(3,5,7,2)=0
SPEX(1,3,7,2)=0.D00    
SPEX(2,3,7,2)=3.67D-19
NEX(3,7,2)=13
ISPEX(3,0,2,7)=7
ISPEX(3,1,2,7)=6
ISPEX(3,2,2,7)=4
ISPEX(3,3,2,7)=1
ISPEX(3,4,2,7)=0
ISPEX(3,5,2,7)=1
SPEX(1,3,2,7)=0.D00     
SPEX(2,3,2,7)=3.67D-19
NEX(3,2,7)=13
!--H2O+O -> HO2+H
ISPEX(2,0,6,4)=6
ISPEX(2,1,6,4)=7
ISPEX(2,2,6,4)=2
ISPEX(2,3,6,4)=1
ISPEX(2,4,6,4)=1
ISPEX(2,5,6,4)=1
SPEX(1,2,6,4)=3.67D-19   
SPEX(2,2,6,4)=-3.67D-19
NEX(2,6,4)=14
ISPEX(2,0,4,6)=6
ISPEX(2,1,4,6)=7
ISPEX(2,2,4,6)=2
ISPEX(2,3,4,6)=1
ISPEX(2,4,4,6)=1
ISPEX(2,5,4,6)=1
SPEX(1,2,4,6)=3.67D-19      
SPEX(2,2,4,6)=-3.67D-19
NEX(2,4,6)=14
!
!--H02+0 -> OH+O2
ISPEX(1,0,7,4)=7
ISPEX(1,1,7,4)=5
ISPEX(1,2,7,4)=3
ISPEX(1,3,7,4)=1
ISPEX(1,4,7,4)=1
ISPEX(1,5,7,4)=0
SPEX(1,1,7,4)=0.D00
SPEX(2,1,7,4)=3.66D-19
NEX(1,7,4)=15
ISPEX(1,0,4,7)=7
ISPEX(1,1,4,7)=5
ISPEX(1,2,4,7)=3
ISPEX(1,3,4,7)=1
ISPEX(1,4,4,7)=1
ISPEX(1,5,4,7)=0
SPEX(1,1,4,7)=0.D00
SPEX(2,1,4,7)=3.66D-19
NEX(1,4,7)=15
!--OH+O2 -> HO2+O
ISPEX(1,0,5,3)=5
ISPEX(1,1,5,3)=7
ISPEX(1,2,5,3)=4
ISPEX(1,3,5,3)=1
SPEX(1,1,5,3)=3.66D-19
SPEX(2,1,5,3)=-3.66D-19
NEX(1,5,3)=16
ISPEX(1,0,3,5)=5
ISPEX(1,1,3,5)=7
ISPEX(1,2,3,5)=4
ISPEX(1,3,3,5)=1
SPEX(1,1,3,5)=3.66D-19
SPEX(2,1,3,5)=-3.66D-19
NEX(1,3,5)=16
END IF
!
IF (JCD == 0) THEN !--TCE model

!--the following data is required if JCD=0 (old reaction model)
!--rate data is based on Shatalov, Ibraguimova, Pavlov, Smekhov and Tunik
! REACTION 1 IS H2+H2->2H+H2
  LE(1)=1 
  ME(1)=1 
  KP(1)=2 
  LP(1)=2 
  MP(1)=1 
  CI(1)=1.
  AE(1)=48350.D00*BOLTZ
  AC(1)=9.03D14/(AVOG*1000.D00)
  BC(1)=0.D00
  ER(1)=-7.24D-19 
!--REACTION 2 IS H2+H2O>2H+H2O
  LE(2)=1 
  ME(2)=6 
  KP(2)=2 
  LP(2)=2
  MP(2)=6 
  CI(2)=1. 
  AE(2)=52530.D00*BOLTZ
  AC(2)=8.43D19/(AVOG*1000.D00)
  BC(2)=-1.1
  ER(2)=-7.24D-19 
!--REACTION 3 IS H+H+H2>2H2
  LE(3)=2 
  ME(3)=2 
  KP(3)=-1 
  LP(3)=1 
  MP(3)=1 
  CI(3)=0.
  AE(3)=0.
  AC(3)=1.D17/(AVOG*1000.D00)**2
  BC(3)=-0.6
  ER(3)=7.24D-19 
!--REACTION 4 IS H+H+H2O>H2+H20
  LE(4)=2 
  ME(4)=2 
  KP(4)=-1
  LP(4)=1 
  MP(4)=6 
  CI(4)=0.
  AE(4)=0.
  AC(4)=1.D19/(AVOG*1000.D00)**2
  BC(4)=-1.1
  ER(4)=7.24D-19 
!--REACTION 5 IS H+H+H>H2+H
  LE(5)=2 
  ME(5)=2 
  KP(5)=-1 
  LP(5)=1 
  MP(5)=2 
  CI(5)=0. 
  AE(5)=0.
  AC(5)=3.2D15/(AVOG*1000.D00)**2
  BC(5)=0.
  ER(5)=7.24D-19 
!--REACTION 6 IS 02+O2>20+O2
  LE(6)=3
  ME(6)=3 
  KP(6)=4 
  LP(6)=4 
  MP(6)=3 
  CI(6)=1.0
  AE(6)=60000.D00*BOLTZ
  AC(6)=9.8D24/(AVOG*1000.D00)
  BC(6)=-2.5 
  ER(6)=-8.28D-19 
!--REACTION 7 IS 02+O>20+O
  LE(7)=3 
  ME(7)=4 
  KP(7)=4 
  LP(7)=4 
  MP(7)=4 
  CI(7)=1.0
  AE(7)=60000.D00*BOLTZ
  AC(7)=3.5D25/(AVOG*1000.D00)
  BC(7)=-2.5 
  ER(7)=-8.28D-19
!--REACTION 8 IS 02+H2O>20+H2O
  LE(8)=3
  ME(8)=6 
  KP(8)=4 
  LP(8)=4 
  MP(8)=6 
  CI(8)=0.5
  AE(8)=60000.D00*BOLTZ                                                                                                                                                                            
  AC(8)=1.2D19/(AVOG*1000.D00)
  BC(8)=-0.5 
  ER(8)=-8.28D-19 
!--REACTION 9 IS O+O+O2>O2+O2
  LE(9)=4 
  ME(9)=4 
  KP(9)=-1 
  LP(9)=3 
  MP(9)=3 
  CI(9)=0. 
  AE(9)=0.
  AC(9)=8.D17/(AVOG*1000.D00)**2
  BC(9)=-1. 
  ER(9)=8.28D-19 
!--REACTION 10 IS O+O+O>O2+O 
  LE(10)=4
  ME(10)=4 
  KP(10)=-1  
  LP(10)=3 
  MP(10)=4 
  CI(10)=0. 
  AE(10)=0.
  AC(10)=2.88D18/(AVOG*1000.D00)**2
  BC(10)=-1. 
  ER(10)=8.28D-19 
!--REACTION 11 IS O+O+H2O>O2+H2O
  LE(11)=4 
  ME(11)=4 
  KP(11)=-1 
  LP(11)=3 
  MP(11)=6 
  CI(11)=0. 
  AE(11)=0.
  AC(11)=3.51D14/(AVOG*1000.D00)**2 
  BC(11)=0. 
  ER(11)=8.28D-19 
!--REACTION 12 IS H+O+H2>OH+H2
  LE(12)=2 
  ME(12)=4 
  KP(12)=-1 
  LP(12)=5 
  MP(12)=1 
  CI(12)=0. 
  AE(12)=0.
  AC(12)=2.D19/(AVOG*1000.D00)**2 
  BC(12)=-1.
  ER(12)=7.11D-19 
!--REACTION 13 IS H+O+O2>OH+O2
  LE(13)=2 
  ME(13)=4 
  KP(13)=-1
  LP(13)=5 
  MP(13)=3 
  CI(13)=0. 
  AE(13)=0.
  AC(13)=1.35D19/(AVOG*1000.D00)**2 
  BC(13)=-1.
  ER(13)=7.11D-19 
!--REACTION 14 IS H+O+H2O>OH+H2O
  LE(14)=2 
  ME(14)=4 
  KP(14)=-1 
  LP(14)=5 
  MP(14)=6 
  CI(14)=0. 
  AE(14)=0.
  AC(14)=1.1D20/(AVOG*1000.D00)**2 
  BC(14)=-1. 
  ER(14)=7.11D-19 
!--REACTION 15 IS H2O+O2>H+OH+O2 
  LE(15)=6 
  ME(15)=3 
  KP(15)=2 
  LP(15)=5 
  MP(15)=3 
  CI(15)=1. 
  AE(15)=52920.D00*BOLTZ
  AC(15)=3.5D15/(AVOG*1000.D00)
  BC(15)=0. 
  ER(15)=-8.29D-19 
!--REACTION 16 IS H2O+H2O>H+OH+H2O 
  LE(16)=6 
  ME(16)=6 
  KP(16)=2 
  LP(16)=5
  MP(16)=6 
  CI(16)=1. 
  AE(16)=52920.D00*BOLTZ
  AC(16)=2.26D16/(AVOG*1000.D00)
  BC(16)=0. 
  ER(16)=-8.29D-19 
!--REACTION 17 IS H+OH+H2>H2O+H2 
  LE(17)=2 
  ME(17)=5 
  KP(17)=-1 
  LP(17)=6 
  MP(17)=1 
  CI(17)=0. 
  AE(17)=0.
  AC(17)=1.61D22/(AVOG*1000.D00)**2
  BC(17)=-2. 
  ER(17)=8.29D-19 
!--REACTION 18 IS H+OH+02>H2O+O2 
  LE(18)=2 
  ME(18)=5 
  KP(18)=-1 
  LP(18)=6 
  MP(18)=3 
  CI(18)=0. 
  AE(18)=0.
  AC(18)=2.2D22/(AVOG*1000.D00)**2
  BC(18)=-2.
  ER(18)=8.29D-19
!--REACTION 19 IS H+OH+0H>H2O+OH 
  LE(19)=2 
  ME(19)=5 
  KP(19)=-1
  LP(19)=6 
  MP(19)=5 
  CI(19)=0.
  AE(19)=0. 
  AC(19)=8.34D15/(AVOG*1000.D00)**2 
  BC(19)=0. 
  ER(19)=8.29D-19
!--REACTION 20 IS H+OH+H2O>H2O+H2O
  LE(20)=2 
  ME(20)=5 
  KP(20)=-1 
  LP(20)=6 
  MP(20)=6
  CI(20)=0. 
  AE(20)=0. 
  AC(20)=1.4D23/(AVOG*1000.D00)**2
  BC(20)=2. 
  ER(20)=8.29D-19
!--REACTION 21 IS O2+H+H2>HO2+H2
  LE(21)=3 
  ME(21)=2 
  KP(21)=-1
  LP(21)=7 
  MP(21)=1
  CI(21)=0. 
  AE(21)=0. 
  AC(21)=8.55D19/(AVOG*1000.D00)**2 
  BC(21)=-1.4 
  ER(21)=3.45D-19
!--REACTION 22 IS O2+H+O2>HO2+O2
  LE(22)=3
  ME(22)=2 
  KP(22)=-1 
  LP(22)=7 
  MP(22)=3
  CI(22)=0. 
  AE(22)=0. 
  AC(22)=5.69D18/(AVOG*1000.D00)**2 
  BC(22)=1.094 
  ER(22)=3.45D-19 
!--REACTION 23 IS O2+H+H2O>HO2+H2O
  LE(23)=3 
  ME(23)=2  
  KP(23)=-1 
  LP(23)=7 
  MP(23)=6
  CI(23)=0. 
  AE(23)=0. 
  AC(23)=3.63D19/(AVOG*1000.D00)**2 
  BC(23)=-1. 
  ER(23)=3.45D-19
!--REACTION 24 IS H2+O2>H+HO2 
  LE(24)=1
  ME(24)=3 
  KP(24)=0 
  LP(24)=2
  MP(24)=7 
  CI(24)=1. 
  AE(24)=26926.D00*BOLTZ
  AC(24)=7.4D05/(AVOG*1000.D00)
  BC(24)=2.43 
  ER(24)=-3.79D-19 
!--REACTION 25 IS H+HO2>H2+O2 
  LE(25)=2
  ME(25)=7 
  KP(25)=0 
  LP(25)=1
  MP(25)=3 
  CI(25)=0. 
  AE(25)=1030.D00*BOLTZ
  AC(25)=1.05D14/(AVOG*1000.D00)
  BC(25)=0. 
  ER(25)=3.79D-19 
!--REACTION 26 IS H+HO2>H2O+O 
  LE(26)=2
  ME(26)=7 
  KP(26)=0 
  LP(26)=6
  MP(26)=4 
  CI(26)=0. 
  AE(26)=866.D00*BOLTZ
  AC(26)=3.D13/(AVOG*1000.D00)
  BC(26)=0. 
  ER(26)=3.67D-19 
!--REACTION 27 IS H2+OH>H+H20 
  LE(27)=1
  ME(27)=5 
  KP(27)=0 
  LP(27)=2
  MP(27)=6 
  CI(27)=0. 
  AE(27)=1740.D00*BOLTZ
  AC(27)=2.17D08/(AVOG*1000.D00)
  BC(27)=1.52 
  ER(27)=1.05D-19 
!--REACTION 28 IS H+H2O>H2+OH 
  LE(28)=2
  ME(28)=6 
  KP(28)=0 
  LP(28)=1
  MP(28)=5 
  CI(28)=0. 
  AE(28)=9030.D00*BOLTZ
  AC(28)=4.52D08/(AVOG*1000.D00)
  BC(28)=1.6 
  ER(28)=-1.05D-19 
!--REACTION 29 IS OH+OH>H20+0 
  LE(29)=5
  ME(29)=5 
  KP(29)=0 
  LP(29)=6
  MP(29)=4 
  CI(29)=0. 
  AE(29)=-970.D00*BOLTZ
  AC(29)=3.35D04/(AVOG*1000.D00)
  BC(29)=2.42 
  ER(29)=1.18D-19 
!--REACTION 30 IS O2+H>OH+O 
  LE(30)=3
  ME(30)=2 
  KP(30)=0 
  LP(30)=5
  MP(30)=4 
  CI(30)=0. 
  AE(30)=7560.D00*BOLTZ
  AC(30)=2.06D14/(AVOG*1000.D00)
  BC(30)=-0.097 
  ER(30)=-1.17D-19 
!--REACTION 31 IS OH+O>H+O2 
  LE(31)=5
  ME(31)=4 
  KP(31)=0 
  LP(31)=2
  MP(31)=3 
  CI(31)=0. 
  AE(31)=-113.D00*BOLTZ
  AC(31)=1.21D14/(AVOG*1000.D00)
  BC(31)=-0.352 
  ER(31)=1.17D-19 
!--REACTION 32 IS H2+0>OH+H 
  LE(32)=1
  ME(32)=4 
  KP(32)=0 
  LP(32)=5
  MP(32)=2 
  CI(32)=0. 
  AE(32)=3165.D00*BOLTZ
  AC(32)=5.1D4/(AVOG*1000.D00)
  BC(32)=2.67 
  ER(32)=-0.13D-19 
!--REACTION 33 IS HO2+0>OH+O2 
  LE(33)=7
  ME(33)=4 
  KP(33)=0 
  LP(33)=5
  MP(33)=3 
  CI(33)=0. 
  AE(33)=-224.D00*BOLTZ
  AC(33)=1.63D13/(AVOG*1000.D00)
  BC(33)=0. 
  ER(33)=3.66D-19 
!--REACTION 34 IS HO2+H>OH+OH 
  LE(34)=7
  ME(34)=2 
  KP(34)=0 
  LP(34)=5
  MP(34)=5 
  CI(34)=0. 
  AE(34)=700.D00*BOLTZ
  AC(34)=4.46D14/(AVOG*1000.D00)
  BC(34)=0. 
  ER(34)=2.49D-19 
!--REACTION 35 IS HO2+OH>H20+O2 
  LE(35)=7
  ME(35)=5 
  KP(35)=0 
  LP(35)=6
  MP(35)=3 
  CI(35)=0. 
  AE(35)=-250.D00*BOLTZ
  AC(35)=2.9D13/(AVOG*1000.D00)
  BC(35)=0. 
  ER(35)=4.84D-19 
!--REACTION 36 IS HO2+OH>H20+O2  !--reference gives sum of two rates for this reaction!
  LE(36)=7
  ME(36)=5 
  KP(36)=0 
  LP(36)=6
  MP(36)=3 
  CI(36)=0. 
  AE(36)=8810.D00*BOLTZ
  AC(36)=9.3D15/(AVOG*1000.D00)
  BC(36)=0. 
  ER(36)=4.84D-19 
!--REACTION 37 IS H2+O2>OH+OH 
  LE(37)=1
  ME(37)=3 
  KP(37)=0 
  LP(37)=5
  MP(37)=5 
  CI(37)=0. 
  AE(37)=24044.D00*BOLTZ
  AC(37)=1.7D13/(AVOG*1000.D00)
  BC(37)=0. 
  ER(37)=-1.3D-19 
!
END IF  
!
RETURN
END SUBROUTINE OXYGEN_HYDROGEN
!
!***************************************************************************
!*************************END OF GAS DATABASE*******************************
!***************************************************************************
!
SUBROUTINE ALLOCATE_GAS
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
ALLOCATE (FSP(MSP,2),SP(5,MSP),SPR(3,MSP),SPM(8,MSP,MSP),ISPR(2,MSP),ISPV(MSP),ENTR(6,MSP,2),NRSP(MSP,MSP),      &
          VMP(MSP,2),VNMAX(MSP),CR(MSP),TCOL(MSP,MSP),ISPRC(MSP,MSP),STAT=ERROR)
!
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT ALLOCATE SPECIES VARIABLES',ERROR
END IF
!



ALLOCATE (NEX(MMEX,MSP,MSP),NSPEX(MSP,MSP),SPEX(2,MMEX,MSP,MSP),ISPEX(MMEX,0:2+MMVM,MSP,MSP),TREACG(4,MSP),         &
          PSF(MMEX),TREACL(4,MSP),TNEX(MEX),STAT=ERROR)
!
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT ALLOCATE Q-K REACTION VARIABLES',ERROR
END IF


!
IF (MMVM > 0) THEN

  ALLOCATE (SPVM(4,MMVM,MSP),ISPVM(2,MMVM,MSP),STAT=ERROR)
  IF (ERROR /= 0) THEN
    WRITE (*,*)'PROGRAM COULD NOT ALLOCATE VIBRATION VARIABLES',ERROR
  END IF
END IF
!
IF (MNRE > 0) THEN
  ALLOCATE (CI(MNRE),AE(MNRE),AC(MNRE),BC(MNRE),ER(MNRE),REA(6,MNRE),LE(MNRE),ME(MNRE),KP(MNRE),LP(MNRE),MP(MNRE),      &
            IREA(2,MNRE),JREA(2,MNRE,2),IRCD(MNRE,MSP,MSP),NREA(2,MNRE),STAT=ERROR)
  IF (ERROR /= 0) THEN
    WRITE (*,*)'PROGRAM COULD NOT ALLOCATE REACTION VARIABLES',ERROR
  END IF
END IF 
!
IF (MTBP > 0) THEN
  ALLOCATE (THBP(MTBP,MSP),STAT=ERROR)
  IF (ERROR /= 0) THEN
    WRITE (*,*)'PROGRAM COULD NOT ALLOCATE THIRD BODY VARIABLES',ERROR
  END IF
END IF
!
IF (MNSR > 0) THEN
  ALLOCATE (ERS(MNSR),LIS(2,MNSR),LRS(6,MNSR),ISRCD(MNSR,MSP),STAT=ERROR)
  IF (ERROR /= 0) THEN
    WRITE (*,*)'PROGRAM COULD NOT ALLOCATE SURFACE REACTION VARIABLES',ERROR
  END IF
END IF
!
RETURN
END SUBROUTINE ALLOCATE_GAS
!
!*****************************************************************************
!
SUBROUTINE READ_RESTART
!
USE MOLECS
USE GEOM
USE GAS
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: ZCHECK
!
101 CONTINUE
OPEN (7,FILE='PARAMETERS.DAT',FORM='BINARY',ERR=101)
READ (7) NCCELLS,NCELLS,MMRM,MMVM,MNM,MNRE,MNSR,MSP,MTBP,ILEVEL,MDIV,IRECOM,MMEX,MEX
CLOSE(7)
!

IF (MMVM > 0) THEN
  ALLOCATE (PX(MNM),PTIM(MNM),PROT(MNM),IPCELL(MNM),IPSP(MNM),ICREF(MNM),IPCP(MNM),PV(3,MNM),      &
       IPVIB(MMVM,MNM),STAT=ERROR)
ELSE
  IF (MMRM > 0) THEN
    ALLOCATE (PX(MNM),PTIM(MNM),PROT(MNM),IPCELL(MNM),IPSP(MNM),ICREF(MNM),IPCP(MNM),PV(3,MNM),STAT=ERROR)
  ELSE
    ALLOCATE (PX(MNM),PTIM(MNM),IPCELL(MNM),IPSP(MNM),ICREF(MNM),IPCP(MNM),PV(3,MNM),STAT=ERROR)
  END IF  
END IF
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR MOLECULE ARRAYS',ERROR
ENDIF
!
ALLOCATE (JDIV(0:ILEVEL,MDIV),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR JDIV ARRAY',ERROR
ENDIF
!
ALLOCATE (CELL(4,NCELLS),ICELL(NCELLS),CCELL(5,NCCELLS),ICCELL(3,NCCELLS),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR CELL ARRAYS',ERROR
ENDIF
!
ALLOCATE (COLLS(NCELLS),WCOLLS(NCELLS),CLSEP(NCELLS),REAC(MNRE),SREAC(MNSR),VAR(21,NCELLS),    &
          VARSP(0:11,NCELLS,MSP),VARS(0:32+MSP,2),CS(0:8+MSP,NCELLS,MSP),CSS(0:8,2,MSP,2),CSSS(6,2),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR SAMPLING ARRAYS',ERROR
ENDIF
!
IF (MMVM > 0) THEN
  ALLOCATE (VIBFRAC(MSP,MMVM,0:150),SUMVIB(MSP,MMVM),STAT=ERROR)
  IF (ERROR /= 0) THEN
    WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR RECOMBINATION ARRAYS',ERROR
  ENDIF
END IF
!
CALL ALLOCATE_GAS
!
102 CONTINUE
OPEN (7,FILE='RESTART.DAT',FORM='BINARY',ERR=102)
READ (7) AC,AE,AVDTM,BC,BOLTZ,CCELL,CELL,CI,CLSEP,COLLS,    &
         CPDTM,CR,CS,CSS,CSSS,CTM,CXSS,DDIV,DPI,DTM,DTSAMP,DTOUT,      &
         ENTMASS,ENTR,ER,ERROR,ERS,FDEN,FMA,FND,FNUM,FRACSAM,FSP,FP,FPR,FREM,FSPEC,     &
         FTMP,FTIME,FVTMP,GASCODE,ICCELL,ICELL,ICREF,IFX,IMTS,IPCELL,IPCP,     &
         IPSP,IPVIB,IRCD,IREA,IREM,ISECS,ISF,ISPEX,ISPR,ISPRC,ISPV,ISPVM,ISRCD,ITYPE,IVB,IWF,JCD,     &
         JDIV,JREA,KP,LE,LIS,LP,LRS,ME,MOLSC,MP,MVER,NCCELLS,NCELLS,    &
         NCIS,NDIV,NEX,NM,NMISAMP,NNC,NOUT,NREA,NRSP,NSAMP,NSPEX,NVER,PI,PROT,PTIM,PV,PX,     &
         REAC,RGFS,RMAS,REA,SP,SPEX,SPI,SPM,SPR,SPV,SPVM,SREAC,SUMVIB,    &
         TCOL,TDISS,TFOREX,TRECOMB,TREVEX,THBP,TISAMP,TPOUT,TREF,TLIM,TOTCOL,TOTDUP,TOTMOV,     &
         TREACG,TREACL,TOUT,TPDTM,TREF,TSAMP,TSURF,VAR,VARS,VARSP,VELOB,VFX,VFY,VIBFRAC,VMP,     &
         VMPM,VNMAX,VSURF,WCOLLS,WFM,XB,XREM,XVELS,YVELS,TNEX,ZCHECK
! 
CLOSE(7)
!
IF (ZCHECK /= 1234567) THEN
  WRITE (9,*) NM,' Molecules, Check integer =',ZCHECK
  STOP
ELSE
  WRITE (9,*) 'Restart file read, Check integer=',ZCHECK  
END IF
!
RETURN
END SUBROUTINE READ_RESTART
!
!*****************************************************************************
!
SUBROUTINE WRITE_RESTART
!
USE MOLECS
USE GEOM
USE GAS
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: ZCHECK
!
ZCHECK=1234567
!
101 CONTINUE
OPEN (7,FILE='PARAMETERS.DAT',FORM='BINARY',ERR=101)
WRITE (7) NCCELLS,NCELLS,MMRM,MMVM,MNM,MNRE,MNSR,MSP,MTBP,ILEVEL,MDIV,IRECOM,MMEX,MEX
CLOSE(7)
!
102 CONTINUE
OPEN (7,FILE='RESTART.DAT',FORM='BINARY',ERR=102)
WRITE (7)AC,AE,AVDTM,BC,BOLTZ,CCELL,CELL,CI,CLSEP,COLLS,    &
         CPDTM,CR,CS,CSS,CSSS,CTM,CXSS,DDIV,DPI,DTM,DTSAMP,DTOUT,      &
         ENTMASS,ENTR,ER,ERROR,ERS,FDEN,FMA,FND,FNUM,FRACSAM,FSP,FP,FPR,FREM,FSPEC,     &
         FTMP,FTIME,FVTMP,GASCODE,ICCELL,ICELL,ICREF,IFX,IMTS,IPCELL,IPCP,     &
         IPSP,IPVIB,IRCD,IREA,IREM,ISECS,ISF,ISPEX,ISPR,ISPRC,ISPV,ISPVM,ISRCD,ITYPE,IVB,IWF,JCD,     &
         JDIV,JREA,KP,LE,LIS,LP,LRS,ME,MOLSC,MP,MVER,NCCELLS,NCELLS,    &
         NCIS,NDIV,NEX,NM,NMISAMP,NNC,NOUT,NREA,NRSP,NSAMP,NSPEX,NVER,PI,PROT,PTIM,PV,PX,     &
         REAC,RGFS,RMAS,REA,SP,SPEX,SPI,SPM,SPR,SPV,SPVM,SREAC,SUMVIB,    &
         TCOL,TDISS,TFOREX,TRECOMB,TREVEX,THBP,TISAMP,TPOUT,TREF,TLIM,TOTCOL,TOTDUP,TOTMOV,     &
         TREACG,TREACL,TOUT,TPDTM,TREF,TSAMP,TSURF,VAR,VARS,VARSP,VELOB,VFX,VFY,VIBFRAC,VMP,     &
         VMPM,VNMAX,VSURF,WCOLLS,WFM,XB,XREM,XVELS,YVELS,TNEX,ZCHECK
!
CLOSE(7)
!
WRITE (9,*) 'Restart files written'
!
RETURN
END SUBROUTINE WRITE_RESTART
!
!*****************************************************************************
!
SUBROUTINE FIND_CELL(X,NCC,NSC)
!
USE MOLECS
USE GEOM
USE CALC
!
IMPLICIT NONE
!
INTEGER :: N,L,M,NSC,NCC,ND
REAL(KIND=8) :: X,FRAC,DSC
!
!--NCC collision cell number
!--NSC sampling cell number
!--X location
!--ND division number
!--DSC the ratio of the sub-division width to the division width
!
ND=(X-XB(1))/DDIV+0.99999999999999D00
!
IF (JDIV(0,ND) < 0) THEN    !the division is a level 0 (no sub-division) sampling cell
  NSC=-JDIV(0,ND)
!  IF (IFX == 0)
  NCC=NCIS*(X-CELL(2,NSC))/(CELL(3,NSC)-CELL(2,NSC))+0.9999999999999999D00
  NCC=NCC+ICELL(NSC)
!  IF (NCC == 0) NCC=1
  RETURN
ELSE  !--the molecule is in a subdivided division
  FRAC=(X-XB(1))/DDIV-DFLOAT(ND-1)
  M=ND
  DO N=1,ILEVEL
    DSC=1.D00/DFLOAT(N+1)
    DO L=1,2  !over the two level 1 subdivisions
      IF (((L == 1).AND.(FRAC < DSC)).OR.((L == 2).AND.(FRAC >= DSC))) THEN
        M=JDIV(N-1,M)+L  !the address in JDIV
        IF (JDIV(N,M) < 0) THEN
          NSC=-JDIV(N,M)
          NCC=NCIS*(X-CELL(2,NSC))/(CELL(3,NSC)-CELL(2,NSC))+0.999999999999999D00 
          IF (NCC == 0) NCC=1
          NCC=NCC+ICELL(NSC)
          RETURN
        END IF
      END IF
    END DO
    FRAC=FRAC-DSC
  END DO
END IF        
WRITE (9,*) 'No cell for molecule at x=',X        
STOP
END SUBROUTINE FIND_CELL
!
!*****************************************************************************
!
SUBROUTINE FIND_CELL_MB(X,NCC,NSC,TIM)
!
USE MOLECS
USE GEOM
USE CALC
!
IMPLICIT NONE
!
INTEGER :: N,L,M,NSC,NCC,ND
REAL(KIND=8) :: X,FRAC,DSC,A,B,C,TIM
!
!--NCC collision cell number
!--NSC sampling cell number
!--X location
!--ND division number
!--DSC the ratio of the sub-division width to the division width
!--TIM the time
!
A=(XB(2)+VELOB*TIM-XB(1))/DFLOAT(NDIV)      !--new DDIV
ND=(X-XB(1))/A+0.99999999999999D00
B=XB(1)+DFLOAT(ND-1)*A
!
!the division is a level 0 sampling cell
NSC=-JDIV(0,ND)
NCC=NCIS*(X-B)/A+0.99999999999999D00
NCC=NCC+ICELL(NSC)
RETURN
WRITE (9,*) 'No cell for molecule at x=',X        
STOP
END SUBROUTINE FIND_CELL_MB
!
!******************************************************************************
!
FUNCTION GAM(X)
!
!--calculates the Gamma function of X.
!
A=1.
Y=X
IF (Y < 1.) THEN
  A=A/Y
ELSE
  Y=Y-1.
  DO WHILE (Y >= 1.)
    A=A*Y
    Y=Y-1.
  END DO
END IF
GAM=A*(1.-0.5748646*Y+0.9512363*Y**2-0.6998588*Y**3+  &
       0.4245549*Y**4-0.1010678*Y**5)
!
RETURN
!
END FUNCTION GAM
!
!*****************************************************************************
!
FUNCTION ERF(S)
!
!--evaluates the error function of S
!
B=ABS(S)
IF (B < 4.) THEN
  C=EXP(-B*B)
  T=1./(1.+0.3275911*B)
  D=1.-(0.254829592*T-0.284496736*T*T+1.421413741*T*T*T- &
    1.453152027*T*T*T*T+1.061405429*T*T*T*T*T)*C
ELSE
  D=1.
END IF
IF (S < 0.) D=-D
ERF=D
RETURN
END FUNCTION ERF
!
!*****************************************************************************
!
SUBROUTINE RVELC(U,V,VMP)
!
USE CALC
!
IMPLICIT NONE
!
!--generates two random velocity components U and V in an equilibrium
!--gas with most probable speed VMP
REAL(KIND=8) :: U,V,VMP,A,B
!
CALL RANDOM_NUMBER(RANF)
A=DSQRT(-DLOG(RANF))
CALL RANDOM_NUMBER(RANF)
B=DPI*RANF
U=A*DSIN(B)*VMP
V=A*DCOS(B)*VMP
RETURN
!
END SUBROUTINE RVELC
!
!*****************************************************************************
!
SUBROUTINE SROT(L,TEMP,ROTE)
!
!--sets a typical rotational energy ROTE of species L
!
USE CALC
USE GAS
!
IMPLICIT NONE
!
INTEGER :: I,L
REAL(KIND=8) :: A,B,ROTE,ERM,TEMP
!    
IF (ISPR(1,L).EQ.2) THEN
  CALL RANDOM_NUMBER(RANF)
  ROTE=-DLOG(RANF)*BOLTZ*TEMP
ELSE
  A=0.5D00*ISPR(1,L)-1.D00
  I=0
  DO WHILE (I == 0)
    CALL RANDOM_NUMBER(RANF)
    ERM=RANF*10.D00
!--there is an energy cut-off at 10 kT
    B=((ERM/A)**A)*DEXP(A-ERM)
    CALL RANDOM_NUMBER(RANF)
    IF (B > RANF) I=1
  END DO
  ROTE=ERM*BOLTZ*TEMP
END IF
!
RETURN
!
END SUBROUTINE SROT
!
!*****************************************************************************
!
SUBROUTINE SVIB(L,TEMP,IVIB,K)
!
!--sets a typical vibrational state at temp. TEMP of mode K of species L
!
USE GAS
USE CALC
!
IMPLICIT NONE
!
INTEGER :: K,L,N
REAL(KIND=8) :: TEMP
INTEGER :: IVIB
!
CALL RANDOM_NUMBER(RANF)
N=-DLOG(RANF)*TEMP/SPVM(1,K,L)                 !eqn(11.24)
!--the state is truncated to an integer
IVIB=N
END
!
!*****************************************************************************
!
SUBROUTINE INIT_REAC
!
!--initialise the reaction variables (in the old reaction model)
!
USE MOLECS
!
USE GAS
USE CALC
USE GEOM
USE OUTPUT
! 
IMPLICIT NONE
!
INTEGER :: I,K,KK,L,M,N,LS,MS,NNRE,NTBP
REAL(KIND=8) :: A,B,C,EPS
REAL :: GAM,AA,BB
!
!--I,K,L,M,N working integers
!--A,B working variables
!--LS,MS species
!--EPS symmetry factor
!
!
!--convert the reaction data into the required form
!
IF (MNRE == 0) THEN
  NNRE=1
ELSE 
  NNRE=MNRE
END IF
IF (MTBP == 0) THEN
  NTBP=1
ELSE
  NTBP=MTBP
END IF
!
IF (MNRE > 0) THEN 
  REA=0.; IREA=0; NREA=0; JREA=0; NRSP=0; IRCD=0 ; REAC=0.
!
  DO  N=1,MNRE
    
    L=LE(N)
    M=ME(N)
!--the pre-reaction species codes are L and M 
    IF ((L > 0).AND.(M > 0)) THEN
      IREA(1,N)=L
      IREA(2,N)=M
      IF ( M.LT.L ) THEN
        K = L
        L = M
        M = K
      END IF
      NRSP(L,M) = NRSP(L,M) + 1
      IF (L.NE.M) NRSP(M,L) = NRSP(M,L) + 1
      K = NRSP(L,M)
!--this is the Kth reaction involving species L and M
      IRCD(K,M,L)=N
      IRCD(K,L,M)=N
	END IF 
    K=KP(N)
    L=LP(N)
    M=MP(N)
    IF (K.EQ.0) THEN
!--two post-collision particles
      NREA(1,N)=1
      NREA(2,N)=1
      JREA(1,N,1)=L
      JREA(2,N,1)=M
    END IF
    IF (K.GT.0) THEN
!--three post-collision particles
      NREA(1,N)=2
      NREA(2,N)=1
      JREA(1,N,1)=K
      JREA(1,N,2)=L
      JREA(2,N,1)=M
    END IF
    IF (K.EQ.-1) THEN
!--one post collision particle
      NREA(1,N)=1
      NREA(2,N)=0
      JREA(1,N,1)=L
      JREA(2,N,1)=M
    END IF
!
    REA(1,N)=CI(N)
    REA(2,N)=AE(N)
    REA(3,N)=AC(N)
    REA(4,N)=BC(N)
    REA(5,N)=ER(N)
  END DO
! 
END IF
!
DO L=1,MNRE
  DO M=1,MSP
    DO N=1,MSP
      IF (IRCD(L,N,M).EQ.0) IRCD(L,N,M)=IRCD(L,M,N)
    END DO
  END DO
END DO
DO K=1,MNRE
  LS=IREA(1,K)
  MS=IREA(2,K)
!--calculate the coefficients of the energy terms in eqns (6.10,6.14)
  EPS=1.D00
  IF (LS == MS) EPS=2.D00
  A=SPI*EPS*REA(3,K)*(SPM(5,LS,MS)**REA(4,K))/(2.*SPM(2,LS,MS))
  A=A*SQRT(SPM(1,LS,MS)/(2.*BOLTZ*SPM(5,LS,MS)))
  I=NREA(1,K)+NREA(2,K)
  IF (I > 1) THEN
!--binary reaction (dissociation or exchange)
    BB=REA(1,K)+REA(4,K)+1.5D00
    IF (BB < 1.D-6) BB=1.D-6
    B=GAM(BB)
    AA=REA(1,K)+2.5D00-SPM(3,LS,MS)
    C=GAM(AA)
    REA(6,K)=A*C/(((BOLTZ*SPM(5,LS,MS))**(REA(4,K)-1.+SPM(3,LS,MS)))*B)
  ELSE
!--ternary reaction (recombination)
    BB=REA(4,K)+1.5D00
    IF (BB < 1.D-6)  BB=1.D-6
    B=GAM(BB)
    AA=2.5D00-SPM(3,LS,MS)
    C=GAM(AA)
    REA(6,K)=A*C/B
  END IF
END DO
!
RETURN
!
END SUBROUTINE INIT_REAC
!
!*****************************************************************************
!
SUBROUTINE MOLECULES_ENTER
!
!--molecules enter boundary at XB(1) and XB(2) and may be removed behind a wave
!
USE MOLECS
USE GAS
USE CALC
USE GEOM
USE OUTPUT
! 
IMPLICIT NONE
!
INTEGER :: K,L,M,N,NENT,II,J,JJ,KK,NTRY
REAL(KIND=8) :: A,B,AA,BB,U,VN,XI,X,DX,DY,DZ
!
!--NENT number to enter in the time step
!
DO J=1,2       !--J is the end
  IF (ITYPE(J) == 0) THEN
    KK=1 !--the entry surface will normally use the reference gas (main stream) properties
    IF ((J == 2).AND.(ISECS == 1).AND.(XB(2) > 0.D00)) KK=2    !--KK is 1 for reference gas 2 for the secondary stream
    DO L=1,MSP
      A=ENTR(1,L,J)*DTM+ENTR(2,L,J)
      NENT=A
      ENTR(2,L,J)=A-NENT
      IF (NENT > 0) THEN
        DO M=1,NENT
          IF (NM >= MNM) THEN
            WRITE (*,*) 'EXTEND_MNM from MOLECULES_ENTER'               
            CALL EXTEND_MNM(1.1)
          END IF
          NM=NM+1
          AA=DMAX1(0.D00,ENTR(3,L,J)-3.D00) 
          BB=DMAX1(3.D00,ENTR(3,L,J)+3.D00)
          II=0
          DO WHILE (II == 0)
            CALL RANDOM_NUMBER(RANF)
            B=AA+(BB-AA)*RANF
            U=B-ENTR(3,L,J)
            A=(2.D00*B/ENTR(4,L,J))*DEXP(ENTR(5,L,J)-U*U)
            CALL RANDOM_NUMBER(RANF)
            IF (A > RANF) II=1
          END DO
          PV(1,NM)=B*VMP(L,KK)
          IF (J == 2) PV(1,NM)=-PV(1,NM)         
!           
          CALL RVELC(PV(2,NM),PV(3,NM),VMP(L,KK))
          PV(2,NM)=PV(2,NM)+VFY(J)
!          
          IF (ISPR(1,L) > 0) CALL SROT(L,FTMP(KK),PROT(NM))
!          
          IF (MMVM > 0) THEN
            DO K=1,ISPV(L)
              CALL SVIB(L,FVTMP(KK),IPVIB(K,NM),K)
            END DO
          END IF
          IPSP(NM)=L
!--advance the molecule into the flow
          CALL RANDOM_NUMBER(RANF)        
          XI=XB(J)
          DX=DTM*RANF*PV(1,NM)
          IF (IFX == 0) X=XI+DX
          IF (IFX > 0) DY=DTM*RANF*PV(2,NM)
          DZ=0.D00
          IF (IFX == 2) DZ=DTM*RANF*PV(3,NM)
          IF (IFX > 0) CALL AIFX(XI,DX,DY,DZ,X,PV(1,NM),PV(2,NM),PV(3,NM))
          IF (IFX == 0) PX(NM)=X
          PTIM(NM)=FTIME
          IF (IVB == 0) CALL FIND_CELL(PX(NM),IPCELL(NM),JJ)
          IF (IVB == 1) CALL FIND_CELL_MB(PX(NM),IPCELL(NM),JJ,PTIM(NM))
          IPCP(NM)=0
          IF (XREM > XB(1)) ENTMASS=ENTMASS+SP(5,L)
        END DO
      END IF
    END DO
  END IF
END DO  
!
!--stagnation streamline molecule removal
IF (XREM > XB(1)) THEN
  ENTMASS=FREM*ENTMASS
  NTRY=0
  DO WHILE ((ENTMASS > 0.D00).AND.(NTRY < 10000))
    NTRY=NTRY+1
    IF (NTRY == 10000) THEN
      WRITE (*,*) 'Unable to find molecule for removal'
      ENTMASS=0.D00
      VNMAX=0.D00
    END IF
    CALL RANDOM_NUMBER(RANF)
    N=NM*RANF+0.9999999D00
    IF (PX(N) > XREM) THEN
      CALL RANDOM_NUMBER(RANF)
      !IF (RANF < ((PX(N)-XREM)/(XB(2)-XREM))**2) THEN
      IF (DABS(VFY(1)) < 1.D-3) THEN
        VN=DSQRT(PV(2,N)*PV(2,N)+PV(3,N)*PV(3,N))   !--AXIALLY SYMMETRIC STREAMLINE
      ELSE
        VN=DABS(PV(3,N))   !--TWO-DIMENSIONAL STREAMLINE
      END IF  
      L=IPSP(N)
      IF (VN > VNMAX(L)) VNMAX(L)=VN
      CALL RANDOM_NUMBER(RANF)
      IF (RANF < VN/VNMAX(L)) THEN
        CALL REMOVE_MOL(N)
        ENTMASS=ENTMASS-SP(5,L)
        NTRY=0
      END IF
      !END IF   
    END IF
  END DO
END IF

!
END SUBROUTINE MOLECULES_ENTER
!
!*****************************************************************************
!
SUBROUTINE EXTEND_MNM(FAC)
!
!--the maximum number of molecules is increased by a specified factor
!--the existing molecules are copied to temporary disk storage
!
USE MOLECS
USE CALC
USE GAS
!
IMPLICIT NONE
!
INTEGER :: M,N,MNMN
REAL :: FAC
!
!--M,N working integers
!--MNMN extended value of MNM
!--FAC the factor for the extension
MNMN=FAC*MNM
WRITE (*,*) 'Maximum number of molecules is to be extended from',MNM,' to',MNMN
WRITE (*,*) '( if the additional memory is available !! )'
OPEN (7,FILE='EXTMOLS.SCR',FORM='BINARY')
WRITE (*,*) 'Start write to temporary disk storage'
DO N=1,MNM
  IF (MMVM > 0) THEN
    WRITE (7) PX(N),PTIM(N),PROT(N),(PV(M,N),M=1,3),IPSP(N),IPCELL(N),ICREF(N),IPCP(N),(IPVIB(M,N),M=1,MMVM)
  ELSE
    IF (MMRM > 0) THEN
      WRITE (7) PX(N),PTIM(N),PROT(N),(PV(M,N),M=1,3),IPSP(N),IPCELL(N),ICREF(N),IPCP(N)
    ELSE
      WRITE (7) PX(N),PTIM(N),(PV(M,N),M=1,3),IPSP(N),IPCELL(N),ICREF(N),IPCP(N)
    END IF  
  END IF  
END  DO
WRITE (*,*) 'Disk write completed'
CLOSE (7)
IF (MMVM > 0) THEN
  DEALLOCATE (PX,PTIM,PROT,PV,IPSP,IPCELL,ICREF,IPCP,IPVIB,STAT=ERROR)
ELSE
  IF (MMRM > 0) THEN
    DEALLOCATE (PX,PTIM,PROT,PV,IPSP,IPCELL,ICREF,IPCP,STAT=ERROR)
  ELSE
    DEALLOCATE (PX,PTIM,PV,IPSP,IPCELL,ICREF,IPCP,STAT=ERROR)
  END IF  
END IF
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT DEALLOCATE MOLECULES',ERROR 
!  STOP
END IF
!
WRITE (*,*) 'Molecule arrays have been deallocated'
!
IF (MMVM > 0) THEN
  ALLOCATE (PX(MNMN),PTIM(MNMN),PROT(MNMN),PV(3,MNMN),IPSP(MNMN),IPCELL(MNMN),ICREF(MNMN),IPCP(MNMN),IPVIB(MMVM,MNMN),STAT=ERROR)
ELSE
  IF (MMRM > 0) THEN
    ALLOCATE (PX(MNMN),PTIM(MNMN),PROT(MNMN),PV(3,MNMN),IPSP(MNMN),IPCELL(MNMN),ICREF(MNMN),IPCP(MNMN),STAT=ERROR)
  ELSE
    ALLOCATE (PX(MNMN),PTIM(MNMN),PV(3,MNMN),IPSP(MNMN),IPCELL(MNMN),ICREF(MNMN),IPCP(MNMN),STAT=ERROR)
  END IF  
END IF
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT ALLOCATE SPACE FOR EXTEND_MNM',ERROR
!  STOP
END IF
!
PX=0.; PTIM=0.; PV=0.; IPSP=0; IPCELL=0; ICREF=0; IPCP=0
IF (MMRM > 0) PROT=0.
IF (MMVM > 0) IPVIB=0
!--restore the original molecules
OPEN (7,FILE='EXTMOLS.SCR',FORM='BINARY')
WRITE (*,*) 'Start read back from temporary disk storage'
DO N=1,MNM
  IF (MMVM > 0) THEN
    READ (7) PX(N),PTIM(N),PROT(N),(PV(M,N),M=1,3),IPSP(N),IPCELL(N),ICREF(N),IPCP(N),(IPVIB(M,N),M=1,MMVM)
  ELSE
    IF (MMRM > 0) THEN
      READ (7) PX(N),PTIM(N),PROT(N),(PV(M,N),M=1,3),IPSP(N),IPCELL(N),ICREF(N),IPCP(N)
    ELSE
      READ (7) PX(N),PTIM(N),(PV(M,N),M=1,3),IPSP(N),IPCELL(N),ICREF(N),IPCP(N)
    END IF  
  END IF
END DO
WRITE (*,*) 'Disk read completed'
CLOSE (7,STATUS='DELETE')
!
MNM=MNMN
!
RETURN
END SUBROUTINE EXTEND_MNM
!
!*****************************************************************************
!
SUBROUTINE MOLECULES_MOVE
!
!--molecule moves appropriate to the time step
!
USE MOLECS
USE GAS
USE GEOM
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: N,L,M,K,NCI,J,II,JJ
REAL(KIND=8) :: A,B,X,XI,XC,DX,DY,DZ,DTIM,S1,XM,R,TI,DTC,POB,UR,WFI,WFR,WFRI
!
!--N working integer
!--NCI initial cell time
!--DTIM time interval for the move
!--POB position of the outer boundary
!--TI initial time
!--DTC time interval to collision with surface
!--UR radial velocity component
!--WFI initial weighting factor
!--WFR weighting factor radius
!--WFRI initial weighting factor radius
!
N=1
DO WHILE (N <= NM)
!
if ((n == 111234).and.(ftime > 3.05e-5)) then
  continue
end if  
!
  NCI=IPCELL(N)
  IF (IMTS == 0) DTIM=DTM
  IF (IMTS == 1) DTIM=2.D00*CCELL(3,NCI)
  IF (FTIME-PTIM(N) > 0.5*DTIM) THEN
    WFI=1.D00
    IF (IWF == 1) WFI=1.D00+WFM*PX(N)**IFX
    II=0 !--becomes 1 if a molecule is removed
    TI=PTIM(N)
    PTIM(N)=TI+DTIM
    TOTMOV=TOTMOV+1
!
    XI=PX(N)
    DX=DTIM*PV(1,N)
    X=XI+DX
!    
    IF (IFX > 0) THEN
      DZ=0.D00
      DY=DTIM*PV(2,N)
      IF (IFX == 2) DZ=DTIM*PV(3,N)
      R=DSQRT(X*X+DY*DY+DZ*DZ)
    END IF  
!
    IF (IFX == 0) THEN  
      DO J=1,2    ! 1 for minimum x boundary, 2 for maximum x boundary
        IF (II == 0) THEN
          IF (((J == 1).AND.(X < XB(1))).OR.((J == 2).AND.(X > (XB(2)+VELOB*PTIM(N))))) THEN  !--molecule crosses a boundary
            IF ((ITYPE(J) == 0).OR.(ITYPE(J) == 3)) THEN          
              IF (XREM > XB(1)) THEN
                L=IPSP(N)
                ENTMASS=ENTMASS-SP(5,L)
              END IF                
              CALL REMOVE_MOL(N)
              N=N-1
              II=1
            END IF
!          
            IF (ITYPE(J) == 1) THEN
              IF ((IVB == 0).OR.(J == 1)) THEN
                X=2.D00*XB(J)-X
                PV(1,N)=-PV(1,N)
              ELSE IF ((J == 2).AND.(IVB == 1)) THEN
                DTC=(XB(2)+TI*VELOB-XI)/(PV(1,N)-VELOB)
                XC=XI+PV(1,N)*DTC
                PV(1,N)=-PV(1,N)+2.*VELOB
                X=XC+PV(1,N)*(DTIM-DTC)            
              END IF  
            END IF            
!    
            IF (ITYPE(J) == 2) THEN
              CALL REFLECT(N,J,X)
            END IF  
          END IF
        END IF
      END DO
    ELSE         !--cylindrical or spherical flow
!--check boundaries    
      IF ((X < XB(1)).AND.(XB(1) > 0.D00)) THEN
        CALL RBC(XI,DX,DY,DZ,XB(1),S1)
        IF (S1 < 1.D00) THEN     !--intersection with inner boundary
          IF (ITYPE(1) == 2) THEN !--solid surface
            DX=S1*DX
            DY=S1*DY
            DZ=S1*DZ
            CALL AIFX(XI,DX,DY,DZ,X,PV(1,N),PV(2,N),PV(3,N))
            CALL REFLECT(N,1,X)     
          ELSE
            CALL REMOVE_MOL(N)
            N=N-1
            II=1
          END IF
        END IF       
      ELSE IF ((IVB == 0).AND.(R > XB(2))) THEN
        CALL RBC(XI,DX,DY,DZ,XB(2),S1)
        IF (S1 < 1.D00) THEN     !--intersection with outer boundary
          IF (ITYPE(2) == 2) THEN !--solid surface
            DX=S1*DX
            DY=S1*DY
            DZ=S1*DZ
            CALL AIFX(XI,DX,DY,DZ,X,PV(1,N),PV(2,N),PV(3,N))
            X=1.001D00*XB(2)
            DO WHILE (X > XB(2))
              CALL REFLECT(N,2,X)
            END DO     
          ELSE
            CALL REMOVE_MOL(N)
            N=N-1
            II=1
          END IF
        END IF 
      ELSE IF ((IVB == 1).AND.(R > (XB(2)+PTIM(N)*VELOB))) THEN
        IF (IFX == 1) UR=DSQRT(PV(1,N)**2+PV(2,N)**2)
        IF (IFX == 2) UR=DSQRT(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
        DTC=(XB(2)+TI*VELOB-XI)/(UR-VELOB)
        S1=DTC/DTIM
        DX=S1*DX
        DY=S1*DY
        DZ=S1*DZ
        CALL AIFX(XI,DX,DY,DZ,X,PV(1,N),PV(2,N),PV(3,N))
        PV(1,N)=-PV(1,N)+2.*VELOB
        X=X+PV(1,N)*(DTIM-DTC)     
      ELSE
        CALL AIFX(XI,DX,DY,DZ,X,PV(1,N),PV(2,N),PV(3,N))
      END IF
      
  !--DIAGNOSTIC    
      IF (X > XB(2)+PTIM(N)*VELOB) THEN
        WRITE (*,*) N,FTIME,X,XB(2)+PTIM(N)*VELOB
      END IF
      
!--Take action on weighting factors
      IF ((IWF == 1).AND.(II >= 0)) THEN
        WFR=WFI/(1.D00+WFM*X**IFX)
        L=0
        WFRI=WFR
        IF (WFR >= 1.D00) THEN
          DO WHILE (WFR >= 1.D00)
            L=L+1
            WFR=WFR-1.D00
          END DO
        END IF
        CALL RANDOM_NUMBER(RANF)
        IF (RANF <= WFR) L=L+1
        IF (L == 0) THEN
          CALL REMOVE_MOL(N)
          N=N-1
          II=1
        END IF
        L=L-1        
        IF (L > 0) THEN
          DO K=1,L
            IF (NM >= MNM) CALL EXTEND_MNM(1.1) 
            NM=NM+1
            PX(NM)=X
            DO M=1,3
              PV(M,NM)=PV(M,N)
            END DO
            IF (MMRM > 0) PROT(NM)=PROT(N)
            IPCELL(NM)=ABS(IPCELL(N))
            IPSP(NM)=IPSP(N)
            IPCP(NM)=IPCP(N)
            IF (MMVM > 0) THEN
              DO M=1,MMVM
                IPVIB(M,NM)=IPVIB(M,N)
              END DO
            END IF
            PTIM(NM)=PTIM(N)    !+5.D00*DFLOAT(K)*DTM
!--note the possibility of a variable time advance that may take the place of the duplication buffer in earlier programs        

            IF (PX(NM) > XB(2)+PTIM(NM)*VELOB) THEN
              WRITE (*,*) 'DUP',NM,FTIME,PX(NM),XB(2)+PTIM(NM)*VELOB
            END IF


          END DO
        END IF
      END IF
    END IF
!    
    IF (II == 0) THEN  
      PX(N)=X
      
      if ((px(n) > xb(1)).and.(px(n) < xb(2))) then
        continue
      else  
        write (*,*) n,'Outside flowfield at',px(n)
      end if
      
      IF (IVB == 0) CALL FIND_CELL(PX(N),IPCELL(N),JJ)
      IF (IVB == 1) CALL FIND_CELL_MB(PX(N),IPCELL(N),JJ,PTIM(N))
    END IF
    
   
    
  END IF
!    
  N=N+1 
END DO
!
RETURN
!
END SUBROUTINE MOLECULES_MOVE
!
!*****************************************************************************
!
SUBROUTINE REFLECT(N,J,X)
!
!--reflects molecule N and samples the surface J properties
!
USE MOLECS
USE GAS
USE GEOM
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: N,J,L,K,M
REAL(KIND=8) :: A,B,VMPS,DTR,X,XI,DX,DY,DZ,WF
!
!--VMPS most probable velocity at the surface temperature
!--DTR time remaining after molecule hits a surface
!
L=IPSP(N)
WF=1.D00
IF (IWF == 1) WF=1.D00+WFM*X**IFX
CSS(0,J,L,1)=CSS(0,J,L,1)+1.D00
CSS(1,J,L,1)=CSS(1,J,L,1)+WF
CSS(2,J,L,1)=CSS(2,J,L,1)+WF*PV(1,N)*SP(5,L)
CSS(3,J,L,1)=CSS(3,J,L,1)+WF*(PV(2,N)-VSURF(J))*SP(5,L)
CSS(4,J,L,1)=CSS(4,J,L,1)+WF*PV(3,N)*SP(5,L)
A=(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
CSS(5,J,L,1)=CSS(5,J,L,1)+WF*0.5D00*SP(5,L)*A
IF (ISPR(1,L) > 0) CSS(6,J,L,1)=CSS(6,J,L,1)+WF*PROT(N)
IF (MMVM > 0) THEN
  IF (ISPV(L).GT.0) THEN
    DO K=1,ISPV(L)
      CSS(7,J,L,1)=CSS(7,J,L,1)+WF*DFLOAT(IPVIB(K,N))*BOLTZ*SPVM(1,K,L)
    END DO
  END IF
END IF 
B=DABS(PV(1,N))
CSSS(1,J)=CSSS(1,J)+WF/B
CSSS(2,J)=CSSS(2,J)+WF*SP(5,L)/B
CSSS(3,J)=CSSS(3,J)+WF*SP(5,L)*PV(2,N)/B
!--this assumes that any flow normal to the x direction is in the y direction
CSSS(4,J)=CSSS(4,J)+WF*SP(5,L)*A/B        
IF (ISPR(1,L) > 0) THEN
  CSSS(5,J)=CSSS(5,J)+WF*PROT(N)/B
  CSSS(6,J)=CSSS(6,J)+WF*ISPR(1,L)/B
END IF   
!
CALL RANDOM_NUMBER(RANF)
IF (FSPEC(J) > RANF) THEN      !--specular reflection
  X=2.D00*XB(J)-X          
  PV(1,N)=-PV(1,N)
  DTR=(X-XB(J))/PV(1,N)
ELSE                         !--diffuse reflection
  VMPS=SQRT(2.D00*BOLTZ*TSURF(J)/SP(5,L))
  DTR=(XB(J)-PX(N))/PV(1,N)
  CALL RANDOM_NUMBER(RANF)
  PV(1,N)=SQRT(-LOG(RANF))*VMPS
  IF (J == 2) PV(1,N)=-PV(1,N)
  CALL RVELC(PV(2,N),PV(3,N),VMPS)
  PV(2,N)=PV(2,N)+VSURF(J)
  IF (ISPR(1,L) > 0) CALL SROT(L,TSURF(J),PROT(N))
  IF (MMVM > 0) THEN
    DO K=1,ISPV(L)
      CALL SVIB(L,TSURF(J),IPVIB(K,N),K)
    END DO
  END IF
END IF  
!
CSS(2,J,L,2)=CSS(2,J,L,2)-WF*PV(1,N)*SP(5,L)
CSS(3,J,L,2)=CSS(3,J,L,2)-WF*(PV(2,N)-VSURF(J))*SP(5,L)
CSS(4,J,L,2)=CSS(4,J,L,2)-WF*PV(3,N)*SP(5,L)
A=(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
CSS(5,J,L,2)=CSS(5,J,L,2)-WF*0.5D00*SP(5,L)*A
IF (ISPR(1,L) > 0) CSS(6,J,L,2)=CSS(6,J,L,2)-WF*PROT(N)
IF (MMVM > 0) THEN
  IF (ISPV(L).GT.0) THEN
    DO K=1,ISPV(L)
      CSS(7,J,L,2)=CSS(7,J,L,2)-WF*DFLOAT(IPVIB(K,N))*BOLTZ*SPVM(1,K,L)
    END DO
  END IF
END IF
B=DABS(PV(1,N))
CSSS(1,J)=CSSS(1,J)+WF/B
CSSS(2,J)=CSSS(2,J)+WF*SP(5,L)/B
CSSS(3,J)=CSSS(3,J)+WF*SP(5,L)*PV(2,N)/B
!--this assumes that any flow normal to the x direction is in the y direction
CSSS(4,J)=CSSS(4,J)+WF*SP(5,L)*A/B        
IF (ISPR(1,L) > 0) THEN
  CSSS(5,J)=WF*CSSS(5,J)+PROT(N)/B
  CSSS(6,J)=CSSS(6,J)+WF*ISPR(1,L)/B
END IF
!
XI=XB(J)
DX=DTR*PV(1,N)
DZ=0.D00
IF (IFX > 0) DY=DTR*PV(2,N)
IF (IFX == 2) DZ=DTR*PV(3,N)    
IF (IFX == 0) X=XI+DX
IF (IFX > 0) CALL AIFX(XI,DX,DY,DZ,X,PV(1,N),PV(2,N),PV(3,N)) 
!
RETURN
!
END SUBROUTINE REFLECT
!
!*****************************************************************************
!
SUBROUTINE RBC(XI,DX,DY,DZ,R,S)
!
!--calculates the trajectory fraction S from a point at radius XI with
!----displacements DX, DY, and DZ to a possible intersection with a
!----surface of radius R, IFX=1, 2 for cylindrical, spherical geometry
!
USE MOLECS
USE GAS
USE GEOM
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
REAL(KIND=8) :: A,B,C,XI,DX,DY,DZ,R,S,DD,S1,S2
!
DD=DX*DX+DY*DY
IF (IFX == 2) DD=DD+DZ*DZ
B=XI*DX/DD
C=(XI*XI-R*R)/DD
A=B*B-C
IF (A >= 0.D00) THEN
!--find the least positive solution to the quadratic
  A=DSQRT(A)
  S1=-B+A
  S2=-B-A
  IF (S2 < 0.D00) THEN
    IF (S1 > 0.D00) THEN
      S=S1
    ELSE
      S=2.D00
    END IF
  ELSE IF (S1 < S2) THEN
    S=S1
  ELSE
    S=S2
  END IF
ELSE
  S=2.D00
!--setting S to 2 indicates that there is no intersection
END IF
!
RETURN
!
END SUBROUTINE RBC
!
!*****************************************************************************
!
SUBROUTINE AIFX(XI,DX,DY,DZ,X,U,V,W) 
!
!--calculates the new radius and realigns the velocity components in
!----cylindrical and spherical flows
!
USE MOLECS
USE GAS
USE GEOM
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
!INTEGER :: 
REAL(KIND=8) :: A,B,C,XI,DX,DY,DZ,X,U,V,W,DR,VR,S
!
IF (IFX == 1) THEN
  DR=DY
  VR=V
ELSE IF (IFX == 2) THEN
  DR=DSQRT(DY*DY+DZ*DZ)
  VR=DSQRT(V*V+W*W)
END IF
A=XI+DX
X=DSQRT(A*A+DR*DR)
S=DR/X
C=A/X
B=U
U=B*C+VR*S
V=-B*S+VR*C
IF (IFX == 2) THEN
  VR=V
  CALL RANDOM_NUMBER(RANF)
  A=DPI*RANF
  V=VR*DSIN(A)
  W=VR*DCOS(A)
END IF
!
RETURN
!
END SUBROUTINE AIFX
!
!*****************************************************************************
!
SUBROUTINE REMOVE_MOL(N)
!
!--remove molecule N and replaces it by NM
USE MOLECS
USE CALC
USE GEOM
USE GAS
!
IMPLICIT NONE
!
INTEGER :: N,NC,M,K
 
!--N the molecule number
!--M,K working integer 
!
IF (N /= NM) THEN
  PX(N)=PX(NM)
  DO M=1,3
    PV(M,N)=PV(M,NM)
  END DO
  IF (MMRM > 0) PROT(N)=PROT(NM)
  IPCELL(N)=ABS(IPCELL(NM))
  IPSP(N)=IPSP(NM)
  IPCP(N)=IPCP(NM)
  IF (MMVM > 0) THEN
    DO M=1,MMVM
      IPVIB(M,N)=IPVIB(M,NM)
    END DO
  END IF
  PTIM(N)=PTIM(NM)
END IF
NM=NM-1
!
RETURN
!
END SUBROUTINE REMOVE_MOL 
!
!*****************************************************************************
!
SUBROUTINE INDEX_MOLS
!
!--index the molecules to the collision cells
!
USE MOLECS
USE CALC
USE GEOM
!
IMPLICIT NONE
!
INTEGER :: N,M,K
!
!--N,M,K working integer
!
DO N=1,NCCELLS
  ICCELL(2,N)=0
END DO
!     
IF (NM /= 0) THEN
  DO N=1,NM
    M=IPCELL(N)
    ICCELL(2,M)=ICCELL(2,M)+1
  END DO
!
  M=0
  DO N=1,NCCELLS
    ICCELL(1,N)=M
      M=M+ICCELL(2,N)
    ICCELL(2,N)=0
  END DO
!
  DO N=1,NM
    M=IPCELL(N)
    ICCELL(2,M)=ICCELL(2,M)+1
    K=ICCELL(1,M)+ICCELL(2,M)
    ICREF(K)=N
  END DO
!
END IF
!
RETURN
!
END SUBROUTINE INDEX_MOLS
!
!*****************************************************************************
!
SUBROUTINE LBS(XMA,XMB,ERM)
!
!--selects a Larsen-Borgnakke energy ratio using eqn (11.9)
!
IMPLICIT NONE
!
REAL(KIND=8) :: PROB,ERM,XMA,XMB,RANF
INTEGER :: I,N
!
!--I is an indicator
!--PROB is a probability
!--ERM ratio of rotational to collision energy
!--XMA degrees of freedom under selection-1
!--XMB remaining degrees of freedom-1
!
I=0
DO WHILE (I == 0)
  CALL RANDOM_NUMBER(RANF)
  ERM=RANF
  IF ((XMA < 1.D-6).OR.(XMB < 1.D-6)) THEN
!    IF (XMA < 1.E-6.AND.XMB < 1.E-6) RETURN
!--above can never occur if one mode is translational
    IF (XMA < 1.D-6) PROB=(1.D00-ERM)**XMB
    IF (XMB < 1.D-6) PROB=(1.D00-ERM)**XMA
  ELSE
    PROB=(((XMA+XMB)*ERM/XMA)**XMA)*(((XMA+XMB)*(1.D00-ERM)/XMB)**XMB)
  END IF
  CALL RANDOM_NUMBER(RANF)
  IF (PROB > RANF) I=1
END DO
!
RETURN 
!
END SUBROUTINE LBS
!
!*****************************************************************************
!
SUBROUTINE INITIALISE_SAMPLES
!
!--start a new sample
!
USE CALC
USE GEOM
USE GAS
USE OUTPUT
USE MOLECS
!
IMPLICIT NONE
!
INTEGER :: N
!
NSAMP=0.
TISAMP=FTIME
NMISAMP=NM
COLLS=0.D00 ; WCOLLS=0.D00 ; CLSEP=0.D00
TCOL=0.
!TDISS=0.D00
!TRECOMB=0.D00
!TFOREX=0.D00
!TREVEX=0.D00
TREACG=0
TREACL=0
!TNEX=0.D00
CS=0. ; CSS=0. ; CSSS=0. 
VIBFRAC=0.D00
SUMVIB=0.D00
!REAC=0.
!SREAC=0.
!
END SUBROUTINE INITIALISE_SAMPLES
!
!*****************************************************************************
!
SUBROUTINE SAMPLE_FLOW
!
!--sample the flow properties
!
USE MOLECS
USE CALC
USE GEOM
USE GAS
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: NC,NCC,LS,N,M,K,L,I,KV
REAL(KIND=8) :: A,TE,TT,WF
!
!--NC the sampling cell number
!--NCC the collision cell number
!--LS the species code
!--N,M,K working integers
!--TE total translational energy
!
NSAMP=NSAMP+1
WRITE (*,*) 'Sample',NSAMP
WRITE (9,*) NM,'Mols. at sample',NSAMP
!
DO L=1,MSP
  IF (ISPV(L) > 0) THEN
    DO KV=1,ISPV(L)
      DO N=1,NM
        IF (IPSP(N) == L) THEN
          SUMVIB(L,KV)=SUMVIB(L,KV)+1.D00
          I=IPVIB(KV,N)
          VIBFRAC(L,KV,I)=VIBFRAC(L,KV,I)+1.
        END IF  
      END DO
    END DO
  END IF
END DO    
!
DO N=1,NM
!--set EMOLS
  NCC=IPCELL(N) 
  NC=ICCELL(3,NCC)
  WF=1.D00
  IF (IWF == 1) WF=1.D00+WFM*PX(N)**IFX
  IF ((NC > 0).AND.(NC <= NCELLS)) THEN
    IF (MSP > 1) THEN
      LS=ABS(IPSP(N))
    ELSE
      LS=1
    END IF
    CS(0,NC,LS)=CS(0,NC,LS)+1.D00
    CS(1,NC,LS)=CS(1,NC,LS)+WF     
	DO M=1,3
      CS(M+1,NC,LS)=CS(M+1,NC,LS)+WF*PV(M,N)
      CS(M+4,NC,LS)=CS(M+4,NC,LS)+WF*PV(M,N)**2
    END DO
    IF (MMRM > 0) CS(8,NC,LS)=CS(8,NC,LS)+WF*PROT(N)
    IF (MMVM > 0) THEN
      IF (ISPV(LS).GT.0) THEN
        DO K=1,ISPV(LS)
          CS(K+8,NC,LS)=CS(K+8,NC,LS)+WF*DFLOAT(IPVIB(K,N))*BOLTZ*SPVM(1,K,LS)
        END DO
      END IF
    END IF
  ELSE
    WRITE (*,*) 'Illegal sampling cell',NC,NCC,' for MOL',N,' at',PX(N)
!    STOP
  END IF
END DO
!
TSAMP=TSAMP+DTSAMP
!
RETURN
!
END SUBROUTINE SAMPLE_FLOW
!
!*****************************************************************************
SUBROUTINE OUTPUT_RESULTS
!
!--calculate the surface and flowfield properties
!--generate TECPLOT files for displaying these properties
!--calculate collisiion rates and flow transit times and reset time intervals
!--add molecules to any flow plane molecule output files
!
USE MOLECS
USE CALC
USE GEOM
USE GAS
USE OUTPUT
! 
IMPLICIT NONE
!
INTEGER :: IJ,J,JJ,K,L,M,N,NN,NMCR,CTIME
INTEGER(KIND=8) :: NNN
REAL :: AO,BO,CO,AS,AT
REAL(KIND=8) :: A,B,C,SDTM,SMCR,DOF,AVW,UU,SVDF,VDOFM,TVIBM,VEL,DTMI,TT,EVIBM
REAL(KIND=8), DIMENSION(0:12) :: SUM 
REAL(KIND=8), DIMENSION(0:8,2) :: SUMS
REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: TVIB,VDOF,PPA
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: TV,THCOL
REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:) :: DF
INTEGER, ALLOCATABLE, DIMENSION(:) :: NMS
!INTEGER, ALLOCATABLE, DIMENSION(:,:) :: 
CHARACTER (LEN=11) :: F
CHARACTER (LEN=4) :: E 
!
!--CTIME  computer time (microseconds)
!--SUMS(N,L) sum over species of CSS(N,J,L,M) for surface properties
! 
!--For flowfield properties,where <> indicates sampled sum
!--SUM(0) the molecular number sum over all species
!--SUM(1) the weighted number sum over all species
!--SUM(2) the weighted sum of molecular masses 
!--SUM(3),(4),(5) the weighted sum over species of m*<u>,<v>,<w>
!--SUM(6) the weighted sum over species of m*(<u**2>+<v**2>+<w**2>)
!--SUM(7) the weighted sum over species of <u**2>+<v**2>+<w**2>
!--SUM(8) the weighted sum of rotational energy
!--SUM(9) the weighted sum of rotational degrees of freedom
!--SUM(10) the weighted sum over species of m*<u**2>
!--SUM(11) the weighted sum over species of m*<v**2>
!--SUM(12) sum over species of m*<w**2>
!--UU velocity squared
!--DOF degrees of freedom
!--AVW the average value of the viscosity-temperature exponent
!--DVEL velocity difference
!--TVEL thermal speed
!--SMCR sum of mcs/mfp over cells
!--NMCR number in the sum
!--VDOFM effective vibrational degrees of freedom of mixture
!--TVIB(L)
!--VDOF(L)
!--TV(K,L) the temperature of vibrational mode K of species L
!--SVDF sum of vibrational degrees of freedom
!--PPA particles per atom
!--NMS number per species
!
!
!--calculate the flowfield properties in the cells
!
ALLOCATE (TV(MMVM,MSP),TVIB(MSP),DF(NCELLS,MMVM,MSP),VDOF(MSP),PPA(MSP),NMS(MSP),THCOL(MSP,MSP),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT ALLOCATE OUTPUT VARIABLES ',ERROR
!  STOP
END IF 
!
NOUT=NOUT+1
WRITE (*,*) 'Generating files for output interval',NOUT
!
IF (ISF == 0) THEN
  OPEN (3,FILE='DS1OUT.DAT')
ELSE
  !--the files are DS1n.DAT, where n is a four digit integer equal to NOUT 
  CALL NUMCHAR4 (NOUT,E)
  F='DS1'//E//'.OUT'
  OPEN (3,FILE=F)
END IF  
!
VAR=0.D00
IF (IFX == 0) A=FNUM/(FTIME-TISAMP)   !--flow X-section area = unity for 1-D flow
DO JJ=1,2
IF (IFX == 1) A=FNUM/(2.D00*PI*XB(JJ)*(FTIME-TISAMP))
IF (IFX == 2) A=FNUM/(4.D00*PI*XB(JJ)*XB(JJ)*(FTIME-TISAMP))
!--JJ=1 for surface at XB(1), JJ=2 for surface at XB(2)
  IF (ITYPE(JJ) == 2) THEN
    SUMS=0.D00
    DO L=1,MSP
      DO J=0,8
        DO IJ=1,2
          SUMS(J,IJ)=SUMS(J,IJ)+CSS(J,JJ,L,IJ)
        END DO
      END DO
    END DO  
!
    VARS(0,JJ)=SUMS(0,1)
    VARS(1,JJ)=SUMS(1,1)
    VARS(2,JJ)=SUMS(1,2)
    VARS(3,JJ)=SUMS(1,1)*A
    VARS(4,JJ)=SUMS(1,2)*A
    VARS(5,JJ)=SUMS(2,1)*A
    VARS(6,JJ)=SUMS(2,2)*A 
    VARS(7,JJ)=SUMS(3,1)*A
    VARS(8,JJ)=SUMS(3,2)*A
    VARS(9,JJ)=SUMS(4,1)*A
    VARS(10,JJ)=SUMS(4,2)*A
    VARS(11,JJ)=SUMS(5,1)*A
    VARS(12,JJ)=SUMS(5,2)*A
    VARS(13,JJ)=SUMS(6,1)*A
    VARS(14,JJ)=SUMS(6,2)*A
    VARS(15,JJ)=SUMS(7,1)*A
    VARS(16,JJ)=SUMS(7,2)*A 
    VARS(17,JJ)=SUMS(8,1)*A
    VARS(18,JJ)=SUMS(8,2)*A
    IF (CSSS(1,JJ) > 1.D-6) THEN
      VARS(19,JJ)=CSSS(3,JJ)/CSSS(2,JJ)
      VARS(20,JJ)=(CSSS(4,JJ)-CSSS(2,JJ)*VARS(19,JJ)*VARS(19,JJ))/(CSSS(1,JJ)*3.D00*BOLTZ)-TSURF(JJ)
      VARS(19,JJ)=VARS(19,JJ)-VSURF(JJ)
      IF (CSSS(6,JJ) > 0.D00) THEN
        VARS(21,JJ)=(2.D000/BOLTZ)*(CSSS(5,JJ)/CSSS(6,JJ))-TSURF(JJ)
      ELSE
        VARS(21,JJ)=0.D00
      END IF    
    ELSE
      VARS(19,JJ)=0.D00
      VARS(20,JJ)=0.D00
      VARS(21,JJ)=0.D00
    END IF
    VARS(22,JJ)=(SUMS(2,1)+SUMS(2,2))*A
    VARS(23,JJ)=(SUMS(3,1)+SUMS(3,2))*A
    VARS(24,JJ)=(SUMS(4,1)+SUMS(4,2))*A
    VARS(25,JJ)=(SUMS(5,1)+SUMS(5,2))*A
    VARS(26,JJ)=(SUMS(6,1)+SUMS(6,2))*A
    VARS(27,JJ)=(SUMS(7,1)+SUMS(7,2))*A
    VARS(28,JJ)=(SUMS(8,1)+SUMS(8,2))*A
    VARS(29,JJ)=VARS(11,JJ)+VARS(13,JJ)+VARS(15,JJ)
    VARS(30,JJ)=VARS(12,JJ)+VARS(14,JJ)+VARS(16,JJ)
    VARS(31,JJ)=VARS(29,JJ)+VARS(30,JJ)
    DO L=1,MSP
      IF (SUMS(1,1) > 0) THEN
        VARS(32+L,JJ)=100.*CSS(1,JJ,L,1)/SUMS(1,1)
      ELSE
        VARS(32+L,JJ)=0.
      END IF
    END DO
  END IF
END DO 
!
VARSP=0.
SMCR=0
NMCR=0
DO N=1,NCELLS
  A=FNUM/(CELL(4,N)*NSAMP)
  IF (IVB == 1) A=A*((XB(2)-XB(1))/(XB(2)+VELOB*0.5D00*(FTIME+TISAMP)-XB(1)))**(IFX+1)
!--check the above for non-zero XB(1) 
  SUM=0.
  NMCR=NMCR+1
  DO L=1,MSP
    SUM(0)=SUM(0)+CS(0,N,L)
    SUM(1)=SUM(1)+CS(1,N,L)
    SUM(2)=SUM(2)+SP(5,L)*CS(1,N,L)
    DO K=1,3
      SUM(K+2)=SUM(K+2)+SP(5,L)*CS(K+1,N,L)
      IF (CS(1,N,L) > 0.1D00) THEN
        VARSP(K+1,N,L)=CS(K+4,N,L)/CS(1,N,L)
!--VARSP(2,3,4 are temporarily the mean of the squares of the velocities
        VARSP(K+8,N,L)=CS(K+1,N,L)/CS(1,N,L)
!--VARSP(9,10,11 are temporarily the mean of the velocities
      END IF
    END DO
    SUM(6)=SUM(6)+SP(5,L)*(CS(5,N,L)+CS(6,N,L)+CS(7,N,L))
    SUM(10)=SUM(10)+SP(5,L)*CS(5,N,L)
    SUM(11)=SUM(11)+SP(5,L)*CS(6,N,L)
    SUM(12)=SUM(12)+SP(5,L)*CS(7,N,L)
    IF (CS(1,N,L) > 0.5D00) THEN
      SUM(7)=SUM(7)+CS(5,N,L)+CS(6,N,L)+CS(7,N,L)
    END IF
    IF (ISPR(1,L) > 0) THEN
      SUM(8)=SUM(8)+CS(8,N,L)
      SUM(9)=SUM(9)+CS(1,N,L)*ISPR(1,L)
    END IF
  END DO
  AVW=0.
  DO L=1,MSP
    VARSP(0,N,L)=CS(1,N,L)
    VARSP(1,N,L)=0.D00
    VARSP(6,N,L)=0.
    VARSP(7,N,L)=0.
    VARSP(8,N,L)=0.
    IF (SUM(1) > 0.1) THEN
      VARSP(1,N,L)=100.*CS(1,N,L)/SUM(1)
      AVW=AVW+SP(3,L)*CS(1,N,L)/SUM(1)
      IF ((ISPR(1,L) > 0).AND.(CS(1,N,L) > 0.5)) VARSP(6,N,L)=(2.D00/BOLTZ)*CS(8,N,L)/(ISPR(1,L)*CS(1,N,L))
    END IF
    VARSP(5,N,L)=0.
    DO K=1,3
      VARSP(K+1,N,L)=(SP(5,L)/BOLTZ)*(VARSP(K+1,N,L)-VARSP(K+8,N,L)**2)
      VARSP(5,N,L)=VARSP(5,N,L)+VARSP(K+1,N,L)
    END DO
    VARSP(5,N,L)=VARSP(5,N,L)/3.      
  END DO
!
  IF (IVB == 0) VAR(1,N)=CELL(1,N)
  IF (IVB == 1) THEN
    C=(XB(2)+VELOB*FTIME-XB(1))/DFLOAT(NDIV)      !--new DDIV
    VAR(1,N)=XB(1)+(DFLOAT(N-1)+0.5)*C
  END IF
  VAR(2,N)=SUM(0)
  IF (SUM(1) > 0.5) THEN
    VAR(3,N)=SUM(1)*A    !--number density 
    VAR(4,N)=VAR(3,N)*SUM(2)/SUM(1)   !--density
    VAR(5,N)=SUM(3)/SUM(2)    !--u velocity component
    VAR(6,N)=SUM(4)/SUM(2)    !--v velocity component
    VAR(7,N)=SUM(5)/SUM(2)    !--w velocity component
    UU= VAR(5,N)**2+VAR(6,N)**2+VAR(7,N)**2
    IF (SUM(1) > 1) THEN
      VAR(8,N)=(ABS(SUM(6)-SUM(2)*UU))/(3.D00*BOLTZ*SUM(1))
!--translational temperature
      VAR(19,N)=(ABS(SUM(10)-SUM(2)*VAR(5,N)**2))/(BOLTZ*SUM(1))
      VAR(20,N)=(ABS(SUM(11)-SUM(2)*VAR(6,N)**2))/(BOLTZ*SUM(1))
      VAR(21,N)=(ABS(SUM(12)-SUM(2)*VAR(7,N)**2))/(BOLTZ*SUM(1))
    ELSE
      VAR(8,N)=1.
      VAR(19,N)=1.
	  VAR(20,N)=1.
	  VAR(21,N)=1.
    END IF
!
    IF (SUM(9) > 0.01D00) THEN
      VAR(9,N)=(2.D00/BOLTZ)*SUM(8)/SUM(9)    !--rotational temperature
    ELSE
      VAR(9,N)=0.
    END IF
    VAR(10,N)=FTMP(1)  !vibration default
    DOF=(3.D00+SUM(9)/SUM(1))
    VAR(11,N)=(3.*VAR(8,N)+(SUM(9)/SUM(1))*VAR(9,N))/DOF
!--overall temperature
    VAR(18,N)=VAR(3,N)*BOLTZ*VAR(8,N)
!--scalar pressure (now (from V3) based on the translational temperature)
    IF (MMVM > 0) THEN
      DO L=1,MSP
        SVDF=0.
!--SVDF is the sum of the degrees of freedom of species L 
        VDOF(L)=0.
        IF (ISPV(L).GT.0) THEN
          DO K=1,ISPV(L)
            IF (CS(K+8,N,L) < BOLTZ) THEN
              TV(K,L)=0.
              DF(N,K,L)=0.
            ELSE
              TV(K,L)=SPVM(1,K,L)/LOG(1.+BOLTZ*SPVM(1,K,L)*CS(1,N,L)/CS(K+8,N,L))
              DF(N,K,L)=2.*(SPVM(1,K,L)/TV(K,L))/(EXP(SPVM(1,K,L)/TV(K,L))-1.)
            END IF
            VDOF(L)=VDOF(L)+DF(N,K,L)
            SVDF=SVDF+DF(N,K,L)
          END DO
          TVIB(L)=0.
          DO K=1,ISPV(L)
            IF (SVDF > 1.D-6) THEN
              TVIB(L)=TVIB(L)+TV(K,L)*DF(N,K,L)/SVDF
            ELSE
              TVIB(L)=FVTMP(1)
            END IF  
          END DO  
        ELSE
          TVIB(L)=TREF
          VDOF(L)=0.
        END IF
        VARSP(7,N,L)=TVIB(L)
        VARSP(8,N,L)=VDOF(L)  !temporary assignment
      END DO
      VDOFM=0.
      EVIBM=0.
      DO L=1,MSP
        VDOFM=VDOFM+VDOF(L)*CS(1,N,L)/SUM(1)
        EVIBM=EVIBM+0.5D00*BOLTZ*VDOF(L)*TVIB(L)*CS(1,N,L)/SUM(1)
      END DO
      IF (VDOFM > 0.001) THEN    
        TVIBM=(2.D00/BOLTZ)*EVIBM/VDOFM
      ELSE
        TVIBM=FTMP(1)
      END IF    
      VAR(10,N)=TVIBM
      DOF=3.D00+SUM(9)/SUM(1)+VDOFM
!--DOF is the number of degrees of freedom
      VAR(11,N)=(3.D00*VAR(8,N)+(SUM(9)/SUM(1))*VAR(9,N)+VDOFM*TVIBM)/DOF
!--the overall temperature now includes vibration, see eqn (11.12)
    END IF
    DO L=1,MSP
!--set the overall temperature for the individual species
      VARSP(8,N,L)=(3.*VARSP(5,N,L)+ISPR(1,L)*VARSP(6,N,L)+     &
                   VARSP(8,N,L)*VARSP(7,N,L))/(3.+ISPR(1,L)+VARSP(8,N,L))
!--convert the species velocity components to diffusion velocities
      DO K=1,3 
        VARSP(K+8,N,L)=VARSP(K+8,N,L)-VAR(K+4,N) 
      END DO
    END DO
! 
    VEL=DSQRT(VAR(5,N)**2+VAR(6,N)**2+VAR(7,N)**2)
    VAR(12,N)=VEL/SQRT((DOF+2.D00)*VAR(11,N)*(SUM(1)*BOLTZ/SUM(2))/DOF)
!--Mach number
    VAR(13,N)=SUM(0)/NSAMP  !--average number of molecules in cell
    IF (COLLS(N).GT.2.) THEN
      VAR(14,N)=0.5D00*(FTIME-TISAMP)*(SUM(1)/NSAMP)/WCOLLS(N)
!--mean collision time
      VAR(15,N)=0.92132D00*DSQRT(DABS(SUM(7)/SUM(1)-UU))*VAR(14,N)
!--mean free path (based on r.m.s speed with correction factor based on equilibrium)
      VAR(16,N)=CLSEP(N)/(COLLS(N)*VAR(15,N))
    ELSE
      VAR(14,N)=1.D10
      VAR(15,N)=1.D10/VAR(3,N)
!--m.f.p set by nominal values        
    END IF
!
  ELSE
    DO L=3,19
      VAR(L,N)=0.
    END DO
  END IF
  VAR(17,N)=VEL
END DO
!
!
IF (IFX == 0) WRITE (3,*) 'DSMC program DS1 for a one-dimensional plane flow'
IF (IFX == 1) WRITE (3,*) 'DSMC program DS1 for a cylindrical flow'
IF (IFX == 2) WRITE (3,*) 'DSMC program DS1 for a spherical flow'
!
WRITE (3,*)
WRITE (3,*) 'Interval',NOUT,'Time ',FTIME, ' with',NSAMP,' samples from',TISAMP
!990 FORMAT(I7,G13.5,I7,G13.5)
NNN=DINT(TOTMOV)
WRITE (*,*) 'TOTAL MOLECULES = ',NM
NMS=0
DO N=1,NM
  M=IPSP(N)
  NMS(M)=NMS(M)+1
END DO
WRITE (3,*) 'Total simulated molecules =',NM
DO N=1,MSP
  WRITE (*,*) 'SPECIES ',N,' TOTAL = ',NMS(N)
  WRITE (3,*) 'Species ',N,' total = ',NMS(N)
END DO
!
WRITE (3,*) 'Total molecule moves   =',NNN
NNN=DINT(TOTCOL)
WRITE (3,*) 'Total collision events =',NNN
NNN=DINT(TOTDUP)
WRITE (3,*) 'Total duplicate collisions =',NNN
!
WRITE (3,*) 'Species dependent collision numbers in current sample'
DO N=1,MSP
  IF (GASCODE /= 8) WRITE (3,901) (TCOL(N,M),M=1,MSP)
  IF (GASCODE == 8) WRITE (3,902) (TCOL(N,M),M=1,MSP)
END DO 
901 FORMAT(5G13.5) 
902 FORMAT(8G13.5)
!
CTIME=MCLOCK()
WRITE (3,*) 'Computation time',FLOAT(CTIME)/1000.,'seconds'
WRITE (3,*) 'Collision events per second',(TOTCOL-TOTCOLI)*1000.D00/DFLOAT(CTIME)
WRITE (3,*) 'Molecule moves per second',(TOTMOV-TOTMOVI)*1000.D00/DFLOAT(CTIME)
!
WRITE (3,*)
IF ((ITYPE(1) == 2).OR.(ITYPE(2) == 2)) WRITE (3,*) 'Surface quantities'
DO JJ=1,2
  IF (ITYPE(JJ) == 2) THEN
    WRITE (3,*)
    WRITE (3,*) 'Surface at',XB(JJ)
    WRITE (3,*) 'Incident sample',VARS(0,JJ) 
    WRITE (3,*) 'Number flux',VARS(3,JJ),' /sq m/s'
    WRITE (3,*) 'Inc pressure',VARS(5,JJ),' Refl pressure',VARS(6,JJ)
    WRITE (3,*) 'Pressure', VARS(5,JJ)+VARS(6,JJ),' N/sq m'
    WRITE (3,*) 'Inc y shear',VARS(7,JJ),' Refl y shear',VARS(8,JJ)
    WRITE (3,*) 'Net y shear',VARS(7,JJ)-VARS(8,JJ),' N/sq m'
    WRITE (3,*) 'Net z shear',VARS(9,JJ)-VARS(10,JJ),' N/sq m'
    WRITE (3,*) 'Incident translational heat flux',VARS(11,JJ),' W/sq m'
    IF (MMRM > 0) WRITE (3,*) 'Incident rotational heat flux',VARS(13,JJ),' W/sq m'
    IF (MMVM > 0) WRITE (3,*) 'Incident vibrational heat flux',VARS(15,JJ),' W/sq m'
    WRITE (3,*) 'Total incident heat flux',VARS(29,JJ),' W/sq m'
    WRITE (3,*) 'Reflected translational heat flux',VARS(12,JJ),' W/sq m'
    IF (MMRM > 0) WRITE (3,*) 'Reflected rotational heat flux',VARS(14,JJ),' W/sq m'
    IF (MMVM > 0) WRITE (3,*) 'Reflected vibrational heat flux',VARS(16,JJ),' W/sq m'
    WRITE (3,*) 'Total reflected heat flux',VARS(30,JJ),' W/sq m'
    WRITE (3,*) 'Net heat flux',VARS(31,JJ),' W/sq m'
    WRITE (3,*) 'Slip velocity (y direction)',VARS(19,JJ),' m/s'
    WRITE (3,*) 'Translational temperature slip',VARS(20,JJ),' K'
    IF (MMRM > 0) WRITE (3,*) 'Rotational temperature slip',VARS(21,JJ),' K'
    IF (MSP > 1) THEN
      DO L=1,MSP
        WRITE (3,*) 'Species',L,' percentage',VARS(L+32,JJ)
      END DO
    END IF
  END IF
END DO 
!
WRITE (*,*)
!
WRITE (3,*)
PPA=0
DO N=1,NCELLS
  DO M=1,MSP
    PPA(M)=PPA(M)+VARSP(0,N,M)
  END DO
END DO
!
WRITE (*,*)
IF ((JCD == 1).AND.(MSP > 1)) THEN
  WRITE (3,*) 'New integrated DSMC chemistry model with no experimentally based rates'
  WRITE (3,*)
  WRITE(3,*) 'GAINS FROM REACTIONS'
  WRITE(3,*) '                          Dissoc.     Recomb. Endo. Exch.  Exo. Exch.'
  DO M=1,MSP
  WRITE (3,*) ' SPECIES',M,TREACG(1,M),TREACG(2,M),TREACG(3,M),TREACG(4,M)   
  END DO
  WRITE (3,*) 
  WRITE(3,*) 'LOSSES FROM REACTIONS'
  WRITE(3,*) '                          Dissoc.     Recomb. Endo. Exch.  Exo. Exch.'
  DO M=1,MSP
  WRITE (3,*) ' SPECIES',M,TREACL(1,M),TREACL(2,M),TREACL(3,M),TREACL(4,M)
  END DO
  WRITE (3,*)
  WRITE (3,*) 'TOTALS'
  DO M=1,MSP
    WRITE (3,*) ' SPECIES',M,' GAINS',TREACG(1,M)+TREACG(2,M)+TREACG(3,M)+TREACG(4,M),' LOSSES',TREACL(1,M)+TREACL(2,M)+TREACL(3,M)+TREACL(4,M)  
  END DO
END IF
IF ((JCD == 0).AND.(MSP > 1)) THEN
  WRITE (3,*) 'Traditional DSMC chemistry model based on rate equations'
  DO M=1,MNRE
    WRITE (3,*) 'Reaction',M,' number',REAC(M)
  END DO 
END IF 
!
WRITE (3,*)
WRITE (3,*) 'Flowfield properties '
WRITE (3,*) NSAMP,' Samples'
WRITE (3,*) 'Overall gas'
WRITE (3,998) 'Cell x coord.      Sample       Number Dens. Density      u velocity   v velocity   w velocity   Trans. Temp. Rot. Temp.   Vib. Temp.   Temperature  Mach no.     Mols/cell    m.c.t        m.f.p        mcs/mfp        speed      Pressure      TTX         TTY          TTZ   Species Fractions '
DO N=1,NCELLS
  WRITE (3,999) N,(VAR(M,N),M=1,21),(VARSP(1,N,L),L=1,MSP)
END DO 
WRITE (3,*) 'Individual molecular species'
DO L=1,MSP
  WRITE (3,*) 'Species',L
  WRITE (3,997) 'Cell x coord.      Sample       Percentage   Species TTx   Species TTy  Species TTz  Trans. Temp.  Rot. Temp.   Vib. Temp.   Spec. Temp  u Diff. Vel. v Diff. Vel. w. Diff Vel.' 
  DO N=1,NCELLS
    WRITE (3,999) N,VAR(1,N),(VARSP(M,N,L),M=0,11)
  END DO
END DO
!  
999 FORMAT (I5,30G13.5)
998 FORMAT (G270.0)
997 FORMAT (G175.0)
!
IF (MMVM > 0) THEN
!  CALL CHECK_VIB_DIST 
!  IF (JCD == 1) CALL CHECK_DISS_DIST
END IF 
!
CLOSE (3)
!
IF (ISF == 1) THEN       !--the "unsteady sampling" option has been chosen
  CALL INITIALISE_SAMPLES
!--write a special output file for vibrational temperature and temperature versus collision number   
!  OPEN (10,FILE='RELAX.DAT',ACCESS='APPEND')
!  AO=2.*TOTCOL/NM
!  BO=0.
!  CO=0.
!  DO N=1,NCELLS
!    BO=BO+VAR(11,N)
!    CO=CO+VAR(10,N)
!  END DO  
!  WRITE (10,*) AO,BO/NCELLS,CO/NCELLS
!  CLOSE (10)
END IF
!
IF ((GASCODE == 8).OR.(GASCODE == 6)) THEN
!--Write a special output file for the composition of a reacting gas as a function of time
  OPEN (10,FILE='COMPOSITION.DAT',ACCESS='APPEND')
  AS=NM
  AT=FTIME*1.E6
  IF (GASCODE == 8) WRITE (10,888) AT,NMS(1)/AS,NMS(2)/AS,NMS(3)/AS,NMS(4)/AS,NMS(5)/AS,NMS(6)/AS,NMS(7)/AS,NMS(8)/AS,VAR(11,1)
  IF (GASCODE == 6) WRITE (10,888) AT,NMS(1)/AS,NMS(2)/AS,NMS(3)/AS,NMS(4)/AS,NMS(5)/AS,VAR(11,1)
888 FORMAT(10G13.5)
  CLOSE (10)
END IF
!
WRITE (*,*) 'Output files written'
!
!--reset collision and transit times etc.
!
DTMI=DTM
DTM=DTM*2.
!--this makes it possible for DTM to increase, it will be reduced as necessary
DO N=1,NCCELLS
!
  NN=ICCELL(3,N)
  B=(CELL(3,NN)-CELL(2,NN))/DFLOAT(NCIS)     !--collision cell width
!
  IF ((NN > 0).AND.(NN <= NCELLS)) THEN
!
    IF (VAR(13,NN) > 20.D00) THEN
!--consider the local collision rate    
      CCELL(3,N)=0.5D00*VAR(14,NN)*CPDTM
!--look also at collision cell transit time based on the local flow speed
      A=0.5D00*(B/(ABS(VAR(5,NN))))*TPDTM
      IF (A < CCELL(3,N)) CCELL(3,N)=A
      IF (2.D00*CCELL(3,N) < DTM) DTM=2.D00*CCELL(3,N)
    ELSE
!-- base the time step on a collision cell transit time at the refence vmp
      A=TPDTM*B/VMPM 
      IF (A < CCELL(3,N)) CCELL(3,N)=A
    END IF 
    IF (1.9D00*CCELL(3,N) < DTM) DTM=1.9D00*CCELL(3,N) 
  END IF
END DO
!
DTSAMP=DTSAMP*DTM/DTMI
DTOUT=DTOUT*DTM/DTMI
!
WRITE (9,*) 'DTM changes  from',DTMI,' to',DTM
!
!--write TECPLOT data files
!
OPEN (7,FILE='PROFILE.DAT',FORM='FORMATTED')
!
995 FORMAT (22G13.5)
DO N=1,NCELLS
  WRITE (7,995) VAR(1,N),VAR(3,N),VAR(11,N),VAR(18,N),VAR(5,N),VAR(12,N),(0.01*VARSP(1,N,M),M=1,MSP)
END DO
CLOSE(7)
!
!--deallocate local variables
!
DEALLOCATE (TV,TVIB,VDOF,THCOL,STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT DEALLOCATE OUTPUT VARIABLES',ERROR
END IF
!
TOUT=TOUT+DTOUT
!
RETURN
!
END SUBROUTINE OUTPUT_RESULTS 
! 
!**************************************************************************************
!
SUBROUTINE NUMCHAR4(NNN,E)
!
!--produces the character equivalent E of a 4 digit integer NNN 
!
CHARACTER(LEN=1) :: A
CHARACTER(LEN=1) :: B
CHARACTER(LEN=1) :: C
CHARACTER(LEN=1) :: D
CHARACTER(LEN=4) :: E
A='0' ; B='0' ; C='0' ; D='0'
N=NNN
IF (N.GT.999) THEN
  L=N/1000
  A=CHAR(48+L)
  N=N-1000*L
END IF
IF (N.GT.99) THEN
  L=N/100
  B=CHAR(48+L)
  N=N-100*L
END IF
IF (N.GT.9) THEN
  L=N/10
  C=CHAR(48+L)
  N=N-10*L
END IF
D=CHAR(48+N)
E=A//B//C//D
!
RETURN
!
END SUBROUTINE NUMCHAR4
! 
!*****************************************************************************
!
SUBROUTINE DISSOCIATION
!
!--dissociate diatomic molecules that have been marked for dissociation by -ve level or -99999 for ground state
!
USE MOLECS
USE GAS
USE CALC
!
IMPLICIT NONE
!
INTEGER ::K,KK,L,N,M,LS,MS,KV,IDISS 
REAL(KIND=8) :: A,B,C,EA,VRR,VR,RMM,RML
REAL(KIND=8), DIMENSION(3) :: VRC,VCM,VRCP
!
N=0
DO WHILE (N < NM)
  N=N+1
  IDISS=0
  L=IPSP(N)
  IF (ISPV(L) > 0) THEN
    DO K=1,ISPV(L)
      M=IPVIB(K,N)     
      IF (M < 0) THEN   
!--dissociation
        M=-M
        IF (M == 99999) M=0    !--ground state 
        IPVIB(K,N)=M
        TDISS=TDISS+1.D00
        IDISS=1
      END IF
    END DO    
    IF (IDISS == 1) THEN
      EA=PROT(N)    !--EA is energy available for relative translational motion of atoms      
      IF (ISPV(L) >0) THEN
        DO K=1,ISPV(L)    
          EA=EA+IPVIB(K,N)*SPVM(1,K,L)*BOLTZ
        END DO
      END IF
      IF (NM >= MNM) CALL EXTEND_MNM(1.1)
      NM=NM+1     
!--set center of mass velocity as that of molecule        
      VCM(1)=PV(1,N)
      VCM(2)=PV(2,N)
      VCM(3)=PV(3,N)
      PX(NM)=PX(N)
      IPCELL(NM)=IPCELL(N)
      LS=IPSP(N)
      TREACL(1,LS)=TREACL(1,LS)-1
      IPSP(NM)=ISPVM(1,1,L)
      MS=IPSP(NM)
      IPSP(N)=ISPVM(2,1,L)
      LS=IPSP(N)
      TREACG(1,LS)=TREACG(1,LS)+1
      TREACG(1,MS)=TREACG(1,MS)+1
      PTIM(NM)=PTIM(N)
      VRR=2.D00*EA/SPM(1,LS,MS)
      VR=DSQRT(VRR)
      RML=SPM(1,LS,MS)/SP(5,MS)
      RMM=SPM(1,LS,MS)/SP(5,LS)
      CALL RANDOM_NUMBER(RANF)
      B=2.D00*RANF-1.D00 
      A=DSQRT(1.D00-B*B)
      VRCP(1)=B*VR
      CALL RANDOM_NUMBER(RANF)
      C=2.D00*PI*RANF
      VRCP(2)=A*COS(C)*VR
      VRCP(3)=A*SIN(C)*VR
      DO KK=1,3
        PV(KK,N)=VCM(KK)+RMM*VRCP(KK)
        PV(KK,NM)=VCM(KK)-RML*VRCP(KK)
      END DO
!--set any internal modes to the ground state        
      IF (ISPV(LS) > 0) THEN
        DO KV=1,ISPV(LS)
          IPVIB(KV,N)=0
        END DO
      END IF
      IF (ISPR(1,LS) > 0) PROT(N)=0.
      IF (ISPV(MS) > 0) THEN
        DO KV=1,ISPV(MS)
          IPVIB(KV,NM)=0
        END DO
      END IF
      IF (ISPR(1,MS) > 0) PROT(NM)=0.
    END IF
  END IF
END DO
!
RETURN
!
END SUBROUTINE DISSOCIATION
!
!************************************************************************************
!
SUBROUTINE CHECK_VIB_DIST
!
!--CHECK THE VIBRATIONAL DISTRIBUTION FUNCTION
!
!
USE MOLECS
USE GAS
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: L,N,I,M,KV
REAL :: A,B,C,T
!
DO L=1,MSP
  IF (ISPV(L) > 0) THEN
    DO KV=1,ISPV(L)
      WRITE (3,*) 'Vibrational distribution in species',L,'mode',KV,'total sample',SUMVIB(L,KV)
      WRITE (3,*)
      WRITE (3,*) 'LEVEL       SAMPLE    ACTUAL FRACTION   BOLTZMANN FRACTION'
!    M=SPVM(4,1,L)/SPVM(1,1,L)+5
      M=50
      T=VARSP(7,KV,L)
      C=1.-EXP(-SPVM(1,KV,L)/T)
      DO N=0,M
        A=VIBFRAC(L,KV,N)/SUMVIB(L,KV)
        B=C*EXP(-N*SPVM(1,KV,L)/T)
        WRITE (3,*) N,VIBFRAC(L,KV,N),A,B
      END DO
    END DO
  END IF
END DO
!
RETURN
!
END SUBROUTINE CHECK_VIB_DIST
!
!************************************************************************************
!
SUBROUTINE CHECK_REACTION(IKA,N,L,M,LS,MS,VRR,VR,VCM,RML,RMM)
!
!--Implement the old collision model (JCD = 0)
!
!
USE MOLECS
USE GAS
USE CALC
USE OUTPUT
USE GEOM
!
IMPLICIT NONE
!
!
INTEGER :: J,K,L,M,N,LS,MS,IKA,NRE,KK,KS,MK,KA,KAA,MKK,JR,KR,JS,KV,IA
REAL(KIND=8) :: A,B,ECT,ECR,ECV,EC,STERT,THBCELL,WF,PSTERT,VR,VRR,RML,RMM,ECM,ECN,AA,BB
REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: STER,ECA
INTEGER, ALLOCATABLE, DIMENSION(:) :: ISTE
REAL(KIND=8), DIMENSION(3) :: VRC,VCM

!
!--A,B,C working variables
!--J,K,KK,MK,KA,KAA,MKK,JR,KR,JS,KV,IA working integers
!--N the cell
!--L,S  the molecule numbers
!--LS,MS,KS species 
!--IKA reaction indicator
!      0 for no reaction
!      1 for reaction 
!--ECR rotational energy
!--ECV vibrational energy
!--ECA available energy
!--STER steric factor
!--ISTE third body species
!--NRE number of reactions that may occur for this pair
!--STERT cumulative steric factor
!--THBCELL number of third body molecules in cell
!--WF weighting factor
!--PSTERT probability
!--VRC relative velocity components
!--VCM center-of-mass velocity components
!--VR relative speed
!--VRR square of rel speed
!--RML,RMM molecule mass parameters
ALLOCATE (STER(MNRE),ISTE(MNRE),ECA(MNRE),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT ALLOCATE STERIC FACTORS',ERROR
!  STOP
END IF
IKA=0
!--IKA becomes 1 if a reaction occurs
!--consider reactions based on the continuum rate equations
!
NRE=NRSP(LS,MS)
IF (NRE > 0) THEN
  ECT=0.5D00*SPM(1,LS,MS)*VRR
  ECR=0.D00
  ECV=0.D00
  ECR=ECR+PROT(L)
  ECR=ECR+PROT(M)
  IF (MMVM > 0) THEN
    IF (ISPV(LS) > 0) THEN
      DO K=1,ISPV(LS)
        ECV=ECV+DFLOAT(IPVIB(K,L))*BOLTZ*SPVM(1,K,LS)
      END DO
    END IF            
    IF (ISPV(MS) > 0) THEN
      DO K=1,ISPV(MS)
        ECV=ECV+DFLOAT(IPVIB(K,M))*BOLTZ*SPVM(1,K,MS)
      END DO
    END IF            
  END IF
  EC=ECT+ECR+ECV
  STERT=0.
  DO J=1,NRE
    K=IRCD(J,LS,MS)
    A=ISPR(1,LS)+ISPR(1,MS)
    IF (A < .1) ECA(J)=ECT
    IF (A > .1) ECA(J)=ECT+ECR*2.*REA(1,K)/A
!--ECA is the energy available for reaction
    STER(J)=0.
    IF (ECA(J) >= REA(2,K)) THEN
      IF (NREA(1,K)+NREA(2,K) < 2) THEN
!--possible recombination reaction
!--select a possible third-body molecule from anywhere in the cell
        CALL RANDOM_NUMBER(RANF)
        IF (RANF < 0.01D00) THEN
!--only one in a hundred possible recombinations are considered and 
!----the probability is increased by a factor of one hundred


          IF (JREA(2,K,1) < 0) THEN
!--third-body molecule may be any species in cell   
            IF (ICCELL(2,N) >= 3) THEN
              MK=L
              DO WHILE ((MK == L).OR.(MK == M))
                CALL RANDOM_NUMBER(RANF)
                KK=INT(RANF*DFLOAT(ICCELL(2,N)))+ICCELL(1,N)+1
                MK=ICREF(KK)
              END DO
              KS=IPSP(MK)
              ISTE(J)=MK
              A=REA(6,K)
              IA=-JREA(2,K,1)
              A=A*THBP(IA,KS)
            ELSE
              A=0.D00
            END IF
            THBCELL=ICCELL(2,N)           
          ELSE
            KS=JREA(2,K,1)
!--the third-body molecule must be species KS
            THBCELL=0.
            MKK=0
            DO KAA=1,ICCELL(2,N)                     
              KA=ICCELL(1,N)+KAA
              MK=ICREF(KA)
              IF (IPSP(MK) == KS) THEN
                THBCELL=THBCELL+1.
                IF ((MK /= L).AND.(MK /= M)) MKK=MK
              END IF
            END DO
            IF (MKK > 0) THEN                              
              MK=MKK
              ISTE(J)=MK
              A=REA(6,K)
            ELSE
              A=0.
            END IF
          END IF
!
          B=THBCELL*FNUM/CCELL(1,N)
          STER(J)=100.D00*B*A*(ECA(J)/(BOLTZ*SPM(5,LS,MS)))**(REA(4,K)-1.D00+SPM(3,LS,MS))
!
!          WRITE (9,*) 'Rea',k,' ec',ECA(J),' nt',B,' steric factor',STER(J)
!
! MK IS THE CODE NUMBER OF THE THIRD BODY
!      WRITE (*,*) ' POSS RECOMB K, STER=',K,STER(J)
          STERT=STERT+STER(J)
        END IF
      ELSE 
!--possible binary reaction

        AA=REA(4,K)+REA(1,K)+0.5
        BB=REA(1,K)+1.5-SPM(3,LS,MS)
 !--note measures to prevent underflows         
        STER(J)=(REA(6,K)*((ECA(J)-REA(2,K))*1.E10)**AA/((ECA(J)*1.D10)**BB))*1.D10**(BB-AA)
        
        STERT=STERT+STER(J)
      END IF
    END IF
  END DO

!IF (STERT.GT.1.) WRITE (*,*) 'PAIR',LS,MS,' TOTAL STERIC FACTOR',STERT
  CALL RANDOM_NUMBER(RANF)
  IF (STERT.GT.RANF) THEN
!--a reaction is to occur              
    CALL RANDOM_NUMBER(RANF)
    A=RANF
    PSTERT=0.
    JR=0
    DO WHILE (PSTERT < A)
       JR=JR+1
      PSTERT=PSTERT+STER(JR)/STERT

!    IF (K == 21) WRITE (9,*) 'JR,A,PSTERT',JR,A,PSTERT

    END DO
    K=IRCD(JR,LS,MS)
!--REACTION K OCCURS
!    WRITE (*,*) 'Reaction',K

!
    IKA=1
    REAC(K)=REAC(K)+1.D00
    EC=EC+REA(5,K)  !post-collision energy
    IF (JREA(1,K,1) > 0) THEN
      IPSP(L)=JREA(1,K,1)
      IPSP(M)=JREA(2,K,1)
    ELSE
      IPSP(M)=JREA(2,K,1)
      IPSP(L)=JREA(1,K,1)
    END IF
!
    LS=IPSP(L)
    MS=IPSP(M)
    KR=NREA(1,K)+NREA(2,K)
    IF (KR /= 2) THEN
      IF (KR == 1) THEN
!  a single post-collision molecule (recombination)
        IF (LS == JREA(2,K,1)) JS=MS
        IF (MS == JREA(2,K,1)) JS=LS
        IF (LS == JREA(2,K,1)) THEN
          PX(L)=PX(M)
          IPSP(L)=IPSP(M)
        END IF
        IPCELL(M)=-IPCELL(M)   !--molecule M is marked for removal
	  !
        DO J=1,3
          PV(J,L)=VCM(J)
        END DO
!--now calc col of mol L with third body and add EC to rel trans energy
        M=ISTE(JR)
        DO J=1,3
          VRC(J)=PV(J,L)-PV(J,M)
        END DO
        VRR=VRC(1)**2+VRC(2)**2+VRC(3)**2
        MS=IPSP(M)
        LS=JS
        EC=EC+0.5D00*SPM(1,LS,MS)*VRR
        VRR=2.D00*EC/SPM(1,LS,MS)
        IF (EC < 0.) THEN
           WRITE (9,*) '-ve EC',EC,' SPECIES',LS,MS,' REA',K
        END IF
        VR=SQRT(VRR)
!--three post-collision molecules (dissociation)
      ELSE
        IF (NM >= MNM) CALL EXTEND_MNM(1.1)
        NM=NM+1
        JS=JREA(1,K,2)
        IF (JS.EQ.0) JS=JREA(2,K,2)
!WRITE (9,*) 'REACTION',K,' ADD SPECIES',JS
        PX(NM)=PX(L)
!--set molecule NM with center of mass velocity
        ICREF(NM)=N
        IPSP(NM)=JS
        IF (ISPR(1,JS) > 0) PROT(NM)=0.
        IF (MMVM > 0) THEN
          IF (ISPV(JS) > 0) THEN
            DO K=1,ISPV(JS)
              IPVIB(K,NM)=0
            END DO
          END IF
        END IF
        PV(1,NM)=VCM(1)
        PV(2,NM)=VCM(2)
        PV(3,NM)=VCM(3)
        CALL RANDOM_NUMBER(RANF)
        PTIM(NM)=FTIME+(-1.+2.*RANF)*CCELL(3,N)
        IPCELL(NM)=IPCELL(N)
      END IF
    END IF
!--set internal energies to zero (L-B will be implemented after a reaction)
    PROT(L)=0.D00
    IF (MMVM.GT.0) THEN
      DO KV=1,MMVM
        IPVIB(KV,L)=0
      END DO
    END IF
    PROT(M)=0.D00
    IF (MMVM.GT.0) THEN
      DO KV=1,MMVM
        IPVIB(KV,M)=0
      END DO
    END IF
    IF (EC < 0.) THEN
      WRITE (9,*) 'NEG EC 2',EC,' SPECIES',LS,MS,' REA',K
      EC=1.E-26
    END IF
    VRR=2.D00*EC/SPM(1,LS,MS)
    VR=SQRT(VRR)
    RML=SPM(1,LS,MS)/SP(5,MS)
    RMM=SPM(1,LS,MS)/SP(5,LS)           
!WRITE (*,*) ' ENERGY CHANGE FROM',ECI,' TO',EC
  END IF
END IF
!
DEALLOCATE (STER,ISTE,ECA,STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT DEALLOCATE STERIC FACTORS',ERROR
!  STOP
END IF
!
RETURN
!
END SUBROUTINE CHECK_REACTION
!
!************************************************************************************
!
SUBROUTINE ADAPT_CELLS
!
!--adapt the sampling cells through the splitting of the divisions into successive levels
!--the collision cells are divisions of the sampling cells
!
USE MOLECS
USE GAS
USE GEOM
USE CALC
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: M,N,L,K,KK,I,J,JJ,MSEG,NSEG,NSEG1,NSEG2
REAL(KIND=8) :: A,B,DDE,DCRIT
INTEGER, ALLOCATABLE, DIMENSION(:) :: KDIV,NC
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ISD
REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: XMIN,XMAX,DRAT
!
!--DCRIT  the number density ratio that causes a cell to be subdivided
!--KDIV(N) the number of divisions/subdivisions (cells or further subdivisions) at level N
!--DRAT(N) the contriburion to the density ratio of element N
!--NC(I) the number of sampling cells at level I
!--DDE the width of an element 
!--MSEG the maximum number of segments (a segment is the size of the smallest subdivision
!--NSEG1 the (first segment-1) in the subdivision
!--NSEG2 the final segment in the subdivision
!--ISD(N,M) 0,1 for cell,subdivided for level N subdivision
!
DCRIT=1.5D00    !--may be altered
!
!--determine the level to which the divisions are to be subdivided
!
A=1.D00
DO N=1,NCELLS
  IF (VAR(3,N)/FND(1) > A) A=VAR(3,N)/FND(1)
END DO
ILEVEL=0
DO WHILE (A > DCRIT)
  ILEVEL=ILEVEL+1
  A=A/2.D00
END DO
WRITE (9,*) 'ILEVEL =',ILEVEL 
NSEG=2**ILEVEL 
MSEG=NDIV*NSEG
!
ALLOCATE (KDIV(0:ILEVEL),DRAT(MSEG),NC(0:ILEVEL),ISD(0:ILEVEL,MSEG),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR KDIV ARRAY',ERROR
ENDIF
!
DDE=(XB(2)-XB(1))/DFLOAT(MSEG)
DO N=1,MSEG
  A=XB(1)+(DFLOAT(N)-0.5D00)*DDE
  CALL FIND_CELL(A,M,L)
  DRAT(N)=VAR(3,L)/(FND(1)*DFLOAT(NSEG))
END DO  
!
!--calculate the number of subdivisions at the various levels of subdivision
KDIV=0
!--also the number of sampling cells at each level
NC=0 
!
DO N=1,NDIV    !--divisions
  ISD=0
  ISD(0,1)=1
  KDIV(0)=KDIV(0)+1
!  WRITE (9,*) 'DIVISION',N
  DO I=0,ILEVEL  !--level of subdivision
!    WRITE (9,*) 'LEVEL',I
    J=2**I  !--number of possible subdivisions at this level
    JJ=NSEG/J  !--number of segments in a subdivision
    DO M=1,J
!      WRITE (9,*) 'SUBDIVISION',M
      IF (ISD(I,M) == 1) THEN 
        NSEG1=(N-1)*NSEG+(M-1)*JJ+1 
        NSEG2=NSEG1+JJ-1
        A=0.D00
!        WRITE (9,*) 'NSEG RANGE',NSEG1,NSEG2
        DO L=NSEG1,NSEG2
          A=A+DRAT(L)
        END DO
!        WRITE (9,*) 'DENS CONTRIB',A
        IF (A < DCRIT) THEN
          NC(I)=NC(I)+1
!          WRITE (9,*) 'LEVEL',I,' CELLS TO', NC(I)
        ELSE
          KDIV(I+1)=KDIV(I+1)+2
!          WRITE (9,*) 'LEVEL',I+1,' SUBDIVISIONS TO',KDIV(I+1)
          DO L=NSEG1-(N-1)*NSEG,NSEG2-(N-1)*NSEG
            ISD(I+1,L)=1
          END DO  
        END IF
      END IF
    END DO
  END DO
END DO 
!
WRITE (9,*) 'KDIV',KDIV
!
WRITE (9,*) 'NC',NC
WRITE (*,*)
WRITE (9,*) 'Number of divisions',NDIV
A=0
NCELLS=0
DO N=0,ILEVEL
  A=A+DFLOAT(NC(N))/(2.D00**N)
  NCELLS=NCELLS+NC(N)
END DO  
WRITE (9,*) 'Total divisions from sampling cells',A
WRITE (9,*) 'Adapted sampling cells',NCELLS
NCCELLS=NCELLS*NCIS
WRITE (9,*) 'Adapted collision cells',NCCELLS
!
DEALLOCATE (JDIV,CELL,ICELL,CCELL,ICCELL,COLLS,WCOLLS,CLSEP,VAR,VARSP,CS,STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT DEALLOCATE ARRAYS IN ADAPT',ERROR
END IF
!
DO N=0,ILEVEL
  IF (KDIV(N) > MDIV) MDIV=KDIV(N)
END DO 
!  
ALLOCATE (JDIV(0:ILEVEL,MDIV),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR JDIV ARRAY IN ADAPT',ERROR
ENDIF
!
ALLOCATE (CELL(4,NCELLS),ICELL(NCELLS),CCELL(5,NCCELLS),ICCELL(3,NCCELLS),XMIN(NCCELLS),XMAX(NCCELLS),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR CELL ARRAYS IN ADAPT',ERROR
ENDIF
!
ALLOCATE (COLLS(NCELLS),WCOLLS(NCELLS),CLSEP(NCELLS),VAR(21,NCELLS),VARSP(0:11,NCELLS,MSP),CS(0:8+MSP,NCELLS,MSP),STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*) 'PROGRAM COULD NOT ALLOCATE SPACE FOR SAMPLING ARRAYS IN ADAPT',ERROR
ENDIF
!
NCCELLS=0
NCELLS=0
!
!--set the JDIV arrays and the sampling cells at the various levels of subdivision
KDIV=0
JDIV=0
!
DO N=1,NDIV    !--divisions
  ISD=0
  ISD(0,1)=1
  KDIV(0)=KDIV(0)+1
  DO I=0,ILEVEL  !--level of subdivision
    J=2**I  !--number of possible subdivisions at this level
    JJ=NSEG/J  !--number of segments in a subdivision
    DO M=1,J
      IF (ISD(I,M) == 1) THEN 
        NSEG1=(N-1)*NSEG+(M-1)*JJ+1 
        NSEG2=NSEG1+JJ-1
        A=0.D00
        DO L=NSEG1,NSEG2
          A=A+DRAT(L)
        END DO
        IF (A < DCRIT) THEN
          NCELLS=NCELLS+1
          XMIN(NCELLS)=XB(1)+DFLOAT(NSEG1-1)*DDE
          XMAX(NCELLS)=XMIN(NCELLS)+DFLOAT(NSEG2-NSEG1+1)*DDE
          WRITE (9,*) NCELLS,I,' XMIN,XMAX',XMIN(NCELLS),XMAX(NCELLS)
          JDIV(I,KDIV(I)-(J-M))=-NCELLS          
!          WRITE (9,*) 'JDIV(',I,',',KDIV(I)-(J-M),')=',-NCELLS
        ELSE
          JDIV(I,KDIV(I)-(J-M))=KDIV(I+1)
!          WRITE (9,*) 'JDIV(',I,',',KDIV(I)-(J-M),')=',KDIV(I+1)         
          KDIV(I+1)=KDIV(I+1)+2
          DO L=NSEG1-(N-1)*NSEG,NSEG2-(N-1)*NSEG
            ISD(I+1,L)=1
          END DO  
        END IF
      END IF
    END DO
  END DO
END DO 
!
!--set the other quantities associated with the sampling cells and the collision cells
!
NCCELLS=0
DO N=1,NCELLS
  CELL(1,N)=(XMIN(N)+XMAX(N))/2.D00
  CELL(2,N)=XMIN(N)
  CELL(3,N)=XMAX(N)
  IF (IFX == 0) CELL(4,N)=XMAX(N)-XMIN(N)    !--calculation assumes unit cross-section
  IF (IFX == 1) CELL(4,N)=PI*(XMAX(N)**2-XMIN(N)**2)
  IF (IFX == 2) CELL(4,N)=1.33333333333333333333D00*PI*(XMAX(N)**3-XMIN(N)**3)
  ICELL(N)=NCCELLS
  DO M=1,NCIS
    NCCELLS=NCCELLS+1
    ICCELL(3,NCCELLS)=N
    CCELL(1,NCCELLS)=CELL(4,N)/DFLOAT(NCIS)
    CCELL(3,NCCELLS)=DTM/2.D00
    CCELL(4,NCCELLS)=2.D00*VMPM*SPM(2,1,1)
    CALL RANDOM_NUMBER(RANF)
    CCELL(2,NCCELLS)=RANF
    CCELL(5,NCCELLS)=FTIME
  END DO
END DO
!
!--assign the molecules to the cells
!
DO N=1,NM
  CALL FIND_CELL(PX(N),IPCELL(N),JJ)
  M=IPCELL(N)
END DO
!
!--deallocate the local variables
DEALLOCATE (KDIV,NC,ISD,XMIN,XMAX,DRAT,STAT=ERROR)
IF (ERROR /= 0) THEN
  WRITE (*,*)'PROGRAM COULD NOT DEALLOCATE LOCAL ARRAYS IN ADAPT',ERROR
END IF
!
RETURN
!
END SUBROUTINE ADAPT_CELLS
!
!*****************************************************************************
!
SUBROUTINE ENERGY(M,I,TOTEN)
!
!--calculate the total energy (all molecules if I=0, otherwise molecule I)
!--used for dianostic purposes only
!
USE MOLECS
USE GAS
USE CALC
!
IMPLICIT NONE
!
INTEGER :: K,L,N,I,II,M,IV,KV,J
REAL(KIND=8) :: TOTEN,TOTENI
!
TOTEN=0.
!
IF ((M == 6).OR.(M == 4)) THEN
  IF (I == 0) THEN
    DO N=1,NM
      IF (IPCELL(N) > 0) THEN
        L=IPSP(N)        
        TOTENI=TOTEN
        IF (M == 6) THEN
          IF (L==3) TOTEN=TOTEN+0.5D00*BOLTZ*SPVM(4,1,1)
          IF (L==4) TOTEN=TOTEN+0.5D00*BOLTZ*SPVM(4,1,2)
          IF (L==5) TOTEN=TOTEN+1.49D-19
        END IF  
        IF (M == 4) THEN
          IF ((L==2).OR.(L == 3)) TOTEN=TOTEN+0.5D00*BOLTZ*SPVM(4,1,1)
        END IF
        TOTEN=TOTEN+0.5D00*SP(5,L)*(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
        IF (ISPR(1,L) > 0) TOTEN=TOTEN+PROT(N)
        IF (ISPV(L) > 0) THEN
          DO KV=1,ISPV(L)
          J=IPVIB(KV,N)
          IF (J <0) THEN
            J=-J
            IF (J == 99999) J=0
          END IF  
            TOTEN=TOTEN+DFLOAT(J)*BOLTZ*SPVM(1,KV,L)
          END DO
        END IF    
      END IF
      IF ((TOTEN-TOTENI) > 1.D-16) WRITE (*,*) 'MOL',N,' ENERGY',TOTEN-TOTENI
    END DO
!
    WRITE (9,*) 'Total Energy =',TOTEN,NM
    WRITE (*,*) 'Total Energy =',TOTEN,NM
  ELSE 
    N=I
    IF (IPCELL(N) > 0) THEN
      L=IPSP(N)
      IF (M == 6) THEN
        IF (L==3) TOTEN=TOTEN+0.5D00*BOLTZ*SPVM(4,1,1)
        IF (L==4) TOTEN=TOTEN+0.5D00*BOLTZ*SPVM(4,1,2)
        IF (L==5) TOTEN=TOTEN+1.49D-19 
      END IF 
      IF ((M == 4).OR.(M > 8)) THEN
!        IF ((L==2).OR.(L == 3)) TOTEN=TOTEN+0.5D00*BOLTZ*SPVM(4,1,1)
      END IF
      TOTEN=TOTEN+0.5D00*SP(5,L)*(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
      IF (ISPR(1,L) > 0) TOTEN=TOTEN+PROT(N)
      IF (ISPV(L) > 0) THEN
        DO KV=1,ISPV(L)
          J=IPVIB(KV,N)
          IF (J <0) THEN
            J=-J
            IF (J == 99999) J=0
          END IF  
          TOTEN=TOTEN+DFLOAT(J)*BOLTZ*SPVM(1,KV,L)
        END DO
      END IF 
    END IF
  END IF
END IF 
IF (M == 8) THEN
  IF (I == 0) THEN
    DO N=1,NM
      IF (IPCELL(N) > 0) THEN
        TOTENI=TOTEN
        L=IPSP(N)
        IF (L==2) TOTEN=TOTEN+3.62D-19
        IF (L==4) TOTEN=TOTEN+4.14D-19
        IF (L==5) TOTEN=TOTEN+0.65D-19
        IF (L==6) TOTEN=TOTEN-4.02D-19
        IF (L==7) TOTEN=TOTEN+0.17D-19
        TOTEN=TOTEN+0.5D00*SP(5,L)*(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
        IF (ISPR(1,L) > 0) TOTEN=TOTEN+PROT(N)
        IF (ISPV(L) > 0) THEN
          DO K=1,ISPV(L)
            IF (IPVIB(K,N) < 0) THEN
              WRITE (*,*) 'Dissociation marked molecule still in flow',N,IPVIB(K,N)
!              STOP
            END IF   
            TOTEN=TOTEN+IPVIB(K,N)*BOLTZ*SPVM(1,K,L)
         END DO
        END IF
        IF ((TOTEN-TOTENI) > 1.D-16) WRITE (*,*) 'MOL',N,' ENERGY',TOTEN-TOTENI
      END IF
    END DO
!
    WRITE (9,*) 'Total Energy =',TOTEN,NM
    WRITE (*,*) 'Total Energy =',TOTEN,NM
  ELSE  
    N=I
    IF (IPCELL(N) > 0) THEN
      L=IPSP(N)
      IF (IPCELL(N) > 0) THEN
        L=IPSP(N)
        IF (L==2) TOTEN=TOTEN+3.62D-19
        IF (L==4) TOTEN=TOTEN+4.14D-19
        IF (L==5) TOTEN=TOTEN+0.65D-19
        IF (L==6) TOTEN=TOTEN-4.02D-19
        IF (L==7) TOTEN=TOTEN+0.17D-19
        TOTEN=TOTEN+0.5D00*SP(5,L)*(PV(1,N)**2+PV(2,N)**2+PV(3,N)**2)
        IF (ISPR(1,L) > 0) TOTEN=TOTEN+PROT(N)
        IF (ISPV(L) > 0) THEN
          DO K=1,ISPV(L)
            IV=IPVIB(K,N)
            IF (IV < 0) THEN
              IF (IV == -99999) THEN
                IV=0
              ELSE
                IV=-IV
              END IF 
              IF (L == 1) TOTEN=TOTEN+7.24E-19
              IF (L == 3) TOTEN=TOTEN+8.28E-19
              IF (L == 5) TOTEN=TOTEN+7.76E-19
              IF (L == 6) TOTEN=TOTEN+8.29E-19
              IF (L == 7) TOTEN=TOTEN+3.45E-19   
            END IF
            TOTEN=TOTEN+IV*BOLTZ*SPVM(1,K,L)
          END DO
        END IF 
        IF (ABS(TOTEN) > 1.D-16) THEN
          CONTINUE
        END IF
      END IF
    END IF
  END IF
END IF
!
RETURN
!
END SUBROUTINE ENERGY
!
!************************************************************************************
!
SUBROUTINE COLLISIONS
!
!--calculate the collisions
!
USE MOLECS
USE CALC
USE GEOM
USE GAS
USE OUTPUT
!
IMPLICIT NONE
!
INTEGER :: N,NN,M,MM,L,LL,K,KK,KT,J,I,II,III,NSP,MAXLEV,IV,NSEL,KV,LS,MS,KS,JS,IKA,IIII,LZ,KL,IS,IREC,NLOOP,IA,IDISS,IE,IEX,     &
           JJ,NPRI,LIMLEV,KVV,KW,ISP1,ISP2,ISP3,INIL,INIM,JI,LV,IVM,NMC,NVM,LSI,JX,KKV,MOLA,KR,KI,JKV,NSC
REAL(KIND=8) :: A,AA,AAA,AB,B,BB,BBB,ASEL,DTC,SEP,VR,VRR,ECT,EVIB,ECC,ZV,COLT,ERM,C,OC,SD,D,CVR,PROB,RML,RMM,ECTOT,EA1,EA2,ETI,EREC,ET2,       &
                XMIN,XMAX,WFC,SFAC,SDF,CENI,CENF,VRRT
REAL(KIND=8),DIMENSION(3) :: VRC,VCM,VRCP,VRCT

!
!--N,M,K working integer
!--LS,MS,KS,JS molecular species 
!--VRC components of the relative velocity
!--RML,RMM molecule mass parameters
!--VCM components of the center of mass velocity
!--VRCP post-collision components of the relative velocity
!--SEP the collision partner separation
!--VRR the square of the relative speed
!--VR the relative speed
!--ECT relative translational energy
!--EVIB vibrational energy
!--ECC collision energy
!--MAXLEV maximum vibrational level
!--ZV vibration collision number
!--COLT collision temperature
!--SDF the number of degrees of freedom associated with the collision
!--ERM rotational energy
!--NSEL integer number of selections
!--CVR product of collision cross-section and relative speed
!--PROB a probability
!--IKA reaction indicator  ( 0 for no reaction, 1 for reaction )
!--KT third body molecule code
!--ECTOT energy added at recmbination
!--IREC initially 0, becomes 1 of a recombination occurs
!--NPRI  the priority collision molecule (the last one rejected as a previous collision molecule)
!--WFC weighting factor in the cell
!--SFAC the sreric factor for an exothermic exchange or chain reaction
!--IEX is the reaction that occurs (1 if only one is possible)
!
!WRITE (*,*) 'START COLLISIONS'
!
DO N=1,NCCELLS
!
!WRITE (9,*) 'COLL CELL',N,' MOLS',ICCELL(2,N),' STEP',CCELL(3,N)
!
  IF (FTIME-CCELL(5,N) > CCELL(3,N)) THEN
!
    DTC=2.D00*CCELL(3,N)
!--calculate collisions appropriate to  time DTC
    IF (ICCELL(2,N) > 1) THEN      !--no collisions calculated if there are less than two molecules in collision cell
      NN=ICCELL(3,N)
      WFC=1.D00
      IF ((IWF == 1).AND.(IVB == 0)) WFC=1.D00+WFM*CELL(1,NN)**IFX
      CCELL(5,N)=CCELL(5,N)+DTC
      IF (IVB == 0) AAA=CCELL(1,N)  
      IF (IVB == 1) THEN
        C=(XB(2)+VELOB*FTIME-XB(1))/DFLOAT(NDIV*NCIS)
        XMIN=XB(1)+DFLOAT(N-1)*C
        XMAX=XMIN+C
        WFC=1.D00+WFM*(0.5D00*(XMIN+XMAX))**IFX
        IF (IFX == 0) AAA=XMAX-XMIN
        IF (IFX == 1) AAA=PI*(XMAX**2-XMIN**2)  !--assumes unit length of full cylinder
        IF (IFX == 2) AAA=1.33333333333333333333D00*PI*(XMAX**3-XMIN**3)    !--flow is in the full sphere
      END IF
!--these statements implement the N(N-1) scheme
!
      ASEL=0.5D00*ICCELL(2,N)*(ICCELL(2,N)-1)*WFC*FNUM*CCELL(4,N)*DTC/AAA+CCELL(2,N)
!
      NSEL=ASEL
      CCELL(2,N)=ASEL-DFLOAT(NSEL)
      IF (NSEL > 0) THEN
        I=0   !--counts the number of selections
        IE=0  !--becomes 1 if the collision is inelastic
        KL=0  !--becomes 1 if it is the last selection

        IIII=0 !--becomes 1 if there is a recombination
        NPRI=0
        DO KL=1,NSEL
          I=I+1
          III=0
!          
          IF (ICCELL(2,N) == 2) THEN
            K=1+ICCELL(1,N)
            L=ICREF(K)
            K=2+ICCELL(1,N)
            M=ICREF(K)
            IF (M == IPCP(L)) THEN
              III = 1  
              CCELL(5,N)=CCELL(5,N)-DTC
            END IF 
          ELSE
            K=NPRI
            IF (K > 0) THEN
              L=K
              NPRI=0
            ELSE
              CALL RANDOM_NUMBER(RANF)
              K=INT(RANF*DFLOAT(ICCELL(2,N)))+ICCELL(1,N)+1
              L=ICREF(K)
            END IF   
!--one molecule has been selected at random
            IF (NNC == 0) THEN
!--select the collision partner at random            
              M=L
              DO WHILE (M == L)
                CALL RANDOM_NUMBER(RANF)
                K=INT(RANF*DFLOAT(ICCELL(2,N)))+ICCELL(1,N)+1
                M=ICREF(K)
              END DO
            ELSE
!--select the nearest from the total number (< 30) or a random 30         
              IF (ICCELL(2,N) < 30) THEN
                LL=ICCELL(2,N)
              ELSE
                LL=30
              END IF
              SEP=1.D10
              M=0    
              DO J=1,LL  
                IF (LL < 30) THEN
                  K=J+ICCELL(1,N)
                ELSE  
                  CALL RANDOM_NUMBER(RANF)
                  K=INT(RANF*DFLOAT(ICCELL(2,N)))+ICCELL(1,N)+1
                END IF  
                MM=ICREF(K)
                IF (MM /= L) THEN   !--exclude the already selected molecule
                  IF (MM /= IPCP(L)) THEN   !  exclude the previous collision partner
                    A=DABS(PX(L)-PX(MM)) 
                    IF ((A < SEP).AND.(A >1.D-8*DDIV)) THEN
                      M=MM
                      SEP=A
                    END IF
                  ELSE
                    NPRI=MM    !MM is made the priority molecule
                  END IF
                END IF  
              END DO
            END IF          
          END IF
          IF (III == 0) THEN
            DO KK=1,3
              VRC(KK)=PV(KK,L)-PV(KK,M)
            END DO
            VRR=VRC(1)*VRC(1)+VRC(2)*VRC(2)+VRC(3)*VRC(3)
            VR=SQRT(VRR)
!--Simple gas            
            IF (MSP == 1) THEN   
              CVR=VR*CXSS*((2.D00*BOLTZ*SP(2,1)/(RMAS*VRR))**(SP(3,1)-0.5D00))*RGFS
              IF (CVR > CCELL(4,N)) CCELL(4,N)=CVR
              CALL RANDOM_NUMBER(RANF)
              IF (RANF < CVR/CCELL(4,N)) THEN               
!--the collision occurs
                IF ((M == IPCP(L)).AND.(L == IPCP(M))) TOTDUP=TOTDUP+1.D00   
                TOTCOL=TOTCOL+1.D00  
                TCOL(1,1)=TCOL(1,1)+2.D00
                COLLS(NN)=COLLS(NN)+1.D000
                WCOLLS(NN)=WCOLLS(NN)+WFC
                SEP=DABS(PX(L)-PX(M)) 
                CLSEP(NN)=CLSEP(NN)+SEP       
                IF ((ISPR(1,1) > 0)) THEN
!--Larsen-Borgnakke serial redistribution 
                  ECT=0.5D00*RMAS*VRR
                  DO NSP=1,2
!--consider the molecules in turn
                    IF (NSP == 1) THEN
                      K=L 
                    ELSE
                      K=M 
                    END IF
                    IF (MMVM > 0) THEN
                      IF (ISPV(1) > 0) THEN
                        DO KV=1,ISPV(1)
                          EVIB=DFLOAT(IPVIB(KV,K))*BOLTZ*SPVM(1,KV,1)                      
                          ECC=ECT+EVIB
                          IF (SPVM(3,KV,1) > 0.) THEN
!--note quantizing of COLT in the following statements
                            MAXLEV=ECC/(BOLTZ*SPVM(1,KV,1))
                            COLT=DFLOAT(MAXLEV)*SPVM(1,KV,1)/(3.5D00-SPM(3,1,1))
                            B=SPVM(4,KV,1)/SPVM(3,KV,1)    !Tdiss/Tref          !
                            A=SPVM(4,KV,1)/COLT       !Tdiss/T
                            ZV=(A**SPM(3,1,1))*(SPVM(3,KV,1)*(B**(-SPM(3,1,1))))**(((A**0.3333333D00)-1.D00)/((B**0.33333D00)-1.D00))
                          ELSE
                            ZV=SPVM(2,KV,1)
			        		MAXLEV=ECC/(BOLTZ*SPVM(1,KV,1))+1
                          END IF
!
                          CALL RANDOM_NUMBER(RANF)
                          IF (1.D00/ZV > RANF) THEN 
                            IE=1
                            II=0
                            DO WHILE (II == 0)
                              CALL RANDOM_NUMBER(RANF)
                              IV=RANF*(MAXLEV+0.99999999D00)                           
                              IPVIB(KV,K)=IV
                              EVIB=DFLOAT(IV)*BOLTZ*SPVM(1,KV,1)
                              IF (EVIB < ECC) THEN
                                PROB=(1.D00-EVIB/ECC)**(1.5D00-SPM(3,KV,1))
!--PROB is the probability ratio of eqn (5.61)
                                CALL RANDOM_NUMBER(RANF)
                                IF (PROB > RANF) II=1
                              END IF
                            END DO
                            ECT=ECC-EVIB
                          END IF
                        END DO
                      END IF
                    END IF
!--now rotation of this molecule
                    IF (ISPR(1,1).GT.0) THEN
                      IF (ISPR(2,1) == 0) THEN
                        B=1.D00/SPR(1,1)
                      ELSE    !--use molecule rather than mean value
                        COLT=ECC/((2.5D00-SP(3,1))*BOLTZ)
                        B=1.D00/(SPR(1,1)+SPR(2,1)*COLT+SPR(3,1)*COLT*COLT)
                      END IF
                      CALL RANDOM_NUMBER(RANF)
                      IF (B > RANF) THEN
                        IE=1
                        ECC=ECT+PROT(K)
                        IF (ISPR(1,1) == 2) THEN
                          CALL RANDOM_NUMBER(RANF)
                          ERM=1.D00-RANF**(1.D00/(2.5D00-SP(3,1)))  !eqn(5.46)
                        ELSE
                          CALL LBS(0.5D00*ISPR(1,1)-1.D00,1.5D00-SP(3,1),ERM)
                        END IF
                        PROT(K)=ERM*ECC
                        ECT=ECC-PROT(K)
                      END IF
                    END IF
                  END DO
!--adjust VR for the change in energy
                  VR=SQRT(2.D00*ECT/SPM(1,1,1))
                END IF
!--end of L-B redistribution
!
                DO KK=1,3
                  VCM(KK)=0.5D00*(PV(KK,L)+PV(KK,M))
                END DO
!
                IF (ABS(SP(4,1)-1.) < 0.001) THEN
!--use the VHS logic
                  CALL RANDOM_NUMBER(RANF)
                  B=2.D00*RANF-1.D00
!--B is the cosine of a random elevation angle             
                  A=DSQRT(1.D00-B*B)
                  VRCP(1)=B*VR
                  CALL RANDOM_NUMBER(RANF)
                  C=2.D00*PI*RANF
!--C is a random azimuth angle
                  VRCP(2)=A*DCOS(C)*VR
                  VRCP(3)=A*DSIN(C)*VR
                ELSE
!--use the VSS logic
                  CALL RANDOM_NUMBER(RANF)
                  B=2.D00*(RANF**SP(4,1))-1.D00
!--B is the cosine of the deflection angle for the VSS model (eqn (11.8)
                  A=SQRT(1.D00-B*B)
                  CALL RANDOM_NUMBER(RANF)
                  C=2.D00*PI*RANF
                  OC=DCOS(C)
                  SD=DSIN(C)
                  D=SQRT(VRC(2)**2+VRC(3)**2)
 !               IF (D.GT.1.E-6) THEN
                  VRCP(1)=B*VRC(1)+A*SD*D
                  VRCP(2)=B*VRC(2)+A*(VR*VRC(3)*OC-VRC(1)*VRC(2)*SD)/D
                  VRCP(3)=B*VRC(3)-A*(VR*VRC(2)*OC+VRC(1)*VRC(3)*SD)/D
   !             ELSE
 !                 VRCP(1)=B*VRC(1)
 !                 VRCP(2)=A*OC*VRC(1)
 !                 VRCP(3)=A*SD*VRC(1)
 !               END IF
!--the post-collision rel. velocity components are based on eqn (2.22)
                END IF
!
                DO KK=1,3
                  PV(KK,L)=VCM(KK)+0.5D00*VRCP(KK)
                  PV(KK,M)=VCM(KK)-0.5D00*VRCP(KK)
                END DO

                IPCP(L)=M
                IPCP(M)=L
              END IF   !--collision occurrence
            ELSE
!--Gas Mixture
              LS=ABS(IPSP(L))
              MS=ABS(IPSP(M)) 
              CVR=VR*SPM(2,LS,MS)*((2.D00*BOLTZ*SPM(5,LS,MS)/(SPM(1,LS,MS)*VRR))**(SPM(3,LS,MS)-0.5D00))*SPM(6,LS,MS)
              IF (CVR > CCELL(4,N)) CCELL(4,N)=CVR
              CALL RANDOM_NUMBER(RANF)
              IF ((RANF < CVR/CCELL(4,N)).AND.(IPCELL(L) > 0).AND.(IPCELL(M) > 0)) THEN    
!--the collision occurs (-ve IPCELL indicates recombined molecule marled for removal)
                IF ((M == IPCP(L)).AND.(L ==IPCP(M))) TOTDUP=TOTDUP+1.D00   
                TOTCOL=TOTCOL+1.D00 
                TCOL(LS,MS)=TCOL(LS,MS)+1.D00
                TCOL(MS,LS)=TCOL(MS,LS)+1.D00
                COLLS(NN)=COLLS(NN)+1.D00
                WCOLLS(NN)=WCOLLS(NN)+WFC
                SEP=DABS(PX(L)-PX(M)) 
                CLSEP(NN)=CLSEP(NN)+SEP                           
                RML=SPM(1,LS,MS)/SP(5,MS)
                RMM=SPM(1,LS,MS)/SP(5,LS)           
                DO KK=1,3
                  VCM(KK)=RML*PV(KK,L)+RMM*PV(KK,M)
                END DO
!
                IEX=0    !--becomes the collision number if a reaction occurs 
                IREC=0   !--becomes 1 if a recombination occurs
!
                IF (JCD == 1) THEN         !--use the Q-K reaction model 
!--consider possible recombinations
!                                  
                  IF ((ISPRC(LS,MS) > 0).AND.(ICCELL(2,N) > 2)) THEN        
!--possible recombination using model based on collision volume for equilibrium
                    CALL RANDOM_NUMBER(RANF)
                    IF (RANF < 0.2) THEN   !--third body probability will be increased by a factor of 5
!--this is to save computational time, the factor can be much higher at very low densities                     
                      KT=L
                      DO WHILE ((KT == L).OR.(KT == M)) 
                        CALL RANDOM_NUMBER(RANF)
                        K=INT(RANF*DFLOAT(ICCELL(2,N)))+ICCELL(1,N)+1
                        KT=ICREF(K)                   
                      END DO
                      KS=IPSP(KT) 
!--the potential third body is KT OF species KS 

!--calculate the radius of the third body molecule in this collision 
!--set it to the radius that corresponding to the relative speed between the third-body and the center of mass velocity
                      DO KK=1,3
                        VRCT(KK)=PV(KK,KT)-VCM(KK)
                      END DO
                      VRRT=VRCT(1)*VRCT(1)+VRCT(2)*VRCT(2)+VRCT(3)*VRCT(3)
                      AA=0.5D00*SP(1,KS)*(16.D00*BOLTZ*SP(2,KS)/(PI*SP(5,KS)*VRRT))**((SP(3,KS)-0.5D00)*0.5D00) !3rd body radius
                      A=SQRT(CVR/(PI*VR))    !--radius of collision cross-section (sum of the molecular radii)
                      AB=0.5D00*A+AA  !average radius of colliding molecules+radius of 3rd body molecule
                      B=(8.D00*PI/3.D00)*AB**3-(2.D00*PI/3.D00)*AA*AA*(3.D00*AB-AA)
!three body collision volume is sum of two spheres with radius = [(average radius of colliding molecules+radius of third body)-the volume lost in the overlap of these spheres]                      
!
                      BB=(1.333333D00*PI)*1.5D00*A*1.5D00*A*1.5D00*A    !--the collision volume that was used in Version 1.8 
!                      WRITE (*,*) 'NEW',B,'  OLD',BB
!
                      B=5.D00*B*ICCELL(2,N)*FNUM/AAA     !--probability of a three body collision 
!                     is that of a molecule being in the  three-body volume
                      IF (B > 1.D00) WRITE (*,*) 'THREE BODY PROBABILITY',B     !note factor of 5 may have to be reduced
                      CALL RANDOM_NUMBER(RANF)
                      IF (RANF < B) THEN                       
                        IREC=1
                        TRECOMB=TRECOMB+1.D00
 !--the collision now becomes a collision between these with L having the center of mass velocity
                        A=0.5D00*SPM(1,LS,MS)*VRR   !--the relative energy of the recombining molecules
                        IF (ISPR(1,LS) > 0) A=A+PROT(L)
                        IF (ISPV(LS) > 0) THEN
                          DO KVV=1,ISPV(LS)
                            JI=IPVIB(KVV,L)
                            IF (JI < 0) JI=-JI
                            IF (JI == 99999) JI=0
                            A=A+DFLOAT(JI)*BOLTZ*SPVM(1,KVV,LS)
                          END DO
                        END IF
                        IF (ISPR(1,MS) > 0) A=A+PROT(M)
                        IF (ISPV(MS) > 0) THEN
                          DO KVV=1,ISPV(MS)
                            JI=IPVIB(KVV,M)
                            IF (JI < 0) JI=-JI
                            IF (JI == 99999) JI=0                          
                            A=A+DFLOAT(JI)*BOLTZ*SPVM(1,KVV,MS)
                          END DO
                        END IF 
                        TREACL(2,LS)=TREACL(2,LS)-1
                        TREACL(2,MS)=TREACL(2,MS)-1
                        LSI=LS
                        LS=ISPRC(LS,MS)
                        IPSP(L)=LS
!--any additional vibrational modes must be set to zero                      
                        IVM=ISPV(LSI)
                        NMC=IPSP(L)
                        NVM=ISPV(NMC)
                        IF (NVM > IVM) THEN
                          DO KV=IVM+1,NVM
                            IPVIB(KV,L)=0
                          END DO
                        END IF 
!                        
                        IPCELL(M)=-IPCELL(M)  !--recombining molecule M marked for removal
                        M=KT                  !--third body molecule is set as molecule M
                        MS=KS 
                        TREACG(2,LS)=TREACG(2,LS)+1                           
                        IF (ISPR(1,LS) > 0) PROT(L)=0.D00
                        IF (ISPV(LS) > 0) THEN
                          DO KVV=1,ISPV(LS)
                            IF (IPVIB(KVV,L) < 0) THEN
                              IPVIB(KVV,L)=-99999
                            ELSE  
                              IPVIB(KVV,L)=0
                            END IF  
                          END DO
                        END IF
                        IF (ISPR(1,MS) > 0) PROT(M)=PROT(KT)
                        IF (ISPV(MS) > 0) THEN
                          DO KVV=1,ISPV(MS)                                                
                            IPVIB(KVV,M)=IPVIB(KVV,KT)
                          END DO
                        END IF
                        ECTOT=A+SPVM(4,1,LS)*BOLTZ    !--the energy added to this collision 
                        DO KK=1,3
                          PV(KK,L)=VCM(KK)
                        END DO   
                        DO KK=1,3
                          VRC(KK)=PV(KK,L)-PV(KK,M)
                        END DO
                        VRR=VRC(1)*VRC(1)+VRC(2)*VRC(2)+VRC(3)*VRC(3)
                        ECT=0.5D00*SPM(1,LS,MS)*VRR+ECTOT
                        VRR=2.D00*ECT/SPM(1,LS,MS)
                        VR=SQRT(VRR)                    
                        RML=SPM(1,LS,MS)/SP(5,MS)
                        RMM=SPM(1,LS,MS)/SP(5,LS)           
                        DO KK=1,3
                          VCM(KK)=RML*PV(KK,L)+RMM*PV(KK,M)
                        END DO
                      END IF
                    END IF                            
                  END IF                          
! 
!--consider exchange and chain reactions 
!
                  IF ((NSPEX(LS,MS) > 0).AND.(IREC == 0)) THEN  !--possible exchange reaction
                    PSF=0.D00    !PSF(MMEX) becomes 1 if the reaction is possible
 !                
                    DO JJ=1,NSPEX(LS,MS)
  
                         IF (LS == ISPEX(JJ,0,LS,MS)) THEN
                          K=L  ;   KS=LS   ;   JS=MS
                        ELSE
                          K=M  ;   KS=MS   ;   JS=LS
                        END IF                         
 !--the pre-collision molecule that splits is K of species KS 
 
 !                     
                          CALL RANDOM_NUMBER(RANF)
                          KI=RANF*ISPV(KS) !--random entry to loop over vibrational modes of molecule A                   
                          DO KKV=1,ISPV(KS)
                            KV=KI+KKV  
                            IF (KV > ISPV(KS)) KV=KV-ISPV(KS)                                                  
                            JI=IPVIB(KV,K)
                            IF (JI < 0) JI=-JI
                            IF (JI == 99999) JI=0
                            ECC=0.5D00*SPM(1,LS,MS)*VRR+DFLOAT(JI)*BOLTZ*SPVM(1,KV,KS)   
                            MAXLEV=ECC/(BOLTZ*SPVM(1,KV,KS))
                            COLT=DFLOAT(MAXLEV)*SPVM(1,KV,KS)/(3.5D00-SPM(3,KS,JS))        !--quantized collision temperature
                             
                            LV=SPEX(1,JJ,KS,JS)/(BOLTZ*SPVM(1,KV,KS))+1
                            IF (LV < 0) LV=0.
!--a negative activation may have been set                             
                            
                             
                            IF (MAXLEV >= LV) THEN
                              II=0
                              DO WHILE (II == 0)
                                CALL RANDOM_NUMBER(RANF)
                                IV=RANF*(MAXLEV+0.99999999D00)
                                EVIB=DFLOAT(IV)*BOLTZ*SPVM(1,KV,KS)
                                IF (EVIB < ECC) THEN
                                  PROB=(1.D00-EVIB/ECC)**(1.5D00-SPM(3,KS,JS))
 !--PROB is the probability ratio of eqn (5.61)
                                  CALL RANDOM_NUMBER(RANF)
                                  IF (PROB > RANF) II=1       !
                                END IF
                              END DO 
                              IF (IV == LV) PSF(JJ)=1.D00          
                            END IF
                          END DO  

                    END DO
 !                   
                    BB=0.D00
                    DO JJ=1,NSPEX(LS,MS)
                      BB=BB+PSF(JJ)
                    END DO
 !--BB is the number of reactions that can occur
                 
                    IEX=0
                    IF (BB > 0.5D00) THEN
                      CALL RANDOM_NUMBER(RANF)
                      BBB=RANF*BB
                      BB=0.D00
                      DO JJ=1,NSPEX(LS,MS)
                        IF (IEX == 0) THEN
                          BB=BB+PSF(JJ)
                          IF (BB > BBB) IEX=JJ
                        END IF   
                      END DO
                    END IF
               
                   
!
                    IF (IEX > 0) THEN    
!-- exchange or chain reaction occurs
                      JX=NEX(IEX,LS,MS)
                      TNEX(JX)=TNEX(JX)+1.D00                   
                      IF (SPEX(2,IEX,LS,MS) < 0.D00) THEN
                        TFOREX=TFOREX+1.D00
                      ELSE
                        TREVEX=TREVEX+1.D00
                      END IF
!                      WRITE (*,*) 'EXCHANGE',TFOREX,TREVEX
!                      WRITE (*,*)  IEX,L,M,LS,MS                                    
                      IPSP(L)=ISPEX(IEX,1,LS,MS)   !--L is now the new molecule that splits
                      IPSP(M)=ISPEX(IEX,2,LS,MS)
!--any additional vibrational modes must be set to zero                      
                      IVM=ISPV(LS)
                      NMC=IPSP(L)
                      NVM=ISPV(NMC)
                      IF (NVM > IVM) THEN
                        DO KV=IVM+1,NVM
                          IPVIB(KV,L)=0
                        END DO
                      END IF 
                      IVM=ISPV(MS)
                      NMC=IPSP(M)
                      NVM=ISPV(NMC)                        
                      IF (NVM > IVM) THEN
                        DO KV=IVM+1,NVM
                          IPVIB(KV,M)=0
                        END DO
                      END IF                        
!--put all pre-collision energies into the relative translational energy and adjust for the reaction energy                       
                      ECT=0.5D00*SPM(1,LS,MS)*VRR
                      IF (ISPR(1,LS) > 0) ECT=ECT+PROT(L)
                      IF (ISPV(LS) > 0) THEN
                        DO KV=1,ISPV(LS)
                          JI=IPVIB(KV,L)
                          IF (JI < 0) JI=-JI
                          IF (JI == 99999) JI=0                       
                          ECT=ECT+DFLOAT(JI)*BOLTZ*SPVM(1,KV,LS)
                        END DO
                      END IF
                      IF (ISPR(1,MS) > 0) ECT=ECT+PROT(M)
                      IF (ISPV(MS) > 0) THEN
                        DO KV=1,ISPV(MS)
                           JI=IPVIB(KV,M)
                          IF (JI < 0) JI=-JI
                          IF (JI == 99999) JI=0
                          ECT=ECT+DFLOAT(JI)*BOLTZ*SPVM(1,KV,MS)
                        END DO
                      END IF
                      ECT=ECT+SPEX(2,IEX,LS,MS)
                      IF (ECT < 0.) THEN
                        WRITE (*,*) '-VE ECT',ECT
                        WRITE (*,*) 'REACTION',JJ-1,' BETWEEN',LS,MS
                        STOP
                      END IF
                      IF (SPEX(2,IEX,LS,MS) < 0.D00) THEN  
                        TREACL(3,LS)=TREACL(3,LS)-1
                        TREACL(3,MS)=TREACL(3,MS)-1                     
                        LS=IPSP(L)
                        MS=IPSP(M)
                        TREACG(3,LS)=TREACG(3,LS)+1
                        TREACG(3,MS)=TREACG(3,MS)+1
                      ELSE
                        TREACL(4,LS)=TREACL(4,LS)-1
                        TREACL(4,MS)=TREACL(4,MS)-1                     
                        LS=IPSP(L)
                        MS=IPSP(M)
                        TREACG(4,LS)=TREACG(4,LS)+1
                        TREACG(4,MS)=TREACG(4,MS)+1
                      END IF  
                      RML=SPM(1,LS,MS)/SP(5,MS)
                      RMM=SPM(1,LS,MS)/SP(5,LS)   
  !--calculate the new VRR to match ECT using the new molecular masses
                      VRR=2.D00*ECT/SPM(1,LS,MS)
                      IF (ISPV(LS) > 0) THEN
                        DO KV=1,ISPV(LS)
                          IF (IPVIB(KV,L) < 0) THEN
                            IPVIB(KV,L)=-99999
                          ELSE  
                            IPVIB(KV,L)=0
                          END IF  
                        END DO
                      END IF
                      IF (ISPR(1,LS) > 0) PROT(L)=0.
                      IF (ISPV(MS) > 0) THEN
                        DO KV=1,ISPV(MS)
                          IF (IPVIB(KV,M) < 0) THEN
                            IPVIB(KV,M)=-99999
                          ELSE                        
                            IPVIB(KV,M)=0
                          END IF  
                        END DO
                      END IF
                      IF (ISPR(1,MS) > 0) PROT(M)=0.      
                    END IF
                  END IF 
                END IF   !--end of integrated reactions other than the deferred dissociation action in the DISSOCIATION subroutine
!
!--consider any reactions that are specified by rate equations                
                IKA=0    
                IF (MNRE.GT.0) THEN
                  CALL CHECK_REACTION(IKA,N,L,M,LS,MS,VRR,VR,VCM,RML,RMM)
                END IF
!
                IF (IREC == 0) THEN   !--recombined redistribution already made
!               
!--Larsen-Borgnakke serial redistribution 
                  ECT=0.5D00*SPM(1,LS,MS)*VRR
                  IDISS=0
                  DO NSP=1,2
                    IF (NSP == 1) THEN
                      K=L ; KS=LS ; JS=MS 
                    ELSE
                      K=M ; KS=MS ; JS=LS
                    END IF                                        
                    IF (MMVM > 0) THEN
                      IF (ISPV(KS) > 0) THEN
                        DO KV=1,ISPV(KS)
                          IF ((IPVIB(KV,K) >= 0).AND.(IDISS == 0)) THEN   !--do not redistribute to a dissociating molecule marked for removal 
                            EVIB=DFLOAT(IPVIB(KV,K))*BOLTZ*SPVM(1,KV,KS)                      
                            ECC=ECT+EVIB
                            IF (SPVM(3,KV,KS) > 0.) THEN
!--note quantizing of COLT in the following statements
                              MAXLEV=ECC/(BOLTZ*SPVM(1,KV,KS))
                              LIMLEV=SPVM(4,KV,KS)/SPVM(1,KV,KS)
                              IF ((MAXLEV > LIMLEV).AND.(JCD == 1).AND.(IEX == 0)) THEN
!--dissociation occurs  -  reflects the infinity of levels past the dissociation limit                       
                                IDISS=1
                                IV=IPVIB(KV,K)
                                ECC=ECC-BOLTZ*SPVM(4,KV,KS)   
                                MAXLEV=ECC/(BOLTZ*SPVM(1,KV,KS))
                              END IF            
                              IF (MAXLEV > LIMLEV) MAXLEV=LIMLEV
                              COLT=DFLOAT(MAXLEV)*SPVM(1,KV,KS)/(3.5D00-SPM(3,KS,JS))        !--quantized collision temperature
                              B=SPVM(4,KV,KS)/SPVM(3,KV,KS)    !Tdiss/Tref          
                              A=SPVM(4,KV,KS)/COLT       !Tdiss/T
                              ZV=(A**SPM(3,KS,JS))*(SPVM(2,KV,KS)*(B**(-SPM(3,KS,JS))))**(((A**0.3333333D00)-1.D00)/((B**0.33333D00)-1.D00))
                            ELSE
                              ZV=SPVM(2,KV,KS)
                            END IF
                            CALL RANDOM_NUMBER(RANF)
                            IF ((1.D00/ZV > RANF).OR.(IREC == 1).OR.(IEX > 0).OR.(IDISS > 0)) THEN  !-- a vibrational redistribution is always made after a chemical reaction 
                              IE=1          
                              II=0
                              DO WHILE (II == 0)
                                CALL RANDOM_NUMBER(RANF)
                                IV=RANF*(MAXLEV+0.99999999D00)
                                IPVIB(KV,K)=IV
                                EVIB=DFLOAT(IV)*BOLTZ*SPVM(1,KV,KS)
                                IF (EVIB < ECC) THEN
                                  PROB=(1.D00-EVIB/ECC)**(1.5D00-SPM(3,KS,JS))
 !--PROB is the probability ratio of eqn (5.61)
                                  CALL RANDOM_NUMBER(RANF)
                                  IF (PROB > RANF) II=1
                                END IF
                              END DO
                              ECT=ECC-EVIB
                              IF (IDISS == 1) THEN
                                IF (IPVIB(KV,K) > 0) THEN
                                  IPVIB(KV,K)=-IPVIB(KV,K)
                                ELSE
                                  IPVIB(KV,K)=-99999  
                                END IF
 !--a negative IPVIB marks a molecule for dissociation, but -99999 represents the ground state                                                             
                              END IF
                            END IF
                          END IF
                        END DO
                      END IF
                    END IF
!--now rotation of this molecule
                    IF (ISPR(1,KS).GT.0) THEN
                      IF ((ISPR(2,KS) == 0).AND.(ISPR(2,JS) == 0)) THEN
                        B=1.D00/SPM(7,KS,JS)
                      ELSE
                        COLT=ECC/((2.5D00-SP(3,KS))*BOLTZ)
                        B=1.D00/(SPR(1,KS)+SPR(2,KS)*COLT+SPR(3,KS)*COLT*COLT)
                      END IF
                      CALL RANDOM_NUMBER(RANF)
                      IF ((B > RANF).OR.(IKA /= 0).OR.(IREC == 1).OR.(IEX > 0)) THEN
                        IE=1
                        ECC=ECT+PROT(K)
                        IF (ISPR(1,KS) == 2) THEN
                          CALL RANDOM_NUMBER(RANF)
                          ERM=1.D00-RANF**(1.D00/(2.5D00-SPM(3,KS,JS)))  !eqn(5.46)
                        ELSE
                          CALL LBS(0.5D00*ISPR(1,KS)-1.D00,1.5D00-SPM(3,KS,JS),ERM)
                        END IF
                        PROT(K)=ERM*ECC
                        ECT=ECC-PROT(K)
                      END IF
                    END IF
                  END DO
!--adjust VR for the change in energy
                  VR=SQRT(2.D00*ECT/SPM(1,LS,MS))
                END IF
!--end of L-B redistribution
                IF (ABS(SPM(8,LS,MS)-1.) < 0.001) THEN
!--use the VHS logic
                  CALL RANDOM_NUMBER(RANF)
                  B=2.D00*RANF-1.D00
!--B is the cosine of a random elevation angle             
                  A=DSQRT(1.D00-B*B)
                  VRCP(1)=B*VR
                  CALL RANDOM_NUMBER(RANF)
                  C=2.D00*PI*RANF
!--C is a random azimuth angle
                  VRCP(2)=A*DCOS(C)*VR
                  VRCP(3)=A*DSIN(C)*VR
                ELSE
!--use the VSS logic
                  CALL RANDOM_NUMBER(RANF)
                  B=2.D00*(RANF**SP(4,1))-1.D00
!--B is the cosine of the deflection angle for the VSS model (eqn (11.8)
                  A=SQRT(1.D00-B*B)
                  CALL RANDOM_NUMBER(RANF)
                  C=2.D00*PI*RANF
                  OC=DCOS(C)
                  SD=DSIN(C)
                  D=SQRT(VRC(2)**2+VRC(3)**2)
                  VRCP(1)=B*VRC(1)+A*SD*D
                  VRCP(2)=B*VRC(2)+A*(VR*VRC(3)*OC-VRC(1)*VRC(2)*SD)/D
                  VRCP(3)=B*VRC(3)-A*(VR*VRC(2)*OC+VRC(1)*VRC(3)*SD)/D
!--the post-collision rel. velocity components are based on eqn (2.22)
                END IF
                DO KK=1,3
                  PV(KK,L)=VCM(KK)+RMM*VRCP(KK)
                  PV(KK,M)=VCM(KK)-RML*VRCP(KK)
                END DO
                IPCP(L)=M
                IPCP(M)=L
! 
              END IF     !--collision occurrence
            END IF
          END IF       !--separate simplegas / mixture coding 
        END DO  
      END IF    
    END IF      
  END IF        
END DO
!
!--remove any recombined atoms
!
DO N=1,NM
  IF (IPCELL(N) < 0)  CALL REMOVE_MOL(N)
END DO
!
!WRITE (*,*) 'END COLLISIONS'
!
RETURN
!
END SUBROUTINE COLLISIONS
!
!*****************************************************************************

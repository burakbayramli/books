C=============================================================
      PROGRAM LAPLACE
C=============================================================
C     THIS PROGRAM SETS UP THE MATRIX EQUATION [A]{F}={Q} WHICH
C     RESULTS FROM FINITE DIFFERENCE DISCRETIZATION OF THE
C     LAPLACE EQUATION USING UNIFORM GRID IN EACH DIRECTION AND
C     CENTRAL DIFFERENCE APPROXIMATION OF THE SECOND DERIVATIVES.
C     BOUNDARY CONDITIONS ARE OF DIRICHLET TYPE: T(0,X,Z) = 
C     T(X,0,Z) = T(X,Y,0)=0., T(1,Y,Z)=Y*Z, T(X,1,Z)=X*Z, 
C     T(X,Y,1)=X*Y. THE EXACT SOLUTION IS: T(X,Y,Z) = X*Y*Z.
C     THE EQUATION CAN THEN BE SOLVED BY A VARIETY OF ITERATIVE
C     SOLVERS.
C                         H. Leister, LSTM, Erlangen, 1992
C                         M. Peric,  IfS,  Hamburg,   1995
C     See Paper by Leister H.-J. and Peric M., Int. J. Numer. Meth.
C     Heat & Fluid Flow, Vol. 4, pp.  159-172 (1994)
C==============================================================
C
      PARAMETER (NX=21,NY=21,NZ=21)
      COMMON NI,NJ,NK,NIM,NJM,NKM,NIJ,NIJK,MAXIT,RESMAX,ALFA
c  Note: in case of 3D/1D index conversions, NI,NJ and NK must
c  be equal to the array dimensions NX,NY and NZ, respectively
c  (or vice versa). These are the numbers of nodes (including
c  boundary nodes) in each direction.
      COMMON /COEF/ AE(NX,NY,NZ),AW(NX,NY,NZ),AN(NX,NY,NZ),
     *       AS(NX,NY,NZ),AT(NX,NY,NZ),AB(NX,NY,NZ),
     *       AP(NX,NY,NZ),Q(NX,NY,NZ),T(NX,NY,NZ)
      DIMENSION X(NX),Y(NY),Z(NZ)
C
C.....INPUT DATA
C
      OPEN (UNIT=6,FILE='RESULT')
      REWIND 6
      PRINT *, ' ENTER: NI, NJ, NK, MAXIT, RESMAX '
      PRINT *, '        '
      READ(*,*) NI,NJ,NK,MAXIT,RESMAX
c  MAXIT is the limit on iterations in the solver (here we have
c  a linear problem - fixed coefficient matrix - and are doing 
c  only inner iterations)
      NIM=NI-1
      NJM=NJ-1
      NKM=NK-1
C
C.....DEFINE THE GRID
C
      DX=1./FLOAT(NIM)
      DY=1./FLOAT(NJM)
      DZ=1./FLOAT(NKM)
      X(1)=0.
      Y(1)=0.
      Z(1)=0.
      DO 10 I=2,NI
   10 X(I)=X(I-1)+DX
      DO 15 J=2,NJ
   15 Y(J)=Y(J-1)+DY
      DO 20 K=2,NK
   20 Z(K)=Z(K-1)+DZ
c  The grid is assumed here to be uniformly spaced in each direction
c  and these are then the coefficients needed to set up the matrix:
      DXR2=-1./DX**2
      DYR2=-1./DY**2
      DZR2=-1./DZ**2
      APC=-2.*(DXR2+DYR2+DZR2)
C
C.....INITIALIZE FIELD VALUES (DEFAULT ZERO!)
C
c  Only the boundary values are set here - the inner values are assumed 
c  to be zero (use compiler option to preset them - indefinite or infinite
c  won't work!).
      DO 25 K=1,NK
      DO 25 I=1,NI
   25 T(I,NJ,K)=X(I)*Z(K)
      DO 30 J=1,NJ
      DO 30 I=1,NI
   30 T(I,J,NK)=X(I)*Y(J)
      DO 40 K=1,NK
      DO 40 J=1,NJ
   40 T(NI,J,K)=Y(J)*Z(K)
C
C.....CALCULATE ELEMENTS OF MATRIX [A]
C
      DO 45 K=2,NKM
      DO 45 J=2,NJM
      DO 45 I=2,NIM
      AE(I,J,K)=DXR2
      AW(I,J,K)=DXR2
      AN(I,J,K)=DYR2
      AS(I,J,K)=DYR2
      AT(I,J,K)=DZR2
      AB(I,J,K)=DZR2
      AP(I,J,K)=APC
   45 CONTINUE
C
C.....ELIMINATE BOUNDARY VALUES (DIRICHLET B.C.)
C
c  Product of the coefficient and a fixed boundary value added to
c  the source term, and the coefficient then set to zero (not
c  necessarily required by the solver, but it is cleaner this way).
      DO 50 K=2,NKM
      DO 50 I=2,NIM
      Q(I,2,K)=Q(I,2,K)-AS(I,2,K)*T(I,1,K)
      AS(I,2,K)=0.
      Q(I,NJM,K)=Q(I,NJM,K)-AN(I,NJM,K)*T(I,NJ,K)
   50 AN(I,NJM,K)=0.
C
      DO 55 K=2,NKM
      DO 55 J=2,NJM
      Q(2,J,K)=Q(2,J,K)-AW(2,J,K)*T(1,J,K)
      AW(2,J,K)=0.
      Q(NIM,J,K)=Q(NIM,J,K)-AE(NIM,J,K)*T(NI,J,K)
   55 AE(NIM,J,K)=0.
C
      DO 65 J=2,NJM
      DO 65 I=2,NIM
      Q(I,J,2)=Q(I,J,2)-AB(I,J,2)*T(I,J,1)
      AB(I,J,2)=0.
      Q(I,J,NKM)=Q(I,J,NKM)-AT(I,J,NKM)*T(I,J,NK)
   65 AT(I,J,NKM)=0.
C
C.....SOLVE EQUATION SYSTEM
C
c  ALFA is an iteration parameter in the Stone's solver, which
c  should be around 0.92 (0 to 1, but above 0.95 iterations may
c  diverge, and 0 means standard ILU (incomplete lower-upper)
c  decomposition.
      PRINT *, ' ENTER: ALFA '
      PRINT *, '        '
      READ(*,*) ALFA
c  Routine VECINT calculates indices of nodes in diagonal planes,
c  so that vector processing is possible. This indirect addressing
c  is necessary, and is easily done for 1D arrays. This routine needs
c  be called only once upon starting the code; the order of nodes is
c  stored in an array and kept for future use.
      CALL VECINT
      CALL VECSIP(T)
c  VECSIP is the vectorized version of the 3D SIP solver which uses
c  1D array indexing. It solves for variable values at nodes between
c  (and including) 2 and NIM, NJM or NKM in each direction, with 1 and
c  NI, NJ and NK being boundary nodes which are assumed to either have
c  prescribed values or being eliminated through the boundary conditions
c  (expressed in terms of some prescribed laws or quantities and inner
c  nodal values).

C
C.....PRINT ERROR NORM AND SOLUTION FIELD
C
c  In this case we know the exact solution and can calculate the error
c  norm (and also check if the solver does the job properly).
      ERR=0.
      DO 80 K=2,NKM
      DO 80 J=2,NJM
      DO 80 I=2,NIM
      ERR=ERR+(T(I,J,K)-X(I)*Y(J)*Z(K))**2
   80 CONTINUE
      ERR=SQRT(ERR/FLOAT(NI*NJ*NK))
      WRITE(6,*) '     ERROR NORM:  ERR = ',ERR
c  Routine PRINT prints a 3D array in a form easy to read.
      CALL PRINT(T)
      STOP
      END
C
C##########################################
      SUBROUTINE PRINT(FI)
C##########################################
      PARAMETER (NX=21,NY=21,NZ=21)
      COMMON NI,NJ,NK,NIM,NJM,NKM,NIJ,NIJK,MAXIT,RESMAX,ALFA
      DIMENSION FI(NX,NY,NZ)
C
      WRITE(6,20) 
      DO 120 K=1,NK
      WRITE(6,*) '   PLANE  K  =',K
      WRITE(6,*) '   =========================='
      WRITE(6,*) '  '
      IS=-11
  100 IS=IS+12
      IE=IS+11
      IE=MIN0(NI,IE)
      WRITE(6,21) (I,I=IS,IE)
      WRITE(6,22)
      DO 101 J=NJ,1,-1
  101 WRITE(6,23) J,(FI(I,J,K),I=IS,IE)
      IF(IE.LT.NI) GO TO 100
      WRITE(6,*) '  '
  120 CONTINUE
   20 FORMAT(2X,26('*-'),7X,'  FI  ',7X,26('-*'))
   21 FORMAT(3X,'I = ',I3,11I10)
   22 FORMAT(2X,'J')
   23 FORMAT(1X,I3,1P12E10.2)
      RETURN
      END
C
C#############################################
      SUBROUTINE VECINT
C#############################################
      PARAMETER (NX=21,NY=21,NZ=21,NNXY=NX+NY,NNPL=NX+NY+NZ,
     *           NXYZ=NX*NY*NZ)
      COMMON NI,NJ,NK,NIM,NJM,NKM,NIJ,NIJK,MAXIT,RESMAX,ALFA
      COMMON /VEC/ IMJM,IMJMKM,ICL(NNPL),LM(NNPL),IJKV(NXYZ),
     *       LK(NZ),LJ(NY)
      DIMENSION IS(NNXY),IE(NNXY),JS(NNXY),JE(NNXY)
C      
C.....NUMBER OF DIAGONAL PLANES
C
      IMJM=NIM+NJM-3
c  This is the number of diagonals in a X*Y plane.
      IMJMKM=NIM+NJM+NKM-5
c  This is the total number of diagonal planes which need be scanned,
c  starting at (2,2,2) and ending at (NIM,NJM,NKM).
      NIJ=NI*NJ
      NIJK=NIJ*NK
      DO K=1,NK
      LK(K)=(K-1)*NIJ
c  Array LK(K) stores the number of nodes in all planes K=const. below
c  the K-th plane.
      END DO
      DO J=1,NJ
      LJ(J)=(J-1)*NI
      END DO
c  Array LJ(J) stores the number of nodes in a plane K=const, in all
c  lines J=const below the current one (J-th).
C
C.....LIMITS ON I AND J IN PLANE K=2
C
c  The diagonal sweeping starts at (2,2,2) node. In each diagonal plane,
c  I and J are within limits denoted by S for start and E for end.
      DO L=1,IMJM
      IF(L.LT.NJM) THEN
       IS(L)=2
       JE(L)=L+1
      ELSE
       IS(L)=IS(L-1)+1
       JE(L)=NJM
      ENDIF
      IF(L.LT.NIM) THEN
       JS(L)=2
       IE(L)=L+1
      ELSE
       JS(L)=JS(L-1)+1
       IE(L)=NIM
      ENDIF
      END DO
C
C.....NUMBER OF NODES PER SWEEPING PLANE
C
      DO L=1,IMJMKM
      KMIN=MAX0(L+2-IMJM,2)
      KMAX=MIN0(L+1,NKM)
c  Here are the limits for K
      M=MIN0(L,IMJM)
      LP=0
      DO K=KMIN,KMAX
      LP=LP+IE(M)-IS(M)+1
      M=M-1
      END DO
      LM(L)=LP
      END DO
c  LM(L) is the number of nodes in the L-th diagonal plane. For L=1,
c  LM(1)=1; LM(2)=3, LM(3)=6, etc. The planes are first triangular, then
c  they may go up to six edges, and end up again at LM(IMJMKM)=1.
C
C.....NUMBER OF NODES IN ALL PRECEEDING PLANES
C
      ICL(1)=0
      DO L=2,IMJMKM
      ICL(L)=ICL(L-1)+LM(L-1)
      END DO
c  ICL(L) is the total number of nodes in all planes before the L-th.
c  Note that we here do not count the boundary nodes - only those solved
c  for.
C
C.....I,J,K INDICES OF NODES IN EACH PLANE
C
      DO L=1,IMJMKM
      KMIN=MAX0(L+2-IMJM,2)
      KMAX=MIN0(L+1,NKM)
      M=MIN0(L,IMJM)
      MI=ICL(L)
      DO K=KMIN,KMAX
      MJ=0
      DO J=JS(M),JE(M)
      MI=MI+1
      I=IE(M)-MJ
      IJKV(MI)=LK(K)+LJ(J)+I
      MJ=MJ+1
      END DO
      M=M-1
      END DO
      END DO
c  Here indices of nodes in each diagonal plane are determined. Each plane
c  is swept from K=KMIN to KMAX, from south to north and from east to
c  west. In the L-th plane, its M-th node has the index  IJKV(ICL(L)+M),
c  ICL(L) are nodes in all planes up to the L-th, and we add M for the 
c  M-th node in the plane. In this order the nodes will be visited later.
C
      RETURN
      END
C
C#################################################
      SUBROUTINE VECSIP(FI)
C#################################################
      PARAMETER (NX=21,NY=21,NZ=21,NNXY=NX+NY,NNPL=NX+NY+NZ,
     *           NXY=NX*NY,NXYZ=NX*NY*NZ)
      COMMON NI,NJ,NK,NIM,NJM,NKM,NIJ,NIJK,MAXIT,RESMAX,ALFA
      COMMON /COEF/ AE(NX,NY,NZ),AW(NX,NY,NZ),AN(NX,NY,NZ),
     *       AS(NX,NY,NZ),AT(NX,NY,NZ),AB(NX,NY,NZ),
     *       AP(NX,NY,NZ),Q(NX,NY,NZ),T(NX,NY,NZ)
      REAL LB,LW,LS,LP
      DIMENSION UN(NXYZ),UE(NXYZ),UT(NXYZ),FI(NX,NY,NZ),LP(NXYZ), 
     *          LB(NXYZ),LW(NXYZ),LS(NXYZ),QE(NXYZ)
c  Ux and Lx are the coefficients of the upper and lower triangular
c  matrix in the Stone's solver (x stands for N,T,B,W,E,S,P). UN, UE
c  UT and RES (which is the residual vector) need be initialized; other
c  arrays need not be, as we use only those elements that are previously
c  calculated. Use compiler option instead explicit initialization
c  used here.
      DIMENSION EB(NXYZ),ES(NXYZ),EW(NXYZ),EP(NXYZ),EN(NXYZ),
     *          EE(NXYZ),ET(NXYZ),RES(NXYZ),FF(NXYZ),RES1(NXY)
c  Ex are 1D counterparts of Ax, elements of the coefficient matrix.
c  EQUIVALENCE statement is used to make these being identical and
c  avoiding increasing storage or copying. NOTE: only when NI=NX,
c  NJ=NY and NK=NZ will the mapping be exact - otherwise the solver
c  won't work (and you see it as it stops because of divisions with
c  zero). QE is a 1D counterpart of the source term Q.
      EQUIVALENCE (EB,AB),(ES,AS),(EW,AW),(EP,AP),(EN,AN),
     *          (EE,AE),(ET,AT),(Q,QE)
      COMMON /VEC/ IMJM,IMJMKM,ICL(NNPL),LM(NNPL),IJKV(NXYZ),
     *       LK(NZ),LJ(NY)
c  These variables are provided by the routine VECIND, which needs
c  be executed only once (while solver may be called many more times
c  if the matrix [A] changes as in the flow calculation).
C
C.....COPY VARIABLE INTO 1D ARRAY
C
      DO K=1,NK
      LKK=LK(K)
      DO J=1,NJ
      LJK=LKK+LJ(J)
      DO I=1,NI
      IJK=LJK+I
      FF(IJK)=FI(I,J,K)
      END DO
      END DO
      END DO
c  The dummy argument FI can not be put in EQUIVALENCE list, which is
c  why we have to copy FI to FF, its 1D counterpart.
C
C.....INITIALIZE ELEMENTS OF [U] AND {R}
C
c  Avoid this by using compiler option, or put the variables in COMMON
c  and initialize once for all; inner values will be recalculated, it
c  is only important that the boundary values are zero.
      DO IJK=1,NIJK
      UN(IJK)=0.
      UE(IJK)=0.
      UT(IJK)=0.
      RES(IJK)=0.
      END DO
C
C.....CALCULATE ELEMENTS OF [L] AND [U] MATRICES
C
c  CDIR$ IVDEP means Cray Directive "Ignore Vector Dependencies".
c  The compiler would not execute the loop below in vector mode, 
c  because of apparent data dependencies. But we know that all data
c  is available, and fortunately we can order vectorwise execution.
c  We go through L diagonal planes, over M nodes in each plane. Cray
c  will execute differently short vector loops (up to 64 elements) and
c  the long ones; the longer, the better - but unfortunately we have some
c  small ones at the begining and end of L-range.
      DO L=1,IMJMKM
      N=ICL(L)
CDIR$ IVDEP
      DO M=1,LM(L)
      IJK=IJKV(M+N)
      LB(IJK)=EB(IJK)/(1.+ALFA*(UN(IJK-NIJ)+UE(IJK-NIJ)))
      LW(IJK)=EW(IJK)/(1.+ALFA*(UN(IJK-1)+UT(IJK-1)))
      LS(IJK)=ES(IJK)/(1.+ALFA*(UE(IJK-NI)+UT(IJK-NI)))
      P1=ALFA*(LB(IJK)*UN(IJK-NIJ)+LW(IJK)*UN(IJK-1)) 
      P2=ALFA*(LB(IJK)*UE(IJK-NIJ)+LS(IJK)*UE(IJK-NI)) 
      P3=ALFA*(LW(IJK)*UT(IJK-1)+LS(IJK)*UT(IJK-NI)) 
      LP(IJK)=1./(EP(IJK)+P1+P2+P3-LB(IJK)*UT(IJK-NIJ)-
     *          LW(IJK)*UE(IJK-1)-LS(IJK)*UN(IJK-NI)+1.E-30)
      UN(IJK)=(EN(IJK)-P1)*LP(IJK) 
      UE(IJK)=(EE(IJK)-P2)*LP(IJK) 
      UT(IJK)=(ET(IJK)-P3)*LP(IJK) 
      END DO
      END DO
C
C.....ITERATION LOOP: Inner iterations (maximum MAXIT)
C
      DO ITER=1,MAXIT
      RESN=0.0
C
C.....CALCULATE RESIDUALS
C
c  Residuals are calculated for one X*Y plane, with vector length
c  (NIM-1)*(NJM-1), which is much more efficient than the sweeping used
c  above. Here we can do it any way as there are no conflicting data
c  dependencies - above we had no choice, that was the only possible
c  vector option. By switching this loop only from the above sweeping 
c  pattern to the one used here, efficiency of the whole routine increased
c  by full 50%! This is because here we have (NKM-1) vectors of constant
c  length - and they are all long, even for a 20x20x20 grid (400).
      DO K=2,NKM
      LKK=LK(K)
CDIR$ IVDEP
      DO IJ=NI+2,NIJ-NI-1
      IJK=LKK+IJ
      RES(IJK)=QE(IJK)-EP(IJK)*FF(IJK)-
     *           EE(IJK)*FF(IJK+1)-EW(IJK)*FF(IJK-1)-
     *           EN(IJK)*FF(IJK+NI)-ES(IJK)*FF(IJK-NI)-
     *           ET(IJK)*FF(IJK+NIJ)-EB(IJK)*FF(IJK-NIJ)
      RES1(IJ)=RES(IJK)
      END DO  
      DO IJ=NI+2,NIJ-NI-1
      RESN=RESN+ABS(RES1(IJ))
      END DO
      END DO
C
C.....FORWARD SUBSTITUTION
C
c  Forward substitution is done in the same order as the first loop
c  calculating elements of lower and upper triangular matrices. Note
c  that RES - residual vector - is overwritten.
      DO L=1,IMJMKM
      N=ICL(L)
CDIR$ IVDEP
      DO M=N+1,N+LM(L)
      IJK=IJKV(M)
      RES(IJK)=(RES(IJK)-LB(IJK)*RES(IJK-NIJ)-LW(IJK)*
     *            RES(IJK-1)-LS(IJK)*RES(IJK-NI))*LP(IJK)
      END DO
      END DO
      IF(ITER.EQ.1) RESNOR=RESN
      RSM=RESN/(RESNOR+1.E-20)
c  Residual sums are normalized by the initial one, to determine
c  the level of residual reduction. 5 orders of magnitude (RSM=1.E-50
c  usually means five digit accuracy, if zero values are used as the
c  initial guess.
C
C.....BACKWARD SUBSTITUTION
C      
c  Backward substitution is done in the reverse order from (NIM,NJM,NKM)
c  to (2,2,2).
      DO L=IMJMKM,1,-1
      N=ICL(L)
CDIR$ IVDEP
      DO M=N+1,N+LM(L)
      IJK=IJKV(M)
      RES(IJK)=RES(IJK)-UN(IJK)*RES(IJK+NI)-UE(IJK)*
     *           RES(IJK+1)-UT(IJK)*RES(IJK+NIJ)
      END DO
      END DO
C
C.....VARIABLE UPDATE
C
c  The 1D variable field is updated efficiently using very long vector
c  (on Cray, we could simply say FF = FF + RES for full vector sumation,
c  but I wanted code to run on a serial computer too).
      DO IJK=NIJ+NI+2,NIJK-NIJ-NI-1
      FF(IJK)=FF(IJK)+RES(IJK)
      END DO
c  Now the field FF must be copied to FI, so that it is appropriately
c  available in the rest of the code which uses 3D indexing.
      DO K=2,NKM
      LKK=LK(K)
      DO J=2,NJM
      LJK=LKK+LJ(J)
      DO I=2,NI
      IJK=LJK+I
      FI(I,J,K)=FF(IJK)
      END DO
      END DO
      END DO
C
C.....CONVERGENCE CHECK AND PRINTOUT
C
      WRITE(6,*) '   ', ITER,' INNER ITER., RSM =',RSM
c  RESMAX can be chosen for desired accuracy - 1.E-4 for 4 digit accuracy,
c  for example.
      IF(RSM.LT.RESMAX) RETURN
      END DO
      RETURN
      END 


c This file contains:

c (i) The COLROW software package: J. C. Díaz, G. Fairweather and 
c P. Keast, Algorithm 603: COLROW and ARCECO: FORTRAN Packages for 
c Solving Certain Almost Block Diagonal Linear Systems by Modified 
c Alternate Row and Column Elimination, ACM Transactions on 
c Mathematical Software, Volume 9,  Issue 3, (September 1983),
c Pages: 376-380, 1983, ISSN:0098-3500.
c This software has been published in the Collected Algorithms of
c the Association for Computing Machinery (ACM) calgo.acm.org and
c is thus subject to ACM Software Copyright and License Agreement
c www.acm.org/pubs/copyright_policy/softwareCRnotice.html.
c We have modified this source code slightly: declarations like 
c array(1) were changed to array(*).

c (ii) From the LINPACK (www.netlib.org/linpack/index.html)numerical 
c software collection: dsvdc (www.netlib.org/linpack/dsvdc.f).
c We have modified this source code slightly: declarations like 
c array(1) were changed to array(*). The LINPACK website does not
c provide any comment on legal restrictions associated with the
c use of this software.

c (iii) From the BLAS (www.netlib.org/blas/) numerical software 
c collection: daxpy,ddot,dscal,dswap,dnrm2,drotg. These routines 
c are subject to the following legal restrictions: 
c www.netlib.org/blas/faq.html#2


c--------------------------------------------------------------
       SUBROUTINE COLROW(N,TOPBLK,NRWTOP,NOVRLP,ARRAY,NRWBLK,
     *             NCLBLK,NBLOKS,BOTBLK,NRWBOT,PIVOT,B,X,IFLAG)
C
C***************************************************************
C
C  THIS PROGRAM SOLVES THE LINEAR SYSTEM  A*X = B  WHERE  A IS
C  AN ALMOST BLOCK DIAGONAL MATRIX OF THE FORM
C
C               TOPBLK
C               ARRAY(1)
C                     ARRAY(2)
C                          .
C                             .
C                                .
C                                   .
C                                    ARRAY(NBLOKS)
C                                           BOTBLK
C
C  WHERE
C           TOPBLK IS  NRWTOP  BY NOVRLP
C           ARRAY(K), K=1,NBLOKS, ARE NRWBLK BY NRWBLK+NOVRLP
C           BOTBLK IS NRWBOT BY NOVRLP,
C  AND
C           NOVRLP = NRWTOP + NRWBOT
C  WITH
C           NOVRLP.LE.NRWBLK .
C
C  THE LINEAR SYSTEM IS OF ORDER  N = NBLOKS*NRWBLK + NOVRLP.
C
C  THE METHOD IMPLEMENTED IS BASED ON GAUSS ELIMINATION WITH
C  ALTERNATE ROW AND COLUMN ELIMINATION WITH PARTIAL PIVOTING,
C  WHICH PRODUCES A STABLE DECOMPOSITION OF THE MATRIX  A
C  WITHOUT INTRODUCING FILL-IN.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  TO OBTAIN A SINGLE PRECISION VERSION OF THIS PACKAGE, REMOVE
C  ALL DOUBLE PRECISION STATEMENTS.  THERE IS ONE SUCH STATEMENT
C  IN C O L R O W, THREE IN C R D C M P, AND TWO IN C R S O L V.
C  IN ADDITION, REFERENCES TO BUILT-IN FUNCTIONS DABS AND DMAX1
C  MUST BE REPLACED BY ABS AND AMAX1, RESPECTIVELY.  DABS OCCURS
C  NINE TIMES, IN C R D C M P.  DMAX1 OCCURS FOUR TIMES, IN
C  C R D C M P.  FINALLY, ZERO IS INITIALISED TO 0.D0 IN A
C  DATA STATEMENT IN C R D C M P.  THIS MUST BE REPLACED BY:
C               DATA ZERO/0.0/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               *****  PARAMETERS  *****
C
C       *** ON ENTRY ...
C
C               N      - INTEGER
C                         THE ORDER OF THE LINEAR SYSTEM,
C                         GIVEN BY NBLOKS*NRWBLK + NOVRLP
C
C               TOPBLK - DOUBLE PRECISION(NRWTOP,NOVRLP)
C                         THE FIRST BLOCK OF THE ALMOST BLOCK
C                         DIAGONAL MATRIX A
C
C               NRWTOP - INTEGER
C                         NUMBER OF ROWS IN THE BLOCK TOPBLK
C
C               NOVRLP - INTEGER
C                         THE NUMBER OF COLUMNS IN WHICH SUCC-
C                         ESSIVE BLOCKS OVERLAP, WHERE
C                                NOVRLP = NRWTOP + NRWBOT
C
C               ARRAY  - DOUBLE PRECISION(NRWBLK,NCLBLK,NBLOKS)
C                         ARRAY(,,K) CONTAINS THE K-TH NRWBLK
C                         BY NCLBLK BLOCK OF THE MATRIX A
C
C               NRWBLK - INTEGER
C                         NUMBER OF ROWS IN K-TH BLOCK
C
C               NCLBLK - INTEGER
C                         NUMBER OF COLUMNS IN K-TH BLOCK
C
C               NBLOKS - INTEGER
C                         NUMBER OF NRWBLK BY NCLBLK BLOCKS IN
C                         THE MATRIX A
C
C               BOTBLK - DOUBLE PRECISION(NRWBOT,NOVRLP)
C                         THE LAST BLOCK OF THE MATRIX A
C
C               NRWBOT - INTEGER
C                         NUMBER OF ROWS IN THE BLOCK BOTBLK
C
C                PIVOT - INTEGER(N)
C                         WORK SPACE
C
C                    B - DOUBLE PRECISION(N)
C                         THE RIGHT HAND SIDE VECTOR
C
C                    X - DOUBLE PRECISION(N)
C                         WORK SPACE
C
C       *** ON RETURN  ...
C
C               TOPBLK,ARRAY,BOTBLK - ARRAYS CONTAINING THE
C                        DESIRED DECOMPOSITION OF THE MATRIX A
C                        (IF IFLAG = 0)
C
C                PIVOT - INTEGER(N)
C                         RECORDS THE PIVOTING INDICES DETER-
C                         MINED IN THE DECOMPOSITION
C
C                    X - DOUBLE PRECISION(N)
C                         THE SOLUTION VECTOR (IF IFLAG = 0)
C
C               IFLAG  - INTEGER
C                         =  1, IF INPUT PARAMETERS ARE INVALID
C                         = -1, IF MATRIX IS SINGULAR
C                         =  0, OTHERWISE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               *****  AUXILIARY PROGRAMS  *****
C
C       CRDCMP(N,TOPBLK,NRWTOP,NOVRLP,ARRAY,NRWBLK,NCLBLK,NBLOKS,
C    *     BOTBLK,NRWBOT,PIVOT,IFLAG)
C            - DECOMPOSES THE MATRIX  A  USING MODIFIED
C              ALTERNATE ROW AND COLUMN ELIMINATON WITH
C              PARTIAL PIVOTING, AND IS USED FOR THIS
C              PURPOSE IN C O L R O W.
C              THE ARGUMENTS ARE AS IN C O L R O W.
C
C       CRSLVE(N,TOPBLK,NRWTOP,NOVRLP,ARRAY,NRWBLK,NCLBLK,NBLOKS,
C    *     BOTBLK,NRWBOT,PIVOT,B,X)
C            - SOLVES THE SYSTEM A*X = B ONCE A IS DECOMPOSED.
C              THE ARGUMENTS ARE ALLAS IN C O L R O W.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       THE SUBROUTINE  C O L R O W  AUTOMATICALLY SOLVES THE
C  INPUT SYSTEM WHEN IFLAG=0.  C O L R O W  IS CALLED ONLY ONCE
C  FOR A GIVEN SYSTEM. THE SOLUTION FOR A SEQUENCE OF P RIGHT
C  HAND SIDES CAN BE OBTAINED BY ONE CALL TO  C O L R O W  AND
C  P-1 CALLS TO CRSLVE ONLY. SINCE THE ARRAYS TOPBLK,ARRAY,
C  BOTBLK AND PIVOT CONTAIN THE DECOMPOSITION OF THE GIVEN
C  COEFFICIENT MATRIX AND PIVOTING INFORMATION ON RETURN FROM
C  C O L R O W , THEY MUST NOT BE ALTERED BETWEEN SUCCESSIVE
C  CALLS TO CRSLVE WITH THE SAME LEFT HAND SIDES. FOR THE
C  SAME REASON, IF THE USER WISHES TO SAVE THE COEFFICIENT
C  MATRIX, THE ARRAYS TOPBLK,ARRAY,BOTBLK MUST BE COPIED
C  BEFORE A CALL TO  C O L R O W .
C
C*************************************************************************
C
C             *****  SAMPLE CALLING PROGRAM  *****
C
C      THE FOLLOWING PROGRAM WILL EXERCISE COLROW, IN THE
C      CASE WHEN THE COEFFICIENT MATRIX IS NON-SINGULAR.
C
C       DOUBLE PRECISION TOP,AR,BOT,B,X
C       DOUBLE PRECISION ERROR,ERR
C       DIMENSION TOP(2,4),AR(4,8,2),BOT(2,4),B(12),X(12)
C       INTEGER PIVOT(12)
C       DATA N,NRWTOP,NOVRLP,NRWBLK,NCLBLK,NBLOKS,NRWBOT/12,2,4,4,8,2,2/
C       DATA TOP(1,1),TOP(1,2),TOP(1,3),TOP(1,4),
C    *       TOP(2,1),TOP(2,2),TOP(2,3),TOP(2,4)/
C    *0.0 D0,-0.98D0,-0.79D0,-0.15D0,
C    *-1.00D0, 0.25D0,-0.87D0, 0.35D0/
C       DATA AR(1,1,1),AR(1,2,1),AR(1,3,1),AR(1,4,1),
C    *       AR(1,5,1),AR(1,6,1),AR(1,7,1),AR(1,8,1)/
C    *0.78D0, 0.31D0,-0.85D0, 0.89D0,-0.69D0,-0.98D0,-0.76D0,-0.82D0/
C       DATA AR(2,1,1),AR(2,2,1),AR(2,3,1),AR(2,4,1),
C    *       AR(2,5,1),AR(2,6,1),AR(2,7,1),AR(2,8,1)/
C    *0.12D0,-0.01D0, 0.75D0, 0.32D0,-1.00D0,-0.53D0,-0.83D0,-0.98D0/
C       DATA AR(3,1,1),AR(3,2,1),AR(3,3,1),AR(3,4,1),
C    *       AR(3,5,1),AR(3,6,1),AR(3,7,1),AR(3,8,1)/
C    *-0.58D0, 0.04D0, 0.87D0, 0.38D0,-1.00D0,-0.21D0,-0.93D0,-0.84D0/
C       DATA AR(4,1,1),AR(4,2,1),AR(4,3,1),AR(4,4,1),
C    *       AR(4,5,1),AR(4,6,1),AR(4,7,1),AR(4,8,1)/
C    *-0.21D0,-0.91D0,-0.09D0,-0.62D0,-1.99D0,-1.12D0,-1.21D0, 0.07D0/
C       DATA AR(1,1,2),AR(1,2,2),AR(1,3,2),AR(1,4,2),
C    *       AR(1,5,2),AR(1,6,2),AR(1,7,2),AR(1,8,2)/
C    *0.78D0,-0.93D0,-0.76D0, 0.48D0,-0.87D0,-0.14D0,-1.00D0,-0.59D0/
C       DATA AR(2,1,2),AR(2,2,2),AR(2,3,2),AR(2,4,2),
C    *       AR(2,5,2),AR(2,6,2),AR(2,7,2),AR(2,8,2)/
C    *-0.99D0, 0.21D0,-0.73D0,-0.48D0,-0.93D0,-0.91D0, 0.10D0,-0.89D0/
C       DATA AR(3,1,2),AR(3,2,2),AR(3,3,2),AR(3,4,2),
C    *       AR(3,5,2),AR(3,6,2),AR(3,7,2),AR(3,8,2)/
C    *-0.68D0,-0.09D0,-0.58D0,-0.21D0, 0.85D0,-0.39D0, 0.79D0,-0.71D0/
C       DATA AR(4,1,2),AR(4,2,2),AR(4,3,2),AR(4,4,2),
C    *       AR(4,5,2),AR(4,6,2),AR(4,7,2),AR(4,8,2)/
C    *0.39D0,-0.99D0,-0.12D0,-0.75D0,-0.68D0,-0.99D0, 0.50D0,-0.88D0/
C       DATA BOT(1,1),BOT(1,2),BOT(1,3),BOT(1,4),
C    *       BOT(2,1),BOT(2,2),BOT(2,3),BOT(2,4)/
C    *0.71D0,-0.64D0, 0.0 D0, 0.48D0,
C    *0.08D0,100.0D0,50.00D0,15.00D0/
C       DATA B(1),B(2),B(3),B(4),B(5),B(6),B(7),B(8),B(9),B(10),B(11),
C    *       B(12)/
C    *-1.92D0,-1.27D0,-2.12D0,-2.16D0,-2.27D0,-6.08D0,-3.03D0,
C    *-4.62D0,-1.02D0,-3.52D0,.55D0,165.08D0/
C
C*************************************************************************
C
C   THE INPUT MATRIX IS GIVEN BY:
C
C  0.0  -0.98 -0.79 -0.15
C -1.00  0.25 -0.87  0.35
C  0.78  0.31 -0.85  0.89 -0.69 -0.98 -0.76 -0.82
C  0.12 -0.01  0.75  0.32 -1.00 -0.53 -0.83 -0.98
C -0.58  0.04  0.87  0.38 -1.00 -0.21 -0.93 -0.84
C -0.21 -0.91 -0.09 -0.62 -1.99 -1.12 -1.21  0.07
C                          0.78 -0.93 -0.76  0.48 -0.87 -0.14 -1.00 -0.59
C                         -0.99  0.21 -0.73 -0.48 -0.93 -0.91  0.10 -0.89
C                         -0.68 -0.09 -0.58 -0.21  0.85 -0.39  0.79 -0.71
C                          0.39 -0.99 -0.12 -0.75 -0.68 -0.99  0.50 -0.88
C                                                  0.71 -0.64  0.0   0.48
C                                                  0.08 100.0 50.00 15.00
C
C       THE RIGHT HAND SIDE IS GIVEN BY:
C
C         B = (-1.92,-1.27,-2.12,-2.16,-2.27,-6.08,-3.03,-4.62,
C              -1.02,-3.52,0.55,165.08)
C
C       THE SOLUTION OF THIS SYSTEM IS GIVEN BY;
C
C          X = (1,1,1,1,1,1,1,1,1,1,1,1)
C
C*************************************************************************
C
C       CALL COLROW(N,TOP,NRWTOP,NOVRLP,AR,NRWBLK,NCLBLK,NBLOKS,
C    *              BOT,NRWBOT,PIVOT,B,X,IFLAG)
C       IF(IFLAG.NE.0)GO TO 1000
C       ERROR = 0.D0
C       DO 10 I=1,N
C          ERR = 1.D0 - X(I)
C          ERROR = DMAX1(ERROR,DABS(ERR))
C          WRITE(6,100)X(I),ERR
C  10   CONTINUE
C       WRITE(6,200)ERROR
C 200   FORMAT(12H MAX ERROR = ,D15.7)
C 100   FORMAT(1H ,F15.7,D15.7)
C       RETURN
C1000   CONTINUE
C       WRITE(6,300)IFLAG
C 300   FORMAT(9H IFLAG =  ,I3)
C       RETURN
C       END
C
C***************************************************************
C
        DOUBLE PRECISION TOPBLK,ARRAY,BOTBLK,B,X
        INTEGER IFLAG, NRWBOT, NBLOKS, NCLBLK, NRWBLK, NOVRLP, NRWTOP, N
        INTEGER PIVOT(*)
        DIMENSION TOPBLK(NRWTOP,*),ARRAY(NRWBLK,NCLBLK,*),
     *          BOTBLK(NRWBOT,*),B(*),X(*)
        EXTERNAL CRDCMP, CRSLVE
        CALL CRDCMP(N,TOPBLK,NRWTOP,NOVRLP,ARRAY,NRWBLK,NCLBLK,NBLOKS,
     *          BOTBLK,NRWBOT,PIVOT,IFLAG)
        IF(IFLAG.NE.0)RETURN
        CALL CRSLVE(TOPBLK,NRWTOP,NOVRLP,ARRAY,NRWBLK,NCLBLK,NBLOKS,
     *          BOTBLK,NRWBOT,PIVOT,B,X)
        RETURN
        END
c---------------------------------------------------------------
        SUBROUTINE CRDCMP(N,TOPBLK,NRWTOP,NOVRLP,ARRAY,NRWBLK,
     *             NCLBLK,NBLOKS,BOTBLK,NRWBOT,PIVOT,IFLAG)
C
C***************************************************************
C
C  C R D C M P DECOMPOSES THE ALMOST BLOCK DIAGONAL MATRIX A
C  USING MODIFIED ALTERNATE ROW AND COLUMN ELIMINATION WITH
C  PARTIAL PIVOTING.  THE MATRIX  A  IS STORED IN THE ARRAYS
C  TOPBLK, ARRAY, AND BOTBLK.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               *****  PARAMETERS  *****
C
C       *** ON ENTRY ...
C
C               N      - INTEGER
C                         THE ORDER OF THE LINEAR SYSTEM,
C                         GIVEN BY NBLOKS*NRWBLK + NOVRLP
C
C               TOPBLK - DOUBLE PRECISION(NRWTOP,NOVRLP)
C                         THE FIRST BLOCK OF THE ALMOST BLOCK
C                         DIAGONAL MATRIX A TO BE DECOMPOSED
C
C               NRWTOP - INTEGER
C                         NUMBER OF ROWS IN THE BLOCK TOPBLK
C
C               NOVRLP - INTEGER
C                         THE NUMBER OF COLUMNS IN WHICH SUCC-
C                         ESSIVE BLOCKS OVERLAP, WHERE
C                                NOVRLP = NRWTOP + NRWBOT
C
C               ARRAY  - DOUBLE PRECISION(NRWBLK,NCLBLK,NBLOKS)
C                         ARRAY(,,K) CONTAINS THE K-TH NRWBLK
C                         BY NCLBLK BLOCK OF THE MATRIX A
C
C               NRWBLK - INTEGER
C                         NUMBER OF ROWS IN K-TH BLOCK
C
C               NCLBLK - INTEGER
C                         NUMBER OF COLUMNS IN K-TH BLOCK
C
C               NBLOKS - INTEGER
C                         NUMBER OF NRWBLK BY NCLBLK BLOCKS IN
C                         THE MATRIX A
C
C               BOTBLK - DOUBLE PRECISION(NRWBOT,NOVRLP)
C                         THE LAST BLOCK OF THE MATRIX A
C
C               NRWBOT - INTEGER
C                         NUMBER OF ROWS IN THE BLOCK BOTBLK
C
C                PIVOT - INTEGER(N)
C                         WORK SPACE
C
C       *** ON RETURN  ...
C
C               TOPBLK,ARRAY,BOTBLK - ARRAYS CONTAINING THE
C                        DESIRED DECOMPOSITION OF THE MATRIX A
C                        (IF IFLAG = 0)
C
C                PIVOT - INTEGER(N)
C                         RECORDS THE PIVOTING INDICES DETER-
C                         MINED IN THE DECOMPOSITION
C
C               IFLAG  - INTEGER
C                         =  1, IF INPUT PARAMETERS ARE INVALID
C                         = -1, IF MATRIX IS SINGULAR
C                         =  0, OTHERWISE
C
C***************************************************************
C
        INTEGER IFLAG, NRWBOT, NBLOKS, NCLBLK, NRWBLK, NOVRLP, NRWTOP, N
        INTEGER JRWBLK, IPVBLK, IRWBLK, INCRN, IPLUSN, INCRJ, LOOP
        INTEGER JMINN, JPLUS1, KPLUS1, K, INCR, L, J, IPVT, IPLUS1
        INTEGER I, NVRLP0, NRWEL1, NROWEL, NRWTP1 
        DOUBLE PRECISION TOPBLK,ARRAY,BOTBLK
        DOUBLE PRECISION ROWMAX,ROWPIV,ROWMLT,COLMAX,COLPIV
        DOUBLE PRECISION SWAP,COLMLT,PIVMAX,ZERO,TEMPIV
        INTEGER PIVOT(*)
        DIMENSION TOPBLK(NRWTOP,*),ARRAY(NRWBLK,NCLBLK,*),
     *          BOTBLK(NRWBOT,*)
        DATA ZERO/0.0D0/
C
C***************************************************************
C
C          ****  DEFINE THE CONSTANTS USED THROUGHOUT  ****
C
C***************************************************************
C
        IFLAG = 0
        PIVMAX = ZERO
        NRWTP1 = NRWTOP+1
        NROWEL = NRWBLK-NRWTOP
        NRWEL1 = NROWEL+1
        NVRLP0 = NOVRLP-1
C
C***************************************************************
C
C          ****  CHECK VALIDITY OF THE INPUT PARAMETERS....
C
C               IF PARAMETERS ARE INVALID THEN TERMINATE AT 10;
C                                         ELSE CONTINUE AT 100.
C
C***************************************************************
C
        IF(N.NE.NBLOKS*NRWBLK+NOVRLP)GO TO 10
        IF(NOVRLP.NE.NRWTOP+NRWBOT)GO TO 10
        IF(NCLBLK.NE.NOVRLP+NRWBLK)GO TO 10
        IF(NOVRLP.GT.NRWBLK)GO TO 10
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          PARAMETERS ARE ACCEPTABLE - CONTINUE AT 100.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        GO TO 100
10      CONTINUE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          PARAMETERS ARE INVALID.  SET IFLAG = 1, AND TERMINATE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        IFLAG = 1
        RETURN
100     CONTINUE
C
C***************************************************************
C
C               ****  FIRST, IN TOPBLK....
C
C***************************************************************
C
C          ***  APPLY NRWTOP COLUMN ELIMINATIONS WITH COLUMN
C                 PIVOTING ....
C
C***************************************************************
C
        DO 190 I = 1,NRWTOP
           IPLUS1 = I+1
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               DETERMINE COLUMN PIVOT AND PIVOT INDEX
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           IPVT = I
           COLMAX = DABS(TOPBLK(I,I))
           DO 110 J = IPLUS1,NOVRLP
              TEMPIV = DABS(TOPBLK(I,J))
              IF(TEMPIV.LE.COLMAX)GO TO 110
                 IPVT = J
                 COLMAX = TEMPIV
110        CONTINUE
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               TEST FOR SINGULARITY:
C
C                       IF SINGULAR THEN TERMINATE AT 1000;
C                                   ELSE CONTINUE.
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           IF(PIVMAX+COLMAX.EQ.PIVMAX)GO TO 1000
           PIVMAX = DMAX1(COLMAX,PIVMAX)
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               IF NECESSARY INTERCHANGE COLUMNS
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           PIVOT(I) = IPVT
           IF(IPVT.EQ.I)GO TO 140
              DO 120 L = I,NRWTOP
                 SWAP = TOPBLK(L,IPVT)
                 TOPBLK(L,IPVT) = TOPBLK(L,I)
                 TOPBLK(L,I) = SWAP
120           CONTINUE
              DO 130 L = 1,NRWBLK
                 SWAP = ARRAY(L,IPVT,1)
                 ARRAY(L,IPVT,1) = ARRAY(L,I,1)
                 ARRAY(L,I,1) = SWAP
130           CONTINUE
140        CONTINUE
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               COMPUTE MULTIPLIERS AND PERFORM COLUMN
C                       ELIMINATION
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           COLPIV = TOPBLK(I,I)
           DO 180 J = IPLUS1,NOVRLP
              COLMLT = TOPBLK(I,J)/COLPIV
              TOPBLK(I,J) = COLMLT
              IF(IPLUS1.GT.NRWTOP)GO TO 160
                 DO 150 L = IPLUS1,NRWTOP
                    TOPBLK(L,J) = TOPBLK(L,J)-COLMLT*TOPBLK(L,I)
150              CONTINUE
160           CONTINUE
              DO 170 L = 1,NRWBLK
                 ARRAY(L,J,1) = ARRAY(L,J,1)-COLMLT*ARRAY(L,I,1)
170           CONTINUE
180        CONTINUE
190     CONTINUE
C
C***************************************************************
C
C          ****  IN EACH BLOCK ARRAY(,,K)....
C
C***************************************************************
C
        INCR = 0
        DO 395 K = 1,NBLOKS
           KPLUS1 = K+1
C
C          *****************************************************
C
C          ***  FIRST APPLY NRWBLK-NRWTOP ROW ELIMINATIONS WITH
C                       ROW PIVOTING....
C
C          *****************************************************
C
           DO 270 J = NRWTP1,NRWBLK
              JPLUS1 = J+1
              JMINN = J-NRWTOP
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               DETERMINE ROW PIVOT AND PIVOT INDEX
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              IPVT = JMINN
              ROWMAX = DABS(ARRAY(JMINN,J,K))
              LOOP = JMINN+1
              DO 210 I = LOOP,NRWBLK
                 TEMPIV = DABS(ARRAY(I,J,K))
                 IF(TEMPIV.LE.ROWMAX)GO TO 210
                 IPVT = I
                 ROWMAX = TEMPIV
210           CONTINUE
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               TEST FOR SINGULARITY:
C
C                       IF SINGULAR THEN TERMINATE AT 1000;
C                                   ELSE CONTINUE.
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              IF(PIVMAX+ROWMAX.EQ.PIVMAX)GO TO  1000
              PIVMAX = DMAX1(ROWMAX,PIVMAX)
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               IF NECESSARY INTERCHANGE ROWS
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              INCRJ = INCR+J
              PIVOT(INCRJ) = INCR+IPVT+NRWTOP
              IF(IPVT.EQ.JMINN)GO TO 230
                 DO 220 L = J,NCLBLK
                    SWAP = ARRAY(IPVT,L,K)
                    ARRAY(IPVT,L,K) = ARRAY(JMINN,L,K)
                    ARRAY(JMINN,L,K) = SWAP
220              CONTINUE
230           CONTINUE
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               COMPUTE MULTIPLERS
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              ROWPIV = ARRAY(JMINN,J,K)
              DO 240 I = LOOP,NRWBLK
                 ARRAY(I,J,K) = ARRAY(I,J,K)/ROWPIV
240           CONTINUE
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               PERFORM ROW ELIMINATION WITH COLUMN INDEXING
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              DO 260 L = JPLUS1,NCLBLK
                 ROWMLT = ARRAY(JMINN,L,K)
                 DO 250 I = LOOP,NRWBLK
                    ARRAY(I,L,K) = ARRAY(I,L,K)
     *                                -ROWMLT*ARRAY(I,J,K)
250              CONTINUE
260           CONTINUE
270        CONTINUE
C
C          *****************************************************
C
C          ***  NOW APPLY NRWTOP COLUMN ELIMINATIONS WITH
C                      COLUMN PIVOTING....
C
C          *****************************************************
C
           DO 390 I = NRWEL1,NRWBLK
              IPLUSN = I+NRWTOP
              IPLUS1 = I+1
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               DETERMINE COLUMN PIVOT AND PIVOT INDEX
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              IPVT = IPLUSN
              COLMAX = DABS(ARRAY(I,IPVT,K))
              LOOP = IPLUSN+1
              DO 310 J = LOOP,NCLBLK
                 TEMPIV = DABS(ARRAY(I,J,K))
                 IF(TEMPIV.LE.COLMAX)GO TO 310
                 IPVT = J
                 COLMAX = TEMPIV
310           CONTINUE
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               TEST FOR SINGULARITY:
C
C                       IF SINGULAR THEN TERMINATE AT 1000;
C                                   ELSE CONTINUE.
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              IF(PIVMAX+COLMAX.EQ.PIVMAX)GO TO 1000
              PIVMAX = DMAX1(COLMAX,PIVMAX)
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               IF NECESSARY INTERCHANGE COLUMNS
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              INCRN = INCR+IPLUSN
              PIVOT(INCRN) = INCR+IPVT
              IRWBLK = IPLUSN-NRWBLK
              IF(IPVT.EQ.IPLUSN)GO TO 340
                 DO 315 L = I,NRWBLK
                    SWAP = ARRAY(L,IPVT,K)
                    ARRAY(L,IPVT,K) = ARRAY(L,IPLUSN,K)
                    ARRAY(L,IPLUSN,K) = SWAP
315              CONTINUE
                 IPVBLK = IPVT-NRWBLK
                 IF(K.EQ.NBLOKS)GO TO 330
                    DO 320 L = 1,NRWBLK
                       SWAP = ARRAY(L,IPVBLK,KPLUS1)
                       ARRAY(L,IPVBLK,KPLUS1)
     *                                 = ARRAY(L,IRWBLK,KPLUS1)
                       ARRAY(L,IRWBLK,KPLUS1) = SWAP
320                 CONTINUE
                    GO TO 340
330              CONTINUE
                 DO 335 L = 1,NRWBOT
                    SWAP = BOTBLK(L,IPVBLK)
                    BOTBLK(L,IPVBLK) = BOTBLK(L,IRWBLK)
                    BOTBLK(L,IRWBLK) = SWAP
335              CONTINUE
340           CONTINUE
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               COMPUTE MULTIPLIERS AND PERFORM COLUMN
C                       ELIMINATION
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              COLPIV = ARRAY(I,IPLUSN,K)
              DO 380 J = LOOP,NCLBLK
                 COLMLT = ARRAY(I,J,K)/COLPIV
                 ARRAY(I,J,K) = COLMLT
                 IF(I.EQ.NRWBLK)GO TO 350
                    DO 345 L = IPLUS1,NRWBLK
                       ARRAY(L,J,K) = ARRAY(L,J,K)
     *                                -COLMLT*ARRAY(L,IPLUSN,K)
345                 CONTINUE
350              CONTINUE
                 JRWBLK = J-NRWBLK
                 IF(K.EQ.NBLOKS)GO TO 370
                    DO 360 L = 1,NRWBLK
                       ARRAY(L,JRWBLK,KPLUS1) =
     *                                  ARRAY(L,JRWBLK,KPLUS1)
     *                         -COLMLT*ARRAY(L,IRWBLK,KPLUS1)
360                 CONTINUE
                    GO TO 380
370              CONTINUE
                 DO 375 L = 1,NRWBOT
                    BOTBLK(L,JRWBLK) = BOTBLK(L,JRWBLK)
     *                              -COLMLT*BOTBLK(L,IRWBLK)
375              CONTINUE
380           CONTINUE
390        CONTINUE
           INCR = INCR + NRWBLK
395     CONTINUE
C
C***************************************************************
C
C          ****  FINALLY, IN BOTBLK....
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          ***  APPLY NRWBOT ROW ELIMINATIONS WITH ROW
C                  PIVOTING....
C
C               IF BOT HAS JUST ONE ROW GO TO 500
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        IF(NRWBOT.EQ.1)GO TO 500
           DO 470 J = NRWTP1,NVRLP0
              JPLUS1 = J+1
              JMINN = J-NRWTOP
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               DETERMINE ROW PIVOT AND PIVOT INDEX
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              IPVT = JMINN
              ROWMAX = DABS(BOTBLK(JMINN,J))
              LOOP = JMINN+1
              DO 410 I = LOOP,NRWBOT
                 TEMPIV = DABS(BOTBLK(I,J))
                 IF(TEMPIV.LE.ROWMAX) GO TO 410
                 IPVT = I
                 ROWMAX = TEMPIV
410           CONTINUE
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               TEST FOR SINGULARITY:
C
C                       IF SINGULAR THEN TERMINATE AT 1000;
C                                   ELSE CONTINUE.
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              IF(PIVMAX+ROWMAX.EQ.PIVMAX)GO TO 1000
              PIVMAX = DMAX1(ROWMAX,PIVMAX)
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               IF NECESSARY INTERCHANGE ROWS
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              INCRJ = INCR+J
              PIVOT(INCRJ) = INCR+IPVT+NRWTOP
              IF(IPVT.EQ.JMINN)GO TO 430
                 DO 420 L = J,NOVRLP
                    SWAP = BOTBLK(IPVT,L)
                    BOTBLK(IPVT,L) = BOTBLK(JMINN,L)
                    BOTBLK(JMINN,L) = SWAP
420              CONTINUE
430           CONTINUE
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               COMPUTE MULTIPLIERS
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              ROWPIV = BOTBLK(JMINN,J)
              DO 440 I = LOOP,NRWBOT
                 BOTBLK(I,J) = BOTBLK(I,J)/ROWPIV
440           CONTINUE
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               PERFORM ROW ELIMINATION WITH COLUMN INDEXING
C
C             CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
              DO 460 L = JPLUS1,NOVRLP
                 ROWMLT = BOTBLK(JMINN,L)
                 DO 450 I = LOOP,NRWBOT
                    BOTBLK(I,L) = BOTBLK(I,L)-ROWMLT*BOTBLK(I,J)
450              CONTINUE
460           CONTINUE
470        CONTINUE
500     CONTINUE
C
C***************************************************************
C
C          DONE PROVIDED THE LAST ELEMENT IS NOT ZERO
C
C***************************************************************
C
        IF(PIVMAX+DABS(BOTBLK(NRWBOT,NOVRLP)).NE.PIVMAX) RETURN
C
C***************************************************************
C
C       ****  MATRIX IS SINGULAR - SET IFLAG = - 1.
C                                  TERMINATE AT 1000.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
1000    CONTINUE
        IFLAG = -1
        RETURN
        END

c--------------------------------------------------------------

        SUBROUTINE CRSLVE(TOPBLK,NRWTOP,NOVRLP,ARRAY,NRWBLK,
     *             NCLBLK,NBLOKS,BOTBLK,NRWBOT,PIVOT,B,X)
C
C***************************************************************
C
C  C R S L V E  SOLVES THE LINEAR SYSTEM
C                       A*X = B
C  USING THE DECOMPOSITION ALREADY GENERATED IN  C R D C M P.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               *****  PARAMETERS  *****
C
C       *** ON ENTRY  ...
C
C               TOPBLK - DOUBLE PRECISION(NRWTOP,NOVRLP)
C                         OUTPUT FROM  C R D C M P
C
C               NOVRLP - INTEGER
C                         THE NUMBER OF COLUMNS IN WHICH SUCC-
C                         ESSIVE BLOCKS OVERLAP, WHERE
C                                NOVRLP = NRWTOP + NRWBOT
C
C               NRWTOP - INTEGER
C                         NUMBER OF ROWS IN THE BLOCK TOPBLK
C
C               ARRAY  - DOUBLE PRECISION(NRWBLK,NCLBLK,NBLOKS)
C                         OUTPUT FROM  C R D C M P
C
C               NRWBLK - INTEGER
C                         NUMBER OF ROWS IN K-TH BLOCK
C
C               NCLBLK - INTEGER
C                         NUMBER OF COLUMNS IN K-TH BLOCK
C
C               NBLOKS - INTEGER
C                         NUMBER OF NRWBLK BY NCLBLK BLOCKS IN
C                         THE MATRIX A
C
C               BOTBLK - DOUBLE PRECISION(NRWBOT,NOVRLP)
C                         OUTPUT FROM  C R D C M P
C
C               NRWBOT - INTEGER
C                         NUMBER OF ROWS IN THE BLOCK BOTBLK
C
C                PIVOT - INTEGER(N)
C                         THE PIVOT VECTOR FROM  C R D C M P
C
C                    B - DOUBLE PRECISION(N)
C                         THE RIGHT HAND SIDE VECTOR
C
C                    X - DOUBLE PRECISION(N)
C                         WORK SPACE
C
C       *** ON RETURN  ...
C
C          
C             X - DOUBLE PRECISION(N)
C                         THE SOLUTION VECTOR
C
C***************************************************************
C
        INTEGER NRWBOT, NBLOKS, NCLBLK, NRWBLK, NOVRLP, NRWTOP
        INTEGER IPVTI, NRWELL, IPVTN, INCRN, IPLUSN, L1, L, NRWBTL
        INTEGER LL, JRWTOP, JPIVOT, INCRI, INCRJ, INCRTP, K, INCR
        INTEGER I, LOOP, J, NBKTOP, NBLKS1, NVRLP0, NRWEL1, NROWEL
        INTEGER NRWBT1, NRWTP0, NVRLP1, NRWBK1, NRWTP1
        DOUBLE PRECISION TOPBLK,ARRAY,BOTBLK,X,B
        DOUBLE PRECISION DOTPRD,XJ,XINCRJ,BINCRJ,SWAP
!       INTEGER PIVOT(1)
!       DIMENSION TOPBLK(NRWTOP,1),ARRAY(NRWBLK,NCLBLK,1),
!    *          BOTBLK(NRWBOT,1),B(1),X(1)
        INTEGER PIVOT(*)
        DIMENSION TOPBLK(NRWTOP,*),ARRAY(NRWBLK,NCLBLK,*),
     *          BOTBLK(NRWBOT,*),B(*),X(*)
C
C***************************************************************
C
C          ****  DEFINE THE CONSTANTS USED THROUGHOUT  ****
C
C***************************************************************
C
        NRWTP1 = NRWTOP+1
        NRWBK1 = NRWBLK+1
        NVRLP1 = NOVRLP+1
        NRWTP0 = NRWTOP-1
        NRWBT1 = NRWBOT+1
        NROWEL = NRWBLK-NRWTOP
        NRWEL1 = NROWEL+1
        NVRLP0 = NOVRLP-1
        NBLKS1 = NBLOKS+1
        NBKTOP = NRWBLK+NRWTOP
C
C***************************************************************
C
C               ****  FORWARD RECURSION  ****
C
C***************************************************************
C
C          ***  FIRST, IN TOPBLK....
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               FORWARD SOLUTION
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        DO 130 J = 1,NRWTOP
           X(J) = B(J)/TOPBLK(J,J)
           IF(J.EQ.NRWTOP)GO TO 120
              XJ = -X(J)
              LOOP = J+1
              DO 110 I = LOOP,NRWTOP
                 B(I) = B(I)+TOPBLK(I,J)*XJ
110           CONTINUE
120        CONTINUE
130     CONTINUE
C
C       ********************************************************
C
C          ***  IN EACH BLOCK ARRAY(,,K)....
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        INCR = 0
        DO 280 K = 1,NBLOKS
           INCRTP = INCR+NRWTOP
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               FORWARD MODIFICATION
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           DO 220 J = 1,NRWTOP
              INCRJ = INCR+J
              XINCRJ = -X(INCRJ)
              DO 210 I = 1,NRWBLK
                 INCRI = INCRTP+I
                 B(INCRI) = B(INCRI)+ARRAY(I,J,K)*XINCRJ
210           CONTINUE
220        CONTINUE
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               FORWARD ELIMINATION
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           DO 240 J = NRWTP1,NRWBLK
              INCRJ = INCR+J
              JPIVOT = PIVOT(INCRJ)
              IF(JPIVOT.EQ.INCRJ)GO TO 225
                 SWAP = B(INCRJ)
                 B(INCRJ) = B(JPIVOT)
                 B(JPIVOT) = SWAP
225           CONTINUE
              BINCRJ = -B(INCRJ)
              LOOP = J-NRWTP0
              DO 230 I = LOOP,NRWBLK
                 INCRI = INCRTP+I
                 B(INCRI) = B(INCRI)+ARRAY(I,J,K)*BINCRJ
230           CONTINUE
240        CONTINUE
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               FORWARD SOLUTION
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           DO 270 J = NRWBK1,NBKTOP
              INCRJ = INCR+J
              JRWTOP = J -NRWTOP
              X(INCRJ) = B(INCRJ)/ARRAY(JRWTOP,J,K)
              IF(J.EQ.NBKTOP)GO TO 260
                 XINCRJ = -X(INCRJ)
                 LOOP = J-NRWTP0
                 DO 250 I = LOOP,NRWBLK
                    INCRI = INCRTP+I
                    B(INCRI) = B(INCRI)+ARRAY(I,J,K)*XINCRJ
250              CONTINUE
260           CONTINUE
270        CONTINUE
           INCR = INCR+NRWBLK
280     CONTINUE
C
C       ********************************************************
C
C          ***  FINALLY, IN BOTBLK....
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               FORWARD MODIFICATION
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        INCRTP = INCR+NRWTOP
        DO 320 J = 1,NRWTOP
           INCRJ = INCR+J
           XINCRJ = -X(INCRJ)
           DO 310 I = 1,NRWBOT
              INCRI = INCRTP+I
              B(INCRI) = B(INCRI)+BOTBLK(I,J)*XINCRJ
310        CONTINUE
320     CONTINUE
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               FORWARD ELIMINATION
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        IF(NRWBOT.EQ.1)GO TO 350
           DO 340 J = NRWTP1,NVRLP0
              INCRJ = INCR+J
              JPIVOT = PIVOT(INCRJ)
              IF(JPIVOT.EQ.INCRJ)GO TO 325
                 SWAP = B(INCRJ)
                 B(INCRJ) = B(JPIVOT)
                 B(JPIVOT) = SWAP
325           CONTINUE
              BINCRJ = -B(INCRJ)
              LOOP = J-NRWTP0
              DO 330 I = LOOP,NRWBOT
                 INCRI = INCRTP+I
                 B(INCRI) = B(INCRI)+BOTBLK(I,J)*BINCRJ
330           CONTINUE
340        CONTINUE
350     CONTINUE
C
C***************************************************************
C
C               ****  BACKWARD RECURSION  ****
C
C***************************************************************
C
C          ***  FIRST IN BOTBLK....
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               BACKWARD SOLUTION
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        DO 430 LL = 1,NRWBOT
           J = NVRLP1-LL
           INCRJ = INCR+J
           NRWBTL = NRWBT1-LL
           X(INCRJ) = B(INCRJ)/BOTBLK(NRWBTL,J)
           IF(LL.EQ.NRWBOT)GO TO 420
              XINCRJ = -X(INCRJ)
              LOOP = NRWBOT-LL
              DO 410 I = 1,LOOP
                 INCRI = INCRTP+I
                 B(INCRI) = B(INCRI)+BOTBLK(I,J)*XINCRJ
410           CONTINUE
420        CONTINUE
430     CONTINUE
C
C       ********************************************************
C
C          ***  THEN IN EACH BLOCK ARRAY(,,K)....
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        DO 490 L = 1,NBLOKS
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               BACKWARD ELIMINATION
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           K = NBLKS1-L
           INCR = INCR-NRWBLK
           DO 450 L1 = NRWEL1,NRWBLK
              I = NRWBLK+NRWEL1-L1
              IPLUSN = I+NRWTOP
              LOOP = IPLUSN+1
              INCRN = INCR+IPLUSN
              DOTPRD = X(INCRN)
              DO 440 J = LOOP,NCLBLK
                 INCRJ = INCR+J
                 DOTPRD = DOTPRD-ARRAY(I,J,K)*X(INCRJ)
440           CONTINUE
              X(INCRN) = DOTPRD
              IPVTN = PIVOT(INCRN)
              IF(INCRN.EQ.IPVTN)GO TO 445
                 SWAP = X(INCRN)
                 X(INCRN) = X(IPVTN)
                 X(IPVTN) = SWAP
445           CONTINUE
450        CONTINUE
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               BACKWARD MODIFICATION
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           INCRTP = INCR+NRWTOP
           DO 460 J = NRWBK1,NCLBLK
              INCRJ = INCR+J
              XINCRJ = -X(INCRJ)
              DO 455 I = 1,NROWEL
                 INCRI = INCRTP+I
                 B(INCRI) = B(INCRI)+ARRAY(I,J,K)*XINCRJ
455           CONTINUE
460        CONTINUE
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               BACKWARD SOLUTION
C
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
           DO 480 LL = 1,NROWEL
              J = NRWBK1-LL
              INCRJ = INCR+J
              NRWELL = NRWEL1-LL
              X(INCRJ) = B(INCRJ)/ARRAY(NRWELL,J,K)
              IF(LL.EQ.NROWEL)GO TO 470
                 XINCRJ = -X(INCRJ)
                 LOOP = NROWEL-LL
                 DO 465 I = 1,LOOP
                    INCRI = INCRTP+I
                    B(INCRI) = B(INCRI)+ARRAY(I,J,K)*XINCRJ
465              CONTINUE
470           CONTINUE
480        CONTINUE
490     CONTINUE
C
C       ********************************************************
C
C          ***  IN TOPBLK FINISH WITH....
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               BACKWARD ELIMINATION
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        DO 520 L = 1,NRWTOP
           I = NRWTP1-L
           LOOP = I+1
           DOTPRD = X(I)
           DO 510 J = LOOP,NOVRLP
              DOTPRD = DOTPRD-TOPBLK(I,J)*X(J)
510        CONTINUE
           X(I) = DOTPRD
           IPVTI = PIVOT(I)
           IF(I.EQ.IPVTI)GO TO 515
                 SWAP = X(I)
                 X(I) = X(IPVTI)
                 X(IPVTI) = SWAP
515        CONTINUE
520     CONTINUE
        RETURN
        END

c-------------------------------------------------------------
c  This program is from LINPACK and has been modified:  
c  declarations like array(1) were changed to array(*).


      subroutine dsvdc(x,ldx,n,p,s,e,u,ldu,v,ldv,work,job,info)
      integer ldx,n,p,ldu,ldv,job,info
      double precision x(ldx,*),s(*),e(*),u(ldu,*),v(ldv,*),work(*)
c
c
c     dsvdc is a subroutine to reduce a double precision nxp matrix x
c     by orthogonal transformations u and v to diagonal form.  the
c     diagonal elements s(i) are the singular values of x.  the
c     columns of u are the corresponding left singular vectors,
c     and the columns of v the right singular vectors.
c
c     on entry
c
c         x         double precision(ldx,p), where ldx.ge.n.
c                   x contains the matrix whose singular value
c                   decomposition is to be computed.  x is
c                   destroyed by dsvdc.
c
c         ldx       integer.
c                   ldx is the leading dimension of the array x.
c
c         n         integer.
c                   n is the number of rows of the matrix x.
c
c         p         integer.
c                   p is the number of columns of the matrix x.
c
c         ldu       integer.
c                   ldu is the leading dimension of the array u.
c                   (see below).
c
c         ldv       integer.
c                   ldv is the leading dimension of the array v.
c                   (see below).
c
c         work      double precision(n).
c                   work is a scratch array.
c
c         job       integer.
c                   job controls the computation of the singular
c                   vectors.  it has the decimal expansion ab
c                   with the following meaning
c
c                        a.eq.0    do not compute the left singular
c                                  vectors.
c                        a.eq.1    return the n left singular vectors
c                                  in u.
c                        a.ge.2    return the first min(n,p) singular
c                                  vectors in u.
c                        b.eq.0    do not compute the right singular
c                                  vectors.
c                        b.eq.1    return the right singular vectors
c                                  in v.
c
c     on return
c
c         s         double precision(mm), where mm=min(n+1,p).
c                   the first min(n,p) entries of s contain the
c                   singular values of x arranged in descending
c                   order of magnitude.
c
c         e         double precision(p), 
c                   e ordinarily contains zeros.  however see the
c                   discussion of info for exceptions.
c
c         u         double precision(ldu,k), where ldu.ge.n.  if
c                                   joba.eq.1 then k.eq.n, if joba.ge.2
c                                   then k.eq.min(n,p).
c                   u contains the matrix of left singular vectors.
c                   u is not referenced if joba.eq.0.  if n.le.p
c                   or if joba.eq.2, then u may be identified with x
c                   in the subroutine call.
c
c         v         double precision(ldv,p), where ldv.ge.p.
c                   v contains the matrix of right singular vectors.
c                   v is not referenced if job.eq.0.  if p.le.n,
c                   then v may be identified with x in the
c                   subroutine call.
c
c         info      integer.
c                   the singular values (and their corresponding
c                   singular vectors) s(info+1),s(info+2),...,s(m)
c                   are correct (here m=min(n,p)).  thus if
c                   info.eq.0, all the singular values and their
c                   vectors are correct.  in any event, the matrix
c                   b = trans(u)*x*v is the bidiagonal matrix
c                   with the elements of s on its diagonal and the
c                   elements of e on its super-diagonal (trans(u)
c                   is the transpose of u).  thus the singular
c                   values of x and b are the same.
c
c     linpack. this version dated 08/14/78 .
c              correction made to shift 2/84.
c     g.w. stewart, university of maryland, argonne national lab.
c
c     dsvdc uses the following functions and subprograms.
c
c     external drot
c     blas daxpy,ddot,dscal,dswap,dnrm2,drotg
c     fortran dabs,dmax1,max0,min0,mod,dsqrt
c
c     internal variables
c
      integer i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,m,maxit,
     *        mm,mm1,mp1,nct,nctp1,ncu,nrt,nrtp1
      double precision ddot,t
      double precision b,c,cs,el,emm1,f,g,dnrm2,scale,shift,sl,sm,sn,
     *                 smm1,t1,test,ztest
      logical wantu,wantv
      external dscal, daxpy, drotg, drot, dswap
c
c
c     set the maximum number of iterations.
c
      maxit = 30
c
c     determine what is to be computed.
c
      wantu = .false.
      wantv = .false.
      jobu = mod(job,100)/10
      ncu = n
      if (jobu .gt. 1) ncu = min0(n,p)
      if (jobu .ne. 0) wantu = .true.
      if (mod(job,10) .ne. 0) wantv = .true.
c
c     reduce x to bidiagonal form, storing the diagonal elements
c     in s and the super-diagonal elements in e.
c
      info = 0
      nct = min0(n-1,p)
      nrt = max0(0,min0(p-2,n))
      lu = max0(nct,nrt)
      if (lu .lt. 1) go to 170
      do 160 l = 1, lu
         lp1 = l + 1
         if (l .gt. nct) go to 20
c
c           compute the transformation for the l-th column and
c           place the l-th diagonal in s(l).
c
            s(l) = dnrm2(n-l+1,x(l,l),1)
            if (s(l) .eq. 0.0d0) go to 10
               if (x(l,l) .ne. 0.0d0) s(l) = dsign(s(l),x(l,l))
               call dscal(n-l+1,1.0d0/s(l),x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
   10       continue
            s(l) = -s(l)
   20    continue
         if (p .lt. lp1) go to 50
         do 40 j = lp1, p
            if (l .gt. nct) go to 30
            if (s(l) .eq. 0.0d0) go to 30
c
c              apply the transformation.
c
               t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
               call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
   30       continue
c
c           place the l-th row of x into  e for the
c           subsequent calculation of the row transformation.
c
            e(j) = x(l,j)
   40    continue
   50    continue
         if (.not.wantu .or. l .gt. nct) go to 70
c
c           place the transformation in u for subsequent back
c           multiplication.
c
            do 60 i = l, n
               u(i,l) = x(i,l)
   60       continue
   70    continue
         if (l .gt. nrt) go to 150
c
c           compute the l-th row transformation and place the
c           l-th super-diagonal in e(l).
c
            e(l) = dnrm2(p-l,e(lp1),1)
            if (e(l) .eq. 0.0d0) go to 80
               if (e(lp1) .ne. 0.0d0) e(l) = dsign(e(l),e(lp1))
               call dscal(p-l,1.0d0/e(l),e(lp1),1)
               e(lp1) = 1.0d0 + e(lp1)
   80       continue
            e(l) = -e(l)
            if (lp1 .gt. n .or. e(l) .eq. 0.0d0) go to 120
c
c              apply the transformation.
c
               do 90 i = lp1, n
                  work(i) = 0.0d0
   90          continue
               do 100 j = lp1, p
                  call daxpy(n-l,e(j),x(lp1,j),1,work(lp1),1)
  100          continue
               do 110 j = lp1, p
                  call daxpy(n-l,-e(j)/e(lp1),work(lp1),1,x(lp1,j),1)
  110          continue
  120       continue
            if (.not.wantv) go to 140
c
c              place the transformation in v for subsequent
c              back multiplication.
c
               do 130 i = lp1, p
                  v(i,l) = e(i)
  130          continue
  140       continue
  150    continue
  160 continue
  170 continue
c
c     set up the final bidiagonal matrix or order m.
c
      m = min0(p,n+1)
      nctp1 = nct + 1
      nrtp1 = nrt + 1
      if (nct .lt. p) s(nctp1) = x(nctp1,nctp1)
      if (n .lt. m) s(m) = 0.0d0
      if (nrtp1 .lt. m) e(nrtp1) = x(nrtp1,m)
      e(m) = 0.0d0
c
c     if required, generate u.
c
      if (.not.wantu) go to 300
         if (ncu .lt. nctp1) go to 200
         do 190 j = nctp1, ncu
            do 180 i = 1, n
               u(i,j) = 0.0d0
  180       continue
            u(j,j) = 1.0d0
  190    continue
  200    continue
         if (nct .lt. 1) go to 290
         do 280 ll = 1, nct
            l = nct - ll + 1
            if (s(l) .eq. 0.0d0) go to 250
               lp1 = l + 1
               if (ncu .lt. lp1) go to 220
               do 210 j = lp1, ncu
                  t = -ddot(n-l+1,u(l,l),1,u(l,j),1)/u(l,l)
                  call daxpy(n-l+1,t,u(l,l),1,u(l,j),1)
  210          continue
  220          continue
               call dscal(n-l+1,-1.0d0,u(l,l),1)
               u(l,l) = 1.0d0 + u(l,l)
               lm1 = l - 1
               if (lm1 .lt. 1) go to 240
               do 230 i = 1, lm1
                  u(i,l) = 0.0d0
  230          continue
  240          continue
            go to 270
  250       continue
               do 260 i = 1, n
                  u(i,l) = 0.0d0
  260          continue
               u(l,l) = 1.0d0
  270       continue
  280    continue
  290    continue
  300 continue
c
c     if it is required, generate v.
c
      if (.not.wantv) go to 350
         do 340 ll = 1, p
            l = p - ll + 1
            lp1 = l + 1
            if (l .gt. nrt) go to 320
            if (e(l) .eq. 0.0d0) go to 320
               do 310 j = lp1, p
                  t = -ddot(p-l,v(lp1,l),1,v(lp1,j),1)/v(lp1,l)
                  call daxpy(p-l,t,v(lp1,l),1,v(lp1,j),1)
  310          continue
  320       continue
            do 330 i = 1, p
               v(i,l) = 0.0d0
  330       continue
            v(l,l) = 1.0d0
  340    continue
  350 continue
c
c     main iteration loop for the singular values.
c
      mm = m
      iter = 0
  360 continue
c
c        quit if all the singular values have been found.
c
c     ...exit
         if (m .eq. 0) go to 620
c
c        if too many iterations have been performed, set
c        flag and return.
c
         if (iter .lt. maxit) go to 370
            info = m
c     ......exit
            go to 620
  370    continue
c
c        this section of the program inspects for
c        negligible elements in the s and e arrays.  on
c        completion the variables kase and l are set as follows.
c
c           kase = 1     if s(m) and e(l-1) are negligible and l.lt.m
c           kase = 2     if s(l) is negligible and l.lt.m
c           kase = 3     if e(l-1) is negligible, l.lt.m, and
c                        s(l), ..., s(m) are not negligible (qr step).
c           kase = 4     if e(m-1) is negligible (convergence).
c
         do 390 ll = 1, m
            l = m - ll
c        ...exit
            if (l .eq. 0) go to 400
            test = dabs(s(l)) + dabs(s(l+1))
            ztest = test + dabs(e(l))
            if (ztest .ne. test) go to 380
               e(l) = 0.0d0
c        ......exit
               go to 400
  380       continue
  390    continue
  400    continue
         if (l .ne. m - 1) go to 410
            kase = 4
         go to 480
  410    continue
            lp1 = l + 1
            mp1 = m + 1
            do 430 lls = lp1, mp1
               ls = m - lls + lp1
c           ...exit
               if (ls .eq. l) go to 440
               test = 0.0d0
               if (ls .ne. m) test = test + dabs(e(ls))
               if (ls .ne. l + 1) test = test + dabs(e(ls-1))
               ztest = test + dabs(s(ls))
               if (ztest .ne. test) go to 420
                  s(ls) = 0.0d0
c           ......exit
                  go to 440
  420          continue
  430       continue
  440       continue
            if (ls .ne. l) go to 450
               kase = 3
            go to 470
  450       continue
            if (ls .ne. m) go to 460
               kase = 1
            go to 470
  460       continue
               kase = 2
               l = ls
  470       continue
  480    continue
         l = l + 1
c
c        perform the task indicated by kase.
c
         go to (490,520,540,570), kase
c
c        deflate negligible s(m).
c
  490    continue
            mm1 = m - 1
            f = e(m-1)
            e(m-1) = 0.0d0
            do 510 kk = l, mm1
               k = mm1 - kk + l
               t1 = s(k)
               call drotg(t1,f,cs,sn)
               s(k) = t1
               if (k .eq. l) go to 500
                  f = -sn*e(k-1)
                  e(k-1) = cs*e(k-1)
  500          continue
               if (wantv) call drot(p,v(1,k),1,v(1,m),1,cs,sn)
  510       continue
         go to 610
c
c        split at negligible s(l).
c
  520    continue
            f = e(l-1)
            e(l-1) = 0.0d0
            do 530 k = l, m
               t1 = s(k)
               call drotg(t1,f,cs,sn)
               s(k) = t1
               f = -sn*e(k)
               e(k) = cs*e(k)
               if (wantu) call drot(n,u(1,k),1,u(1,l-1),1,cs,sn)
  530       continue
         go to 610
c
c        perform one qr step.
c
  540    continue
c
c           calculate the shift.
c
            scale = dmax1(dabs(s(m)),dabs(s(m-1)),dabs(e(m-1)),
     *                    dabs(s(l)),dabs(e(l)))
            sm = s(m)/scale
            smm1 = s(m-1)/scale
            emm1 = e(m-1)/scale
            sl = s(l)/scale
            el = e(l)/scale
            b = ((smm1 + sm)*(smm1 - sm) + emm1**2)/2.0d0
            c = (sm*emm1)**2
            shift = 0.0d0
            if (b .eq. 0.0d0 .and. c .eq. 0.0d0) go to 550
               shift = dsqrt(b**2+c)
               if (b .lt. 0.0d0) shift = -shift
               shift = c/(b + shift)
  550       continue
            f = (sl + sm)*(sl - sm) + shift
            g = sl*el
c
c           chase zeros.
c
            mm1 = m - 1
            do 560 k = l, mm1
               call drotg(f,g,cs,sn)
               if (k .ne. l) e(k-1) = f
               f = cs*s(k) + sn*e(k)
               e(k) = cs*e(k) - sn*s(k)
               g = sn*s(k+1)
               s(k+1) = cs*s(k+1)
               if (wantv) call drot(p,v(1,k),1,v(1,k+1),1,cs,sn)
               call drotg(f,g,cs,sn)
               s(k) = f
               f = cs*e(k) + sn*s(k+1)
               s(k+1) = -sn*e(k) + cs*s(k+1)
               g = sn*e(k+1)
               e(k+1) = cs*e(k+1)
               if (wantu .and. k .lt. n)
     *            call drot(n,u(1,k),1,u(1,k+1),1,cs,sn)
  560       continue
            e(m-1) = f
            iter = iter + 1
         go to 610
c
c        convergence.
c
  570    continue
c
c           make the singular value  positive.
c
            if (s(l) .ge. 0.0d0) go to 580
               s(l) = -s(l)
               if (wantv) call dscal(p,-1.0d0,v(1,l),1)
  580       continue
c
c           order the singular value.
c
  590       if (l .eq. mm) go to 600
c           ...exit
               if (s(l) .ge. s(l+1)) go to 600
               t = s(l)
               s(l) = s(l+1)
               s(l+1) = t
               if (wantv .and. l .lt. p)
     *            call dswap(p,v(1,l),1,v(1,l+1),1)
               if (wantu .and. l .lt. n)
     *            call dswap(n,u(1,l),1,u(1,l+1),1)
               l = l + 1
            go to 590
  600       continue
            iter = 0
            m = m - 1
  610    continue
      go to 360
  620 continue
      return
      end

c--------------------------------------------------------------------
c  These routines are from BLAS collection.

      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),da
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end


      double precision function ddot(n,dx,incx,dy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end

      DOUBLE PRECISION FUNCTION DNRM2 ( N, X, INCX )
*     .. Scalar Arguments ..
      INTEGER                           INCX, N
*     .. Array Arguments ..
      DOUBLE PRECISION                  X( * )
*     ..
*
*  DNRM2 returns the euclidean norm of a vector via the function
*  name, so that
*
*     DNRM2 := sqrt( x'*x )
*
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to DLASSQ.
*     Sven Hammarling, Nag Ltd.
*
*
*     .. Parameters ..
      DOUBLE PRECISION      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      INTEGER               IX
      DOUBLE PRECISION      ABSXI, NORM, SCALE, SSQ
*     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
*     ..
*     .. Executable Statements ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
*
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( X( IX ).NE.ZERO )THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI )THEN
                  SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SSQ   = SSQ   +     ( ABSXI/SCALE )**2
               END IF
            END IF
   10    CONTINUE
         NORM  = SCALE * SQRT( SSQ )
      END IF
*
      DNRM2 = NORM
      RETURN
*
*     End of DNRM2.
*
      END

      subroutine  drot (n,dx,incx,dy,incy,c,s)
c
c     applies a plane rotation.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp,c,s
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = c*dx(ix) + s*dy(iy)
        dy(iy) = c*dy(iy) - s*dx(ix)
        dx(ix) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
   20 do 30 i = 1,n
        dtemp = c*dx(i) + s*dy(i)
        dy(i) = c*dy(i) - s*dx(i)
        dx(i) = dtemp
   30 continue
      return
      end

      subroutine drotg(da,db,c,s)
c
c     construct givens plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      double precision da,db,c,s,roe,scale,r,z
c
      roe = db
      if( dabs(da) .gt. dabs(db) ) roe = da
      scale = dabs(da) + dabs(db)
      if( scale .ne. 0.0d0 ) go to 10
         c = 1.0d0
         s = 0.0d0
         r = 0.0d0
         z = 0.0d0
         go to 20
   10 r = scale*dsqrt((da/scale)**2 + (db/scale)**2)
      r = dsign(1.0d0,roe)*r
      c = da/r
      s = db/r
      z = 1.0d0
      if( dabs(da) .gt. dabs(db) ) z = s
      if( dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0 ) z = 1.0d0/c
   20 da = r
      db = z
      return
      end

      subroutine  dscal(n,da,dx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end

      subroutine  dswap (n,dx,incx,dy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
c
c       clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end

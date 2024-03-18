      SUBROUTINE ERFIN
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1985-09-23 ERFIN  Lawson  Initial code.
C
      COMMON/M77ERR/IDELTA,IALPHA
      SAVE /M77ERR/
C
      PRINT 1003
      IF (IALPHA.GE.2) STOP
      RETURN
 1003 FORMAT(1X,72('$')/' ')
      END

      SUBROUTINE ERMSG(SUBNAM,INDIC,LEVEL,MSG,FLAG)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1992-10-20 ERMSG  WV Snyder  added ERLSET, ERLGET
C>> 1985-09-25 ERMSG  Lawson  Initial code.
C
C     --------------------------------------------------------------
C
C     Four entries: ERMSG, ERMSET, ERLGET, ERLSET
C     ERMSG initiates an error message. This subr also manages the
C     saved value IDELOC and the saved COMMON block M77ERR to
C     control the level of action. This is intended to be the
C     only subr that assigns a value to IALPHA in COMMON.
C     ERMSET resets IDELOC & IDELTA.  ERLGET returns the last value
C     of LEVEL passed to ERMSG.  ERLSET sets the last value of LEVEL.
C     ERLSET and ERLGET may be used together to determine the level
C     of error that occurs during execution of a routine that uses
C     ERMSG.
C
C     --------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     SUBNAM   A name that identifies the subprogram in which
C              the error occurs.
C
C     INDIC    An integer printed as part of the mininal error
C              message. It together with SUBNAM can be used to
C              uniquely identify an error.
C
C     LEVEL    The user sets LEVEL=2,0,or -2 to specify the
C              nominal action to be taken by ERMSG. The
C              subroutine ERMSG contains an internal variable
C              IDELTA, whose nominal value is zero. The
C              subroutine will compute IALPHA = LEVEL + IDELTA
C              and proceed as follows:
C              If (IALPHA.GE.2)        Print message and STOP.
C              If (IALPHA=-1,0,1)      Print message and return.
C              If (IALPHA.LE.-2)       Just RETURN.
C
C     MSG      Message to be printed as part of the diagnostic.
C
C     FLAG     A single character,which when set to '.' will
C              call the subroutine ERFIN and will just RETURN
C              when set to any other character.
C
C     --------------------------------------------------------------
C
C     C.Lawson & S.Chan, JPL, 1983 Nov
C
C     ------------------------------------------------------------------
      INTEGER OLDLEV
      COMMON/M77ERR/IDELTA,IALPHA
      CHARACTER*(*) SUBNAM,MSG
      CHARACTER*1 FLAG
      SAVE/M77ERR/,IDELOC,OLDLEV
      DATA IDELOC/0/, OLDLEV /0/
      OLDLEV = LEVEL
      IDELTA = IDELOC
      IALPHA = LEVEL + IDELTA
      IF (IALPHA.GE.-1) THEN
c
c            Setting FILE = 'CON' works for MS/DOS systems.
c
c
        WRITE (*,1001) SUBNAM,INDIC
        WRITE (*,*) MSG
        IF (FLAG.EQ.'.') CALL ERFIN
      ENDIF
      RETURN
C
 1001 FORMAT('0',72('$')/' SUBPROGRAM ',A,' REPORTS ERROR NO. ',I4)
C
C
      ENTRY ERMSET(IDEL)
      IDELTA=IDEL
      IDELOC=IDEL
      RETURN
C
C
      ENTRY ERLSET (LEVEL)
      OLDLEV = LEVEL
      RETURN
C
C     
      ENTRY ERLGET (LEVEL)
      LEVEL = OLDLEV
      RETURN
      END

      INTEGER FUNCTION ISAMAX(N,SX,INCX)
C>> 1987-12-09 ISAMAX Lawson  Initial code.
C
C     FIND SMALLEST INDEX OF MAXIMUM MAGNITUDE OF SINGLE PRECISION SX.
C     ISAMAX =  FIRST I, I = 1 TO N, TO MINIMIZE  ABS(SX(1-INCX+I*INCX))
C
      REAL SX(*),SMAX,XMAG
      ISAMAX = 0
      IF(N.LE.0) RETURN
      ISAMAX = 1
      IF(N.LE.1)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      SMAX = ABS(SX(1))
      NS = N*INCX
      II = 1
          DO 10 I=1,NS,INCX
          XMAG = ABS(SX(I))
          IF(XMAG.LE.SMAX) GO TO 5
          ISAMAX = II
          SMAX = XMAG
    5     II = II + 1
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
   20 SMAX = ABS(SX(1))
      DO 30 I = 2,N
         XMAG = ABS(SX(I))
         IF(XMAG.LE.SMAX) GO TO 30
         ISAMAX = I
         SMAX = XMAG
   30 CONTINUE
      RETURN
      END

      SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY)
C>> 1985-08-02 SAXPY  Lawson  Initial code.
C
C     OVERWRITE SINGLE PRECISION SY WITH SINGLE PRECISION SA*SX +SY.
C     FOR I = 0 TO N-1, REPLACE  SY(LY+I*INCY) WITH SA*SX(LX+I*INCX) +
C       SY(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N,
C       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
C
      REAL SX(*),SY(*),SA
      IF(N.LE.0.OR.SA.EQ.0.E0) RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
C
C        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        SY(IY) = SY(IY) + SA*SX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4.
C
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SY(I) = SY(I) + SA*SX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        SY(I) = SY(I) + SA*SX(I)
        SY(I + 1) = SY(I + 1) + SA*SX(I + 1)
        SY(I + 2) = SY(I + 2) + SA*SX(I + 2)
        SY(I + 3) = SY(I + 3) + SA*SX(I + 3)
   50 CONTINUE
      RETURN
C
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
C
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          SY(I) = SA*SX(I) + SY(I)
   70     CONTINUE
      RETURN
      END

      subroutine SGEFA(A,LDA,N,IPVT,INFO)
C>> 1987-08-18 SGEFA  Lawson  Initial code.
C
C     SGEFA computes the LU factorization of the N x N matrix A by
c     Gaussian elimination.  This produces matrices, L and U, that
c     satisfy L * U = A, where U is an upper triangular matrix and
c     L is a permutation of a lower triangular matrix. Use of this
c     subroutine would typically be followed by use of other
c     subroutines that would use this factorization to solve a
c     system of linear equations, or to compute the inverse matrix
c     or the determinant of A.
C
C     SGEFA may be referenced indirectly via _GECO, but it can be called
C     directly with a saving in time if the reciprocal condition number
C     RCOND is not needed.
C     (Time for _GECO) = (1 + 9/N)*(Time for SGEFA) .
C
c     ------------------------------------------------------------------
c                        Subroutine arguments
C
C     A(,)  [inout]  An array of size at least N x N.  On entry must
c            contain an N x N matrix, A, to be factored.  On return will
c            contain the LU factors of A.
C
C     LDA  [in]  Leading dimensioning parameter for the array A(,).
C
C     N  [in]  The order of the matrix, A.
C
C     IPVT()  [in]  An integer array of length at least N, containg a
c           record of the row interchanges made during factorization of
c           A.
c
C      INFO  [out]  Indicate status on return
C              = 0  NORMAL VALUE.
C              = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C              CONDITION FOR THIS SUBROUTINE, but it does
C              indicate that the usual following steps to solve
c              equations or compute an inverse matrix cannot
c              be done, at least by the usual straightforward
c              algorithms.  Use RCOND in _GECO for a more reliable
C              indication of singularity.
c     ------------------------------------------------------------------
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted from LINPACK for the JPL Math77 library by
c     C. L. Lawson, JPL, Aug 1987.
c     ------------------------------------------------------------------
C     Subprograms referenced: SAXPY,SSCAL,ISAMAX
c     ------------------------------------------------------------------
      integer ISAMAX
      integer LDA,N,IPVT(N),INFO, J,K,KP1,L,NM1
      real A(LDA,N), T, ONE, ZERO
      parameter(ONE=1.E0, ZERO=0.E0)
c     ------------------------------------------------------------------
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
      INFO = 0
      NM1 = N - 1
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         L = ISAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. ZERO) THEN
           INFO = K
         ELSE
C           INTERCHANGE IF NECESSARY
           IF (L .NE. K) THEN
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
           END IF
C
C           COMPUTE MULTIPLIERS
C
            T = -ONE/A(K,K)
            CALL SSCAL(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .NE. K) THEN
                  A(L,J) = A(K,J)
                  A(K,J) = T
               END IF
               CALL SAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         ENDIF
   60 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. ZERO) INFO = N
      RETURN
      END

      subroutine SGEFS(A,LDA,N,B,LDB,NB,IPVT,INFO)
C>> 1987-08-18 SGEFS  Lawson  Initial code.
c
c     Solves a system of linear equations,  A * X = B,
c     where A is a square nonsingular matrix of order N and B is an
c     N by NB matrix.  The solution is the N by NB matrix X that will
c     be stored on return in place of B in the array B().
c     ------------------------------------------------------------------
c     Uses subroutines derived from LINPACK.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted for the JPL Math77 library by C. L. Lawson, JPL, Aug 1987.
c     ------------------------------------------------------------------
c                    Subroutine arguments
c
c     A(,)  [inout]  On entry contains the N by N matrix A.
c           On return contains the LU factorization of A as computed by
c           LINPACK subroutines.
c
c     LDA  [in]  Leading dimensioning parameter for the array A(,).
c           Require LDA .ge. N.
c
c     N  [in]  The order of the matrix A and number of rows in the
c           matrices B and X.
c
c     B(,)  [inout]  On entry contains the N by NB matrix, B.  On return
c           contains the N by NB solution matrix, X.  Could be a
c           Singly subscripted array if NB .eq. 1.
c
c     LDB  [in]  Leading dimensioning parameter for the array B(,).
c           Require LDB .ge. N.
c
c     NB  [in]  Number of columns in the matrices B and X.  If NB .lt. 1
c           the matrix, A, will be factored but no reference will be
c           made to the array B(,).
c
c     IPVT()  [out]  Integer array of length at least N.  On return will
c           contain a record of the row interchanges done during
c           factorization of A.
c
c     INFO  [out]  Set to zero if all diagonal elements in the U matrix
c           of the LU factorization are found to be nonzero.  If nonzero
c           it is the index of the first diagonal element of U that was
c           found to be zero.  In this latter case the solution X will
c           not be computed.
c     ------------------------------------------------------------------
c     Subprograms called: SGEFA, SGESLD
c     ------------------------------------------------------------------
      integer LDA, N, LDB, NB, IPVT(N), INFO
      real A(LDA,N), B(LDB,*)
c     ------------------------------------------------------------------
      call SGEFA(A,LDA,N,IPVT,INFO)
      if( INFO .ne. 0) return
      do 10 J = 1,NB
         call SGESLD(A,LDA,N,IPVT,B(1,J))
   10 continue
      return
      END

      subroutine SGESLD(A,LDA,N,IPVT,B)
C>> 1987-08-18 SGESLD Lawson  Initial code.
 
C     This subroutine solves the system of equations  A * X = B
C     using the LU factorization of A given in the array A().
c     ------------------------------------------------------------------
c           Subroutine arguments
C
C     A(,)  [in]  An array of size at least N x N.  On entry must
c         contain the LU factors of an N x N  matrix, A.  It is
c         expected that this factorization will have been computed by
c         use of _GEFA, either directly or indirectly via use of
c         _GEFS or _GEFSC.  This subr will not alter the contents of
c         A(,)
C
C     LDA  [in]  Leading dimensioning parameter for the array A(,).
C
C     N  [in]  The order of the original matrix, A.
C
C     IPVT()  [in]  An integer array of length at least N, containg a
c           record of the row interchanges made during factorization of
c           A.
C
c     B()  [inout]  On entry contains the right-side N-vector for the
c           problem, A * X = B.  On return contains the solution
c           N-vector, X.
c     ------------------------------------------------------------------
C     ERROR CONDITION
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        setting of LDA.  The user can avoid sending a singular matrix
c        to this subr by testing INFO (set by _GEFS or _GEFA) or
c        RCOND (set by _GEFSC or _GERC) before calling this subr.
c        Nonsingularity is indicated by INFO .eq. 0 or RCOND .ne. ZERO.
C     ------------------------------------------------------------------
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted from LINPACK for the JPL Math77 library by
c     C. L. Lawson, JPL, Aug 1987.
C     ------------------------------------------------------------------
c     Subprograms referenced: SAXPY
C     ------------------------------------------------------------------
      integer LDA,N,IPVT(N), K, KB, L, NM1
      real A(LDA,N), B(N), T, ZERO
      parameter ( ZERO = 0.E0 )
C     ------------------------------------------------------------------
      NM1 = N - 1
 
C        SOLVE  A * X = B
C        FIRST SOLVE  L*Y = B
 
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .ne. K) THEN
              B(L) = B(K)
              B(K) = T
            END IF
            CALL SAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   20    CONTINUE
C
C        NOW SOLVE  U*X = Y
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            IF (A(K,K) .NE. ZERO) THEN
              B(K) = B(K)/A(K,K)
            ELSE
              CALL ERMSG('SGESLD',1,0,'A diagonal element is zero','.')
              RETURN
            END IF
            T = -B(K)
            CALL SAXPY(K-1,T,A(1,K),1,B(1),1)
   40    CONTINUE
      RETURN
      END

      SUBROUTINE  SSCAL(N,SA,SX,INCX)
C>> 1985-08-02 SSCAL  Lawson  Initial code.
C
C     REPLACE SINGLE PRECISION SX BY SINGLE PRECISION SA*SX.
C     FOR I = 0 TO N-1, REPLACE SX(1+I*INCX) WITH  SA * SX(1+I*INCX)
C
      REAL SA,SX(*)
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      NS = N*INCX
          DO 10 I = 1,NS,INCX
          SX(I) = SA*SX(I)
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SX(I) = SA*SX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        SX(I) = SA*SX(I)
        SX(I + 1) = SA*SX(I + 1)
        SX(I + 2) = SA*SX(I + 2)
        SX(I + 3) = SA*SX(I + 3)
        SX(I + 4) = SA*SX(I + 4)
   50 CONTINUE
      RETURN
      END


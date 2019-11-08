C+-----------------------------------------------------------------------+
C| Program       : Direct.f                                              |
C| Last modified : 2-7-96                                                |
C| Written by    : Joerg Gablonsky                                       |
C| The algorithm DIRECT, which is a method to find the global minimum of |
C| a n-dimensional function with Lipschitzian Optimization.              |
C+-----------------------------------------------------------------------+

      SUBROUTINE Direct(fcn,x,n,eps,maxf,maxT,dwrit,fmin,
     +                  l,u,cheat,kmax,writed,loadH)

C+-----------------------------------------------------------------------+
C|    SUBROUTINE Direct                                                  |
C| On entry								 |
C|     fcn -- The argument containing the name of the user-supplied	 |
C|            subroutine that returns values for the function to be	 |
C|            minimized.						 |
C|       n -- An Integer. The dimension of the problem.                  |
C|    maxf -- An Integer. The max number of function evaluations.	 |
C|    maxT -- An Integer. The max number of iterations.                  |
C|            Direct stops when either the maximum number of iterations  |
C|            is reached or more than maxf function-evalutions were made.|
C|     eps -- Exceeding value.						 |
C|   dwrit -- An Integer. 						 |
C|              If equal 1, values of f are writen to the standart 	 |
C|              output.                                                  |
C|              IF equal 2, values of f are writen to the standart	 |
C|              output and the file "test.dat".                          |
C|       l -- The lower bounds of the hyperbox.                          |
C|       u -- The upper bounds of the hyperbox.                          |
C|   cheat -- IF cheat is equal 1, cheat with the Lipschitz constant.    |
C|    kmax -- The value to cheat with.					 |
C|  writed -- An Integer						 |
C|		If equal 1, write down the hole history of the           |
C|              optimization						 |
C|   loadH -- An Integer						 |
C|		If equal 1, load the history of a previous run.          |
C|    									 |
C| On return								 |
C|									 |
C|       x -- The final point obtained in the optimization process.	 |
C|            X should be a good approximation to the global minimum	 |
C|            for the function within the hyper-box.			 |
C|									 |
C|   fmin -- The value of the function at x.    			 |
C|									 |
C| Subroutines and functions used :					 |
C|									 |
C|  DIRpreprc,Maxf_to_high,MaxT_to_high,InitList,Init,Choose,Get_I,      |
C|  Samplepoints,Divide,InsertList,getmaxdeep,header			 |
C+-----------------------------------------------------------------------+


      IMPLICIT None
C PVM-Code
c      include '/usr/include/pvm3/fpvm3.h'
c      INTEGER mytid,mype,npe,master,pe
c      INTEGER nbuf,nbytes,msgtag,info


      INTEGER maxfunc
C     The maximum of function evaluations.
      INTEGER maxdeep,n,maxor
C     The maximum dept of the algorithem.

      PARAMETER (Maxfunc = 90000)
      PARAMETER (maxdeep = 500)
      PARAMETER (maxor = 20)
c
c     m = max problem size
c
      PARAMETER (m=32)

      INTEGER maxf,dwrit,n1
      Real*8  fcn,x(n),fmin,eps,l(n),u(n)
      Real*8  f(maxfunc)
      Integer anchor(0:maxdeep),S(maxdeep,2)
      Integer point(maxfunc), free
      Real*8  c(maxfunc,m)
      Real*8  thirds(0:maxdeep)
      Integer length(maxfunc,m),t,MaxT,j,actdeep,loadH
      Integer Minpos,file,maxpos,help,numfunc,file2
      Integer Logfile,ArrayI(m),maxi,new,oops,cheat,writed
      Integer List2(m,2),DIRGetMaxDeep,i,actmaxdeep,oldpos
      Integer tstart,start,Newtosample
      Real*8  w(m),kmax
      CHARACTER*12 A

C+-----------------------------------------------------------------------+
C| Valve - specific variables !                                          |
C+-----------------------------------------------------------------------+

      Integer iiii,ind,iprfck
      Integer isys,irange,iopt,ibounce,irpm,ibest
      Real*8 costmin,bestx
c      Real*8  dyncost,area
      Real*8  xs1(maxor),xs2(maxor)

c
c isys = 1 : system identification
c      = 2 : cam design
c
c      common/isystem/ isys
c      common/control/ irange,iopt,ibounce,irpm
c
c  ibest: 1=found ; 0=not found
c
      common /jbest/ ibest
      common /best/  costmin,bestx(20)
C+-----------------------------------------------------------------------+
C| End of Valve - specific variables !                                   |
C+-----------------------------------------------------------------------+

C+-----------------------------------------------------------------------+
C|       f -- values of functions.					 |
C|  anchor -- anchors of lists with deep i				 |
C|       S -- List of potentially optimal points			 |
C|   point -- lists							 |
C|    free -- first free position					 |
C|       c -- midpoints of arrays					 |
C|  thirds -- Length of intervals.					 |
C|  length -- Length of intervall (index)				 |
C|       t -- actual iteration						 |
C|       j -- loop-variable						 |
C| actdeep -- the actual minimal interval-length index			 |
C|  Minpos -- position of the actual minimum				 |
C|    file -- The filehandle for a datafile.                             |
C|  maxpos -- The number of intervalls, which are truncated.		 |
C|    help -- A help variable.						 |
C| numfunc -- The actual number of functionevaluations.                  |
C|   file2 -- The filehandle for an other datafile.			 |
C| Logfile -- The filehandle for the Log-file.                           |
C|  ArrayI -- Array with the indexes of the sides with maximum length.	 |
C|    maxi -- Number of directions with maximal side length.             |
C|     new -- Index of the first inserted point.                         |
C|    oops -- Flag which shows if anything went wrong in the 		 |
C|            initialisation.                                            |
C|   List2 -- List of indicies of intervalls, which are to be truncated. |
C|       i -- Another loop-variable.                                     |
C| actdeep -- The actual maximum (minimum) of possible Intervallength.   |
C|  oldpos -- The old index of the minimum. Used to print only, if there |
C|            is a new minimum found.                                    |
C|       w -- Array used to divide the intervalls                        |
C|       A -- Name of file to story steps of the algorithem.             |
C|  tstart -- The start of the outer loop.				 |
C|   start -- The postion of the starting point in the inner loop.       |
C| Newtosample -- The total number of points to sample in the inner loop.|
C+-----------------------------------------------------------------------+


      logfile = 15
      OPEN(logfile,file="log.dat")
C+-----------------------------------------------------------------------+
C| Write the header of the logfile.					 |
C+-----------------------------------------------------------------------+
      CALL DIRheader(logfile)  
C+-----------------------------------------------------------------------+
C| If there are to many function evaluations or to many iteration, note  |
C| this and STOP.							 |
C+-----------------------------------------------------------------------+
      IF ((maxf+20) .GT. maxfunc) THEN
         CALL DIRmaxf_to_high(maxf,maxfunc,dwrit,logfile)
         STOP
      END IF
      IF (maxT .GT. maxdeep) THEN
         CALL DIRMaxT_to_high(maxT,maxdeep,dwrit,logfile)
         STOP
      END IF
      Write(*,*) Maxf, maxt

C+-----------------------------------------------------------------------+
C| Start of valve-specific initialisation				 |
C+-----------------------------------------------------------------------+

c  iiii: random number index
c      iiii=7342
c
c  f: unscaled
      
C      x(1) =   .186927E+08   
C      x(2) =.183722E+04   
C      x(3) =.119165E+08   
C      x(4) =.101684E+04
C      x(5) =.135621E+08   
C      x(6) =.641078E+03   
C      x(7) =.909669E+08   
C      x(8) =.167301E+04
C      x(9) =.124735E+00   
C      x(10) =.216653E+01   
C      x(11) =.325213E+00   
C      x(12) =.348176E-01

c      if(isys.eq.1 .or. isys.eq.3) call fcostrs(n,x,fmin,ind)
c      if(isys.eq.2) call fcost2(n,x,fmin,iprfck,dyncost,area,ind)
c
c  iopt=2 : print out simulation result
c      if(iopt.eq.2) return
c      if(iprfck.eq.1) then
c        write(6,3412)
c        write(11,3412)
c3412    format(' Initial cam profile is not usable.')
c        f=1000.
c      endif
c      if(ind.lt.0) fmin=1000.
c      write(6,600) fmin
c      write(11,600) fmin
c600   format(' Cost Function= ',e13.6)
c      write(6,113) (x(i),i=1,n)
c      write(11,113) (x(i),i=1,n)
c113   format(4(e13.6,1x))
c
c Initial the best cost function and  parameters and set
c the "find flag" to be 0=not found
c
c      ibest = 0
c      costmin = fmin
c      do 2 i=1,n
c2       bestx(i) = x(i)
c155   continue
C+-----------------------------------------------------------------------+
C| End of valve-specific initialisation				         |
C+-----------------------------------------------------------------------+
      CALL DIRInitSpecific(x,n)

      IF (cheat .EQ. 1) THEN
        Write(*,1113) eps,kmax
      ELSE
        Write(*,1112) eps
      END IF
1112  FORMAT("eps = ",f5.3,"  no cheating")
1113  FORMAT("eps = ",f5.3,"  kmax = ",f6.3)

      file = 12       
      file2 = 0
C+-----------------------------------------------------------------------+
C| Initialiase the Lists.						 |
C+-----------------------------------------------------------------------+
      CALL DIRInitList(anchor,free,point,f,maxfunc,maxdeep)
      IF (loadH .EQ. 1) THEN 
C+-----------------------------------------------------------------------+
C| Initialise the algorithmen DIRECT.                                    |
C+-----------------------------------------------------------------------+
        CALL DIRInit(f,fcn,c,length,actdeep,point,anchor,free,
     +   dwrit,logfile,ArrayI,maxI,List2,w,x,xs1,xs2,fmin,minpos,
     +   thirds,maxfunc,maxdeep,n)
        CALL DIRLoadHist(x,fmin,eps,l,u,f,anchor,
     +           point,free,c,length,tstart,actdeep,Minpos,numfunc,
     +           cheat,kmax,maxfunc,maxdeep,n)
      ELSE
C+-----------------------------------------------------------------------+
C| Call the routine to initialise the mapping of x from the n-dimensional|
C| unit cube to the hypercube given by u and l.                          |
C+-----------------------------------------------------------------------+
        CALL DIRpreprc(u,l,n,maxf,dwrit,xs1,xs2,oops)
        IF (oops .GT. 0) THEN
          Write(*,*) "Initialisation failed !!!"
          Return
        END IF
        tstart = 2

C+-----------------------------------------------------------------------+
C| Initialise the algorithmen DIRECT.                                    |
C+-----------------------------------------------------------------------+
        CALL DIRInit(f,fcn,c,length,actdeep,point,anchor,free,
     +   dwrit,logfile,ArrayI,maxI,List2,w,x,xs1,xs2,fmin,minpos,
     +   thirds,maxfunc,maxdeep,n,iprfck)
        numfunc = 1 + maxI + maxI
        Write(*,*) "Init"
        Write(*,1001) t,numfunc,fmin,minpos
        actmaxdeep = 1
        oldpos = 0
        tstart = 2
      END IF
C+-----------------------------------------------------------------------+
C| Main loop!								 |
C+-----------------------------------------------------------------------+
      DO 10,t=tstart,MaxT
        actdeep = t-1
C+-----------------------------------------------------------------------+
C| Choose the sample points.						 |
C+-----------------------------------------------------------------------+
        CALL DIRChoose(anchor,S,t-1,f,fmin,eps,thirds,maxpos,
     +            length,maxfunc,maxdeep,n,logfile,dwrit,cheat,kmax)
        oldpos = minpos
C+-----------------------------------------------------------------------+
C| Initialisiese the number of sample points in this outer loop.	 |
C+-----------------------------------------------------------------------+
        Newtosample = 0
        DO 20,j=maxpos,1,-1
C+-----------------------------------------------------------------------+
C| If the actual index is a point to sample, do it.			 |
C+-----------------------------------------------------------------------+
          IF (S(j,1) .GT. 0) THEN
            actdeep = S(j,2)
            IF (actdeep .GT. actmaxdeep) THEN
              actmaxdeep = actdeep
            END IF
            help = S(j,1)
            anchor(actdeep) = point(help)
C+-----------------------------------------------------------------------+
C| Get the Directions in which to decrease the intervall-length.         |
C+-----------------------------------------------------------------------+
            CALL DIRGet_I(length,help,ArrayI,maxI,n,maxfunc)
C+-----------------------------------------------------------------------+
C| Sample the function.                                                  |
C+-----------------------------------------------------------------------+
            CALL DIRSamplepoints(c,ArrayI,thirds(actdeep+1),help,
     +           start,
     +	         length,dwrit,logfile,f,free,maxI,point,fcn,x,xs2,
     +           fmin,minpos,xs1,n,maxfunc,maxdeep,oops)
            IF (oops .GT. 0) THEN
               Write(*,*) "Error Samplepoints"
            END IF
            Newtosample = newtosample + maxI
            CALL DIRSamplef(c,ArrayI,thirds(actdeep+1),help,start,
     +	         length,dwrit,logfile,f,free,maxI,point,fcn,x,xs1,
     +           fmin,minpos,xs2,n,maxfunc,maxdeep,oops,iprfck)
C+-----------------------------------------------------------------------+
C| Divide the intervalls.						 |
C+-----------------------------------------------------------------------+
            CALL DIRDivide(start,actdeep,length,point,
     +	           ArrayI,help,List2,w,maxI,f,maxfunc,maxdeep,n)
C+-----------------------------------------------------------------------+
C| Insert the new intervalls in the list (sorted).			 |
C+-----------------------------------------------------------------------+
            CALL DIRInsertList(start,anchor,point,f,maxI,length,
     +                    maxfunc,maxdeep,n,help)

C+-----------------------------------------------------------------------+
C| Increase the number of function evaluations.                          |
C+-----------------------------------------------------------------------+
            numfunc = numfunc + maxI + maxI
          END IF
C+-----------------------------------------------------------------------+
C| End of main loop.							 |
C+-----------------------------------------------------------------------+
20      CONTINUE
C+-----------------------------------------------------------------------+
C| If there is a new minimum, show the actual iteration, the number of   |
C| function evaluations, the minimum value of f (so far) and the position|
C| in the array.							 |
C+-----------------------------------------------------------------------+
C        IF (oldpos .LT. minpos) THEN
          Write(*,1001) t,numfunc,fmin,minpos
	  Write(*,1002) (bestx(i),i=1,n)
1002      format(3(e18.12,1x))
C        END IF
        IF (numfunc .GT. maxf) THEN
          GOTO 100
        END IF
10    CONTINUE
C+-----------------------------------------------------------------------+
C| Store the position of the minimum in x.				 |
C+-----------------------------------------------------------------------+
100   DO 50,i=1,n
	x(i) = bestx(i)
50    CONTINUE
      fmin = costmin
      IF (writed .EQ. 1) THEN
        CALL DIRWriteDown(x,fmin,eps,l,u,f,anchor,
     +           point,free,c,length,t,actdeep,Minpos,numfunc,
     +           cheat,kmax,maxfunc,maxdeep,n)
      END IF
      Write(*,*) "Number of function evaluations total : ",
     +            numfunc
      CLOSE(logfile)
1001  FORMAT(i4," & ",i5," & ",f18.14," & ",i5," \\")

      END


      SUBROUTINE DIRWriteDown(x,fmin,eps,l,u,f,anchor,
     +           point,free,c,length,t,actdeep,Minpos,numfunc,
     +           cheat,kmax,maxfunc,maxdeep,n)
C+-----------------------------------------------------------------------+
C|    SUBROUTINE DIRWriteDown                                            |
C|    Writes into a file the whole history of the iterations.            |
C+-----------------------------------------------------------------------+
      IMPLICIT None
      INTEGER maxfunc
C     The maximum of function evaluations.
      INTEGER maxdeep,n
C     The maximum dept of the algorithem.
      Real*8  x(n),fmin,eps,l(n),u(n)
      Real*8  f(maxfunc)
      Integer anchor(0:maxdeep)
      Integer point(maxfunc),free
      Real*8  c(maxfunc,n)
      Integer length(maxfunc,n),t,j,actdeep
      Integer Minpos,numfunc
      Integer cheat
      Integer i
      Real*8  kmax

      OPEN(12,FILE = "direct_.dat",FORM = "UNFORMATTED")
      Write(12) numfunc
      Write(12) fmin
      Write(12) eps
      Write(12) t
      Write(12) Minpos
      Write(12) cheat
      Write(12) kmax
      Write(12) n
      Write(12) free
      DO 10,i=1,n
        Write(12) x(i)
        Write(12) l(i)
        Write(12) u(i)
10    CONTINUE
      DO 20,i=1,numfunc
        Write(12) f(i)
        Write(12) point(i)
        DO 30,j=1,n
          Write(12) c(i,j)
          Write(12) length(i,j)
30      CONTINUE
20    CONTINUE
      DO 40,i=0,t
        Write(12) anchor(i)
40    CONTINUE
      CLOSE(12)
      END


      SUBROUTINE DIRLoadHist(x,fmin,eps,l,u,f,anchor,
     +           point,free,c,length,t,actdeep,Minpos,numfunc,
     +           cheat,kmax,maxfunc,maxdeep,n)
C+-----------------------------------------------------------------------+
C|    SUBROUTINE DIRLoadHist                                                |
C|    Load the whole history of a previous run und go on with the optimi-|
C|    zation.								 |
C+-----------------------------------------------------------------------+
      IMPLICIT None
      INTEGER maxfunc
C     The maximum of function evaluations.
      INTEGER maxdeep,n,nhelp
C     The maximum dept of the algorithem.
      Real*8  x(n),fmin,eps,l(n),u(n)
      Real*8  f(maxfunc)
      Integer anchor(0:maxdeep)
      Integer point(maxfunc),free
      Real*8  c(maxfunc,n)
      Integer length(maxfunc,n),t,j,actdeep
      Integer Minpos,numfunc
      Integer cheat
      Integer i
      Real*8  kmax

      OPEN(12,FILE = "direct_.dat",FORM = "UNFORMATTED")
      Read(12) numfunc
      Read(12) fmin
      Read(12) eps
      Read(12) t
      Read(12) Minpos
      Read(12) cheat
      Read(12) kmax
      Read(12) nhelp
      Read(12) free
      IF (.NOT. (nhelp .EQ. n)) THEN
        Write(*,*) "The dimension of the loaded problem is not ",
     +	           "the same as the current dimension of the ",
     +		   "problem"
        STOP
      END IF
      IF (numfunc .GT. maxfunc) THEN
        Write(*,*) "There are not enough free spaces for functions"
        STOP
      END IF
      IF (t .GT. maxdeep) THEN
        Write(*,*) "The actual maxdeep is not big enough."
        STOP
      END IF
      DO 10,i=1,n
        Read(12) x(i)
        Read(12) l(i)
        Read(12) u(i)
10    CONTINUE
      DO 20,i=1,numfunc
        Read(12) f(i)
        Read(12) point(i)
        DO 30,j=1,n
          Read(12) c(i,j)
          Read(12) length(i,j)
30      CONTINUE
20    CONTINUE
      DO 40,i=0,t
        Read(12) anchor(i)
40    CONTINUE
      CLOSE(12)
      END


      SUBROUTINE DIRChoose(anchor,S,actdeep,f,fmin,eps,thirds,maxpos,
     +           length,maxfunc,maxdeep,n,logfile,dwrit,cheat,kmax)
      IMPLICIT None
      Integer maxfunc,maxdeep,n
      Integer Anchor(0:maxdeep),S(maxdeep,2),length(maxfunc,n)
      Integer actdeep,DIRGetMaxdeep,dwrit
      Real*8 f(maxfunc),fmin,eps,thirds(0:maxfunc)
      Integer maxpos,i,j,k,i_,j_,logfile,cheat
      Real*8 help2,helplower,helpgreater,kmax

        helplower = 1.D20
        helpgreater = 0.D0
        k = 1
        DO 10,j=0,actdeep
          IF (anchor(j) .GT. 0) THEN
            S(k,1) = anchor(j)
            S(k,2) = DIRGetmaxdeep(S(k,1),length,maxfunc,n)
            k = k + 1
          END IF
10      CONTINUE
        maxpos = k - 1
        IF (maxpos .LE. 2) THEN
          IF (dwrit .EQ. 2) THEN
            WRITE(logfile,*) "maxpos <= 2",maxpos
          END IF
          RETURN
        ENDIF
        DO 11,j=k-1,maxdeep
          S(k,1) = 0
11      CONTINUE
       DO 40,j=maxpos,1,-1
         helplower = 1.D20
         helpgreater = 0.D0
         j_ = S(j,1)
         DO 30,i=1,maxpos
           i_ = S(i,1)
           IF ((i_ .GT. 0) .AND. .NOT. (i .EQ. j)) THEN
             help2 = thirds(S(i,2)) - thirds(S(j,2))
             IF (help2 .GT. 0) THEN
                help2 = (f(i_) - f(j_))/help2
                IF (help2 .LE. 0.D0) THEN
                  IF (dwrit .EQ. 2) THEN
                    Write(logfile,*) "thirds > 0,help2 <= 0"
                  END IF
                  GOTO 60
                END IF
                IF (help2 .LT. helplower) THEN
                  IF (dwrit .EQ. 2) THEN
                    Write(logfile,*) "helplower = ",help2
                  END IF
                  helplower = help2
                END IF
             ELSE
               help2 = (f(i_) - f(j_))/help2
               IF (help2 .LE. 0.D0) THEN
                  IF (dwrit .EQ. 2) THEN
                    Write(logfile,*) "thirds < 0,help2 <= 0"
                  END IF
                  GOTO 60
                END IF
                IF (help2 .GT. helpgreater) THEN
                  IF (dwrit .EQ. 2) THEN
                    Write(logfile,*) "helpgreater = ",help2
                  END IF
                  helpgreater = help2
                END IF
             END IF
           END IF
30       CONTINUE
         IF (helpgreater .LE. helplower) THEN
           IF ((cheat .EQ. 1) .AND. (helplower .GT. Kmax)) THEN
             helplower = Kmax
           END IF
           IF ((f(j_) - helplower * thirds(S(j,2))) .GT. 
     +        (fmin - eps*abs(fmin))) THEN
              IF (dwrit .EQ. 2) THEN
                Write(logfile,*) "> fmin - eps|fmin|"
              END IF
              GOTO 60
            END IF
         ELSE
           IF (dwrit .EQ. 2) THEN
           Write(logfile,*) "helpgreater > helplower",helpgreater,
     +            helplower,helpgreater - helplower
           END IF
           GOTO 60
         END IF
         GOTO 40
60       S(j,1) = 0
40     CONTINUE

      END




C+-----------------------------------------------------------------------+
C| INTEGER Function GetmaxDeep						 |
C| function to get the maximal length (1/length) of the n-dimensional    |
C| rectangle with midpoint pos.                                          |
C|									 |
C| On Return :								 |
C|    the maximal length						 |
C|									 |
C| pos     -- the position of the midpoint in the array length		 |
C| length  -- the array with the dimensions				 |
C| maxfunc -- the leading dimension of length				 |
C| n	   -- the dimension of the problem				 |
C+-----------------------------------------------------------------------+

      INTEGER Function DIRGetmaxDeep(pos,length,maxfunc,n)
      IMPLICIT None
      INTEGER pos,maxfunc,n,length(maxfunc,n),help,i

      help = length(pos,1)
      DO 10,i = 2,n
        IF (length(pos,i) .LT. help) THEN
           help = length(pos,i)
        END IF
10    CONTINUE
      DIRGetMaxDeep = help
      END

      SUBROUTINE DIRInsertSamp(new,anchor,point,f,maxI,
     +                      length,maxfunc,maxdeep,n,samp)
      IMPLICIT None
      INTEGER maxfunc,maxdeep,j,maxI,n,samp
      INTEGER pos1,pos2,pos,new,deep,anchor(0:maxdeep)
      INTEGER point(maxfunc),DIRGetMaxdeep,length(maxfunc,n)
      Real*8  f(maxfunc)

      deep = DIRGetMaxdeep(samp,length,maxfunc,n)
      pos = anchor(deep)
      IF (pos .EQ. 0) THEN
        anchor(deep) = samp
      ELSE
        IF (f(samp) .LT. f(pos)) THEN
           anchor(deep) = samp
           point(samp) = pos
        ELSE
         CALL DIRInsert(pos,samp,point,f,maxfunc)
        END IF
      END IF
      END

      SUBROUTINE DIRInsertList(new,anchor,point,f,maxI,
     +                      length,maxfunc,maxdeep,n,samp)
      IMPLICIT None
      INTEGER maxfunc,maxdeep,j,maxI,n,samp,i
      INTEGER pos1,pos2,pos,new,deep,anchor(0:maxdeep)
      INTEGER point(maxfunc),DIRGetMaxdeep,length(maxfunc,n)
      Real*8  f(maxfunc)

      DO 10,j = 1,maxI
        pos1 = new
        pos2 = point(pos1)
        new = point(pos2)
        deep = DIRGetMaxdeep(pos1,length,maxfunc,n)
        IF (anchor(deep) .EQ. 0) THEN
          IF (f(pos2) .LT. f(pos1)) THEN
            anchor(deep) = pos2
            point(pos2) = pos1
            point(pos1) = 0
          ELSE
            anchor(deep) = pos1
            point(pos2) = 0
          END IF
        ELSE
          pos = anchor(deep)
          IF (f(pos2) .LT. f(pos1)) THEN
            IF (f(pos2) .LT. f(pos)) THEN
              anchor(deep) = pos2
              point(pos2) = pos
            ELSE
              CALL DIRInsert(pos,pos2,point,f,maxfunc)
            END IF
            CALL DIRInsert(pos,pos1,point,f,maxfunc)
          ELSE
            IF (f(pos1) .LT. f(pos)) THEN
              anchor(deep) = pos1
              point(pos1) = pos
            ELSE
              CALL DIRInsert(pos,pos1,point,f,maxfunc)
            END IF
            CALL DIRInsert(pos,pos2,point,f,maxfunc)
          END IF
        END IF
10    CONTINUE
      deep = DIRGetMaxdeep(samp,length,maxfunc,n)
      pos = anchor(deep)
      IF (f(samp) .LT. f(pos)) THEN
         anchor(deep) = samp
         point(samp) = pos
      ELSE
         CALL DIRInsert(pos,samp,point,f,maxfunc)
      END IF
      END

      SUBROUTINE DIRInsertList_2(start,j,k,List2,w,maxI,n)
      IMPLICIT None
      INTEGER start,n,j,k
      INTEGER List2(n,2)
      Real*8 w(n)
      INTEGER pos,i,maxI

        pos = start
        IF (start .EQ. 0) THEN
          List2(j,1) = 0
          start = j
          GOTO 50
        END IF
        IF (w(start) .GT. w(j)) THEN
          List2(j,1) = start
          start = j
        ELSE
          DO 10,i=1,maxI
            IF (List2(pos,1) .EQ. 0) THEN
              List2(j,1) = 0
              List2(pos,1) =  j
              GOTO 50
            ELSE
              IF (w(j) .LT. w(List2(pos,1))) THEN
                List2(j,1) = List2(pos,1)
                List2(pos,1) = j
                GOTO 50
              END IF
            END IF
            pos = List2(pos,1)
10	  CONTINUE
        END IF
50     List2(j,2) = k 

      END

      SUBROUTINE DIRSearchmin(start,List2,pos,k,n)
      IMPLICIT None
      Integer start,pos,k,n
      INTEGER List2(n,2)

        k = start
        pos = List2(start,2)
        start = List2(start,1)
      END

      SUBROUTINE DIRInit(f,fcn,c,length,actdeep,point,anchor,free,
     + dwrit,logfile,ArrayI,maxI,List2,w,x,l,u,fmin,minpos,thirds,
     + maxfunc,maxdeep,n,iprfck)
      IMPLICIT None
      Integer maxfunc,maxdeep,n
      Real*8  f(maxfunc),c(maxfunc,n),fmin,thirds(0:maxdeep)
      Real*8  x(n),delta
      Integer length(maxfunc,n),actdeep,minpos,i,oops
      Integer point(maxfunc),anchor(0:maxdeep),free,iprfck
      Integer ArrayI(n),maxI,new,dwrit,logfile,List2(n,2)
      Real*8  w(n)
      Real*8 fcn
C      EXTERNAL fcn
      Real*8 help2,l(n),u(n)
C      Integer isys
      Integer ibest
      Real*8 costmin,bestx
c
c isys = 1 : system identification
c      = 2 : cam design
C      common/isystem/ isys
c
c  ibest: 1=found ; 0=not found
c
      common /jbest/ ibest
      common /best/  costmin,bestx(20)

        fmin = 1.D20
	costmin = fmin
        help2 = 3.D0
        DO 10,i = 1,maxdeep
          thirds(i) = 1.D0 / help2
          help2 = help2 * 3.D0
10      CONTINUE
        thirds(0) = 1.D0
        DO 20,i=1,n
          c(1,i) = 0.5D0
          x(i) = 0.5D0
          length(1,i) = 0
20      CONTINUE
C        CALL DIRinfcn(fcn,x,l,u,n,f(1))
         CALL ifcinfcn2(fcn,x,l,u,n,f(1),iprfck)
C	 Write(*,*) fmin,costmin
	 fmin = f(1)
	 minpos = 3
	 costmin = f(1)
	 do 31,i=1,n
	   bestx(i) = x(i)
31       continue
C	 Write(*,*) (x(i),i=1,12)
C	 Write(*,*) f(1)
C	 Write(*,*) 'DIRInit 2'

C PVM Code
C+-----------------------------------------------------------------------+
C| Send the common data to all slave processes.                          |
C+-----------------------------------------------------------------------+
        CALL DIRSend_Common(l,u,n)
C        f(1) = fcn(x,c,1,l,u,n,maxfunc)
        actdeep = 2
        point(1) = 0
        free = 2
        delta = thirds(1)
        CALL DIRGet_I(length,1,ArrayI,maxI,n,maxfunc)
        new = free
        CALL DIRSamplepoints(c,ArrayI,delta,1,new,length,
     +           dwrit,logfile,f,free,maxI,point,fcn,x,l,
     +           fmin,minpos,u,n,
     +           maxfunc,maxdeep,oops)
        CALL DIRSamplef(c,ArrayI,delta,1,new,length,
     +           dwrit,logfile,f,free,maxI,point,fcn,x,l,
     +           fmin,minpos,u,n,
     +           maxfunc,maxdeep,oops,iprfck)
        CALL DIRDivide(new,0,length,point,
     +	   ArrayI,1,List2,w,maxI,f,maxfunc,maxdeep,n)
        CALL DIRInsertList(new,anchor,point,f,maxI,length,
     +                      maxfunc,maxdeep,n,1)
      END

      SUBROUTINE DIRDivide(new,currentlength,length,point,
     +	   ArrayI,sample,List2,w,maxI,f,maxfunc,maxdeep,n)
      IMPLICIT None
      INTEGER start,new,maxfunc,maxdeep,n,sample
      INTEGER currentlength,length(maxfunc,n)
      INTEGER point(maxfunc)
      INTEGER List2(n,2),maxI,ArrayI(n)
      Real*8 f(maxfunc),w(n)
      INTEGER pos,i,j,k,pos2

        start = 0
        pos = new
        DO 10,i=1,maxI
          j = ArrayI(i)
          w(j) = f(pos)
          k = pos
          pos = point(pos)
          IF (f(pos) .LT. w(j)) THEN
            w(j) = f(pos)
          END IF
          pos = point(pos)
          CALL DIRInsertList_2(start,j,k,list2,w,maxI,n)
10     CONTINUE
       IF (pos .GT. 0) THEN
           Write(*,*) "Error Divide"
           STOP
       END IF
       DO 20,j=1,maxI
         CALL DIRSearchmin(start,List2,pos,k,n)
         pos2 = start
         length(sample,k) = currentlength + 1
         DO 30,i=1,maxI-j+1
           length(pos,k) = currentlength + 1
           pos = point(pos)
           length(pos,k) = currentlength + 1
           pos = List2(pos2,2)
           pos2 = List2(pos2,1)
30       CONTINUE
20     CONTINUE
      END


      SUBROUTINE DIRSamplepoints(c,ArrayI,delta,sample,start,length,
     +           dwrit,logfile,f,free,maxI,point,fcn,x,l,fmin,minpos,
     +           u,n,
     +           maxfunc,maxdeep,oops) 
      IMPLICIT None
      INTEGER n,maxfunc,maxdeep,oops
      INTEGER maxI,ArrayI(n),sample,new
      INTEGER length(maxfunc,n),free,point(maxfunc),i
      Real*8 c(maxfunc,n),delta,fcn,x(n),l(n),u(n),f(maxfunc)
      Real*8 fmin
      INTEGER pos,j,k,dwrit,logfile,minpos
      Integer start

      oops = 0
      pos = free
      start = free
      DO 10,k=1,maxI+maxI
        DO 20,j=1,n
          length(free,j) = length(sample,j)
          c(free,j) = c(sample,j)
20      CONTINUE
        pos = free
        free = point(free)
        IF (free .EQ. 0) THEN
           Write(*,1000) 
           Write(*,1001) 
           IF (dwrit .EQ. 2) THEN
             Write(logfile,1000)
             Write(logfile,1001)
           END IF
           oops = 1
           RETURN
1000  FORMAT("Error, no more free positions !")
1001  FORMAT("Increase maxfunc !")
        END IF
10    CONTINUE
      point(pos) = 0
      pos = start
      DO 30,j=1,maxI
         c(pos,ArrayI(j)) = c(sample,ArrayI(j)) + delta
         pos = point(pos)
         c(pos,ArrayI(j)) = c(sample,ArrayI(j)) - delta
         pos = point(pos)
30    CONTINUE
      IF (pos .GT. 0) THEN
          Write(*,2000)
          IF (dwrit .EQ. 2) THEN
             Write(logfile,2000)
           END IF
          STOP
2000      FORMAT("Error ! ") 
      END IF
      END

      SUBROUTINE DIRGet_I(length,pos,ArrayI,maxi,n,maxfunc)
      IMPLICIT None
      Integer maxfunc,n,maxi,pos
      Integer length(maxfunc,n),ArrayI(n),i,help,j

      j = 1
      help = length(pos,1)
      DO 10,i = 2,n
        IF (length(pos,i) .LT. help) THEN
           help = length(pos,i)
        END IF
10    CONTINUE
      DO 20,i = 1,n
        IF (length(pos,i) .EQ. help) THEN
           ArrayI(j) = i
           j = j + 1
        END IF
20    CONTINUE
      maxi = j - 1
      END

      SUBROUTINE DIRInitList(anchor,free,point,f,maxfunc,maxdeep)
      IMPLICIT None
      Integer maxdeep,maxfunc
      Real*8 f(maxfunc)
C   f -- values of functions.
      Integer anchor(0:maxdeep)
C   anchor -- anchors of lists with deep i
      Integer point(maxfunc), free
C   point -- lists
C   free  -- first free position
      Integer i

      DO 10,i = 0,maxdeep
        anchor(i) = 0
10    CONTINUE
      DO 20,i = 1,maxfunc
        f(i) = 0.D0
        point(i) = i + 1
C       point(i) = 0
20    CONTINUE
      point(maxfunc) = 0
      free = 1
      END

      SUBROUTINE DIRShowlist(point,f,third,c,anchor,file,file2,
     +                    l,u,maxfunc,n)
      IMPLICIT None
      INTEGER maxfunc,anchor,file,file2,n
      INTEGER point(maxfunc),pos,i
      Real*8 f(maxfunc),c(maxfunc,n),third
      Real*8 l(n),u(n)

      pos = anchor
      DO 10,i = 1,maxfunc
        Write(file,1000) third,f(pos)
        pos = point(pos)
        IF (pos .EQ. 0) THEN 
          GOTO 20
        END IF
10    CONTINUE
1000  FORMAT(f21.18,"   ",f22.18)
20    END


      SUBROUTINE DIRInsert3(pos1,pos2,pos3,deep,anchor,point,free,
     +                   f,fmin,minpos,maxfunc,maxdeep)
      IMPLICIT None
      INTEGER maxfunc,maxdeep
      INTEGER deep,free,pos1,pos2,pos3
      INTEGER anchor(0:maxdeep),point(maxfunc)
      Real*8 f(maxfunc),fmin
      INTEGER pos,minpos

      CALL DIRSort3(pos1,pos2,pos3,f,maxfunc)
      IF (anchor(deep) .EQ. 0) THEN
        anchor(deep) = pos1
        point(pos1) = pos2
        point(pos2) = pos3
        point(pos3) = 0
      ELSE
        pos = anchor(deep)
        IF (f(pos1) .LT. f(pos)) THEN
          anchor(deep) = pos1
          point(pos1) = pos
        ELSE
          CALL DIRInsert(pos,pos1,point,f,maxfunc)
        END IF
        CALL DIRInsert(pos,pos2,point,f,maxfunc)
        CALL DIRInsert(pos,pos3,point,f,maxfunc)	  	
      END IF
        IF (f(pos1) .LT. fmin) THEN
           fmin = f(pos1)
           minpos = pos1
        END IF
      END

      SUBROUTINE DIRInsert(start,ins,point,f,maxfunc)
      IMPLICIT None
      INTEGER maxfunc,start,ins
      INTEGER point(maxfunc)
      Real*8 f(maxfunc)
      INTEGER i,help

      DO 10,i = 1,maxfunc
        IF (f(ins) .LT. f(point(start))) THEN
          help = point(start)
          point(start) = ins
          point(ins) = help
          GOTO 20
        END IF
        IF (point(start) .EQ. 0) THEN
           point(start) = ins
           point(ins) = 0
           GOTO 20
        END IF
        start = point(start)   
10    CONTINUE
20    END

      SUBROUTINE DIRSort3(pos1,pos2,pos3,f,maxfunc)
      IMPLICIT None
      INTEGER maxfunc
      Real*8 f(maxfunc)
      INTEGER pos1,pos2,pos3,help

      IF (f(pos1) .LT. f(pos2)) THEN
         IF (f(pos1) .LT. f(pos3)) THEN
           IF (f(pos3) .LT. f(pos2)) THEN
             help = pos2
             pos2 = pos3
             pos3 = help
           END IF
         ELSE
           help = pos1
           pos1 = pos3
           pos3 = pos2
           pos2 = help
         END IF 
      ELSE 
         IF (f(pos2) .LT. f(pos3)) THEN
           IF (f(pos3) .LT. f(pos1)) THEN
             help = pos1
             pos1 = pos2
             pos2 = pos3
             pos3 = help
           ELSE
             help = pos1
             pos1 = pos2
             pos2 = help
           END IF
         ELSE
           help = pos1
           pos1 = pos3
           pos3 = help
         END IF
      END IF
      END


      subroutine DIRpreprc(u,l,n,maxit,
     + writ,xs1,xs2,oops)

        integer n,i,maxit,writ,oops

        real*8 u(n),l(n),xs1(n),xs2(n)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                       SUBROUTINE DIRPREPRC
C
C Subroutine DIRpreprc is the preprocessing subroutine for iffco.
C Subroutine DIRpreprc uses an afine mapping to map the hyper-box given by
C the constraints on the variable x onto the n-dimensional unit cube.
C This mapping is done using the following equation:
C
C               x(i)=x(i)/(u(i)-l(i))-l(i)/(u(i)-l(i)).
C
C DIRpreprc checks the following:
C
C      1. if the bounds l and u are well-defined. That is, if
C
C               l(i) < u(i) forevery i.
C
C      2. if the maximum number of iterates for each scale is set.
C         If not, maxit is set to 200.
C
C
C On entry
C
C          u -- A double-precision vector of length n. The vector
C               containing the upper bounds for the n independent
C               variables.
C
C          l -- A double-precision vector of length n. The vector
C               containing the lower bounds for the n independent
C               variables.
C
C          n -- An integer. The dimension of the problem.
C
C      maxit -- A user-supplied integer. Maxit specifies the maximum number
C               of iterations the algorithm is allowed to take at a given
C               scale.
C
C       writ -- A user-supplied integer. Writ specifies the verbosity
C               of the algorithm:
C
C                   writ = 1     the algorithm writes error
C                                messages and the values of
C                                user-supplied parameters
C                                to standard output.
C
C                   writ = 2     the algorithm produces the
C                                same information as for write=1
C                                plus a history of the iterative
C                                process.
C
C               If writ is equal to any other number no output is
C               produced.
C
C
C On return
C
C        xs1 -- A double-precision vector of length n, used for scaling
C               and unscaling the vector x.
C
C        xs2 -- A double-precision vector of length n, used for scaling
C               and unscaling the vector x.
C
C
C       oops -- An integer. If an upper bound is less than a lower
C               bound or if the initial point is not in the 
C               hyper-box oops is set to 1 and iffco terminates.
C 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        oops=0

        do 20 i=1,n

C
C Check if the hyper-box is well-defined.
C
            if(u(i).le.l(i))then
            if(writ.eq.1.or.writ.eq.2)then
               write(6,10)i
10             format('Upper bound less than or equal to lower bound ',
     +'for constraint number',i5.1)
            end if
               oops=1
               return
            end if

20    continue

C
C Check if maximum number of iterates for each scale maxit is set.
C If not, set maxit = 200.
C
            if(maxit.le.0)then
               maxit=200
               if(writ.eq.1.or.writ.eq.2)then
                  write(6,*)'Default maximum number of iterates ',
     +'used. maxit=200'
               end if
            end if


C
C Scale the initial iterate so that it is in the unit cube.
C
      do 50 i=1,n
            xs1(i)=(u(i)-l(i))
            xs2(i)=l(i)/xs1(i)
 50     continue

        return
        end

      subroutine ifcinfcn2(fcn,x,xs1,xs2,n,f,iprfck)
      implicit real*8 (a-h,o-z)
      integer n,i
      real*8 xb(20),x(n),xs1(n),xs2(n),f,fcn
c
c isys = 1 : system identification
c      = 2 : cam design
      common/isystem/ isys
c
c  ibest: 1=found ; 0=not found
c
      common /jbest/ ibest
      common /best/  costmin,bestx(20)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                       SUBROUTINE IFCINFCN
C
C Subroutine ifcinfcn unscales the variable x for use in the user-supplied
C function-evaluation subroutine fcn. After fcn returns to ifcinfcn, ifcinfcn
C then rescales x for use by iffco. Ifcinfcn also scales the function value
C f returned by fcn by dividing f with the user-supplied constant fscale.
C Ifcinfcn also updates the function-evaluation counter func.
C
C On entry
C
C        fcn -- The argument containing the name of the user-supplied
C               subroutine that returns values for the function to be
C               minimized.
C
C          x -- A double-precision vector of length n. The point at which
C               the derivative is to be evaluated.
C
C        xs1 -- A double-precision vector of length n. Used for scaling
C               and unscaling the vector x by ifcinfcn.
C
C        xs2 -- A double-precision vector of length n. Used for scaling
C               and unscaling the vector x by ifcinfcn.
C
C     fscale -- A user-supplied double-precision constant used to scale
C               the function. Fscale should be an approximation to the
C               maximum size of the absolute value of the function f in
C               the hyper-box.
C
C          n -- An integer. The dimension of the problem.
C
C On return
C
C       func -- An integer. The function-evaluation counter. Func is updated
C               by 1 every time ifcinfcn is called.
C
C          f -- A double-precision scalar. The function value at x scaled for use
C               by iffco, ifcgrad, or ifclines.
C
C Subroutines and Functions
C
C The subroutine whose name is passed through the argument fcn.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C Unscale the variable x.
C xb: scaled  ; x: unscaled
      do 20 i=1,n
        xb(i)=x(i)
        x(i)=x(i)*xs1(i)+xs1(i)*xs2(i)
20    continue
C
C Call the function-evaluation subroutine fcn.
C
      if(isys.eq.1) then
         call fcostrs(n,x,f,ind)
      elseif(isys.eq.2) then
         call fcost2(n,x,f,iprfck,dyncost,area,ind)
      end if
      if(ind.lt.0) iprfck=1
      if(f.lt.costmin) then
         ibest = 1
         costmin = f
         do 27 i=1,n
27         bestx(i) = x(i)
      endif

      return
      end

      subroutine DIRinfcn(fcn,x,c1,c2,n,f)

      integer n,i

      real*8 x(n),c1(n),c2(n),f
C     real*8 fcn

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                       SUBROUTINE DIRINFCN
C
C Subroutine DIRinfcn unscales the variable x for use in the user-supplied 
C function-evaluation subroutine fcn. After fcn returns to DIRinfcn, DIRinfcn 
C then rescales x for use by DIRECT. 
C
C On entry
C
C        fcn -- The argument containing the name of the user-supplied
C               subroutine that returns values for the function to be
C               minimized.
C
C          x -- A double-precision vector of length n. The point at which
C               the derivative is to be evaluated.
C
C        xs1 -- A double-precision vector of length n. Used for scaling
C               and unscaling the vector x by DIRinfcn.
C
C        xs2 -- A double-precision vector of length n. Used for scaling
C               and unscaling the vector x by DIRinfcn.
C
C          n -- An integer. The dimension of the problem.
C
C On return
C
C          f -- A double-precision scalar. The function value at x scaled for use
C               by iffco, ifcgrad, or ifclines.
C
C Subroutines and Functions
C
C The subroutine whose name is passed through the argument fcn.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
C Unscale the variable x.
C
      do 20 i=1,n
        x(i)=x(i)*c1(i)+c1(i)*c2(i)
 20   continue

C
C Call the function-evaluation subroutine fcn.
C         write(6,*)'here i am'	
C
          call fcn(x,n,f)

C
C Rescale the variable x.
C
      do 30 i=1,n
        x(i)=x(i)/c1(i)-c2(i)
 30   continue

      return

      end

      SUBROUTINE DIRSplit(f,c,fcn,l,u,length,pos1,anchor,point,
     +           free,maxfunc,fmin,x,minpos,maxdeep,n)
      IMPLICIT None
      INTEGER maxfunc,maxdeep,n
      Real*8 f(maxfunc),c(maxfunc,n)
      Real*8 fcn,x(n)
      Real*8 delta,fmin,l(n),u(n)
      Integer length(maxfunc,n),anchor(0:maxdeep),point(maxfunc)
      Integer free,pos1,pos2,pos3,minpos

        pos2 = free
        pos3 = point(free)
        free = point(pos3)

        length(pos1,1) = length(pos1,1) + 1
        length(pos2,1) = length(pos1,1)
        length(pos3,1) = length(pos1,1)

        delta = 3 ** (length(pos1,1))
        delta = 1.D0 / delta

        c(pos2,1) = c(pos1,1) - delta
        c(pos3,1) = c(pos1,1) + delta
        f(pos2) = fcn(x,c,pos2,l,u,n,maxfunc)
        f(pos3) = fcn(x,c,pos3,l,u,n,maxfunc)

        CALL DIRInsert3(pos1,pos2,pos3,length(pos1,1),anchor,
     +               point,free,f,fmin,minpos,maxfunc,maxdeep)
      END


C+-----------------------------------------------------------------------+
C| Subroutines to keep the books.					 |
C+-----------------------------------------------------------------------+


      SUBROUTINE DIRWritedown1(point,f,thirds,c,anchor,actdeep,file,
     +               l,u,file2,maxfunc,maxdeep,n)
      IMPLICIT None
      Integer maxfunc,maxdeep,actdeep,file,file2,n
      Real*8 f(maxfunc),thirds(maxfunc),c(maxfunc,n),l(n),u(n)
      Integer anchor(0:maxdeep),i,point(maxfunc)

      DO 10,i = 0,actdeep
        IF (anchor(i) .GT. 0) THEN
          CALL DIRShowlist(point,f,thirds(i),c,anchor(i),file,
     +        	      file2,l,u,maxfunc,n)
          Write(file,*) 
        END IF
10    CONTINUE
      END


      SUBROUTINE DIRHeader(logfile)
      IMPLICIT None
      Integer logfile

      Write(logfile,*) "---------------------- Logfile ",
     +                 "----------------------"
      END

      SUBROUTINE DIRMaxf_to_high(maxf,maxfunc,dwrit,logfile)
      IMPLICIT None
      INTEGER maxf,maxfunc,dwrit,logfile

      Write(*,10001) maxf
      Write(*,10002) maxfunc
      Write(*,10003)
      Write(*,10004)
      IF (dwrit .EQ. 2) THEN
         Write(logfile,10001) maxf
         Write(logfile,10002) maxfunc
         Write(logfile,10003)
         Write(logfile,10004)
      END IF

10001 FORMAT("The maximum number of function evaluations (",
     +       I6,") is ")
10002 FORMAT("higher then the constant maxfunc (",I6,
     +       "). Increase ")
10003 FORMAT("maxfunc in the SUBROUTINE DIRECT or decrease ")
10004 FORMAT("the maximum number of function evaluations.")
      END

      SUBROUTINE DIRMaxT_to_high(maxT,maxdeep,dwrit,logfile)
      IMPLICIT None
      INTEGER maxT,maxdeep,dwrit,logfile

      Write(*,10001) maxT
      Write(*,10002) maxdeep
      Write(*,10003)
      IF (dwrit .EQ. 2) THEN
         Write(logfile,10001) maxT
         Write(logfile,10002) maxdeep
         Write(logfile,10003)
      END IF

10001 FORMAT("The maximum number of iterations (",I5,
     +        ") is higher ")
10002 FORMAT("then the constant maxdeep (",I5,
     +       "). Increase maxdeep ")
10003 FORMAT("or decrease the number of iterations. ")
      END


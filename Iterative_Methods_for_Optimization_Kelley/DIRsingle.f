C+-----------------------------------------------------------------------+
C| Parallel-version of the sampling.					 |
C+-----------------------------------------------------------------------+
      SUBROUTINE DIRSamplef(c,ArrayI,delta,sample,new,length,
     +           dwrit,logfile,f,free,maxI,point,fcn,x,l,fmin,
     +           minpos,u,n,
     +           maxfunc,maxdeep,oops,iprfck) 
      IMPLICIT None

      INTEGER n,maxfunc,maxdeep,oops
      INTEGER maxI,ArrayI(n),sample,new
      INTEGER length(maxfunc,n),free,point(maxfunc),i
      Real*8 c(maxfunc,n),delta,fcn,x(n),l(n),u(n),f(maxfunc)
      Real*8 fmin
      INTEGER pos,j,k,dwrit,logfile,minpos,iprfck

      Integer ii,helppoint,ibest
      Real*8  costmin,bestx
c
c  ibest: 1=found ; 0=not found
c
      common /jbest/ ibest
      common /best/  costmin,bestx(20)

      ii = 1
      pos = new
      helppoint = pos

      DO 40,j=1,maxI + maxI
         DO 60,i=1,n
           x(i) = c(pos,i)
60       CONTINUE
	 CALL ifcinfcn2(fcn,x,l,u,n,f(pos),iprfck)
         IF (f(pos) .LT. costmin) THEN
          fmin = f(pos)
          minpos = pos
	  costmin = fmin
	  do 20,ii=1,n
	    bestx(ii) = x(ii)
20        continue
        END IF
        pos = point(pos)
40    CONTINUE
      END

C+-----------------------------------------------------------------------+
C| Valve-Initialisation                                                  |
C+-----------------------------------------------------------------------+
      SUBROUTINE DIRInitSpecific(x,n)
      IMPLICIT None
C+-----------------------------------------------------------------------+
C| Valve - specific variables !                                          |
C+-----------------------------------------------------------------------+

      Integer iiii,ind,iprfck,n
      Integer isys,irange,iopt,ibounce,irpm,ibest
      Real*8  costmin,bestx
      Real*8  dyncost,area
      Real*8  x(n),fmin
      Integer i

c
c isys = 1 : system identification
c      = 2 : cam design
c
      common/isystem/ isys
      common/control/ irange,iopt,ibounce,irpm
c
c  ibest: 1=found ; 0=not found
c
      common /jbest/ ibest
      common /best/  costmin,bestx(20)
C+-----------------------------------------------------------------------+
C| End of Valve - specific variables !                                   |
C+-----------------------------------------------------------------------+

C+-----------------------------------------------------------------------+
C| Start of valve-specific initialisation				 |
C+-----------------------------------------------------------------------+

c  iiii: random number index
      iiii=7342

      if(isys.eq.1 .or. isys.eq.3) call fcostrs(n,x,fmin,ind)
      if(isys.eq.2) call fcost2(n,x,fmin,iprfck,dyncost,area,ind)
c
c  iopt=2 : print out simulation result
      if(iopt.eq.2) return
      if(iprfck.eq.1) then
        write(6,3412)
        write(11,3412)
3412    format(' Initial cam profile is not usable.')
C        f=1000.
      endif
      if(ind.lt.0) fmin=1000.
      write(6,600) fmin
      write(11,600) fmin
600   format(' Cost Function= ',e13.6)
      write(6,113) (x(i),i=1,n)
      write(11,113) (x(i),i=1,n)
113   format(4(e13.6,1x))
c
c Initial the best cost function and  parameters and set
c the "find flag" to be 0=not found
c
      ibest = 0
      costmin = fmin
      do 2 i=1,n
2       bestx(i) = x(i)
155   continue
C+-----------------------------------------------------------------------+
C| End of valve-specific initialisation				         |
C+-----------------------------------------------------------------------+
      end




C+-----------------------------------------------------------------------+
C| Send the common data to all slave processes.                          |
C+-----------------------------------------------------------------------+

      SUBROUTINE DIRSend_Common(l,u,n)
      IMPLICIT None

      Integer n
      Real*8 l(n),u(n)

      END


c
c
c
c     ===============================================================
      subroutine claw1ez(maxmx,meqn,mwaves,mbc,maux,mwork,mthlim,
     &                   q,work,aux)
c     ===============================================================
c
c     An easy-to-use clawpack driver routine for simple applications
c
c     Author: Randall J. LeVeque
c     Version of August, 1999 --  CLAWPACK Version 4.0
c
      implicit double precision (a-h,o-z)
      external bc1,rp1,src1,b4step1

      dimension    q(1-mbc:maxmx+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, maux)
      dimension work(mwork)
      dimension mthlim(mwaves)
c
      dimension method(7),dtv(5),cflv(4),nv(2),mthbc(2)
      dimension tout(100)
      logical outt0
c
      open(55,file='claw1ez.data',status='old',form='formatted')
      open(10,file='fort.info',status='unknown',form='formatted')
      open(11,file='fort.nplot',status='unknown',form='formatted')
c
c
c     # Read the input in standard form from input.claw:
c     # See input.doc for description of each input variable

c     Number of grid cells:
      read(55,*) mx

c     i/o variables
      read(55,*) nout
      read(55,*) outstyle
        if (outstyle.eq.1) read(55,*) tfinal
        if (outstyle.eq.2) read(55,*) (tout(i), i=1,nout)
        if (outstyle.eq.3) read(55,*) nsteps


c     timestepping variables
      read(55,*) dtv(1)
      read(55,*) dtv(2)
      read(55,*) cflv(1)
      read(55,*) cflv(2)
      read(55,*) nv(1)
c

c     # input parameters for clawpack routines
      read(55,*) method(1)
      read(55,*) method(2)
      read(55,*) method(3)
      read(55,*) method(4)
      read(55,*) method(5)
      read(55,*) method(6)  
      read(55,*) method(7) 

      read(55,*) meqn1
      read(55,*) mwaves1
      read(55,*) (mthlim(mw), mw=1,mwaves)

c     # physical domain:
      read(55,*) t0
      read(55,*) xlower
      read(55,*) xupper
c
c     # boundary conditions:
      read(55,*) mbc1
      read(55,*) mthbc(1)
      read(55,*) mthbc(2)

      if (method(7) .ne. maux) then
         write(6,*) '*** ERROR ***  maux set wrong in input or driver'
         stop
         endif

      if (meqn1 .ne. meqn) then
	 write(6,*) '*** ERROR ***  meqn set wrong in input or driver'
	 stop
	 endif
      if (mwaves1 .ne. mwaves) then
	 write(6,*) '*** ERROR ***  mwaves set wrong in input or driver'
	 stop
	 endif
      if (mbc1 .ne. mbc) then
	 write(6,*) '*** ERROR ***  mbc set wrong in input or driver'
	 stop
	 endif
c
      if ((mthbc(1).eq.2 .and. mthbc(2).ne.2) .or.
     &    (mthbc(2).eq.2 .and. mthbc(1).ne.2)) then
	 write(6,*) '*** ERROR ***  periodic boundary conditions require'
	 write(6,*) '  mthbc(1) and mthbc(2) BOTH be set to 2'
	 stop 
	 endif

c
c     # check that enough storage has been allocated:
c

      mwork1 = (maxmx + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)
c
      if (mx.gt.maxmx .or. mwork.lt.mwork1) then
c        # insufficient storage
         maxmx1 = max0(mx,maxmx)

         mwork1 = (maxmx1 + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)

         write(6,*) ' '
         write(6,*) '*** ERROR *** Insufficient storage allocated'
         write(6,*) 'Recompile after increasing values in driver.f:'
         write(6,611) maxmx1
         write(6,613) mwork1
 611     format(/,'parameter (maxmx = ',i5,')')
 613     format('parameter (mwork = ',i7,')',/)
         stop
         endif

c
c
      write(6,*) 'running...'
      write(6,*) ' '
c
c     # grid spacing
      dx = (xupper - xlower) / float(mx)
c

c     # time increments between outputing solution:
      if (outstyle .eq. 1) then
         dtout = (tfinal - t0)/float(nout)
         endif
c
      write(11,1101) nout
c
 1101 format(i5)
c        
c     # call user's routine setprob to set any specific parameters
c     # or other initialization required.
c
      call setprob
c        
c     # set aux array:
c
      if (maux .gt. 0)  then
         call setaux(maxmx,mbc,mx,xlower,dx,maux,aux)
         endif
c
c     # set initial conditions:
c
      call qinit(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux)
c
      outt0 = .true.
      if (outt0) then
c        # output initial data
         call out1(maxmx,meqn,mbc,mx,xlower,dx,q,t0,0)
         endif

c
c     ----------
c     Main loop:
c     ----------
c
      tend = t0
      do 100 n=1,nout
         tstart = tend
         if (outstyle .eq. 2) then
              tend = tout(n)
            else
              tend = tstart + dtout
            endif
c
         call claw1(maxmx,meqn,mwaves,mbc,mx,
     &		 q,aux,xlower,dx,tstart,tend,dtv,cflv,nv,method,mthlim,
     &           mthbc,work,mwork,info,bc1,rp1,src1,b4step1)
c
c        # check to see if an error occured:
         if (info .ne. 0) then
            write(6,*) '*** ERROR in claw1 ***  info =',info
	    if (info.eq.1) then
	       write(6,*) '***   either mx > maxmx or mbc < 2'
	       endif
	    if (info.eq.2) then
	       write(6,*) '***   dt does not divide (tend - tstart)'
	       write(6,*) '***   and dt is fixed since method(1)=0'
	       endif
	    if (info.eq.3) then
	       write(6,*) '***   method(1)=1 and cflv(2) > cflv(1)'
	       endif
	    if (info.eq.4) then
	       write(6,*) '***   mwork is too small'
	       endif
	    if (info.eq.11) then
	       write(6,*) '***   Too many times steps, n > nv(1)'
	       endif
	    if (info.eq.12) then
	       write(6,*) '***   The Courant number is greater than cflv(1)'
	       write(6,*) '***   and dt is fixed since method(1)=0'
	       endif

c           go to 999
            endif
c
         dtv(1) = dtv(5)  !# use final dt as starting value on next call
c
c        # output solution at this time
c        ------------------------------
         do 50 i=1,mx
               if (dabs(q(i,1)) .lt. 1d-90) q(i,1) = 0.d0
   50          continue
c
         call out1(maxmx,meqn,mbc,mx,xlower,dx,q,tend,n)
c
c        # write out information about this call to claw:
c
c
         write(6,601) n,tend
  601    format('CLAW1EZ: Frame ',i4,
     &           ' matlab plot files done at time t =',
     &           d12.4,/)
c
c
         write(10,1010) tend,info,dtv(3),dtv(4),dtv(5),
     &           cflv(3),cflv(4),nv(2)
 1010    format('tend =',d15.4,/,
     &       'info =',i5,/,'smallest dt =',d15.4,/,'largest dt =',
     &       d15.4,/,'last dt =',d15.4,/,'largest cfl =',
     &         d15.4,/,'last cfl =',d15.4,/,'steps taken =',i4,/)
c
  100    continue
c
  999 continue
c
      return 
      end

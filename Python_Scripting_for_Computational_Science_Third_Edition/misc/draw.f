
      real*8 function ran3(idum)
      integer idum
c     Returns a uniform random deviate in (0.0, 1.0). Set idum to any
c     negative value to initialize or reinitialize the sequence
      integer MBIG, MSEED, MZ
      real*8 FAC
      parameter (MBIG=1000000000, MSEED=161803398, MZ=0, FAC=1./MBIG)
      integer i, iff, ii, inext, inextp, k
      integer mj, mk, ma(55)
      save iff, inext, inextp, ma
      data iff /0/
      
      if (idum .lt. 0 .or. iff .eq. 0) then
         iff=1
         mj = abs(MSEED - abs(idum))
         mj = mod(mj, MBIG)
         ma(55) = mj
         mk=1
         
         do i=1,54
            ii = mod(21*i, 55)
            ma(ii) = mk
            mk = mj - mk
            if (mk.lt.MZ) mk = mk + MBIG
            mj = ma(ii)
         enddo 

         do k=1,4
            do i=1,55
               ma(i) = ma(i) - ma(1 + mod(i+30, 55))
               if (ma(i).lt.MZ) ma(i) = ma(i) + MBIG
            enddo
         enddo

         inext = 0
         inextp = 31
         idum = 1
      endif

      inext = inext + 1
      if (inext.eq.56) inext = 1
      inextp = inextp +1
      if (inextp.eq.56) inextp = 1
      mj = ma(inext) - ma(inextp)
      if (mj.lt.MZ) mj = mj + MBIG
      ma(inext) = mj
      ran3 = mj*FAC
      return 
      end


      subroutine setseed(seed)
      integer seed
c     just initialize ran3
      integer ncalls
      real*8  next_value
      common /gaussdraw/ next_value, ncalls
      real*8 foo, ran3

      ncalls = 0
      foo = ran3((-1)*abs(seed))
      return
      end



      real*8 function gauss(mean, stdev)
      real*8 mean, stdev
c     get a random number from the normal distrobution based on a given
c     mean and standard deviation
      real*8 tmp1, tmp2, u1, u2, ran3, PI
      integer ncalls
      real*8  next_value
      parameter(PI=3.14159265359)
      common /gaussdraw/ next_value, ncalls
      data ncalls/0/
      data next_value/0.0/

      ncalls = ncalls + 1
      if (mod(ncalls,2) .ne. 0) then
         u1 = ran3(1)
         u2 = ran3(1)
         tmp1 = sqrt(-2.*log(u1))
         tmp2 = 2*PI*u2
         next_value = mean + stdev*tmp1*sin(tmp2)
         gauss = mean + stdev*tmp1*cos(tmp2)
      else 
         gauss = next_value
      endif
      return
      end
      
      real*8 function beam(F, L, E, I)
      real*8 F, L, E, I
c     End deflection of a cantilever beam
      beam = (F*L**3)/(3*E*I)
      return 
      end

      subroutine test()
c     do not omit parenthesis in empty argument lists
c     although this is legal F77 syntax
      integer n,i
      real*8 x, ran3
      call setseed(-1)
      write(*,*) 'Routine for testing I/O from F77'
      n = 100
      do i=1,n
         x = ran3(1)
         write(*,*) 'x=',x
      end do
      return
      end

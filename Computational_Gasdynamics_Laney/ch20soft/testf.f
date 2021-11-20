      program testf
c...This program generates test functions.
      parameter(nmax=2000,pi=3.141592653589793)
      real u(nmax+1), lambda

      open(unit=8,file='nb.dat',status='old',err=500)
      read(8,*) n, lambda, tfinal

      xmax=1.
      xmin=-1.
 5    write(*,*) '-----------------------------------------'
      write(*,*) '1 = sinewave (one period)'
      write(*,*) '2 = sine squared (one period)'
      write(*,*) '3 = square wave from 0 to 1'
      write(*,*) '4 = ellipse'
      write(*,*) '5 = symmetric spike'
      write(*,*) '6 = sawtooth spike'
      write(*,*) '7 = thin square wave'
      write(*,*) '8 = square wave from -1 to 1 (centered)'
      write(*,*) '9 = square wave from -1 to 1 (offset)'
c     write(*,*) '10 = square wave from 1.58543 to 0'
      write(*,*) '10 = square wave from 0.7993972 to -0.55176'
      write(*,*) '11 = square wave from 0.5 to 2'
      write(*,*) 'enter selection number...'
      read(*,*,err=700) k
      if(k.lt.1.or.k.gt.11) then
        write(*,*) '-----------------------------------------'
        write(*,*) '***try again***'
        goto 5
      endif

      if(n.gt.nmax.or.n.lt.2) then
        write(*,*) '***n out of bounds in nb.dat***'
        close(unit=8)
        stop
      endif
      if(lambda.lt.0.01) then
        write(*,*) '***lambda small or negative in nb.dat***'
        close(unit=8)
        stop
      endif
      if(tfinal.le.0.) then
        write(*,*) '***tfinal negative in nb.dat***'
        close(unit=8)
        stop
      endif
      delta_x = (xmax-xmin)/real(n)
      delta_t = lambda*delta_x
      itert = nint(tfinal/delta_t)
      if(itert.gt.6000) then
        write(*,*) '***too many iterations: itert= ', itert,'***'
        close(unit=8)
        stop
      endif

c...SINEWAVE
      if(k.eq.1) then
        do 10, i=1,n+1
          u(i) = sin(2.*pi*real(i-1)/real(n))
 10     continue

c...SINE SQUARED
      elseif(k.eq.2) then
        do 20, i=1,n+1
          u(i) = sin(pi*real(i-1)/real(n))
          u(i) = u(i)*u(i)
 20     continue

c...SQUARE WAVE FROM 0 TO 1
      elseif(k.eq.3) then
        do 30, i=1,n/3+1
          u(i) = 0.
 30     continue
        do 31, i=n/3+2,2*n/3+1
          u(i) = 1.
 31     continue
        do 32, i=2*n/3+2,n+1
          u(i) = 0.
 32     continue
       
c...ELLIPSE
      elseif(k.eq.4) then
        do 40, i=1,n/3+1
          u(i) = 0.
 40     continue
        do 41, i=n/3+2,2*n/3+1
          x = xmin + delta_x*real(i-1)
          arg = max(0.,1.-9.*x**2)
          u(i) = sqrt(arg)
 41     continue
        do 42, i=2*n/3+2,n+1
          u(i) = 0.
 42     continue

c...SYMMETRIC SPIKE
      elseif(k.eq.5) then
        do 50, i=1,n/3+1
          u(i) = 0.
 50     continue

        do 51, i=n/3+2,n/2+1
c         u(i) = 6.*(real(i)-real(n)/3.-1.)/real(n)
          u(i) = real(6*i-6)/real(n)-2.
 51     continue

        do 52, i=n/2+2,2*n/3+1
c         u(i) = 6.*(-real(i)+2.*real(n)/3.+1.)/real(n)
          u(i) = real(-6*i+6)/real(n)+4.
 52     continue

        do 53, i=2*n/3+2,n+1
          u(i) = 0.
 53     continue

c...SAWTOOTH SPIKE
      elseif(k.eq.6) then
	do 60, i=0,n/3+1
	  u(i) = 0.
 60     continue
	do 61, i=n/3+2,2*(n/3)+1
          u(i) = real(i-n/3-1)/real(n/3)
 61     continue
        do 62, i=2*n/3+2,n+1
          u(i) = 0.
 62     continue

c...SQUARE WAVE
      elseif(k.eq.7) then
        do 70, i=1,n/2-1
          u(i) = 0.
 70     continue
        do 71, i=n/2,n/2+2
          u(i) = 1.
 71     continue
        do 72, i=n/2+3,n+1
          u(i) = 0.
 72     continue

c...SQUARE WAVE FROM -1 TO 1
      elseif(k.eq.8) then
        do 80, i=1,n/3+1
          u(i) = -1.
 80     continue
        do 81, i=n/3+2,2*n/3+1
          u(i) = 1.
 81     continue
        do 82, i=2*n/3+2,n+1
          u(i) = -1.
 82     continue

      elseif(k.eq.9) then
        do 90, i=1,n/2+1
          u(i) = -1.
 90     continue
        do 91, i=n/2+2,n
          u(i) = 1.
 91     continue
        u(n+1) = -1.

c...SQUARE WAVE FROM 1.58543 TO 0
      elseif(k.eq.10) then
        do 100, i=1,n/2+1
          u(i) =  0.7993972
 100     continue
        do 101, i=n/2+2,n
          u(i) = -.55176
 101     continue
        u(n+1) = 0.7993972

c...SQUARE WAVE FROM 0.5 TO 2
      elseif(k.eq.11) then
        do 110, i=1,2*n/5+1
          u(i) = 0.5
 110     continue
        do 111, i=2*n/5+2,3*n/5+1
          u(i) = 2.
 111    continue
        do 112, i=3*n/5+2,n+1
          u(i) = 0.5
 112    continue

      endif

      rewind(unit=8)
	write(8,*)  n, lambda, tfinal
      do 200, i=1,n
        write(8,1000) xmin+delta_x*real(i-1), u(i)
 200  continue
      write(8,1000) xmax,u(n+1)

      close(unit=8)
      stop

  500 write(*,*) '***error opening nb.dat***'
      stop
  700 write(*,*) '***entry not understood...try again***'
      goto 5
 1000 format(f14.9,5x,f14.9)
      end

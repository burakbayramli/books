      program cell
c...Generates a set of cell edges and cell-centered-samples.
c...Also generates a set of evaluation points.
      double precision rpi
      parameter(nmax=700,n1=500,rpi=3.141592653589793D0)
      double precision x(nmax),y(nmax),h,f,r,cc

c...Define function f. 

c     f(r) = 2.+r*(2.-r*(7.+r*(11.-r*(1.+r*(5. + 9*r)))))
c     f(r) = r*r*r*r*r*r
c     f(r) = r*r
c     f(r) =  dsin(rpi*r)
c     f(r) = -(4.d0*r*r-1.d0)/3.d0
c     f(r) = -dsin(rpi*r)
c     f(r) = (-dsin(rpi*r))**3
c     f(r) = exp(-r*r) - .75D0
c     f(r) = cos(rpi*r/2.)*cos(rpi*r/2.)
c     f(r) = dexp(-10.D0*r*r) 
c     f(r) = 1.D0/(1.D0+6.D0*r*r)


      write(*,*) 'number of cells?'
      read(*,*) n
      if(n+1.gt.nmax) then
        write(*,*) 'This is too large'
        stop
      endif

c     write(*,*) 'number of plotting points'
c     read(*,*) n1
c     n1 = n1+1
c     if(n1.gt.n1max) then
c       write(*,*) 'This is too large'
c       stop
c     endif
c     if(n1.lt.n) then
c       write(*,*) 'This is too small'
c       stop
c     endif

      open(unit=10,file='cell.dat')

      write(10,200) n,0.8,4.0
      write(10,200) -1
c     write(10,200) 1

      n = n+1

      do 10, i=1,n
        x(i) = -1.D0 + 2.D0*real(i-1)/real(n-1)
        write(10,100) x(i)
 10   continue
c...Evaluate f(x) at cell centers.
c     do 11, i=1,n-1
      do 11, i=1,n
        cc = .5D0*(x(i+1)+x(i)) 
c       y(i) = f(cc)
c       y(i) = f(x(i))
c       if(x(i).ge.-1.D0/3.D0.and.x(i).le.1.D0/3.D0) then
        if(cc.ge.-1.D0/3.D0.and.cc.le.1.D0/3.D0) then
          y(i) = 1.D0
        else
          y(i) = 0.D0
c         y(i) = -1.D0
        endif
        write(10,100) y(i)
 11   continue

      write(10,*) n1
      do 30, i=1,n1
        h = -1.D0 + 2.D0*real(i-1)/real(n1-1)
        if(h.ge.-1.D0/3.D0.and.h.le.1.D0/3.D0) then
          t = 1.D0
        else
          t = 0.D0
c         t = -1.D0
        endif
c       t = f(h)
        write(10,100) h,t
 30   continue

      close(unit=10)

 100  format(f32.25,6x,f32.25)

 200  format(2x,i3,2x,f10.6,2x,f10.6)

      stop
      end

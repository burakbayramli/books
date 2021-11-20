      program burgers  
c...Computes some very simple exact solutions to Burgers' equation
c...Assume periodic domain [-1,1]. 
      open(unit=9,file='burgers.dat')

 5    write(*,*) "input 1 or 2"
      read(*,*) input
      if(input.ne.1.and.input.ne.2) then
        write(*,*) "try again"
        go to 5
      endif

      if (input.eq.1) then
c...Solution 1.  Jump from -1 to 1 at x=-1/3.  Jump from
c...1 to -1 at x=1/3.
      t = 0.3
      a1 = - t - 1./3.
      a2 =   t - 1./3.
      a3 =       1./3.
      do 10, i=0,500
        x = -1.0 + real(2*i)/500.
        if(x.lt.a1) then
          u = -1.
        elseif(x.lt.a2) then
          u = 2.*(x-a1)/(a2-a1)-1.
        elseif(x.lt.a3) then
          u = 1.
        else
          u = -1.
        endif
        write(9,1000) x,u
 10   continue
      endif

      if (input.eq.2) then
c...Solution 2.  Jump from 0 to 1 at x=-1/3.  Jump from
c...1 to 0 at x=1/3. 
      t = 0.6
      a1 =      - 1./3.
      a2 =  t   - 1./3.
      a3 = .5*t + 1./3.
      do 20, i=0,500
        x = -1.0 + real(2*i)/500.
        if(x.lt.a1) then
          u = 0.
        elseif(x.lt.a2) then
          u = (x-a1)/(a2-a1)
        elseif(x.lt.a3) then
          u = 1.
        else
          u = 0.
        endif
        write(9,1000) x,u
 20   continue
      endif

 1000 format(f14.9,5x,f14.9)

      end

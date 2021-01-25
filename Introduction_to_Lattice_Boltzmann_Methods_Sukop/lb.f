C A short and simple gravity-driven LBM solver based on the code snippets 
C in Sukop and Thorne's 'Lattice Boltzmann Modeling'

C Note indexing differences between book's C code and FORTRAN: 
C C uses 0 for the first index value, while FORTRAN starts at one.
C Numerous changes are needed. In some places, we have just 
C explicitly added one to the C index.

      parameter(ly=11,lx=3)
      
      integer is_solid_node(ly,lx)
      real rho(ly,lx),f(ly,lx,9),ftemp(ly,lx,9),ex(9),ey(9),
     +     u_x(ly,lx),u_y(ly,lx),x(ly,lx),y(ly,lx),feq(ly,lx,9)

      tau = 1.
      g=0.001

C Set solid nodes at walls on top and bottom

      is_solid_node=0
      do i=1,lx
        is_solid_node(1,i)=1
        is_solid_node(LY,i)=1
      enddo
        
C Set initial density
      rho=1.

      do j=1,ly
      do i=1,lx
      f(j,i,1) = (4./9. )*rho(j,i)
      f(j,i,2) = (1./9. )*rho(j,i)
      f(j,i,3) = (1./9. )*rho(j,i)
      f(j,i,4) = (1./9. )*rho(j,i)
      f(j,i,5) = (1./9. )*rho(j,i)
      f(j,i,6) = (1./36.)*rho(j,i)
      f(j,i,7) = (1./36.)*rho(j,i)
      f(j,i,8) = (1./36.)*rho(j,i)
      f(j,i,9) = (1./36.)*rho(j,i)
      enddo
      enddo


C Define lattice velocity vectors

      ex(0+1)= 0
      ey(0+1)= 0
      ex(1+1)= 1
      ey(1+1)= 0
      ex(2+1)= 0
      ey(2+1)= 1
      ex(3+1)=-1
      ey(3+1)= 0
      ex(4+1)= 0
      ey(4+1)=-1
      ex(5+1)= 1
      ey(5+1)= 1
      ex(6+1)=-1
      ey(6+1)= 1
      ex(7+1)=-1
      ey(7+1)=-1
      ex(8+1)= 1
      ey(8+1)=-1


C Time loop

      do ts=1,300
    
        write(*,*) ts
    
C Computing macroscopic density, rho, and velocity, u=(ux,uy).
        do j=1,ly
        
        do i=1,lx
            
      u_x(j,i) = 0.0
      u_y(j,i) = 0.0
      rho(j,i) = 0.0

        
            if(is_solid_node(j,i).eq.0) then
                
                do k=1,9
                    
                    rho(j,i) = rho(j,i) + f(j,i,k)
                                       
                    u_x(j,i) = u_x(j,i) + ex(k)*f(j,i,k)
                    u_y(j,i) = u_y(j,i) + ey(k)*f(j,i,k)
                    
                enddo

                u_x(j,i) = u_x(j,i)/rho(j,i)
                u_y(j,i) = u_y(j,i)/rho(j,i)

            endif
                        
C Add space matricies for plotting
            x(j,i)=i
            y(j,i)=j
            

      enddo
      enddo

C Compute the equilibrium distribution function, feq.

      f1=3.
      f2=9./2.
      f3=3./2.

      do j=1,ly   
        
        do i=1,lx
                   
            if(is_solid_node(j,i).eq.0) then
                

                rt0 = (4./9. )*rho(j,i)
                rt1 = (1./9. )*rho(j,i)
                rt2 = (1./36.)*rho(j,i)

                ueqxij =  u_x(j,i)+tau*g
                ueqyij =  u_y(j,i)
                uxsq   =  ueqxij * ueqxij
                uysq   =  ueqyij * ueqyij
                uxuy5  =  ueqxij +  ueqyij
                uxuy6  = -ueqxij +  ueqyij
                uxuy7  = -ueqxij -ueqyij
                uxuy8  =  ueqxij -ueqyij
                usq    =  uxsq + uysq

                
                feq(j,i,0+1) = rt0*(1.                      - f3*usq)
                feq(j,i,1+1) = rt1*(1.+ f1*ueqxij+f2*uxsq - f3*usq)
                feq(j,i,2+1) = rt1*(1.+ f1*ueqyij+f2*uysq - f3*usq)
                feq(j,i,3+1) = rt1*(1.- f1*ueqxij+f2*uxsq - f3*usq)
                feq(j,i,4+1) = rt1*(1.- f1*ueqyij+f2*uysq - f3*usq)
                feq(j,i,5+1) = rt2*(1.+ f1*uxuy5 +f2*uxuy5*uxuy5-f3*usq)
                feq(j,i,6+1) = rt2*(1.+ f1*uxuy6 +f2*uxuy6*uxuy6-f3*usq)
                feq(j,i,7+1) = rt2*(1.+ f1*uxuy7 +f2*uxuy7*uxuy7-f3*usq)
                feq(j,i,8+1) = rt2*(1.+ f1*uxuy8 +f2*uxuy8*uxuy8-f3*usq)

                
            endif
         enddo
         
      enddo
        
C Collision step.
      do j=1,ly
      do i=1,lx
                 
            if(is_solid_node(j,i).eq.1) then               

C Standard bounceback

                temp= f(j,i,1+1)
                f(j,i,1+1) = f(j,i,3+1)
                f(j,i,3+1) = temp
                temp= f(j,i,2+1)
                f(j,i,2+1) = f(j,i,4+1)
                f(j,i,4+1) = temp
                temp= f(j,i,5+1)
                f(j,i,5+1) = f(j,i,7+1)
                f(j,i,7+1) = temp
                temp= f(j,i,6+1)
                f(j,i,6+1) = f(j,i,8+1)
                f(j,i,8+1) = temp
                
            else  
C Regular collision
                
                do k=1,9
                                   
                    f(j,i,k) = f(j,i,k)-( f(j,i,k) - feq(j,i,k))/tau
                    
                enddo 
                
            endif
        enddo
        
      enddo
        
C Streaming step; subtle changes to periodicity here due to indexing

      do j=1,ly

        if (j.gt.1) then 
            jn = j-1
        else
            jn = LY
        endif
        
        if (j.lt.ly) then
            jp = j+1
        else 
            jp = 1
        endif

        do i=1,lx

            if (i.gt.1) then
                in = i-1
            else 
                in = LX 
            endif
            if (i.lt.LX) then 
                ip = i+1
            else 
                ip = 1
            endif

            ftemp(j,i,0+1)  = f(j,i,0+1)
            ftemp(j,ip,1+1) = f(j,i,1+1)
            ftemp(jp,i,2+1) = f(j,i,2+1)
            ftemp(j,in,3+1) = f(j,i,3+1)
            ftemp(jn,i ,4+1) = f(j,i,4+1)
            ftemp(jp,ip,5+1) = f(j,i,5+1)
            ftemp(jp,in,6+1) = f(j,i,6+1)
            ftemp(jn,in,7+1) = f(j,i,7+1)
            ftemp(jn,ip,8+1) = f(j,i,8+1)

      enddo
      enddo
    
      f=ftemp
    
C End time loop
      enddo

      open(unit=20,file='x.dat',status='unknown')
      open(unit=21,file='y.dat',status='unknown')

      do j=1,ly
      do i=1,lx
      
        write(20,*) x(j,i),y(j,i),u_x(j,i)
        write(21,*) x(j,i),y(j,i),u_y(j,i)
      enddo
      enddo


      pause
      end
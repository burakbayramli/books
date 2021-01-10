css
css     Lattice BGK simple start-up code
css     along with the book:
css     The Lattice Boltzmann equation 
css     for fluid dynamics and beyond
css 
css     Oxford Univ. Press, 2001
css     Author: sauro Succi
css
css     The code is a simple start-up 
css     bug-freedom not guaranteed
css     The author disclaims any responsibility
css     for any incorrect result obtained via this code.
css     
css      Sauro Succi, Rome, April 2001
c ================================
	program lbe2D
c ================================
	implicit double precision(a-h,o-z)
	include'lbe.par'

c --- input parameters

	call input
	
c --- initialisation

	call inithydro
	call equili
        call initpop

c ------- MAIN LOOP
        iconf=0
	do 10 istep = 1,nsteps

c periodic boundary conditions
css     call pbc
c mixed boundary conditions c (Poiseuille flow)

           call mbc
	   
	   call hydrovar
	   call equili
	   call collis
   	   call move

	   if (iforce)  then
	       call force(istep,frce)
	   endif

c Obstacle ?
	   if (iobst) then
               call plate
	   endif

	   if (mod(istep,ndiag).eq.0) then
	      call diag0D
	   endif

	   if (mod(istep,nout).eq.0) then
	      call profil(istep,frce)
	   endif

	   if (mod(istep,2*nout).eq.0) then
	      call config(istep,iconf)
              iconf=iconf+1
	   endif

c-------- end of main loop

10	continue
	end
c--------------------------------------------------
	subroutine input

	implicit double precision(a-h,o-z)
	include'lbe.par'
c---------------------------------------------------
	print*,' Number of steps'
        read(5,*)nsteps

	print*,' Number of steps between printing profile'
        read(5,*)nout

	print*,' Number of steps between performing diagnostics'
	read(5,*)ndiag

        print*,' Relaxation frequency omega'
        read(5,*)omega

	print*,'Applied force  (.TRUE. or .FALSE.) ?'
	read(5,*)iforce 

	print*,' Initial density and velocity for the Poiseuille force'
	read(5,*)rho0,u0

	print*,' Final velocity for the Poise force'
	read(5,*) uf

	print*,' Linear obstacle (T or F?'
	read(5,*) iobst
	read(5,*) nobst

        print*,' Length of the obstacle (multiple of 2)',nobst

        print*,' File for output: 5 chars'
        read(5,'(A)')fileout

        open(10,file=fileout//'.uy')
        open(11,file=fileout//'.vy')
        open(14,file=fileout//'.uvx')
        open(16,file=fileout//'.pop')
        open(50,file=fileout//'.probe')

	print*,'*****************************************'
	print*,' Lattice BGK model, 2D with 9 velocities'
	print*,'*****************************************'
	print*,'Number of cells :',nx,'*',ny
	print*,'Nsteps :',nsteps
	print*,'Relaxation frequency :',omega
	print*,'Initial velocity for this Poiseuille force :',u0
	print*,'Initial density :',rho0
	print*,'Applied force :',iforce
	if (iobst) then
	    print*,' Linear Obstacle with length :',nobst
	endif
	write(6,'(A)')'Output file :',fileout
css        pause

c constants

	cs2  = 1.0d0 / 3.0d0
	cs22 = 2.0d0 * cs2
	cssq = 2.0d0 / 9.0d0

c reduced density

	den = rho0/float(npop) 

c calculation of the viscosity

	visc = (1.0d0 / omega - 0.5d0) * cs2
	print*,' Viscosity :',visc

c calculation of the constant applied force

	fpois = 8.0d0 * visc * uf / dfloat(ny) / dfloat(ny)
        fpois = rho0*fpois/6.  ! # of biased populations
	print*,' Intensity of the applied force ',fpois
	
	return
	end
c--------------------------------------------------
	subroutine inithydro
	
	implicit double precision(a-h,o-z)
	include'lbe.par'
c---------------------------------------------------
        write(6,*) 'u0',u0
	do j = 0, ny+1
	  do i = 0, nx+1
           rho(i,j)  = rho0
	      u(i,j) = u0
	      v(i,j) = 0.0d0
cdiag          write(6,*) i,j,rho(i,j),u(i,j),v(i,j)
	  enddo
        enddo

	rt0 = rho0* 4.0d0 / 9.0d0
	rt1 = rho0/ 9.0d0
	rt2 = rho0/ 36.0d0

	return	
	end
c --------------------------------------------------
	subroutine initpop
	
	implicit double precision(a-h,o-z)
	include'lbe.par'
c---------------------------------------------------
	do j = 0, ny+1
	  do i = 0, nx+1
            do ip=0,npop-1
              f(ip,i,j)=feq(ip,i,j)
            end do
          end do
        end do

        return
        end
c----------------------------------------------
	subroutine move
c----------------------------------------------
	implicit double precision(a-h,o-z)
	include'lbe.par'
c---------------------------------------------
	do j = ny,1,-1
	   do i = 1, nx
              f(2,i,j) = f(2,i,j-1)
              f(6,i,j) = f(6,i+1,j-1)
	   enddo
        enddo

	do j = ny,1,-1
	   do i = nx,1,-1
	      f(1,i,j) = f(1,i-1,j)
	      f(5,i,j) = f(5,i-1,j-1)
	   enddo
	enddo

	do j = 1,ny
	   do i = nx,1,-1
	   f(4,i,j) = f(4,i,j+1)
	   f(8,i,j) = f(8,i-1,j+1)
	   enddo
	enddo

	do j = 1,ny
	   do i = 1,nx
	   f(3,i,j) = f(3,i+1,j)
	   f(7,i,j) = f(7,i+1,j+1)
           enddo
	enddo

	return
	end
c---------------------------------------------
	subroutine hydrovar

	implicit double precision(a-h,o-z)
	include'lbe.par'
c--------------------------------------------
c Calculation of velocities

	do j = 1,ny
          do i = 1,nx
            
            rho(i,j) = f(1,i,j)+f(2,i,j)+f(3,i,j)
     &        + f(4,i,j)+f(5,i,j)+f(6,i,j)
     &        + f(7,i,j)+f(8,i,j)+f(0,i,j)
            
          rhoi=1./rho(i,j)
	  u(i,j)=(f(1,i,j)-f(3,i,j)+f(5,i,j)- 
     .            f(6,i,j)-f(7,i,j)+f(8,i,j))*rhoi 

	  v(i,j)=(f(5,i,j)+f(2,i,j)+f(6,i,j)
     .          - f(7,i,j)-f(4,i,j)-f(8,i,j))*rhoi

	  enddo
	enddo
		
	return
	end
c-------------------------------------------------
	subroutine equili

	implicit double precision(a-h,o-z)
	include'lbe.par'
c-------------------------------------------------
	do j = 0,ny+1
	  do i = 0,nx+1
            rl=rho(i,j)/rho0
	    usq = u(i,j) * u(i,j) 
	    vsq = v(i,j) * v(i,j)
	    sumsq = (usq + vsq) / cs22
	    sumsq2 = sumsq * (1.0d0 - cs2) / cs2
	    u2 = usq / cssq 
            v2 = vsq / cssq
	    ui = u(i,j) / cs2
	    vi = v(i,j) / cs2
	    uv = ui * vi

	    feq(0,i,j) = rl*rt0*(1.0d0 - sumsq)
	    feq(1,i,j) = rl*rt1*(1.0d0 - sumsq + u2 + ui)
	    feq(2,i,j) = rl*rt1*(1.0d0 - sumsq + v2 + vi)
	    feq(3,i,j) = rl*rt1*(1.0d0 - sumsq + u2 - ui)
	    feq(4,i,j) = rl*rt1*(1.0d0 - sumsq + v2 - vi)
	    feq(5,i,j) = rl*rt2*(1.0d0 + sumsq2 + ui + vi + uv)
	    feq(6,i,j) = rl*rt2*(1.0d0 + sumsq2 - ui + vi - uv)
	    feq(7,i,j) = rl*rt2*(1.0d0 + sumsq2 - ui - vi + uv)
	    feq(8,i,j) = rl*rt2*(1.0d0 + sumsq2 + ui - vi - uv)
	
	   enddo
	enddo
check on equilibria 
        eps=1.e-3
	do j = 1,ny
	  do i = 1,nx
           znorm = 1./rho(i,j)
	   ueq=(feq(1,i,j) - feq(3,i,j) + feq(5,i,j) - 
     .          feq(6,i,j) - feq(7,i,j) + feq(8,i,j))*znorm 
	   veq=(feq(5,i,j) + feq(2,i,j) + feq(6,i,j)
     .         -feq(7,i,j) - feq(4,i,j) - feq(8,i,j))*znorm
c check only u for now
           if(u(i,j).gt.1.e-5) then
            eu=abs(u(i,j)/ueq-1.d0)
            if(eu.gt.eps) then
             write(6,*) 'EQUILI U PROBLEM!'
             write(6,*) i,j,u(i,j),ueq,eu 
             stop
            endif
           endif

	  enddo
	enddo
		
	return
	end
c----------------------------------------------------------
	subroutine collis

	implicit double precision(a-h,o-z)
	include'lbe.par'
c----------------------------------------------------------
	do k = 0,npop-1
	 do j = 1,ny
	  do i = 1,nx
	   f(k,i,j)=f(k,i,j)*(1.0d0-omega)+omega*feq(k,i,j)
	  enddo
	 enddo
	enddo
	    
	return 
	end  
c ==========================================
	subroutine force(it,frce)
c ==========================================
	implicit double precision(a-h,o-z)
	include'lbe.par'
c--------------------------------------------------------
        frce = fpois
	do j = 1,ny
	   do i = 1,nx
	      f(1,i,j) = f(1,i,j) + frce
	      f(5,i,j) = f(5,i,j) + frce
	      f(8,i,j) = f(8,i,j) + frce

	      f(3,i,j) = f(3,i,j) - frce
              f(6,i,j) = f(6,i,j) - frce
              f(7,i,j) = f(7,i,j) - frce
	   enddo
	enddo

	return
	end
c =========================
	subroutine pbc
c =========================
	implicit double precision(a-h,o-z)
	include'lbe.par'
c-----------------------------------------------------------
c EAST case
	
	do j = 1,ny
	   f(1,0,j) = f(1,nx,j)
	   f(5,0,j) = f(5,nx,j)
	   f(8,0,j) = f(8,nx,j)
	enddo

c WEST case
	do j = 1,ny
	   f(3,nx+1,j) = f(3,1,j)
	   f(6,nx+1,j) = f(6,1,j)
	   f(7,nx+1,j) = f(7,1,j)
	enddo

c NORTH case
	do i = 1,nx
	   f(2,i,0) = f(2,i,ny)
	   f(5,i,0) = f(5,i,ny)
           f(6,i,0) = f(6,i,ny)
	enddo

c SOUTH case
	do i = 1,nx
	   f(4,i,ny+1) = f(4,i,1)
           f(7,i,ny+1) = f(7,i,1)
           f(8,i,ny+1) = f(8,i,1)
	enddo

	return
	end
c-------------------------------------------------------------
	subroutine mbc
	
	implicit double precision(a-h,o-z)
	include'lbe.par'
c-------------------------------------------------------------
c EAST case

	 do j = 1,ny
           f(1,0,j) = f(1,nx,j)
           f(5,0,j) = f(5,nx,j)
           f(8,0,j) = f(8,nx,j)
        enddo

c WEST case

	do j = 1,ny
           f(3,nx+1,j) = f(3,1,j)
           f(6,nx+1,j) = f(6,1,j)
           f(7,nx+1,j) = f(7,1,j)
        enddo

c NORTH case

	do i = 1,nx
	   f(4,i,ny+1) = f(2,i,ny)
	   f(8,i,ny+1) = f(6,i+1,ny)
	   f(7,i,ny+1) = f(5,i-1,ny)
	enddo

c SOUTH case

	do i = 1,nx
           f(2,i,0) = f(4,i,1)
           f(6,i,0) = f(8,i-1,1)
           f(5,i,0) = f(7,i+1,1)
        enddo

	return
	end
c ==========================
	subroutine plate
c ==========================
	implicit double precision(a-h,o-z)
        include'lbe.par'
        integer i,jtop,jbot 
c--------------------------------------------------------
	i = nx / 4	
        jtop=ny/2+nobst/2+1
        jbot=ny/2-nobst/2
        do j = jbot,jtop
          f(1,i,j) = f(3,i+1,j)
          f(5,i,j) = f(7,i+1,j+1)
          f(8,i,j) = f(6,i+1,j-1)
          
          f(3,i,j) = f(1,i-1,j)
          f(7,i,j) = f(5,i-1,j-1)
          f(6,i,j) = f(8,i-1,j+1)
        enddo
c top    of plate
        f(2,i,jtop) = f(4,i,jtop+1)
        f(5,i,jtop) = f(7,i+1,jtop+1)
        f(6,i,jtop) = f(8,i-1,jtop+1)
c bottom of plate        
        f(4,i,jtop) = f(2,i,jtop-1)
        f(7,i,jtop) = f(5,i-1,jtop-1)
        f(8,i,jtop) = f(6,i+1,jtop-1)
	return
	end

c-----------------------------------------------------------
	subroutine profil(it,frce)
c-----------------------------------------------------------
	implicit double precision(a-h,o-z)
	include'lbe.par'
c----------------------------------------------------------
        write(6,*) 'ucenter,force',u(nx/2,ny/2),frce
	do j = 1,ny
	  write(10,*) j,u(nx/4,j),u(nx/2,j),u(3*nx/4,j)
	  write(11,*) j,v(nx/4,j),v(nx/2,j),v(3*nx/4,j)
	enddo
	write(10,'(bn)')
	write(11,'(bn)')

	do i = 1,nx
	  write(14,*) i,u(i,ny/2),v(i,ny/2)
	  write(16,*) i,f(1,i,ny/2),f(3,i,ny/2)
	enddo
	write(14,'(bn)')

        write(50,*) it,u(nx/2,ny/2)
   
	return
	end
c---------------------------------------------------------
	subroutine diag0D
c---------------------------------------------------------
	implicit double precision(a-h,o-z)
        include'lbe.par'
c----------------------------------------------------------
	densit = 0.0d0

	do k = 0,npop-1
	   do j= 1,ny
	      do i = 1,nx
	         densit = densit + f(k,i,j)
	      enddo
	   enddo
	enddo

	densit = densit / dfloat(nx*ny) /dfloat(npop)

	umoy = 0.0d0
	vmoy = 0.0d0

	do j = 1,ny
	   do i = 1,nx
	      umoy = umoy + u(i,j)
	      vmoy = vmoy + v(i,j)
	   enddo
	enddo
	
	umoy = umoy / dfloat(nx*ny)
	vmoy = vmoy / dfloat(nx*ny)

	print*,'diagnostic 0D : istep density umoy and vmoy',
     .          istep,densit,umoy,vmoy


	return
	end
c =========================================================
	subroutine config(istep,iconf) 
c =========================================================
	implicit double precision(a-h,o-z)
        include'lbe.par'
c -------------------------------------------
        character*12 Buffer
        character us,ds,cs,ms
        integer aux 
        
        
        aux = istep/nout
        ms = char(ichar('0') + aux/1000)
        aux = mod(aux,1000)
        cs = char(ichar('0') + aux/100)
        aux = mod(aux,100)
        ds = char(ichar('0') + aux/10)
        aux = mod(aux,10)
        us = char(ichar('0') + aux)

        Buffer = fileout
        Buffer(6:6) = ms
        Buffer(7:7) = cs
        Buffer(8:8) = ds
        Buffer(9:9) = us
        Buffer(10:10) = ' '
        Buffer(11:11) = char(0)
        
 
        open( 60, file=Buffer )
c        iout=60+iconf
        iout = 60

cap	do j = 1,ny
cap	  do i = 1,nx
cap            write(iout,*) i,j,u(i,j),v(i,j)
cap	   enddo
cap	enddo

	do j = 1,ny
c	  do i = 1,nx
c            write(iout,*) u(i,j)
c	   enddo
        write(iout,444) (u(i,j), i=1,nx)
        write (iout,*) ' '
 444	format(1x,<nx>(F20.16,1x))
	enddo

c        write(6,*) 'configuration at time and file>>>>>> ',istep,iout
        write(6,*) 'configuration at time and file>>>>>> ',istep, Buffer
	
	return
	end











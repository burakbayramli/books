! dsmcne - Program to simulate a dilute gas using DSMC algorithm
! This version simulates planar Couette flow

      program dsmcne
      integer*4 MAXnpart, MAXncell
      parameter( MAXnpart = 10000, MAXncell = 500 )
      integer*4 npart, i, ncell, colSum, strikes(2), strikeSum(2)
      integer*4 istep, nstep, col, nsamp, seed
      integer*4 cell_n(MAXncell), index(MAXncell), Xref(MAXnpart)
      real*8 pi, boltz, mass, diam, T, density, L, Volume, eff_num
      real*8 mfp, mpv, vwall_m, vwall, x(MAXnpart), v(MAXnpart,3)
      real*8 tau, vrmax(MAXncell), selxtra(MAXncell), coeff, delv(2)
      real*8 tsamp, dvtot(2), dverr(2)
      real*8 ave_n(MAXncell), ave_ux(MAXncell), ave_uy(MAXncell)
      real*8 ave_uz(MAXncell), ave_T(MAXncell)
      real*8 force(2), ferr(2), vgrad, visc, viscerr, eta
      integer*4 colider
      real*8 rand, randn
      common /SortList/ ncell, npart, cell_n, index, Xref
      common /SampList/ ave_n, ave_ux, ave_uy, ave_uz, ave_T,
     &                  nsamp

      !* Initialize constants  (particle mass, diameter, etc.)
      pi = 3.141592654
      boltz = 1.3806e-23     ! Boltzmann's constant (J/K)
      mass = 6.63e-26        ! Mass of argon atom (kg)
      diam = 3.66e-10        ! Effective diameter of argon atom (m)
      T = 273                ! Temperature (K)
      density = 2.685e25     ! Number density of argon at STP (m^-3)
      L = 1e-6               ! System size is one micron
      Volume = L**3          ! Volume of the system
      write(*,*) 'Enter number of simulation particles: '
      read(*,*) npart
      eff_num = density*L**3/npart
      write(*,*) 'Each particle represents ', eff_num, ' atoms'
      mfp = Volume/(sqrt(2.0)*pi*diam**2*npart*eff_num)
      write(*,*) 'System width is ', L/mfp, ' mean free paths'
      mpv = sqrt(2*boltz*T/mass)  ! Most probable initial velocity
      write(*,*) 'Enter wall velocity as Mach number: '
      read(*,*) vwall_m
      vwall = vwall_m * sqrt(5./3. * boltz*T/mass)
      write(*,*) 'Wall velocities are ', -vwall, ' and ',
     &                                    vwall, ' m/s'

      !* Assign random positions and velocities to particles
      seed = 1       ! Initial seed for rand (DO NOT USE ZERO)
      do i=1,npart
        x(i) = L*rand(seed)         ! Assign random positions
        ! Initial velocities are Maxwell-Boltzmann distributed
        v(i,1) = sqrt(boltz*T/mass) * randn(seed)
        v(i,2) = sqrt(boltz*T/mass) * randn(seed)
        v(i,3) = sqrt(boltz*T/mass) * randn(seed)
        ! Add velocity gradient to the y-component
        v(i,2) = v(i,2) + vwall * 2*(x(i)/L - 0.5)
      enddo

      !* Initialize variables used for evaluating collisions
      ncell = 20                        ! Number of cells
      tau = 0.2*(L/ncell)/mpv           ! Set timestep tau
      do i=1, ncell
        vrmax(i) = 3*mpv        ! Estimated max rel. speed
        selxtra(i) = 0.0        ! Used by routine 'colider'
      enddo
      coeff = 0.5*eff_num*pi*diam**2*tau/(L**3/ncell)

      !* Declare object for lists used in sorting
      !! FORTRAN program uses common blocks to pass these lists

      !* Initialize object and variables used in statistical sampling
      !! FORTRAN program uses common blocks to pass statistics lists
      nsamp = 0
      do i=1,ncell
        ave_n(i)  = 0.0
        ave_ux(i) = 0.0
        ave_uy(i) = 0.0
        ave_uz(i) = 0.0
        ave_T(i)  = 0.0
      enddo
      tsamp = 0.0                   ! Total sampling time
      dvtot(1) = 0.0                ! Total momentum change at a wall
      dvtot(2) = 0.0
      dverr(1) = 0.0                ! Used to find error in dvtot
      dverr(2) = 0.0

      !* Loop for the desired number of time steps
      colSum = 0              ! Count total collisions
      strikeSum(1) = 0.0      ! Count strikes on each wall
      strikeSum(2) = 0.0
      write(*,*) 'Enter total number of time steps: '
      read(*,*) nstep
      do istep = 1,nstep

        !* Move all the particles
        delv(1) = 0.0
        delv(2) = 0.0
        call mover( x, v, npart, L, mpv, vwall,
     &              tau, strikes, delv, seed )
        strikeSum(1) = strikeSum(1) + strikes(1)
        strikeSum(2) = strikeSum(2) + strikes(2)

        !* Sort the particles into cells
        call sorter(x,L)

        !* Evaluate collisions among the particles
        col = colider(v,vrmax,tau,seed,selxtra,coeff)
        colSum = colSum + col  ! Increment collision count

        !* After initial transient, accumulate statistical samples
        if(istep .gt. nstep/10) then
          call sampler(x,v,npart,ncell,L)
          ! Cummulative velocity change for particles striking walls
          dvtot(1) = dvtot(1) + delv(1)
          dvtot(2) = dvtot(2) + delv(2)
          dverr(1) = dverr(1) + delv(1)*delv(1)
          dverr(2) = dverr(2) + delv(2)*delv(2)
          tsamp = tsamp + tau
        endif

        !* Periodically display the current progress
        if( mod(istep,100) .lt. 1 ) then
          write(*,*) 'Done ', istep, ' of ', nstep, ' steps; ',
     &                 colSum, ' collisions'
          write(*,*) 'Total wall strikes: ', strikeSum(1), ' (left) ',
     &                 strikeSum(2), ' (right)'
        endif
      enddo

      !* Normalize the accumulated statistics
      do i=1,ncell
        ave_n(i) = ave_n(i)*(eff_num/(Volume/ncell))/nsamp
        ave_ux(i) = ave_ux(i)/nsamp
        ave_uy(i) = ave_uy(i)/nsamp
        ave_uz(i) = ave_uz(i)/nsamp
        ave_T(i) = ave_T(i) * mass/(3*boltz*nsamp)
      enddo
      dverr(1) = dverr(1)/(nsamp-1) - (dvtot(1)/nsamp)*(dvtot(1)/nsamp)
      dverr(1) = sqrt(dverr(1)*nsamp)
      dverr(2) = dverr(2)/(nsamp-1) - (dvtot(2)/nsamp)*(dvtot(2)/nsamp)
      dverr(2) = sqrt(dverr(2)*nsamp)

      !* Compute viscosity from drag force on the walls
      force(1) = (eff_num*mass*dvtot(1))/(tsamp*L**2)
      force(2) = (eff_num*mass*dvtot(2))/(tsamp*L**2)
      ferr(1) = (eff_num*mass*dverr(1))/(tsamp*L**2)
      ferr(2) = (eff_num*mass*dverr(2))/(tsamp*L**2)
      write(*,*) 'Force per unit area is'
      write(*,*) 'Left wall:  ', force(1), ' +/- ', ferr(1)
      write(*,*) 'Right wall: ', force(2), ' +/- ', ferr(2)
      vgrad = 2*vwall/L    ! Velocity gradient
      visc = 0.5*(-force(1)+force(2))/vgrad  ! Average viscosity
      viscerr = 0.5*(ferr(1)+ferr(2))/vgrad  ! Error
      write(*,*) 'Viscosity = ', visc, ' +/- ', viscerr, 'N s/m^2'
      eta = 5.*pi/32.*mass*density*(2./sqrt(pi)*mpv)*mfp
      write(*,*) 'Theoretical value of viscoisty is ', eta, 'N s/m^2'


      !* Print out the plotting variables:
      !    xcell, ave_n, ave_ux, ave_uy, ave_uz, ave_T
      open(11,file='xcell.txt',status='unknown')
      open(12,file='ave_n.txt',status='unknown')
      open(13,file='ave_ux.txt',status='unknown')
      open(14,file='ave_uy.txt',status='unknown')
      open(15,file='ave_uz.txt',status='unknown')
      open(16,file='ave_T.txt',status='unknown')
      do i=1,ncell
        write(11,*) (i-0.5)*L/ncell
        write(12,*) ave_n(i)
        write(13,*) ave_ux(i)
        write(14,*) ave_uy(i)
        write(15,*) ave_uz(i)
        write(16,*) ave_T(i)
      enddo

      stop
      end
!***** To plot in MATLAB; use the script below ********************
!load xcell.txt;  load ave_n.txt;     load ave_ux.txt;
!load ave_uy.txt; load ave_uz.txt;    load ave_T.txt;
!figure(1); clf;
!plot(xcell,ave_n); xlabel('position');  ylabel('Number density');
!figure(2); clf;
!plot(xcell,ave_ux,xcell,ave_uy,xcell,ave_uz);
!xlabel('position');  ylabel('Velocities');
!legend('x-component','y-component','z-component');
!figure(3); clf;
!plot(xcell,ave_T); xlabel('position');  ylabel('Temperature');
!******************************************************************

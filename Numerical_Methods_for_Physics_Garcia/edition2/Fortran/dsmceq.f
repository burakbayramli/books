!  dsmceq - Dilute gas simulation using DSMC algorithm
!  This version illustrates the approach to equilibrium

      program dsmceq
      integer*4 MAXnpart, MAXncell
      parameter( MAXnpart = 10000, MAXncell = 500 )
      integer*4 npart, seed, i, plusMinus, ncell, coltot, col
      integer*4 istep, nstep
      integer*4 cell_n(MAXncell), index(MAXncell), Xref(MAXnpart)
      real*8 pi, boltz, mass, diam, T, density, L, eff_num, v_init
      real*8 x(MAXnpart), v(MAXnpart,3)
      real*8 vmagI(MAXnpart), vmagF(MAXnpart)
      real*8 tau, vrmax(MAXncell), selxtra(MAXncell), coeff
      common /SortList/ ncell, npart, cell_n, index, Xref
      integer*4 colider
      real*8 rand

      !* Initialize constants  (particle mass, diameter, etc.)
      pi = 3.141592654
      boltz = 1.3806e-23    ! Boltzmann's constant (J/K)
      mass = 6.63e-26       ! Mass of argon atom (kg)
      diam = 3.66e-10       ! Effective diameter of argon atom (m)
      T = 273               ! Temperature (K)
      density = 1.78        ! Density of argon at STP (kg/m^3)
      L = 1e-6              ! System size is one micron
      write(*,*) 'Enter number of simulation particles: '
      read(*,*) npart
      eff_num = density/mass*L**3/npart
      write(*,*) 'Each particle represents ', eff_num, ' atoms'

      !* Assign random positions and velocities to particles
      seed = 1       ! Initial seed for rand (DO NOT USE ZERO)
      v_init = sqrt(3.0*boltz*T/mass)    ! Initial speed
      do i=1,npart
        x(i) = L*rand(seed)   ! Assign random positions
        plusMinus = (1 - 2*int(2*rand(seed)))  ! +1 or -1 (equal prob.)
        v(i,1) = plusMinus * v_init
        v(i,2) = 0.0   ! Only x-component is non-zero
        v(i,3) = 0.0
      enddo

      !* Record inital particle speeds
      do i=1,npart
        vmagI(i) = sqrt( v(i,1)**2 + v(i,2)**2 + v(i,3)**2 )
      enddo

      !* Initialize variables used for evaluating collisions
      ncell = 15                       ! Number of cells
      tau = 0.2*(L/ncell)/v_init       ! Set timestep tau
      do i=1,ncell
        vrmax(i) = 3*v_init     ! Estimated max rel. speed
        selxtra(i) = 0.0        ! Used by routine 'colider'
      enddo
      coeff = 0.5*eff_num*pi*diam**2*tau/(L**3/ncell)
      coltot = 0                ! Count total collisions

      !* Declare object for lists used in sorting
      !! FORTRAN program uses common blocks to pass these lists

      !* Loop for the desired number of time steps
      write(*,*) 'Enter total number of time steps: '
      read(*,*) nstep
      do istep = 1,nstep

        !* Move all the particles ballistically
        do i=1,npart
          x(i) = x(i) + v(i,1)*tau       ! Update x position of particle
          x(i) = dmod(x(i)+L,L)          ! Periodic boundary conditions
        enddo

        !* Sort the particles into cells
        call sorter(x,L)

        !* Evaluate collisions among the particles
        col = colider(v,vrmax,tau,seed,selxtra,coeff)
        coltot = coltot + col   ! Increment collision count

        !* Periodically display the current progress
        if( mod(istep,10) .lt. 1 ) then
          write(*,*) 'Done ', istep, ' of ', nstep, ' steps; ',
     &                 coltot, ' collisions'
        endif
      enddo

      ! Record final particle speeds
      do i=1,npart
        vmagF(i) = sqrt( v(i,1)**2 + v(i,2)**2 + v(i,3)**2 )
      enddo

      !* Print out the plotting variables: vmagI, vmagF
      open(11,file='vmagI.txt',status='unknown')
      open(12,file='vmagF.txt',status='unknown')
      do i=1,npart
        write(11,*) vmagI(i)
        write(12,*) vmagF(i)
      enddo

      stop
      end

!***** To plot in MATLAB; use the script below ********************
!load vmagI.txt; load vmagF.txt;
!%* Plot the histogram of the initial speed distribution
!figure(1); clf;
!vbin = 50:100:1050;    % Bins for histogram
!hist(vmagI,vbin);  title('Initial speed distribution');
!xlabel('Speed (m/s)');  ylabel('Number');
!%* Plot the histogram of the final speed distribution
!figure(2); clf;
!hist(vmagF,vbin);
!title(sprintf('Final speed distribution'));
!xlabel('Speed (m/s)');  ylabel('Number');
!*****************************************************************

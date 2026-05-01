      subroutine mover( x, v, npart, L, mpv, vwall, tau,
     &                  strikes, delv, seed )
      integer*4 MAXnpart, MAXncell
      parameter( MAXnpart = 10000, MAXncell = 500 )
      integer*4 npart, seed, strikes(2)
      real*8 x(MAXnpart), v(MAXnpart,3), L, mpv, vwall, tau, delv(2)

! mover - Function to move particles by free flight
!         Also handles collisions with walls
! Inputs
!    x        Positions of the particles
!    v        Velocities of the particles
!    npart    Number of particles in the system
!    L        System length
!    mpv      Most probable velocity off the wall
!    vwall    Wall velocities
!    tau      Time step
!    seed     Random number seed
! Outputs
!    x,v      Updated positions and velocities
!    strikes  Number of particles striking each wall
!    delv     Change of y-velocity at each wall
!    seed     Random number seed

      integer*4 i, flag
      real*8 x_old(MAXnpart), xwall(2), vw(2), direction(2), stdev
      real*8 vyInitial, dtr
      real*8 rand, randn

      !* Move all particles pretending walls are absent
      do i=1,npart
        x_old(i) = x(i)            ! Remember original position
        x(i) = x_old(i) + v(i,1)*tau
      enddo

      !* Check each particle to see if it strikes a wall
      strikes(1) = 0
      strikes(2) = 0
      delv(1) = 0.0
      delv(2) = 0.0
      xwall(1) = 0
      xwall(2) = L    ! Positions of walls
      vw(1) = -vwall
      vw(2) = vwall   ! Velocities of walls
      stdev = mpv/sqrt(2.0)

      direction(1) = 1
      direction(2) = -1  ! Direction of particle leaving wall
      do i=1,npart

        !* Test if particle strikes either wall
        flag = 0
        if( x(i) .le. 0 ) then
          flag=1        ! Particle strikes left wall
        else if( x(i) .ge. L ) then
          flag=2        ! Particle strikes right wall
        endif

        !* If particle strikes a wall, reset its position
        !  and velocity. Record velocity change.
        if( flag .ne. 0 )  then
          strikes(flag) = strikes(flag) + 1
          vyInitial = v(i,2)
          !* Reset velocity components as biased Maxwellian,
          !  Exponential dist. in x; Gaussian in y and z
          v(i,1) = direction(flag)*sqrt(-dlog(1.0-rand(seed))) * mpv
          v(i,2) = stdev*randn(seed) + vw(flag)  ! Add wall velocity
          v(i,3) = stdev*randn(seed)
          ! Time of flight after leaving wall
          dtr = tau*(x(i)-xwall(flag))/(x(i)-x_old(i))
          !* Reset position after leaving wall
          x(i) = xwall(flag) + v(i,1)*dtr
          !* Record velocity change for force measurement
          delv(flag) = delv(flag) + (v(i,2) - vyInitial)
        endif
      enddo

      return
      end

      integer*4 function colider( v, crmax, tau, seed,
     &                            selxtra, coeff )
      integer*4 MAXnpart, MAXncell
      parameter( MAXnpart = 10000, MAXncell = 500 )
      integer*4 seed
      real*8 v(MAXnpart,3), crmax(MAXncell), tau,
     &        selxtra(MAXncell), coeff
! colide - Function to process collisions in cells
! Inputs
!    v         Velocities of the particles
!    crmax     Estimated maximum relative speed in a cell
!    tau       Time step
!    seed      Current random number seed
!    selxtra   Extra selections carried over from last timestep
!    coeff     Coefficient in computing number of selected pairs
! Outputs
!    v         Updated velocities of the particles
!    crmax     Updated maximum relative speed
!    selxtra   Extra selections carried over to next timestep
!    col       Total number of collisions processed    (Return value)

      integer*4 col, jcell, number, nsel, isel, k, kk, ip1, ip2
      real*8 pi, select, cr, crm, vcm(3), vrel(3), cos_th, sin_th, phi
      integer*4 ncell, npart
      integer*4 cell_n(MAXncell), index(MAXncell), Xref(MAXnpart)
      common /SortList/ ncell, npart, cell_n, index, Xref
      real*8 rand

      col = 0          ! Count number of collisions
      pi = 3.141592654

      !* Loop over cells, processing collisions in each cell
      do jcell=1,ncell

       !* Skip cells with only one particle
       number = cell_n(jcell)
       if( number .gt. 1 ) then

        !* Determine number of candidate collision pairs
        !  to be selected in this cell
        select = coeff*number**2*crmax(jcell) + selxtra(jcell)
        nsel = int(select)            ! Number of pairs to be selected
        selxtra(jcell) = select-nsel  ! Carry over any left-over fraction
        crm = crmax(jcell)            ! Current maximum relative speed

        !* Loop over total number of candidate collision pairs
        do isel=1,nsel

          !* Pick two particles at random out of this cell
          k = int(rand(seed)*number)
          kk = mod( int(k+rand(seed)*(number-1))+1, number )
          ip1 = Xref( k+index(jcell) )      ! First particle
          ip2 = Xref( kk+index(jcell) )     ! Second particle

          !* Calculate pair's relative speed
          cr = sqrt( (v(ip1,1)-v(ip2,1))**2 +
     &               (v(ip1,2)-v(ip2,2))**2 +
     &               (v(ip1,3)-v(ip2,3))**2 )
          if( cr .gt. crm ) then    ! If relative speed larger than crm,
            crm = cr                ! then reset crm to larger value
          endif

          !* Accept or reject candidate pair according to relative speed
          if( cr/crmax(jcell) .gt. rand(seed) ) then
            !* If pair accepted, select post-collision velocities
            col = col + 1                     ! Collision counter
            do k=1,3
              vcm(k) = 0.5*(v(ip1,k) + v(ip2,k))       ! Center of mass velocity
            enddo
            cos_th = 1.0 - 2.0*rand(seed)       ! Cosine and sine of
            sin_th = sqrt(1.0 - cos_th**2)      ! collision angle theta
            phi = 2.0*pi*rand(seed)             ! Collision angle phi
            vrel(1) = cr*cos_th                 ! Compute post-collision
            vrel(2) = cr*sin_th*cos(phi)        ! relative velocity
            vrel(3) = cr*sin_th*sin(phi)
            do  k=1,3
              v(ip1,k) = vcm(k) + 0.5*vrel(k)   ! Update post-collision
              v(ip2,k) = vcm(k) - 0.5*vrel(k)   ! velocities
            enddo

          endif
          crmax(jcell) = crm     ! Update max relative speed

        enddo   ! Loop over pairs
       endif
      enddo   ! Loop over cells

      colider = col  ! Return the number of collisions
      return
      end

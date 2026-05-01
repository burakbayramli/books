      subroutine sampler( x, v, npart, ncell, L )
      integer*4 MAXnpart, MAXncell
      parameter( MAXnpart = 10000, MAXncell = 500 )
      integer*4 npart, ncell
      real*8 x(MAXnpart), v(MAXnpart,3), L
! sampler - Function to sample density, velocity and temperature
! Inputs
!    x       Particle positions
!    v       Particle velocities
!    npart   Number of particles
!    ncell   Number of cells
!    L       System size
!  SampList  Common block with sampling data
! Outputs
!  SampList  Common block with (updated) sampling data

      integer*4 jx(MAXnpart), i, jCell
      real*8 sum_n(MAXncell), sum_vx(MAXncell), sum_vy(MAXncell),
     &       sum_vz(MAXncell), sum_v2(MAXncell)
      integer*4 nsamp
      real*8 ave_n(MAXncell), ave_ux(MAXncell), ave_uy(MAXncell)
      real*8 ave_uz(MAXncell), ave_T(MAXncell)
      common /SampList/ ave_n, ave_ux, ave_uy, ave_uz, ave_T,
     &                  nsamp

      !* Compute cell location for each particle
      do i=1,npart
        jx(i) = int(ncell*x(i)/L) + 1
      enddo

      !* Initialize running sums of number, velocity and v^2
      do i=1,ncell
        sum_n(i)  = 0.0
        sum_vx(i) = 0.0
        sum_vy(i) = 0.0
        sum_vz(i) = 0.0
        sum_v2(i) = 0.0
      enddo

      !* For each particle, accumulate running sums for its cell
      do i=1,npart
        jcell = jx(i)  ! Particle i is in cell jcell
        sum_n(jcell) = sum_n(jcell) + 1
        sum_vx(jcell) = sum_vx(jcell) + v(i,1)
        sum_vy(jcell) = sum_vy(jcell) + v(i,2)
        sum_vz(jcell) = sum_vz(jcell) + v(i,3)
        sum_v2(jcell) = sum_v2(jcell) + v(i,1)**2 +
     &                      v(i,2)**2 + v(i,3)**2
      enddo

      !* Use current sums to update sample number, velocity
      !  and temperature
      do i=1,ncell
        sum_vx(i) = sum_vx(i)/sum_n(i)
        sum_vy(i) = sum_vy(i)/sum_n(i)
        sum_vz(i) = sum_vz(i)/sum_n(i)
        sum_v2(i) = sum_v2(i)/sum_n(i)
        ave_n(i) = ave_n(i) + sum_n(i)
        ave_ux(i) = ave_ux(i) + sum_vx(i)
        ave_uy(i) = ave_uy(i) + sum_vy(i)
        ave_uz(i) = ave_uz(i) + sum_vz(i)
        ave_T(i) = ave_T(i) + sum_v2(i) - (sum_vx(i)**2 +
     &                      sum_vy(i)**2 + sum_vz(i)**2)
      enddo
      nsamp = nsamp + 1

      return
      end

      real*8 function randn( seed )
      integer*4 seed
! Random number generator; Normal (Gaussian) dist.
! Input
!   seed    Integer seed  (DO NOT USE A SEED OF ZERO)
! Output
!   randn   Random number, Gaussian distributed
      real*8 rand

      randn = sqrt(-2*dlog(1.-rand(seed)))
     &            * cos(6.283185307 * rand(seed))

      return
      end

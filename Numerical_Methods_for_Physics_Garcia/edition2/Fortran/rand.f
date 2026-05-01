      real*8 function rand( seed )
      integer*4 seed
! Random number generator; Uniform dist. in [0,1)
! Input
!   seed    Integer seed (DO NOT USE A SEED OF ZERO)
! Output
!   rand    Random number uniformly distributed in [0,1)

      static a,m
      real*8 a,m,temp
      data a,m/16807.0, 2147483647.0/

      temp = a*seed
      seed = dmod(temp,m)
      rand = seed/m

      return
      end

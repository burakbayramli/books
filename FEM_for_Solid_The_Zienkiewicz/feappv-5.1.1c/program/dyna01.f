!$Id:$
      subroutine dyna01(du,urate,nneq,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Perform ODE updates using Newmark Algorithm

!      Inputs:
!         du(*)             Increment to displacement
!         urate(nneq,*)     Rate vectors - fixed by ALGO
!         nneq              numnp * ndf
!         isw               Control switch
!                            1  STARTING update: begining of time step
!                            2  UPDATE at an iteration within time step
!                            3  BACK solution to begining of time step
!         theta(3)          Integration parameters:

!      Outputs:
!         urate(nneq,nn)    Rate vectors fixed by ALGO
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'ddata.h'
      include  'tdata.h'

      integer       :: n, nneq,isw
      real (kind=8) :: c6,ur1

      real (kind=8) :: du(*),urate(nneq,*)

      save

!     Newmark updates: urate(n,1) = velocity
!                      urate(n,2) = acceleration

!     (1) Update solution vectors to begin a step

      if(isw.eq.1) then

        c6 = dt*c1
        do n = 1,nneq
          urate(n,nrt-1) =  urate(n,1)
          urate(n,nrt  ) =  urate(n,2)
          ur1            = -c6*urate(n,1) + c3*urate(n,2)
          urate(n,1)     =  c4*urate(n,1) + c5*urate(n,2)
          urate(n,2)     =  ur1
        end do ! n

!     (2) Update with in time step
      elseif(isw.eq.2) then

        do n = 1,nneq
          urate(n,1) = urate(n,1) + c2*du(n)
          urate(n,2) = urate(n,2) + c1*du(n)
        end do ! n

!     (3) Backup solution vectors to reinitiate a step
      elseif(isw.eq.3) then

        do n = 1,nneq
          urate(n,1) = urate(n,nrt-1)
          urate(n,2) = urate(n,nrt)
        end do ! n

      endif

      end subroutine dyna01

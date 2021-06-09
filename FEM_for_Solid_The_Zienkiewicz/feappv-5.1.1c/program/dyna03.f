!$Id:$
      subroutine dyna03(du,urate,nneq,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Perform static and ODE updates using SSpj Algorithm

!      Inputs:
!         du(*)             Increment to displacement
!         urate(nneq,*)     Rate vectors - fixed by ALGO
!         nneq              numnp * ndf
!         isw               Control switch
!                            1  STARTING update: begining of time step
!                            2  UPDATE at an iteration within time step
!                            3  BACK solution to begining of time step
!         theta(3)          Integration parameters:
!                            1-parameter   = SS11
!                            2-parameters  = SS22

!      Outputs:
!         urate(nneq,nn)    Rate vectors fixed by ALGO
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'ddata.h'
      include  'tdata.h'

      integer       :: n, nneq,isw
      real (kind=8) :: cr

      real (kind=8) :: du(*),urate(nneq,*)

      save

!     SSpj updates: urate(n,1) = velocity
!                   urate(n,2) = acceleration

!     (1) Update solution vectors to begin a step
      if(isw.eq.1) then

!       Initialize step: SS11
        if(theta(2).eq.0.0d0) then

          do n = 1,nneq
            urate(n,nrt-1) = urate(n,1)
            urate(n,1)     = 0.0d0
            urate(n,3)     = du(n)
          end do

!       Initialize step: SS22
        else

          cr = 2.d0/dt
          c1 = theta(1)*dt
          c2 = theta(2)*dt*dt*0.5d0
          do n = 1,nneq
            urate(n,nrt-1) = urate(n,1)
            urate(n,nrt  ) = urate(n,2)

            urate(n,3)     =    du(n)+ c1*urate(n,1) + c2*urate(n,2)
            urate(n,4) =    urate(n,1) + c1*urate(n,2)

            urate(n,1) =   -urate(n,1)
            urate(n,2) = cr*urate(n,1)
          end do

        endif

!     (2) Update with in time step
      elseif(isw.eq.2) then

!       SS11 Algorithm
        if(theta(2).eq.0.0d0) then

          cr = theta(1)*dt
          do n = 1,nneq
            urate(n,1) = urate(n,1) +    du(n)
            urate(n,3) = urate(n,3) + cr*du(n)
          end do

!       SS22 Algorithm
        else

          c1 = theta(1)*dt
          c2 = theta(2)*dt*dt*0.5d0
          do n = 1,nneq
            urate(n,1) = urate(n,1) + dt*du(n)
            urate(n,2) = urate(n,2) +    du(n)
            urate(n,3) = urate(n,3) + c2*du(n)
            urate(n,4) = urate(n,4) + c1*du(n)
          end do

        endif

!     (3) Backup solution vectors to reinitiate a step
      elseif(isw.eq.3) then

        do n = 1,nneq
          urate(n,1) = urate(n,nrt-1)
          urate(n,2) = urate(n,nrt)
        end do

      endif

      end subroutine dyna03

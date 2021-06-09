!$Id:$
      subroutine dyna02(du,urate,nneq,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Perform static and ODE updates using GNpj Algorithm

!      Inputs:
!         du(*)             Increment to displacement
!         urate(nneq,*)     Rate vectors - fixed by ALGO
!         nneq              numnp * ndf
!         isw               Control switch
!                            1  STARTING update: begining of time step
!                            2  UPDATE at an iteration within time step
!                            3  BACK solution to begining of time step
!         theta(3)          Integration parameters:
!                            1-parameter   = GN11
!                            2-parameters  = GN22

!      Outputs:
!         urate(nneq,nn)    Rate vectors fixed by ALGO
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'ddata.h'
      include  'tdata.h'

      integer       :: n, nneq,isw
      real (kind=8) :: cr,cs,ct,cu, ur1

      real (kind=8) :: du(*),urate(nneq,*)

      save

!     GNpj updates: urate(n,1) = velocity
!                   urate(n,2) = acceleration

!     (1) Update solution vectors to begin a step
      if(isw.eq.1) then

!       Initialize step: GN11
        if(theta(2).eq.0.0d0) then

          cr = 1.d0 - 1.d0/theta(1)
          do n = 1,nneq
            urate(n,nrt-1) =    urate(n,1)
            urate(n,1    ) = cr*urate(n,1)
          end do

!       Initialize step: GN22
        else

          cr =  1.d0 - 2.d0*theta(1)/theta(2)
          cs = (1.d0 - theta(1)/theta(2))*dt
          ct =  2.d0/(theta(2)*dt)
          cu =  1.d0 - 1.d0/theta(2)
          do n = 1,nneq
            urate(n,nrt-1) =  urate(n,1)
            urate(n,nrt  ) =  urate(n,2)
            ur1            =  cr*urate(n,1) + cs*urate(n,2)
            urate(n,2)     = -ct*urate(n,1) + cu*urate(n,2)
            urate(n,1)     =  ur1
          end do

        endif

!     (2) Update with in time step
      elseif(isw.eq.2) then

!       GN11 Algorithm
        if(theta(2).eq.0.0d0) then

          do n = 1,nneq
            urate(n,1) = urate(n,1) + du(n)
          end do

!       GN22 Algorithm
        else

          cr = theta(1)*dt
          do n = 1,nneq
            urate(n,1) = urate(n,1) + cr*du(n)
            urate(n,2) = urate(n,2) +    du(n)
          end do

        endif

!     (3) Backup solution vectors to reinitiate a step
      elseif(isw.eq.3) then

        do n = 1,nneq
          urate(n,1) = urate(n,nrt-1)
          urate(n,2) = urate(n,nrt)
        end do

      endif

      end subroutine dyna02

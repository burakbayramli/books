c$Id:$
      subroutine dyna02(du,urate,nneq,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Perform static and ODE updates using SSpj Algorithm

c      Inputs:
c         du(*)             Increment to displacement
c         urate(nneq,*)     Rate vectors - fixed by ALGO
c         nneq              numnp * ndf
c         isw               Control switch
c                            1  STARTING update: begining of time step
c                            2  UPDATE at an iteration within time step
c                            3  BACK solution to begining of time step
c         theta(3)          Integration parameters:
c                            1-parameter   = SS11
c                            2-parameters  = SS22

c      Outputs:
c         urate(nneq,nn)    Rate vectors fixed by ALGO
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'ddata.h'
      include  'tdata.h'

      integer   n, nneq,isw
      real*8    cr

      real*8    du(*),urate(nneq,*)

      save

c     SSpj updates: urate(n,1) = velocity
c                   urate(n,2) = acceleration

c     (1) Update solution vectors to begin a step

      if(isw.eq.1) then

c       Initialize step: SS11

        if(theta(2).eq.0.0d0) then

          do n = 1,nneq
            urate(n,6) =   urate(n,1)
            urate(n,1) =   0.0d0
            urate(n,3) =   du(n)
          end do

c       Initialize step: SS22

        else

          cr = 2.d0/dt
          c1 = theta(1)*dt
          c2 = theta(2)*dt*dt*0.5d0
          do n = 1,nneq
            urate(n,6) =    urate(n,1)
            urate(n,7) =    urate(n,2)

            urate(n,3) =    du(n)+ c1*urate(n,1) + c2*urate(n,2)
            urate(n,4) =    urate(n,1) + c1*urate(n,2)

            urate(n,1) =   -urate(n,1)
            urate(n,2) = cr*urate(n,1)
          end do

        endif

c     (2) Update with in time step

      elseif(isw.eq.2) then

c       SS11 Algorithm

        if(theta(2).eq.0.0d0) then

          cr = theta(1)*dt
          do n = 1,nneq
            urate(n,1) = urate(n,1) +    du(n)
            urate(n,3) = urate(n,3) + cr*du(n)
          end do

c       SS22 Algorithm

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

c     (3) Backup solution vectors to reinitiate a step

      elseif(isw.eq.3) then

        do n = 1,nneq
          urate(n,1) = urate(n,6)
          urate(n,2) = urate(n,7)
        end do

      endif

      end

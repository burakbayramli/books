c$Id:$
      subroutine dyna01(du,urate,nneq,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Perform static and ODE updates using GNpj Algorithm

c      Inputs:
c         du(*)             Increment to displacement
c         urate(nneq,*)     Rate vectors - fixed by ALGO
c         nneq              numnp * ndf
c         isw               Control switch
c                            1  STARTING update: begining of time step
c                            2  UPDATE at an iteration within time step
c                            3  BACK solution to begining of time step
c         theta(3)          Integration parameters:
c                            1-parameter   = GN11
c                            2-parameters  = GN22

c      Outputs:
c         urate(nneq,nn)    Rate vectors fixed by ALGO
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'ddata.h'
      include  'tdata.h'

      integer   n, nneq,isw
      real*8    cr,cs,ct,cu, ur1

      real*8    du(*),urate(nneq,*)

      save

c     GNpj updates: urate(n,1) = velocity
c                   urate(n,2) = acceleration

c     (1) Update solution vectors to begin a step

      if(isw.eq.1) then

c       Initialize step: GN11

        if(theta(2).eq.0.0d0) then

          cr = 1.d0 - 1.d0/theta(1)
          do n = 1,nneq
            urate(n,6) =    urate(n,1)
            urate(n,1) = cr*urate(n,1)
          end do

c       Initialize step: GN22

        else

          cr =  1.d0 - 2.d0*theta(1)/theta(2)
          cs = (1.d0 - theta(1)/theta(2))*dt
          ct =  2.d0/(theta(2)*dt)
          cu =  1.d0 - 1.d0/theta(2)
          do n = 1,nneq
            urate(n,6) =     urate(n,1)
            urate(n,7) =     urate(n,2)
            ur1        =  cr*urate(n,1) + cs*urate(n,2)
            urate(n,2) = -ct*urate(n,1) + cu*urate(n,2)
            urate(n,1) =     ur1
          end do

        endif

c     (2) Update with in time step

      elseif(isw.eq.2) then

c       GN11 Algorithm

        if(theta(2).eq.0.0d0) then

          do n = 1,nneq
            urate(n,1) = urate(n,1) + du(n)
          end do

c       GN22 Algorithm

        else

          cr = theta(1)*dt
          do n = 1,nneq
            urate(n,1) = urate(n,1) + cr*du(n)
            urate(n,2) = urate(n,2) +    du(n)
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

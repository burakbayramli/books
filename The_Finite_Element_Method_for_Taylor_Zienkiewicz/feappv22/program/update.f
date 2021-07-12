c$Id:$
      subroutine update(id,f,u,urate,du,fdyn,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Static and Dynamic UPDATE algorithms for F E A P

c      Inputs:
c       id (ndf,*)   - ID-array (vector storage)
c       f  (nneq,2)  - Nodal force/displ: 1 = current; 2 = previous
c       u (3*nneq)   - Displacement vectors
c       urate(nneq,*)- Rate vectors fixed by ALGO
c       du(nneq)     - Displacement increment from SOLVER
c       nneq         - numnp * ndf
c       ndf          - Number of DOF/node
c       fdyn         - Flag: true for dynamics
c       isw          - Control switch
c                          1  STARTING update: begining of time step
c                          2  UPDATE at an iteration within time step
c                          3  BACK solution to begining of time step

c      Outputs:
c       u (3*nneq)   - Displacement vectors
c       urate(nneq,*)- Rate vectors fixed by ALGO
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'ddata.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'print.h'
      include  'sdata.h'
      include  'tdatb.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   fdyn
      integer   j, n, nneq2,isw, nod, id(*)
      real*8    ub,vnorm,anorm, f(nneq,*),u(*),urate(nneq,*),du(*)

      save

c     Update solution vectors to begin a step

      nneq2 = nneq + nneq

      if(isw.eq.1) then

c       Compute and output norms

        if(pfr) then
        vnorm = 0.0d0
        do n = 1,nneq
          vnorm = vnorm + urate(n,1)*urate(n,1)
        end do
        vnorm = sqrt(vnorm)
        anorm = 0.0d0
        if(noi.eq.1 .or. noi.eq.3 .or.
     &     noi.eq.4 .or. noi.eq.5 .or.
     &     noi.eq.8               ) then
          do n = 1,nneq
            anorm = anorm + urate(n,2)*urate(n,2)
          end do
          anorm = sqrt(anorm)
        endif
        if(prnt.and.ior.lt.0) write(*,2000) vnorm,anorm
        write(iow,2000) vnorm,anorm
        endif

c       Update rate terms

c       Generalized Newmark updates

        if(noi.eq.1) then

          call dyna01(u,urate,nneq,1)

c       SSpj updates

        elseif(noi.eq.2) then

          call dyna02(u,urate,nneq,1)

c       USER:  User generated routine

        elseif(noi.eq.-1) then

          call udynam(u(nneq2+1),u,urate,nneq,1)

        endif

c     UPDATES WITHIN A TIME STEP

      elseif(isw.eq.2) then

c       STEP 1: Update displacement and its increments within step.

        nod = 0
        do n = 1,nneq
          nod = nod + 1
          j = id(n)
          if (j.gt.0) then

c           For active dof compute values from solution
c           where 'du(j)' is increment of 'u' for active dof 'j'.

            u(n)       = u(n)      + cc1*du(j)
            u(n+nneq)  = u(n+nneq) + cc2*du(j)
            u(n+nneq2) =                 du(j)

          else

c           Compute values from forced inputs for fixed dof

            ub         = f(n,1)

c           Incremental boundary solution

            if(cc3.ne.0.0d0) then
              u(n+nneq2) = (ub - u(n))*cc3
            else
              u(n+nneq2) = 0.0d0
              if(ub-u(n).ne.0.0d0) then
                write(iow,*)' WARNING - infinite acceleration'
              endif
            endif
            u(n)       = ub
            u(n+nneq)  = u(n+nneq) + cc2*u(n+nneq2)
          endif
        end do

c       STEP 2: For Dynamics, update rate terms [urate-vectors]

        if(fdyn) then

c         Generalized Newmark updates

          if(noi.eq.1) then

            call dyna01(u(nneq2+1),urate,nneq,2)

c         SSpj Updates

          elseif(noi.eq.2) then

            call dyna02(u(nneq2+1),urate,nneq,2)

c         USER:  User generated routine

          elseif(noi.eq.-1) then

            call udynam(u(nneq2+1),u,urate,nneq,2)

          endif
        endif

c     BACKUP SOLUTION VECTORS: Reinitiate a transient step

      elseif(isw.eq.3) then

        if(fdyn)then

c         Generalize Newmark updates

          if(noi.eq.1) then

            call dyna01(u(nneq+1),urate,nneq,3)

c         SSpj updates

          elseif(noi.eq.2) then

            call dyna02(u(nneq+1),urate,nneq,3)

c         USER:  User generated routine

          elseif(noi.eq.-1) then

            call udynam(u(nneq2+1),u,urate,nneq,3)

          endif

        endif

c       Back up solution vectors: u(*)

        vnorm = 1.d0/cc2
        do n = 1,nneq
          ub = u(n+nneq)*vnorm
          j  = id(n)

c         Compute values from current solution

          if (j.gt.0) then
            u(n)       = u(n) - ub
            u(n+nneq)  = 0.
            u(n+nneq2) = 0.

c           Compute values from forced inputs

          else
            du(neq-j)  = 0.
            u(n+nneq2) = 0.
            u(n+nneq)  = 0.
            f(n,1)     = f(n,2)
            u(n)       = f(n,2)
          endif
        end do
      endif

c     Formats

2000  format('   N o r m s   f o r   D y n a m i c s'/
     1   10x,'Velocity:',e13.5,' Acceleration:',e13.5)

      end

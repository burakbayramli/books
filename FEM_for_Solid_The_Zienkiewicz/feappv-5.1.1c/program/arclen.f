!$Id:$
      subroutine arclen(du,u1,u2,f,al,au,ad,jp,id,ndf,numnp,neq,time)

!     * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:     Perform arc length solution to find limit states

!      Programmed - 08/19/86: Peter Wriggers
!      Modified   - 10/25/94: Sanjay Govindjee
!                   12/20/95: R.L. Taylor

!      Inputs:
!         du(neq) - Solution increment from last solution
!         u1(*)   - Displacement before first iteration (N/R)
!         u2(*)   - Displacement for load level 1.0 (N/R)
!         f(*)    - Load vector (N/R)
!         al(*)   - Tangent matrix: Lower part
!         au(*)   - Tangent matrix: Upper part
!         ad(*)   - Tangent matrix: Diagonals
!         jp(*)   - Pointer to rows/columns of 'al and au'
!         id(*)   - Equation numbers for each degree-of-freedom
!         ndf     - Number dof/node
!         numnp   - Number of nodes
!         neq     - Number of active equations
!         time    - Time

!      Outputs:

!      Scratch:
!         u1(neq) - Working vector space
!         u2(neq) - Working vector space

!      Solution options:
!        kflag    = 0: Mod.  N/R
!                 = 1: Updated normal plane iteration Mod.  N/R
!                 = 2: Orig. N/R
!                 = 3: Updated normal plane iteration Orig. N/R
!                 = 4: Displacement control Mod.  N/R
!                 = 5: Displacement control Orig. N/R
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'arclei.h'
      include  'arcler.h'
      include  'eqsym.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'pointer.h'
      include  'print.h'
      include  'prlod.h'
      include  'tdata.h'

      integer       :: i,j,jj,ndf,numnp,neq,nneg
      real (kind=8) :: time, cs,ds,dcs,damp,rr,rat
      real (kind=8) :: alfa,alfa1,alfold, dtold,energy,tol

      integer       :: jp(*),id(ndf,*)
      real (kind=8) :: du(*),u1(*),u2(*),f(ndf,*),al(*),au(*),ad(*)

      real (kind=8) :: dot

      save

      data      tol /1.d-08/, dtold /0.0d0/

!     Exit on near zero proportional loading

      if( abs(prop).lt.1.0d-10) return

!     Initialization at beginning of load step

      if(time.ne.timold) then

        ite = 0
        timold = time

!       Calculate arc length at beginning of time step

        ds = sqrt(dot(du,du,neq))

!       Check for limit points

        do j = 1,ndf
          do i = 1,numnp
            jj = id(j,i)
            if (jj.gt.0) u1(jj) = f(j,i)
          end do ! i
        end do ! j
        rr = dot(du,u1,neq)

!       Set arc length constant  (only in very first time step)
!       sign of r positive
!       Safe energy value at beginning of loading

!       First load/time step only

        if (nastep.le.1) then
          r     = 1.0d0
          ds0   = ds
          c0    = rr
          dtold = dt
        endif

!       Rescale arclength for changing time steps

        if(dt.ne.dtold.and.dtold.ne.0.0d0) then
          ds0   = ds0*abs(dt/dtold)
          dtold = dt
        end if

!       Current stiffness parameter of current load increment

        cs = c0/rr
        rr = sign(1.d00,rr)

!       Check if stiffness parameter passes infinity
!       or limit point (then change of sign!)

        if (nastep.gt.2) then
          if (cs02.gt.0.0d0.and.cs.lt.0.0d0) then
            dcs = cs02 - cs01
!           if (dcs.lt.0.0d0) r = rr
            if (dcs.lt.0.0d0) r = -sign(1.d0,r)
          elseif (cs02.lt.0.0d0.and.cs.gt.0.0d0) then
            dcs = cs02 - cs01
!           if (dcs.gt.0.0d0) r = rr
            if (dcs.gt.0.0d0) r = -sign(1.d0,r)
          endif
        endif

!       Save current stiffness values

        if (nastep.eq.1) then
          cs01 = cs
        else
          cs01 = cs02
        endif
        cs02 = cs

!       Constant arc length method

        if (kflag.le.3.or.kflag.eq.6) then

!         Calculate reducing factor

          alfa0 = ds0/ds

!         Change direction if passing limit points

          alfa0 = alfa0*r

!         Actual load level

          rlnew = rlnew + alfa0 - dt

!         Save displacement vector for this load step
!         for modified and original Newton/Raphson

          do i = 1,neq
            du(i) = du(i)*alfa0
            u1(i) = du(i)
          end do ! i

          if(prnt) then
            write (iow,2001) time,ds,alfa0
            write (iow,2002) rlnew*prop
            write (iow,2005) cs,cs01,cs02
            if(ior.lt.0.and.pfr) then
              write (*,2001) time,ds,alfa0
              write (*,2002) rlnew*prop
              write (*,2005) cs,cs01,cs02
            endif
          endif

!       Displacement control (beginning of time step)
!       calculate displacements for load level 1.0

        elseif (kflag.eq.4.or.kflag.eq.5) then

          do j = 1,ndf
            do i = 1,numnp
              jj = id(j,i)
              if (jj.gt.0) u1(jj) = f(j,i)*prop
            end do ! i
          end do ! j
          call dasol(al,au,ad,u1,jp,neqs,neq,energy)

!         Load factor for displacement control

          alfa = (alfa0 - du(ndis))/u1(ndis)

!         Update displacements

          do i = 1,neq
            du(i) = du(i) + alfa*u1(i)
          end do ! i

!         Update load level

          rlnew = rlnew + alfa
          if(prnt) then
            write (iow,2006) alfa*prop,rlnew*prop
            write (iow,2005) cs,cs01,cs02
            if(ior.lt.0) then
              if(pfr) then
                write (*,2006) alfa*prop,rlnew*prop
                write (*,2005) cs,cs01,cs02
              else
                write (*,2002) rlnew*prop
              endif
            endif
          else
            write (iow,2002) rlnew*prop
          endif
        endif

!     Iteration starts

      else
        ite = ite + 1

!       Rescale arclength for changing time steps

        if(dt.ne.dtold.and.dtold.ne.0.0d0) then
          ds0   = ds0*abs(dt/dtold)
          dtold = dt
        end if

        if(kflag.gt.1.and.kflag.ne.4) then

!         Calculate Newton displacement for load level 1.0

          do j = 1,ndf
            do i = 1,numnp
              jj = id(j,i)
              if (jj.gt.0) u2(jj) = f(j,i)
            end do ! i
          end do ! j
          call dasol(al,au,ad,u2,jp,neqs,neq,energy)
        endif

!       Update 1. Modified and original Newton Raphson
!       Calculate reducing factor by iteration on normal plane
!         (unscaled displ. vector)

        if (kflag.eq.0) then
          alfa = dot(du,u1,neq)
          alfa = - alfa*alfa0/(ds0*ds0)

!       Iteration on updated normal plane

        elseif (kflag.eq.1) then
          if (ite.eq.1) then
            do i = 1,neq
              u2(i) = u1(i)
            end do ! i
          endif
          alfa  = dot(du,u2,neq)
          alfa1 = dot(u1,u2,neq)
          alfa  = alfa/alfa1

!       Single displacement control

        elseif (kflag.eq.4) then
          alfa = - du(ndis)/u1(ndis)

!       Displacement control

        elseif (kflag.eq.5) then
          alfa = - du(ndis)/u2(ndis)

!       Arc length (unscaled displacement vector)

        else
          alfa  = dot(du,u1,neq)
          alfa1 = dot(u1,u2,neq)
          if(abs(alfa1).gt.tol*abs(alfa)) then
            alfa = - alfa/alfa1    ! previously, only possibility
          else
            alfa = - alfa/(alfa1 + sign(tol,alfa1))
          endif
        endif

!       Numerical damping if sign of alfa is varying

        damp = 1.d0
        if (ndamp.eq.0) then
          if (ite.eq.1) alfold = alfa
          if(abs(alfold).gt.tol) then
            rat = alfa/alfold
          else
            rat = alfa/(alfold + tol)
          endif
          if (-1.0d0.lt.rat.and.rat.lt.0.0d0) damp = 0.5d0
          alfa   = alfa*damp
          alfold = alfa
        endif

!       Update new load level

        rlnew = rlnew + alfa
        if(prnt) then
          write (iow,2003) ite,alfa*prop,rlnew*prop
          write (iow,2004) damp
          if(ior.lt.0) then
            if(pfr) then
              write (*,2003) ite,alfa*prop,rlnew*prop
              write (*,2004) damp
            else
              write (*,2002) rlnew*prop
            endif
          endif
        else
          write (iow,2002) rlnew*prop
        endif

!       Update displacements in iteration step

        if(kflag.le.1.or.kflag.eq.4) then
          do i = 1,neq
            du(i) = du(i)*damp + u1(i)*alfa
          end do ! i

!         Updated normal plane iteration

          if (kflag.eq.1) then
            do i = 1,neq
              u2(i) = u1(i) + du(i)
            end do ! i
          endif

!       Update displacements in iteration step

        else

          do i = 1,neq
            du(i) = u2(i)*alfa + du(i)*damp
          end do ! i

!         Only for updated normal plane iteration

          if (kflag.eq.3) then
            do i = 1,neq
              u1(i) = du(i) + u1(i)
            end do ! i

!         New tangent plane

          elseif (kflag.eq.6) then
            do i = 1,neq
              u1(i) =  du(i)
            end do ! i
          endif
        endif

!       Calculate determinant

        det  = 0.0d0
        nneg = 0
        do i = 1,neq
          if(abs(ad(i)).gt.1.d-30) then
            if(ad(i).lt.0.0d0) then
              nneg = nneg + 1
            endif
            det = det + log(abs(ad(i)))
          endif
        end do ! i
        if(time.le.dt) det0 = det
        det = max(-30.d0,min(30.d0,det0 - det))
        det = exp(det)
        if(prnt) then
          write(iow,2007) det,nneg
          if(ior.lt.0.and.pfr) then
            write(*,2007) det,nneg
          endif
        endif

      endif

!     Formats

 2001 format(/3x,'A r c   L e n g t h   M e t h o d'//3x,'Time =',g9.2,
     &           ' Arc Length = ',g12.5,' Red. factor = ',g12.5)

 2002 format( 3x,'Load level = ',g12.5)

 2003 format(/3x,'Iteration:',i5,' Reduct.factor = ',g12.5,
     &           ' Load level = ',g12.5)

 2004 format( 3x,'Damp. fact = ',g12.5)

 2005 format( 3x,'Cs-param.  = ',g12.5,/,3x,'Cs01       = ',g12.5,/,
     &        3x,'Cs02       = ',g12.5)

 2006 format( 3x,'Displacement Control Parameters',/,
     &        3x,'Reduction factor  = ',g12.5,' Load level = ',g12.5)

 2007 format( 3x,'Determinant Ratio = ',g12.5,' Neg. diagonals =',i3)

      end subroutine arclen

!$Id:$
      subroutine ptimpl

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Store time history plot information for converged step
!      Inputs:
!         none

!      Outputs:
!         none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'aceang.h'
      include   'arcler.h'
      include   'cdata.h'
      include   'eltran.h'
      include   'endata.h'
      include   'fdata.h'
      include   'gltran.h'
      include   'hdatam.h'
      include   'iofile.h'
      include   'print.h'
      include   'prlod.h'
      include   'ptdat1.h'
      include   'ptdat2.h'
      include   'ptdat3.h'
      include   'ptdat4.h'
      include   'ptdat5.h'
      include   'ptdat6.h'
      include   'ptdat7.h'
      include   'ptdat8.h'
      include   'ptdat9.h'
      include   'sdata.h'
      include   'tdata.h'
      include   'tdato.h'

      include   'pointer.h'
      include   'comblk.h'

      integer       :: i
      real (kind=8) :: dtsav

      save

!     Store time history plot information for converged step

      if(max(naplts,ncplts,ndplts,neplts,nlplts,nrplts,
     &       nsplts,ntplts,nuplts,nvplts).gt.0 ) then

        ntstep = ntstep + 1

!       Check for active output increment
        if(mod(ntstep-1,ntincr).eq.0) then

!         Set history update flag to false (no updates)
          hflgu  = .false.
          h3flgu = .false.

!         Set transient parameters for current
          if(fl(9)) call dsetci
          do i = 1,3
            ctan(i) = gtan(i)
          end do

          if(max(nsplts,nrplts,nuplts).gt.0) then
            if(.not.rfl) then
              dtsav = dt
              dt    = dtold
              call pzero(hr(np(26)),nneq)
              pltmfl = .true.
              call formfe(np(40),np(26),np(26),np(26),
     &                   .false.,.true.,.false.,6,1,numel,1)
              pltmfl = .false.
              rfl = .true.
              dt  = dtsav
            end if
          end if

!         Set file name for displacements
          if(ndplts.gt.0) then
            call pltmv(dpl,idpl,hr(np(40)),ndplts,1.d0)
            call ptmplt('dis', ttim, dpl,ndplts, ntstep)
          end if

!         Set file name for velocities
          if(nvplts.gt.0) then
            call pltmv(dpl,ivpl,hr(np(42)),nvplts,1.d0)
            call ptmplt('vel', ttim, dpl,nvplts, ntstep)
          end if

!         Set file name for accelerations
          if(naplts.gt.0) then
            call pltmv(dpl,iapl,hr(np(42)+nneq),naplts,1.d0)
            call ptmplt('acc', ttim, dpl,naplts, ntstep)
          end if

!         Set file name for reactions
          if(nrplts.gt.0) then
            call pltmv(rpl,irpl,hr(np(26)),nrplts,-1.d0)
            call ptmplt('rea', ttim, rpl,nrplts, ntstep)
          end if

!         Set file name for stresses
          if(nsplts.gt.0) then
            call ptmplt('str', ttim, spl,nsplts,ntstep)
          end if

!         Set file name for user stresses
          if(nuplts.gt.0) then
            call ptmplt('use', ttim, upl,nuplts, ntstep)
          end if

!         Set file name for arclength
          if(nlplts.gt.0) then
            call pltmv(lpl,ilpl,hr(np(40)),nlplts,1.d0)
            call ptmplt('arc', rlnew*prop, lpl,nlplts, ntstep)
          end if

        end if  ! End of increment check

      end if

      end subroutine ptimpl

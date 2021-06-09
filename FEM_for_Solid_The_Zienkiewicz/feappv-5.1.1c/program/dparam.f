!$Id:$
      subroutine dparam(ct,lct)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set integration parameters for Time-stepping Algorithms

!      Inputs:
!        ct(3)   := Algorithmic parameters
!        lct     := 'off ' (noi=0)  Standard Static
!                   'newm' (noi=1)  Classical Newmark
!                   'GNpj' (noi=2)  Generalized Newmark
!                   'SSpj' (noi=3)  Weighted residual FE

!                   'user' (noi=-1) User time integration routine
!                   'init'          Initialize the nrt variable

!      Outputs:
!        ct(3)   := (possibly) redefined algorithmic parameters
!        theta(i):= Set identical to ct(i), i=1,2,3.
!        noi     := Number of the specific integrator (see above)
!        nrm     := Acceleration vector (pointer) in urate(nneq,*)
!        nrk     := Solution vector (pointer)
!        nrc     := Velocity vector (pointer)
!        nrt     := Maximun number of vectors in urate(nnneq,*)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'fdata.h'
      include  'gltran.h'
      include  'iofile.h'
      include  'ddata.h'
      include  'dyndat.h'

      character (len=4) ::lct

      logical       :: pcomp
      integer       :: i

      integer       :: ntot(3)
      real (kind=8) :: ct(3)

      save

!     Set maximum number of vectors for each integration type
      data ntot/   2 ,  2 ,   4 /
!                NEWM, GNpj, SSpj

!     Initialize the nrt variable

      if(pcomp(lct,'init',4)) then

        nrt = max(ntot(1),ntot(2),ntot(3))
        i   = 0
        call uparam(ct,nrk,nrc,nrm,i,0)
        nrt = max(nrt,i) + 2

      else

        nrk = 0
        nrc = 0
        nrm = 1

        dynflg = .true.

!       Classical Newmark-beta method
        if(pcomp(lct,'    ',4).or.pcomp(lct,'newm',4)) then

!         Location for stiffness, damping and mass
          noi = 1
          nrk = 0
          nrc = 1
          nrm = 2

!         Newmark ct(1) = beta  ;  ct(2) = gamma
          if(ct(1).eq.0.0d0) ct(1) = 0.25d0
          if(ct(2).eq.0.0d0) ct(2) = 0.5d0

          write(iow,2011) ct(1),ct(2)
          if(ior.lt.0) write(*,2011) ct(1),ct(2)
          ct(3) = 1.0d0

!       GNpj method
        elseif(pcomp(lct,'gn11',4).or.pcomp(lct,'gn22',4)) then

!         Location for stiffness, damping and mass

          noi = 2
          nrk = 0
          nrc = 1

          if(pcomp(lct,'gn11',4)) then

            if(ct(1).eq.0.0d0) ct(1) = 1.0d0
            ct(2) = 0.0d0
            write(iow,2012) ct(1)
            if(ior.lt.0) write(*,2012) ct(1)
            nrm = 1

          elseif(pcomp(lct,'gn22',4)) then

            if(ct(1).eq.0.0d0) ct(1) = 0.5d0
            if(ct(2).eq.0.0d0) ct(2) = 0.5d0
            write(iow,2013) ct(1),ct(2)
            if(ior.lt.0) write(*,2013) ct(1),ct(2)
            nrm = 2

          endif
          ct(3) = 1.0d0

!       SSpj Algorithm
        elseif(pcomp(lct,'ss11',4).or.pcomp(lct,'ss22',4)) then

          noi = 3
          nrk = 0
          nrc = 1

          if(pcomp(lct,'ss11',4)) then

            if(ct(1).eq.0.0d0) ct(1) = 1.0d0
            ct(2) = 0.0d0
            write(iow,2021) ct(1)
            if(ior.lt.0) write(*,2021) ct(1)
            nrm = 1
            nrk = 3

          elseif(pcomp(lct,'ss22',4)) then

            if(ct(1).eq.0.0d0) ct(1) = 0.5d0
            if(ct(2).eq.0.0d0) ct(2) = 0.5d0
            write(iow,2022) ct(1),ct(2)
            if(ior.lt.0) write(*,2022) ct(1),ct(2)
            nrm = 2
            nrk = 3
            nrc = 4

          endif
          ct(3) = 1.0d0

!       User time integration routine
        elseif(pcomp(lct,'user',4)) then

          noi    = -1
          call uparam(ct,nrk,nrc,nrm,i,1)
          dynflg = i.gt.0
          fl(9)  = dynflg

!       Standard static algorithm (noi = 0)
        elseif(pcomp(lct,'stat',4).or.pcomp(lct,'off',3)) then

          noi     = 0
          nrk     = 0
          nrc     = 0
          nrm     = 0

          ct(1)   = 0.0d0
          ct(2)   = 0.0d0
          ct(3)   = 1.0d0

          dynflg  = .false.
          fl(9)   = .false.

          gtan(1) = 1.0d0
          gtan(2) = 0.0d0
          gtan(3) = 0.0d0

          write(iow,2000)
          if(ior.lt.0) write(*,2000)

!       ERROR - write message
        else

          if(ior.lt.0) then
            write(*,3000) lct
          else
            write(iow,3000) lct
            call plstop(.true.)
          endif
        endif

!       Transfer values to 'theta' in common /ddata/
        do i = 1,3
          theta(i) = ct(i)
        end do
        numint =  noi

      endif

2000  format(/' Standard Static Algorithm'/)
2011  format(/' Newmark Parameters:',
     &        ' Beta = ',f9.4,' Gamma = ',f9.4/1x)
2012  format(/' GN11 Parameter:',' Beta_1 = ',f9.4/1x)
2013  format(/' GN22 Parameters:',
     &        ' Beta_1 = ',f9.4,' Beta_2 = ',f9.4/1x)

2021  format(/' SS11 Parameter:',
     &        ' Theta_1 = ',f9.4/1x)
2022  format(/' SS22 Parameters:',
     &        ' Theta_1 = ',f9.4,' Theta_2 = ',f9.4/1x)

3000  format(/' *ERROR* ',a,' Not an implemented method'/1x)

      end subroutine dparam

c$Id:$
      subroutine dparam(ct,lct)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set integration parameters for Time-stepping Algorithms

c      Inputs:
c        ct(3)   := Algorithmic parameters
c        lct     := 'off ' (noi=0)  Standard Static
c                   'newm' (noi=1)  Classical Newmark
c                   'GNpj' (noi=1)  Generalized Newmark
c                   'SSpj' (noi=2)  Weighted residual FE

c                   'user' (noi=-1) User time integration routine
c                   'init'          Initialize the nrt variable

c      Outputs:
c        ct(3)   := (possibly) redefined algorithmic parameters
c        theta(i):= Set identical to ct(i), i=1,2,3.
c        noi     := Number of the specific integrator (see above)
c        nrm     := Acceleration vector (pointer) in urate(nneq,*)
c        nrk     := Solution vector (pointer)
c        nrc     := Velocity vector (pointer)
c        nrt     := Maximun number of vectors in urate(nnneq,*)
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'fdata.h'
      include  'gltran.h'
      include  'iofile.h'
      include  'ddata.h'
      include  'dyndat.h'

      logical   pcomp
      character lct*4
      integer   i

      integer   ntot(2)
      real*8    ct(3)

      save

c     Set maximum number of vectors for each integration type
      data ntot/   4 ,   4 /
c                GNpj, SSpj

c     Initialize the nrt variable

      if(pcomp(lct,'init',4)) then

        nrt = max(ntot(1),ntot(2))
        i   = 0
        call uparam(ct,nrk,nrc,nrm,i,0)
        nrt = max(nrt,i) + 2

      else

        nrk = 0
        nrc = 0
        nrm = 1

        dynflg = .true.

c       GNpj or Classical Newmark-beta method

        if(pcomp(lct,'    ',4).or.pcomp(lct,'newm',4).or.
     &     pcomp(lct,'gn11',4).or.pcomp(lct,'gn22',4)) then

c         Location for stiffness, damping and mass

          noi = 1
          nrk = 0
          nrc = 1

c         Newmark ct(1) = beta  ;  ct(2) = gamma

          if(pcomp(lct,'    ',4).or.pcomp(lct,'newm',4)) then

            if(ct(1).eq.0.0d0) ct(1) = 0.25d0
            if(ct(2).eq.0.0d0) ct(2) = 0.5d0

            write(iow,2011) ct(1),ct(2)
            if(ior.lt.0) write(*,2011) ct(1),ct(2)
            ct(1) = 2.d0*ct(1)
            nrm   = 2

          elseif(pcomp(lct,'gn11',4)) then

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

c       SSpj Algorithm

        elseif(pcomp(lct,'ss11',4).or.pcomp(lct,'ss22',4)) then

          noi = 2
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

c       User time integration routine

        elseif(pcomp(lct,'user',4)) then

          noi    = -1
          call uparam(ct,nrk,nrc,nrm,i,1)
          dynflg = i.gt.0
          fl(9)  = dynflg

c       Standard static algorithm (noi = 0)

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

c       ERROR - write message

        else

          if(ior.lt.0) then
            write(*,3000) lct
          else
            write(iow,3000) lct
            call plstop()
          endif
        endif

c       Transfer values to 'theta' in common /ddata/

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

      end

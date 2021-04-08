!$Id:$
      subroutine umacr1(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Compute mode shapes from NURBS values

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'evdata.h'
      include  'idptr.h'
      include  'iofile.h'
      include  'umac1.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   pcomp
      character lct*15
      integer   nfreq
      real*8    ctl(3)

      save

!     Set command word

      if(pcomp(uct,'mac1',4)) then      ! Usual    form
        uct = 'emod'                    ! Specify 'EMOD'e
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

!       Check if eigenmodes exist

        if(mf.gt.0) then

          nfreq = max(1,min(mf,nint(ctl(1))))
          call uemode(nfreq,hr(np(25)),mr(np(31)) ,mr(np(32)),
     &                mr(np(33)),hr(np(41)),hr(np(43)),hr(np(263)),
     &                hr(np(44)),hr(np(264)),
     &                hr(np(39)),hr(np(36)),hr(np(35)),
     &                hr(np(76)),hr(np(77)))

        else
          write(*,*) ' *ERROR* Compute eigenpairs first'
        endif

      endif

      end

      subroutine uemode(nfreq,d,id, ie,ix, ul, x,wt, xl,wl, tl, s,p,
     &                  eval,evec)

      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'eldata.h'
      include   'evdata.h'
      include   'iofile.h'
      include   'sdata.h'

      integer    id(ndf,*), ie(nie,*), ix(nen1,*)
      real*8     d(ndd,*), x(ndm,*), wt(*), xl(ndm,*), wl(*), tl(*)
      real*8     s(*), p(*)
      real*8     ul(ndf,*), eval(*), evec(neq,*)

      integer    nfreq, i,j,n, ii

!     Loop over elements

      write(iow,2000) nfreq,eval(nfreq)

      do n = 1,numel

        ma    = ix(nen1,n)
        iel   = ie(nie-1,ma)
        eltyp = ix(nen+7,n)
        nel   = 0
        do i = 1,nen
          if(ix(i,n).gt.0) then
            ii  = ix(i,n)
            nel = i
            do j = 1,ndm
              xl(j,i) = x(j,ix(i,n))
            end do ! j
            if(eltyp.ne.0) wl(i) = wt(ix(i,n))

!           Insert eigenvector into ul
            do j = 1,ndf
              if(id(j,ii).gt.0) then
                ul(j,i) = evec(id(j,ii),nfreq)
              else
                ul(j,i) = 0.0d0
              endif
            end do ! j

          else
            do j = 1,ndm
              xl(j,i) = 0.0d0
            end do ! j
            if(eltyp.ne.0) wl(i) = 0.0d0
            do j = 1,ndf
              xl(j,i) = 0.0d0
            end do ! j
          endif

        end do ! i

        call elmlib(d(1,ma),ul,xl,ix(1,n),tl,s,p,
     &              ndf,ndm,nst,iel, 44)

      end do ! n

!     Formats

2000  format(' Frequency Number =',i4,' Eigenvalue =',1p,1e15.7)

      end

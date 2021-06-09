!$Id:$
      subroutine pform(ul,xl,tl,ld,p,s,ie,d,id,x,ix,f,t,jp,
     &                 u,ud,b,a,al,ndd,nie,ndf,ndm,nen1,nst,
     &                 afl,bfl,dfl,isw,nn1,nn2,nn3)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/05/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute element arrays and assemble global arrays

!      Inputs:
!         ie(nie,*)   - Element information for material set
!         d(ndd,*)    - Material set parameters
!         id(ndf,*)   - Equation numbers for each active dof
!         x(ndm,*)    - Nodal coordinates of mesh
!         ix(nen1,*)  - Element nodal connections of mesh
!         f(ndf,*,2)  - Nodal force and displacement values
!         t(*)        - Nodal temperature values
!         jp(*)       - Pointer array for row/columns of tangent
!         u(*)        - Nodal solution values
!         ud(*)       - Nodal rate values
!         ndd         - Dimension for d array
!         nie         - Dimension for ie array
!         ndf         - Number dof/node
!         ndm         - Spatial dimension of mesh
!         nen1        - Dimension for ix array
!         nst         - Dimension for element array
!         afl         - Flag, assemble coefficient array if true
!         bfl         - Flag, assemble vector if true
!         dfl         - Flag, assemble reactions if true
!         isw         - Switch to control quantity computed
!         nn1         - First element number to process
!         nn2         - Last element number to process
!         nn3         - Increment to nn1

!      Local element arrays:
!         ul(*)       - Element solution and rate values
!         xl(*)       - Element nodal coordinates
!         tl(*)       - Element nodal temperatures
!         ld(*)       - Element local/global equation numbers
!         p(nst,*)    - Element vector
!         s(nst,*)    - Element array

!      Outputs:
!         b(*)        - Global vector
!         a(*)        - Global matrix, diagonal and upper part
!         al(*)       - Global matrix, lower part
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'arcler.h'
      include   'cdata.h'
      include   'ddata.h'
      include   'debugs.h'
      include   'elcount.h'
      include   'eldata.h'
      include   'elpers.h'
      include   'iofile.h'
      include   'hdata.h'
      include   'hdatam.h'
      include   'oelmt.h'
      include   'pointer.h'
      include   'prld1.h'
      include   'prlod.h'
      include   'setups.h'
      include   'comblk.h'

      logical       :: afl,bfl,dfl,efl
      logical       :: rvfl, svfl, setval, palloc
      integer       :: isw, jsw, ksw
      integer       :: i, n,nn1, nn2, nn3, nst, nneq
      integer       :: ndf, ndm, ndd, nie, nen1
      integer       :: ld(*), ie(nie,*), id(ndf,*), ix(nen1,*)
      integer       :: jp(*)
      real (kind=8) :: xl(ndm,*), d(ndd,*), ul(ndf,nen,*)
      real (kind=8) :: p(nst,*), s(nst,*)
      real (kind=8) :: x(ndm,*) ,f(ndf,numnp),u(ndf,*),ud(*),t(*),tl(*)
      real (kind=8) :: prope, b(*), a(*), al(*)

      save

      if(debug) then
        call udebug(' pform',isw)
      endif

!     Initialize data
      if(isw.eq.3 .or. isw.eq.6) then
        v_avg  = 0.0d0
        v_rho  = 0.0d0
        v_c    = 0.0d0
        sig_33 = 0.0d0
      endif

      nproc  = 0
      svfl  = ntasks.gt.1 .and. rank.eq.0
      rvfl  = .false.
      if(isw.eq.14) then
        nsend = 0
      endif

!     Set element proportional loading value
      prope = (theta(3)*(prop - propo) + propo)
      if(isw.ne.23) then        ! Element arclength case
        prope = prope*rlnew
      endif

!     Set nh1, nh2, nh3 pointers for local history variables
      nh1 = np(50)
      nh2 = np(51)
      nh3 = np(52)

!     Set program and user material count parameters
      do i = 1,10
        nomats(1,i) = 0
        nomats(2,i) = 0
        unmats(1,i) = 0
        unmats(2,i) = 0
      end do ! i

!     Initialize RVELM array -- stores list of elements to send-receive
      if(isw.eq.14 .and. svfl) then
        if(np(259).eq.0) then
          setval = palloc(259,'RVELM',numel,1)
        endif
        do n = 1,numel
          mr(np(259)+n-1) = 0
        end do ! n
      endif

!     Set flags and parameters for solution
      iel = 0
      if(isw.eq.3 .and. bfl) then
        efl = .true.
      elseif(isw.eq.6 .and. .not.dfl) then
        efl = .true.
      else
        efl = .false.
      endif

!     Other cases
      if(isw.eq.19) then
        if(bfl) efl = .true.
        jsw = 5
        ksw = 5
      else
        jsw = isw
        ksw = 3
      endif

      nneq   = numnp*ndf
      nrkn   = nrk*nneq - nneq
      nrcn   = nrc*nneq - nneq
      nrmn   = nrm*nneq - nneq
      nrvn   = nrt*nneq - nneq - nneq

!     Loop over active elements: Compute any local elements and the
!     deformation gradient for send/receive microscale points.
      call pforma(ul,xl,tl,ld,p,s,ie,d,id,x,ix,f,t,jp,
     &            u,ud,b,a,al,ndd,nie,ndf,ndm,nen1,nst,nneq,
     &            prope,afl,bfl,dfl,efl,
     &            rvfl,svfl,jsw,ksw,nn1,nn2,nn3)

!     Parallel send/receives for RVE
      if(svfl .and. nsend.gt.0 .and. .not.pltmfl) then
        if(isw.eq.14) then
          setval = palloc(260,'FRVEL', dsend*nsend, 2)
          setval = palloc(261,'SRVEL', drecv*nsend, 2)
        elseif(isw.eq.3 .or. isw.eq.6) then
          call rvesr(hr(np(260)),hr(np(261)),mr(np(270)), isw)

!         Form remaining micro-scale contributions
          call pforma(ul,xl,tl,ld,p,s,ie,d,id,x,ix,f,t,jp,
     &                u,ud,b,a,al,ndd,nie,ndf,ndm,nen1,nst,nneq,
     &                prope,afl,bfl,dfl,efl,
     &                .true.,.false.,jsw,ksw,nn1,nn2,nn3)
        endif
      endif

      end subroutine pform

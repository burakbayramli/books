!$Id:$
      subroutine pform(ul,xl,tl,ld,p,s,ie,d,eq,x,ix,f,t,jp,
     &                 u,ud,b,a,al,ndd,nie,ndf,ndm,nen1,nst,
     &                 afl,bfl,dfl,isw,nn1,nn2,nn3)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute element arrays and assemble global arrays

!      Inputs:
!         ie(nie,*)   - Assembly information for material set
!         d(ndd,*)    - Material set parameters
!         eq(ndf,*)   - Equation numbers for each active dof
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
!         afl         - Flag, assemble matrix array if true
!         bfl         - Flag, assemble vector if true
!         dfl         - Flag, assemble reactions if true
!         isw         - Switch to control quantity computed
!         nn1         - First element number to process
!         nn2         - Last element number to process
!         nn3         - Increment to nn1

!      Scratch:
!         ul(ndf,*)   - Element solution and rate values
!         xl(ndm,*)   - Element nodal coordinates
!         tl(*)       - Element nodal temperatures
!         ld(*)       - Element local/global equation numbers
!         p(nst,*)    - Element vector
!         s(nst,*)    - Element array

!      Outputs:
!         b(*)        - Global vector
!         a(*)        - Global matrix, diagonal and upper part
!         al(*)       - Global matrix, lower part
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'crotas.h'
      include  'ddata.h'
      include  'elcount.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'eqsym.h'
      include  'erotas.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'hdata.h'
      include  'hdatam.h'
      include  'mdata.h'
      include  'modreg.h'
      include  'oelmt.h'
      include  'p_int.h'
      include  'pointer.h'
      include  'prld1.h'
      include  'prlod.h'
      include  'prstrs.h'
      include  'ptdat1.h'
      include  'ptdat2.h'
      include  'ptdat8.h'
      include  'rdata.h'
      include  'rdat0.h'
      include  'region.h'
      include  'tdata.h'
      include  'tdatb.h'
      include  'comblk.h'

      logical       :: afl,bfl,dfl,efl, mdfl
      integer       :: isw, jsw, ksw
      integer       :: i, jj, n, nn1, nn2, nn3, nst, nl1, nneq, nov
      integer       :: numnp2, ndf, ndm, nrot, ndd, nie, nen1
      real (kind=8) :: un(20), dun(20), temp, prope

      integer       :: ld(*), ie(nie,*), eq(ndf,*), ix(nen1,*), jp(*)
      real (kind=8) :: xl(ndm,*), p(nst,*),s(nst,*), d(ndd,*),ul(nst,*)
      real (kind=8) :: x(ndm,*) ,f(ndf,numnp),u(ndf,*),ud(*),t(*),tl(*)
      real (kind=8) :: b(*), a(*), al(*)

      save

!     Initialize data
      if(isw.eq.3 .or. isw.eq.6) then
        v_avg  = 0.0d0
        v_rho  = 0.0d0
        v_c    = 0.0d0
        sig_33 = 0.0d0
      endif

!     Set element proportional loading value
      prope = theta(3)*(prop - propo) + propo

!     Recover nh1, nh2, nh3 pointers
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

!     Set up local arrays before calling element library
      iel = 0
      efl = .false.
      if(.not.dfl.and.isw.eq.6) efl = .true.
      if(bfl.and.isw.eq.3)      efl = .true.

      if(isw.eq.19) then
        if(bfl) efl = .true.
        jsw = 5
        ksw = 5
      else
        jsw = isw
        ksw = 3
      endif

!     Set stiffness, damping and mass pointers
      nl1    = ndf*nen + 1
      numnp2 = numnp + numnp
      nneq   = numnp*ndf
      nrkn   = nrk*nneq - nneq
      nrcn   = nrc*nneq - nneq
      nrmn   = nrm*nneq - nneq
      nrvn   = nrt*nneq - nneq - nneq

!     Loop over active elements
      do n = nn1,nn2,nn3

        n_el = n

!       Check for active regions
        if((nreg.lt.0 .and. ix(nen1-1,n).ge.0)
     &                .or. (abs(ix(nen1-1,n)).eq.nreg)) then

!        Set up local arrays
         nov = 0        ! Prevent accumualtion of projection weight
         do ma = 1, nummat

          if(ie(nie-2,ma).eq.ix(nen1,n)) then

!           Compute address and offset for history variables
            ht1 = np(49) + ix(nen+1,n) + ie(nie-3,ma)
            ht2 = np(49) + ix(nen+2,n) + ie(nie-3,ma)
            ht3 = np(49) + ix(nen+3,n) + ie(nie-4,ma)

!           If history variables exist move into nh1,nh2
            if(ie(nie,ma).gt.0) then
              do i = 0,ie(nie,ma)-1
                hr(nh1+i) = hr(ht1+i)
                hr(nh2+i) = hr(ht2+i)
              end do
            endif

!           If Element variables exist move into nh3
            if(ie(nie-5,ma).gt.0) then
              do i = 0,ie(nie-5,ma)-1
                hr(nh3+i) = hr(ht3+i)
              end do
            endif

            if(ie(nie-1,ma).ne.iel) mct = 0
            iel   = ie(nie-1,ma)
            rotyp = ie(nie-6,ma)

!           Set local arrays for element
            fp(1) = ndf*nen*(ma-1) + np(240)        ! iedof
            call plocal(ld,eq,mr(np(31)+nneq),ix(1,n),ie(1,ma),
     &                  mr(fp(1)),xl,ul,
     &                  tl,p(1,3),x,f,u,ud,t,un,dun, nrot, dfl, jsw)

!           Form element array - rotate parameters if necessary
            if(nrot.gt.0) then
              if(iel.gt.0) then
                call ptrans(ia(1,iel),hr(np(46)),ul,p,s,
     &                      nel,ndf,nst,1)
                if(ir(1,iel).ne.0) then
                  call ptrans(ir(1,iel),hr(np(46)),ul,p,s,
     &                        nel,ndf,nst,1)
                endif
              else
                call ptrans(ea(1,-iel),hr(np(46)),ul,p,s,
     &                      nel,ndf,nst,1)
                if(er(1,-iel).ne.0) then
                  call ptrans(er(1,-iel),hr(np(46)),ul,p,s,
     &                        nel,ndf,nst,1)
                endif
              endif
            endif
            if(jsw.eq.8) then
              erav = hr(np(207)+n-1)
            else
              erav = 0.0d0
            endif
            dm = prope
            call elmlib(d(1,ma),ul,xl,ix(1,n),tl,s,p,
     &                  ndf,ndm,nst,iel,jsw)

!           Store time history plot data from element
            if(jsw.eq.6) then

!             Standard element values
              do i = 1,nsplts
                if(ispl(1,i).eq.n) then
                  jj = max(ispl(2,i),1)
                  spl(i) = tt(jj)
                endif
              end do

!             Standard user element values
              do i = 1,nuplts
                if(iupl(1,i).eq.n) then
                  jj = max(iupl(2,i),1)
                  upl(i) = ut(jj)
                endif
              end do

            endif

!           Modify for rotated dof's
            if(nrot.gt.0) then
              if(iel.gt.0) then
                call ptrans(ia(1,iel),hr(np(46)),ul,p,s,
     &                      nel,ndf,nst,2)
                if(ir(1,iel).ne.0) then
                  call ptrans(ir(1,iel),hr(np(46)),ul,p,s,
     &                        nel,ndf,nst,2)
                endif
              else
                call ptrans(ea(1,-iel),hr(np(46)),ul,p,s,
     &                      nel,ndf,nst,2)
                if(er(1,-iel).ne.0) then
                  call ptrans(er(1,-iel),hr(np(46)),ul,p,s,
     &                        nel,ndf,nst,2)
                endif
              endif
            endif

!           Position update terms 'nt1,nt2' from 'nh1,nh2' to save
            if(hflgu .and. ie(nie,ma).gt.0) then
              do i = 0,ie(nie,ma)-1
                temp      = hr(ht1+i)
                hr(ht1+i) = hr(nh1+i)
                hr(nh1+i) = temp
                temp      = hr(ht2+i)
                hr(ht2+i) = hr(nh2+i)
                hr(nh2+i) = temp
              end do
            endif

!           Position update terms 'nt3' from 'nh3' to save
            if(h3flgu .and. ie(nie-5,ma).gt.0) then
              do i = 0,ie(nie-5,ma)-1
                hr(ht3+i) = hr(nh3+i)
              end do
            endif

!           Modify for non-zero displacement boundary conditions
            mdfl = .false.
            do i = 1,ndf
              if(dun(i).gt.1.0d-10*un(i)) then
                mdfl = .true.
                exit
              endif
            end do ! i

            if(efl.and.mdfl) then

!             Get current element tangent matrix
              if (.not.afl) then
                dm = prop
                call elmlib(d(1,ma),ul,xl,ix(1,n),tl,s,p,
     &                      ndf,ndm,nst,iel,ksw)
                if(nrot.gt.0) then
                  if(iel.gt.0) then
                    call ptrans(ia(1,iel),hr(np(46)),ul,p,s,
     &                          nel,ndf,nst,2)
                    if(ir(1,iel).ne.0) then
                      call ptrans(ir(1,iel),hr(np(46)),ul,p,s,
     &                            nel,ndf,nst,2)
                    endif
                  else
                    call ptrans(ea(1,-iel),hr(np(46)),ul,p,s,
     &                          nel,ndf,nst,2)
                    if(er(1,-iel).ne.0) then
                      call ptrans(er(1,-iel),hr(np(46)),ul,p,s,
     &                            nel,ndf,nst,2)
                    endif
                  endif
                endif
              end if

!             Modify for specified non-zero nodal displacements
              do i = 1,nst
                p(i,3) = p(i,3)*cc3
              end do
              call modify(p,s,p(1,3),nst,nst)
            end if

!           Add to total array
            pstyp = ie(1,ma)
            call passble(s,p,ld,ix(1,n), jp,a,al,b,
     &                   afl,bfl, nst,nov, jsw)
          end if

         end do ! ma

        end if ! regions

      end do ! n

      end subroutine pform

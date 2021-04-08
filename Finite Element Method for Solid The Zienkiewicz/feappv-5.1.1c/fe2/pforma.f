!$Id:$
      subroutine pforma(ul,xl,tl,ld,p,s,ie,d,id,x,ix,f,t,jp,
     &                  u,ud,b,a,al,ndd,nie,ndf,ndm,nen1,nst,nneq,
     &                  prope,afl,bfl,dfl,efl,
     &                  rvfl,svfl,jsw,ksw,nn1,nn2,nn3)

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
!         prope       - Proportional load value
!         afl         - Flag, assemble coefficient array if true
!         bfl         - Flag, assemble vector if true
!         dfl         - Flag, assemble reactions if true
!         efl         - Flag, element tangent may be necessary
!         rvfl        - Flag, parallel receive flag
!         svfl        - Flag, parallel send    flag
!         jsw         - Switch to control quantity computed
!         ksw         - Switch to control for tangent to be computed
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
      include   'compas.h'
      include   'counts.h'
      include   'ddata.h'
      include   'debugs.h'
      include   'elcount.h'
      include   'eldata.h'
      include   'elplot.h'
      include   'eltran.h'
      include   'erotas.h'
      include   'iofile.h'
      include   'hdata.h'
      include   'hdatam.h'
      include   'mdata.h'
      include   'pbody.h'
      include   'pdata3.h'
      include   'pointer.h'
      include   'prld1.h'
      include   'prlod.h'
      include   'prstrs.h'
      include   'ptdat1.h'
      include   'ptdat2.h'
      include   'ptdat8.h'
      include   'region.h'
      include   'setups.h'
      include   'strnum.h'
      include   'tdatb.h'
      include   'comblk.h'

      include   'p_int.h'

      logical       :: afl,bfl,dfl,efl
      logical       :: mdfl,rvfl,svfl
      integer       :: jsw, ksw
      integer       :: i, jj, n, nn1, nn2, nn3, nst, nov
      integer       :: ndf, ndm, nrot, ndd, nie, nen1, nneq
      real (kind=8) :: temp, prope

      integer       :: ld(*), ie(nie,*), id(ndf,*), ix(nen1,*)
      integer       :: jp(*)
      real (kind=8) :: xl(ndm,*),d(ndd,*),ul(ndf,nen,*)
      real (kind=8) :: p(nst,*),s(nst,nst,2)
      real (kind=8) :: x(ndm,*) ,f(ndf,numnp),u(ndf,*),ud(*),t(*),tl(*)
      real (kind=8) :: un(20,2), dun(20,2), b(*), a(*), al(*)

      save

      if(debug) then
        call udebug('   pforma',jsw)
      endif

      nrecv = 0
      do n = nn1,nn2,nn3

        n_el = n

!       Set send flag for send/receive

        if(svfl) then
          sendfl = mr(np(259)+n-1).eq.1
        else
          sendfl = .false.
        endif

!       Skip elements not requiring send/receive

        if(rvfl) then
          if(mr(np(259)+n-1).eq.0) then
            go to 1100
          else
            recvfl = .true.
          endif
        endif

!       Check for active regions

        if((nreg.lt.0 .and. ix(nen1-1,n).ge.0)
     &                .or. (abs(ix(nen1-1,n)).eq.nreg)) then

!         Loop over material types to check for superposed elements

          nov   = 0
          do ma = 1, nummat

            if(ie(nie-2,ma).eq.ix(nen1,n) ) then

!             Compute address and offset for history variables

              ht1 = np(49) + ix(nen+1,n) + ie(nie-3,ma)
              ht2 = np(49) + ix(nen+2,n) + ie(nie-3,ma)
              ht3 = np(49) + ix(nen+3,n) + ie(nie-4,ma)

!             Move history variables into local nh1,nh21 area

              if(ie(nie,ma).gt.0) then
                do i = 0,ie(nie,ma)-1
                  hr(nh1+i) = hr(ht1+i)
                  hr(nh2+i) = hr(ht2+i)
                end do ! i
              endif

!             Move element variables into nh3

              if(ie(nie-5,ma).gt.0) then
                do i = 0,ie(nie-5,ma)-1
                  hr(nh3+i) = hr(ht3+i)
                end do ! i
              endif

!             Set element type, rotation type and reset print counter

              if(ie(nie-1,ma).ne.iel) mct = 0
              iel   = ie(nie-1,ma)
              rotyp = ie(nie-6,ma)

!             Set local arrays for element

              fp(1) = ndf*nen*(ma-1) + np(240)        ! iedof
              call plocal(ld,id,mr(np(31)+nneq),ix(1,n),ie(1,ma),
     &                    mr(fp(1)),xl,ul,tl,p(1,3),x,f,u,ud,t,
     &                    un,dun, nrot, dfl, jsw)

!             Form element array - rotate parameters if necessary

              if(nrot.gt.0) then
                if(iel.gt.0) then
                  call ptrans(ia(1,iel),hr(np(46)),ul,p,s,
     &                        nel,ndf,nst,1)
                  if(ir(1,iel).ne.0) then
                    call ptrans(ir(1,iel),hr(np(46)),ul,p,s,
     &                          nel,ndf,nst,1)
                  endif
                else
                  call ptrans(ea(1,-iel),hr(np(46)),ul,p,s,
     &                        nel,ndf,nst,1)
                  if(er(1,-iel).ne.0) then
                    call ptrans(er(1,-iel),hr(np(46)),ul,p,s,
     &                          nel,ndf,nst,1)
                  endif
                endif
              endif

!             Set for projections

              if(jsw.eq.8) then
                erav = hr(np(207)+n-1)
              else
                erav = 0.0d0
              endif

!             Call element library

              dm = prope
              call elmlib(d(1,ma),ul,xl,ix(1,n),tl,s,p,
     &                    ndf,ndm,nst,iel,jsw)

!             Check for representative volume elements

              if(svfl .and. sendfl) then
                if(nrecv.gt.0) then
                  sendfl          = .false.
                  go to 1000
                endif
                if(jsw.eq.14) then
                  mr(np(259)+n-1) = 1
                  sendfl          = .false.
                  go to 1000
                endif
              endif

!             Store time history plot data from element

              if(jsw.eq.6) then

!               Standard element stress values

                do i = 1,nsplts
                  if(ispl(1,i).eq.n) then
                    jj = max(ispl(2,i),1)
                    spl(i) = tt(jj)
                  endif
                end do ! i

!               Standard user element values

                do i = 1,nuplts
                  if(iupl(1,i).eq.n) then
                    jj = max(iupl(2,i),1)
                    upl(i) = ut(jj)
                  endif
                end do ! i

              endif

!             Modify for rotated dof's

              if(nrot.gt.0) then
                if(iel.gt.0) then
                  call ptrans(ia(1,iel),hr(np(46)),ul,p,s,
     &                        nel,ndf,nst,2)
                  if(ir(1,iel).ne.0) then
                    call ptrans(ir(1,iel),hr(np(46)),ul,p,s,
     &                          nel,ndf,nst,2)
                  endif
                else
                  call ptrans(ea(1,-iel),hr(np(46)),ul,p,s,
     &                        nel,ndf,nst,2)
                  if(er(1,-iel).ne.0) then
                    call ptrans(er(1,-iel),hr(np(46)),ul,p,s,
     &                          nel,ndf,nst,2)
                  endif
                endif
              endif

!             Position update terms 'ht1,ht2' from 'nh1,nh2' to save

              if(hflgu .and. ie(nie,ma).gt.0) then
                do i = 0,ie(nie,ma)-1
                  temp      = hr(ht1+i)
                  hr(ht1+i) = hr(nh1+i)
                  hr(nh1+i) = temp
                end do ! i
                do i = 0,ie(nie,ma)-1
                  temp      = hr(ht2+i)
                  hr(ht2+i) = hr(nh2+i)
                  hr(nh2+i) = temp
                end do ! i
              endif

!             Position update terms 'ht3' from 'nh3' to save

              if(h3flgu .and. ie(nie-5,ma).gt.0) then
                do i = 0,ie(nie-5,ma)-1
                  hr(ht3+i) = hr(nh3+i)
                end do ! i
              endif

!             Modify for non-zero displacement boundary conditions

              mdfl = .false.
              do i = 1,ndf
                if(dun(i,1).gt.1.0d-10*un(i,1)) then
                  mdfl = .true.
                endif
              end do ! i

              if(efl.and.mdfl) then

!               Get current element tangent matrix

                if (.not.afl) then
                  dm = prop
                  call elmlib(d(1,ma),ul,xl,ix(1,n),tl,s,p,
     &                        ndf,ndm,nst,iel,ksw)
                  if(nrot.gt.0) then
                    if(iel.gt.0) then
                      call ptrans(ia(1,iel),hr(np(46)),ul,p,s,
     &                            nel,ndf,nst,2)
                      if(ir(1,iel).ne.0) then
                        call ptrans(ir(1,iel),hr(np(46)),ul,p,s,
     &                              nel,ndf,nst,2)
                      endif
                    else
                      call ptrans(ea(1,-iel),hr(np(46)),ul,p,s,
     &                            nel,ndf,nst,2)
                      if(er(1,-iel).ne.0) then
                        call ptrans(er(1,-iel),hr(np(46)),ul,p,s,
     &                              nel,ndf,nst,2)
                      endif
                    endif
                  endif
                endif

!               Modify for displacements

                do i = 1,nst
                  p(i,3) = p(i,3)*cc3
                end do ! i
                call modify(p,s,p(1,3),nst,nst)

              endif

!             Assemble arrays as necessary

              pstyp = ie(1,ma)
              call passble(s,p,ld,ix(1,n), jp,a,al,b,
     &                       afl,bfl, nst,nov, jsw)

            endif
1000        continue
          end do ! ma

        endif ! regions
1100    continue

      end do ! n

      end subroutine pforma

c$Id:$
      subroutine pform(ul,xl,tl,ld,p,s,ie,d,id,x,ix,f,t,jp,
     &  u,ud,b,a,al,ndd,nie,ndf,ndm,nen1,nst,aufl,bfl,dfl,
     &  isw,nn1,nn2,nn3)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Compute element arrays and assemble global arrays

c      Inputs:
c         ie(nie,*)   - Assembly information for material set
c         d(ndd,*)    - Material set parameters
c         id(ndf,*)   - Equation numbers for each active dof
c         x(ndm,*)    - Nodal coordinates of mesh
c         ix(nen1,*)  - Element nodal connections of mesh
c         f(ndf,*,2)  - Nodal force and displacement values
c         t(*)        - Nodal temperature values
c         jp(*)       - Pointer array for row/columns of tangent
c         u(*)        - Nodal solution values
c         ud(*)       - Nodal rate values
c         ndd         - Dimension for d array
c         nie         - Dimension for ie array
c         ndf         - Number dof/node
c         ndm         - Spatial dimension of mesh
c         nen1        - Dimension for ix array
c         nst         - Dimension for element array
c         aufl        - Flag, assemble coefficient array if true
c         bfl         - Flag, assemble vector if true
c         dfl         - Flag, assemble reactions if true
c         isw         - Switch to control quantity computed
c         nn1         - First element number to process
c         nn2         - Last element number to process
c         nn3         - Increment to nn1

c      Scratch:
c         ul(ndf,*)   - Element solution and rate values
c         xl(ndm,*)   - Element nodal coordinates
c         tl(*)       - Element nodal temperatures
c         ld(*)       - Element local/global equation numbers
c         p(*)        - Element vector
c         s(nst,*)    - Element array

c      Outputs:
c         b(*)        - Global vector
c         a(*)        - Global matrix, diagonal and upper part
c         al(*)       - Global matrix, lower part
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'crotas.h'
      include  'ddata.h'
      include  'elcount.h'
      include  'eldata.h'
      include  'elplot.h'
      include  'eluser.h'
      include  'eqsym.h'
      include  'erotas.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'hdata.h'
      include  'hdatam.h'
      include  'mdata.h'
      include  'modreg.h'
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

      logical   aufl,bfl,dfl,efl
      integer   nrkn, nrcn, nrmn, ii, iid, ild, isw, jsw, ksw
      integer   i, j, k, jj, nn1, nn2, nn3, nst, nl1, nneq
      integer   numnp2, ndf, ndm, nrot, ndd, nie, nen1
      integer   nt1, nt2, nt3
      real*8    un, dun, temp, prope

      integer   ld(*), ie(nie,*), id(ndf,*), ix(nen1,*), jp(*)
      real*8    xl(ndm,*), p(*), s(nst,*), d(ndd,*), ul(nst,*)
      real*8    x(ndm,*) ,f(ndf,numnp),u(ndf,*),ud(*),t(*),tl(*)
      real*8    b(*), a(*), al(*)

      save

c     Set element proportional loading value

      prope = theta(3)*(prop - propo) + propo

c     Recover nh1, nh2, nh3 pointers

      nh1 = np(51)
      nh2 = np(52)
      nh3 = np(53)

c     Set program and user material count parameters

      do i = 1,10
        nomats(1,i) = 0
        nomats(2,i) = 0
        unmats(1,i) = 0
        unmats(2,i) = 0
      end do ! i

c     Set up local arrays before calling element library

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

      nl1    = ndf*nen + 1
      numnp2 = numnp + numnp
      nneq   = numnp*ndf
      nrkn   = nrk*nneq - nneq
      nrcn   = nrc*nneq - nneq
      nrmn   = nrm*nneq - nneq

c     Loop over active elements

      do n = nn1,nn2,nn3

c       Check for active regions

        if((nreg.lt.0 .and. ix(nen1-1,n).ge.0)
     &                .or. (abs(ix(nen1-1,n)).eq.nreg)) then

c        Set up local arrays

         do ma = 1, nummat

          if(ie(nie-2,ma).eq.ix(nen1,n)) then

c           Compute address and offset for history variables

            nt1 = np(50) + ix(nen+1,n) + ie(nie-3,ma)
            nt2 = np(50) + ix(nen+2,n) + ie(nie-3,ma)
            nt3 = np(50) + ix(nen+3,n) + ie(nie-4,ma)

c           If history variables exist move into nh1,nh2

            if(ie(nie,ma).gt.0) then
              do i = 0,ie(nie,ma)-1
                hr(nh1+i) = hr(nt1+i)
                hr(nh2+i) = hr(nt2+i)
              end do
            endif

c           If Element variables exist move into nh3

            if(ie(nie-5,ma).gt.0) then
              do i = 0,ie(nie-5,ma)-1
                hr(nh3+i) = hr(nt3+i)
              end do
            endif

            if(ie(nie-1,ma).ne.iel) mct = 0
            iel   = ie(nie-1,ma)
            rotyp = ie(nie-6,ma)

c           Zero array used to store local displ, veloc, and accel

            do i = 1,nst
              ld(i) = 0
              do j = 1,7
                ul(i,j) = 0.0d0
              end do
            end do

c           Zero array used to store local tl and coordinates

            do i = 1,nen
              tl(i) = 0.0d0
              do j = 1,ndm
                xl(j,i) = 0.0d0
              end do
            end do
            un   = 0.0d0
            dun  = 0.0d0

c           Set up local nodal coord rotation array for sloping b.c.

            call pangl(ix(1,n),nen,hr(np(46)),hr(np(45)),nrot)
            do i = 1,nen

c             Set up localized solution parameters

              ii = ix(i,n)
              if(ii.gt.0) then
                iid = ii*ndf - ndf
                ild =  i*ndf - ndf
                nel = i
                tl(i) = t(ii)
                do j = 1,ndm
                  xl(j,i) = x(j,ii)
                end do
                do j = 1,ndf
                  jj = ie(j,ma)
                  if(jj.gt.0) then

c                   Set solution, total increment, last increment

                    ul(j+ild,1) = u(jj,ii)
                    ul(j+ild,2) = u(jj,ii+numnp)
                    ul(j+ild,3) = u(jj,ii+numnp2)

c                   Set dynamics solutions

                    if(fl(9)) then
                      k = iid+jj
                      if(nrk.gt.0) then
                        ul(j+ild,1) = ud(nrkn+k)
                      endif

                      if(jsw.eq.13) then
                        ul(j+ild,1) = u(jj,ii)
                        ul(j+ild,4) = ud(k)
                      else
                        if(nrc.gt.0) ul(j+ild,4) = ud(nrcn+k)
                        if(nrm.gt.0) ul(j+ild,5) = ud(nrmn+k)
                      endif

c                   Set acceleration for specified shift

                    elseif(shflg) then
                      ul(j+ild,5) = -shift*ul(j+ild,1)
                    endif

                    un          = max( un,abs(u(jj,ii)))

c                   Set increment for specified boundary values

                    if( id(jj,ii).le.0) then
                      ul(j+ild,7) = (f(jj,ii) - u(jj,ii))
                      dun         = max(dun,abs(ul(j+ild,7)))
                    endif

c                   Set k for reactions

                    if(dfl) then
                      k = iid + jj

c                   Set k for compressed assembly

                    else
                       k = id(jj,ii)
                    endif

c                 Reset k for inactive equation

                  else
                    k = 0
                  endif

c                 Form assembly array

                  ld(j+ild) = k

                end do
              endif
            end do

c           Form element array - rotate parameters if necessary

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
              erav = hr(np(60)+n-1)
            else
              erav = 0.0d0
            endif
            dm = prope
            call elmlib(d(1,ma),ul,xl,ix(1,n),tl,s,p,
     &                  ndf,ndm,nst,iel,jsw)

c           Store time history plot data from element

            if(jsw.eq.6) then

c             Standard element values

              do i = 1,nsplts
                if(ispl(1,i).eq.n) then
                  jj = max(ispl(2,i),1)
                  spl(i) = tt(jj)
                endif
              end do

c             Standard user element values

              do i = 1,nuplts
                if(iupl(1,i).eq.n) then
                  jj = max(iupl(2,i),1)
                  upl(i) = ut(jj)
                endif
              end do

            endif

c           Modify for rotated dof's

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

c           Add to total array

            if(aufl.or.bfl) then
              call dasble(s,p,ld,jp,nst,neqs,aufl,bfl,
     &                    b,al,a(neq+1),a)
            endif

c           Position update terms 'nt1,nt2' from 'nh1,nh2' to save

            if(hflgu .and. ie(nie,ma).gt.0) then
              do i = 0,ie(nie,ma)-1
                temp      = hr(nt1+i)
                hr(nt1+i) = hr(nh1+i)
                hr(nh1+i) = temp
                temp      = hr(nt2+i)
                hr(nt2+i) = hr(nh2+i)
                hr(nh2+i) = temp
              end do
            endif

c           Position update terms 'nt3' from 'nh3' to save

            if(h3flgu .and. ie(nie-5,ma).gt.0) then
              do i = 0,ie(nie-5,ma)-1
                hr(nt3+i) = hr(nh3+i)
              end do
            endif

c           Modify for non-zero displacement boundary conditions

            if(efl.and.dun.gt.1.0d-7*un) then

c             Get current element tangent matrix

              if (.not.aufl) then
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

c             Modify for displacements

              do i = 1,nst
                ul(i,7) = ul(i,7)*cc3
              end do
              call modify(b,ld,s,ul(1,7),nst)
            end if

          end if

         end do ! ma

        end if ! regions

      end do ! n

      end

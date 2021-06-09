!$Id:$
      subroutine plocal(ld,eq,id,ix,ie,iedof,xl,ul,tl,ub, x,f,u,ud,
     &                  t,un,dun, nrot, dfl, jsw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set local arrays for each element

!      Inputs:
!        eq(*)    - Global equation numbers
!        id(*)    - Boundary restraints
!        ie(*)    - Element descriptor parameters
!        iedof(*) - Element descriptor parameters
!        x(*)     - Global nodal coordinates
!        f(*)     - Global nodal forces/displacements
!        u(*)     - Global nodal solution parameters
!        ud(*)    - Global nodal rate parameters
!        t(*)     - Global temp variables
!        dfl      - Set to assemble reactions if true
!        jsw      - Switching parameter

!      Scratch
!        ubl(*)   - Local array for boundary displacements

!      Outputs:
!        ld(*)    - Element global equation numbers
!        xl(*)    - Element nodal coordinates
!        ul(*)    - Element nodal solution parameters
!        tl(*)    - Element temp values
!        ub(*)    - Element boundary displacement modify values
!        un,dun   - Boundary modification indicators
!        nrot     - Number dof's with rotated directions
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'cdat1.h'
      include   'corset.h'
      include   'crotas.h'
      include   'ddata.h'
      include   'eldata.h'
      include   'fdata.h'
      include   'lmdata.h'
      include   'mdata.h'
      include   'pglob1.h'
      include   'p_int.h'
      include   'qudshp.h'
      include   'rdata.h'
      include   'rdat0.h'
      include   'sdata.h'
      include   'setups.h'
      include   'pointer.h'
      include   'comblk.h'

      logical       :: dfl
      integer       :: nrot, jsw, i,j,k, iid,ild
      integer       :: ld(nst),eq(ndf,*),id(ndf,*),ix(*),ie(*)
      integer       :: iedof(ndf,*)
      real (kind=8) :: un(*),dun(*), ang
      real (kind=8) :: xl(ndm,*),ul(ndf,nen,*),tl(*), ub(*), ubl(20)
      real (kind=8) :: x(ndm,*),f(ndf,*),u(ndf,*),ud(*),t(*)

      save

!     Zero array used to store local displ, veloc, and accel
      do i = 1,nst
        ld(i) = 0
        ub(i) = 0.0d0
      end do ! i

      do k = 1,7
        do j = 1,nen
          do i = 1,ndf
            ul(i,j,k) = 0.0d0
          end do ! i
        end do ! j
      end do ! k

!     Zero array used to store local tl and coordinates
      do i = 1,nen
        tl(i) = 0.0d0
        do j = 1,ndm
          xl(j,i) = 0.0d0
        end do ! j
      end do ! i

      do j = 1,ndf
        un(j)  =  0.0d0
        dun(j) =  0.0d0
      end do ! j

!     Set up local nodal rotation array for inclined b.c.
c     call pangl(ix,nen,hr(np(46)),hr(np(45)),nrot)
      nrot = 0

!     Set element type
      eltyp = ix(nen+7)  ! N.B. FE elements have negative type
      elty2 = ix(nen+8)  ! Used for NURBS 2-d & 3-d
      elty3 = ix(nen+9)  ! Used for NURBS 3-d

!     Set individual nodal values
      do i = 1,nen

        if(ix(i).gt.0) then

!         Set up localized solution parameters
          iid = ix(i)*ndf - ndf
          ild =     i*ndf - ndf
          nel = i
          tl(i) = t(ix(i))
          do j = 1,ndm
            xl(j,i) = x(j,ix(i))
          end do ! j
          if(eltyp.gt.0 .or. nurbfl) then
            hr(np(264)+i-1) = hr(np(263)+ix(i)-1)  ! NURB weight
          endif
          do j = 1,ndf
            ubl(j) = u(j,ix(i))
          end do ! j
          ang = hr(np(46)+i-1)
          if(ang.ne.0.0d0) then
            call upang(ia(1,iel),ang,ubl,ndf,1)
            if(ir(1,iel).gt.0) then
              call upang(ir(1,iel),ang,ubl,ndf,1)
            endif
          endif
          do j = 1,ndf
            if(iedof(j,i).gt.0) then

!             Set solution, total increment, last increment
              ul(j,i,1) = u(iedof(j,i),ix(i))
              ul(j,i,2) = u(iedof(j,i),ix(i)+numnp)
              ul(j,i,3) = u(iedof(j,i),ix(i)+numnp*2)

!             Set dynamics solutions
              if(fl(9)) then
                k = iid+iedof(j,i)
                if(nrk.gt.0) then
                  ul(j,i,1) = ud(nrkn+k)
                endif

                if(jsw.eq.13) then
                  ul(j,i,1) = u(iedof(j,i),ix(i))
                  ul(j,i,4) = ud(k)
                else
                  if(nrc.gt.0) ul(j,i,4) = ud(nrcn+k)
                  if(nrm.gt.0) ul(j,i,5) = ud(nrmn+k)
                endif

!               Set velocity at t_n
                ul(j,i,6) = ud(nrvn+k)

!             Set acceleration for specified shift
              elseif(shflg) then
                ul(j,i,5) = -shift*ul(j,i,1)
              endif

              un(j) = max(un(j),abs(u(iedof(j,i),ix(i))))

!             Set increment for specified boundary values
              if( id(iedof(j,i),ix(i)).gt.0) then
                ub(j+ild) = f(iedof(j,i),ix(i)) - ubl(iedof(j,i))
                dun(j)    = max(dun(j),abs(ub(j+ild)))
              endif

!             Set local/global map for assembly step
              if(dfl) then

!               Set k for reactions
                k = iid + iedof(j,i)
              else

!               Set k for assembly
                k = eq(iedof(j,i),ix(i))
              endif

!             Form assembly array
              ld(j+ild) = k

            endif
          end do ! j

        endif
      end do ! i

!     Lagrange multipliers
      if(ie(nie-8).gt.0 .and. np(210).ne.0) then

!       Set equation numbers and check active partition
        if(.not.dfl) then
          if(ie(nie-9).eq.0) then
            fp(2) = np(210) + ndl*(ix(nen+4)-1) - 1
            do j = 1,ie(nie-8)
              if(mr(fp(2)+j).gt.0) then
                ld(la+j) = mr(fp(2)+j) + ix(nen+5)
              endif
            end do ! j
          endif
        endif

!       Set element solution parameters
        fp(1) = np(213) - 1 + 3*ndl*(ix(nen+4)-1)
        do j = 1,ie(nie-8)
          ule(j,1) = hr(fp(1)+j      )
          ule(j,2) = hr(fp(1)+j+ndl  )
          ule(j,3) = hr(fp(1)+j+ndl*2)
        end do ! j
      endif

      end subroutine plocal

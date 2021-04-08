!$Id:$
      subroutine xpline(x,ie,ix,id,ic,ip,numnp,numel,ndm,
     &                 nen1,nen,nie,ct,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Determine plot line sequence to display 2-d mesh or
!               outline of 2-d mesh

!      Inputs:
!         x(ndm,*)  - Nodal coordinates for mesh
!         ie(nie,*) - Material set assembly data
!         ix(nen1,*)- Element nodal connection list
!         ip(*)     - Symmetry sorts for element sequences to plot
!         numnp     - Number of nodes in mesh
!         numel     - Number of elements in mesh
!         ndm       - Dimension of x array
!         nen1      - Dimension of ix array
!         nen       - Number of nodes/element
!         nie       - Dimension of ie array
!         ct        - Plot by material numbers if negative
!         isw       - Flag, plot mesh if true, otherwise do outline

!      Scratch:
!         id(*)     - Number of elements connected to nodes
!         ic(*)     - Element numbers connected to each node

!      Outputs:
!         none      - Plot to screen/file
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pbody.h'
      include  'pdatas.h'
      include  'pdata4.h'
      include  'pdatxt.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: ifl,iend,isw
      integer       :: numnp,numel,ndm,nen1,nen,nie, pstyp,nel
      integer       :: i, j, k, ii, jj, ij, iju, n, n1, n2, ni, nn
      real (kind=8) :: ct, x3

      integer       :: ie(nie,*),ix(nen1,*),ic(*),ip(*),id(*),jplt(30)
      real (kind=8) :: x(ndm,*)

      save

!     Initialize connection array

      do i = 1,id(numnp+1)
        ic(i) = 0
      end do

!     Loop through elements to set up list

      do nn = 1,numel
        n  = ip(nn)
        if(n.gt.0) then
          if(ix(nen1-1,n).ge.0) then
            ii    = ix(nen1,n)
            pstyp = ie(1,ii)

!           Plot material number: maplt (0 = all); ii > 0 active material

            jj = maplt
            if(pstyp.ne.0 .and. (jj.eq.0 .or. ii.eq.jj)) then
              if(ii.eq.jj .and. ct.lt.0.0d0) call pppcol(jj,1)
              do i = nen,1,-1
                if(ix(i,n).gt.0) then
                  nel = i
                  exit
                endif
              end do ! i
              if(ix(nen+7,n).eq.-22) then
                call vem_compp(ix(nen+8,n), jplt, nel, iju)
              elseif(ix(nen+7,n).eq.-23) then
                write(*,*) ' ERROR: VEM 3D not implemented'
              else
                call plftyp(pstyp,nel,ie(nie-1,ii))
                call pltord(ix(1,n),ie(nie-1,ii), iju,jplt)
              endif

!             Look up element nodes

              ii = abs(ix(jplt(1),n))
              do ij = 2,iju
                j = jplt(ij)
                if((j.le.nen).and.(j.gt.0).and.(ix(j,n).ne.0)) then
                  jj = abs(ix(j,n))
                  if(jj.ne.ii) then
                    n1 = min(ii,jj)
                    n2 = max(ii,jj)
                    do k = id(n1),id(n1+1)-1
                      if(ic(k).eq.0) then
                        ic(k) =  n2
                        go to 100
                      elseif(abs(ic(k)).eq.n2) then
                        ic(k) = -abs(n2)
                        go to 100
                      endif
                    end do
100                 ii = jj
                  endif
                endif
              end do
            endif
          endif
        endif
      end do

!     Change signs to permit mesh plot

      if(isw) then
        do n = 1,numnp
          do i = id(n),id(n+1)-1
            ic(i) = abs(ic(i))
          end do
        end do
      endif

!     Plot outline of part with continuous lines

      x3 = 0.0d0
      do ni = 1,numnp
        iend = .true.
        do n = 1,numnp
          ifl = .true.
          n1  =  n
101       do i = id(n1),id(n1+1)-1
            if(ic(i).gt.0) then
              go to 102
            elseif(ic(i).eq.0) then
              go to 103
            endif
          end do
          go to 103
102       iend = .false.
          if(ifl) then
            if(ndm.ge.3) x3 = x(3,n1)
            call plotl(x(1,n1),x(2,n1),x3,3)
            ifl = .false.
          endif
          n2    =  ic(i)
          ic(i) = -n2
          if(ndm.ge.3) x3 = x(3,n2)
          call plotl(x(1,n2),x(2,n2),x3,2)
          n1 = n2
          go to 101
103       continue
        end do
        if(iend) go to 104
      end do

104   continue

      end subroutine xpline

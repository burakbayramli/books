c$Id:$
      subroutine xpline(x,ie,ix,id,ic,ip,numnp,numel,ndm,
     &                 nen1,nen,nie,ct,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Determine plot line sequence to display 2-d mesh or
c               outline of 2-d mesh

c      Inputs:
c         x(ndm,*)  - Nodal coordinates for mesh
c         ie(nie,*) - Material set assembly data
c         ix(nen1,*)- Element nodal connection list
c         ip(*)     - Symmetry sorts for element sequences to plot
c         numnp     - Number of nodes in mesh
c         numel     - Number of elements in mesh
c         ndm       - Dimension of x array
c         nen1      - Dimension of ix array
c         nen       - Number of nodes/element
c         nie       - Dimension of ie array
c         ct        - Plot by material numbers if negative
c         isw       - Flag, plot mesh if true, otherwise do outline

c      Scratch:
c         id(*)     - Number of elements connected to nodes
c         ic(*)     - Element numbers connected to each node

c      Outputs:
c         none      - Plot to screen/file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pbody.h'
      include  'pdatas.h'
      include  'pdata4.h'
      include  'pdatxt.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   ifl,iend,isw
      integer   numnp,numel,ndm,nen1,nen,nie, jplt(30)
      integer   i, j, k, ii, jj, ij, iju, n, n1, n2, ni, nn
      real*8    ct, x3

      integer   ie(nie,*),ix(nen1,*),ic(*),ip(*),id(*)
      real*8    x(ndm,*)

      save

c     Initialize connection array

      do i = 1,id(numnp+1)
        ic(i) = 0
      end do

c     Loop through elements to set up list

      do nn = 1,numel
        n  = ip(nn)
        if(n.gt.0) then
          if(ix(nen1-1,n).ge.0) then
            ii = ix(nen1,n)

c           Plot material number: maplt (0 = all); ii > 0 active material

            jj = maplt
            if(jj.eq.0 .or. ii.eq.jj) then
              if(ii.eq.jj .and. ct.lt.0.0d0) call pppcol(jj,1)
              call pltord(ix(1,n),ie(nie-1,ii), iju,jplt)

c             Look up element nodes

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

c     Change signs to permit mesh plot

      if(isw) then
        do n = 1,numnp
          do i = id(n),id(n+1)-1
            ic(i) = abs(ic(i))
          end do
        end do
      endif

c     Plot outline of part with continuous lines

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
          if(ndm.ge.3) x3 = x(3,n1)
          if(ifl) then
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

      end

!$Id:$
      subroutine opnum(ix,nd,ln,ne,ndw,msum,nfrnt,nnid,
     &                 numnp,numel,nen,nen1,prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Calculation of best order to number equations.
!               Numbers equations for minimum front width/profile.

!               Ref: M. Hoit and E.L. Wilson, 'An Equation
!                    Numbering Algorithm Based on a Minimum
!                    Front Criteria,' Computers & Structures,
!                    v 16, No. 1-4, pp225-239, 1983.
!               Modified: R.L. Taylor; 1 December 2000
!                         Ignores overlayed elements and fixed nodes in
!                         computing weights.

!      Inputs:
!         ix(nen1,*)     - Element nodal connection list
!         nnid(numnp)    - Number dof at each node
!         numnp          - Number of nodes in mesh
!         numel          - Number of elements in mesh
!         nen            - Maximum number of nodes/element
!         nen1           - Dimension of ix  array
!         prt            - Print results if true

!      Scratch:
!         nd             - Nodes currently in front
!         ln             - Pointer array for element number
!         ne             - Element numbers connected to nodes
!         ndw            - Node weights
!         msum           - Element weights

!      Outputs:
!         nfrnt(i)       - Original node for new number i
!         msum(i)        - New element number for element i
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      logical       :: prt
      integer       :: ie, l, m,minw,ml,mm,mh, n,nf,nn,nsum
      integer       :: numnp,numel,nen,nen1
      integer       :: node,nume,nstart,numb
      integer       :: ix(nen1,*),nd(numnp),ln(numnp),ne(*)
      integer       :: ndw(numnp),msum(numel),nfrnt(numnp)
      integer       :: nnid(numnp)

      save

!     Initialization

      if(prt) write (iow,2000)
      node = 0
      nume = 0

!     Set start lists

      call nodel(ix,nnid,nd,ln,ne,numnp,numel,nsum,nen,nen1)

!     Initialize the element weight

      do m = 1,numel
        msum(m) = 1
      end do ! m

!     Locate starting node

  100 nstart  = 0
      call nodew(ndw,msum,ix,nnid,nen,nen1,numnp,numel,nstart)
      if(nstart.ne.0) then
        if(prt) write (iow,2004) nstart
        nfrnt(1) = nstart
        numb = 1

!       Find next element to be added to front

  110   ie = 0
        minw = 32000000

!       Loop over existing nodes on front

        do nn = 1,numb
          nf = nfrnt(nn)
          mh = ln(nf)
          ml = 1
          if(nf.ne.1) ml = ln(nf-1) + 1

!         For each node on front check elements ---

          do mm = ml,mh

            m = ne(mm)
            if(m.gt.0 .and.m.le.numel) then
              if(msum(m).gt.0 .and. ix(nen1-1,m).ge.0) then

!               Evaluate increase or decrease in front --

                nsum = 0
                do l = 1,nen
                  n = abs(ix(l,m))
                  if(n.gt.0) then
                    if(nnid(n).gt.0) then
                      if(ndw(n).eq.msum(m)) nsum = nsum - 1
                      if(nd(n).ge.0)        nsum = nsum + 1
                    endif
                  endif
                end do ! l

!               Compare with previous minimum

                if(nsum.lt.minw) then
                  minw = nsum
                  ie   = m
                endif
              endif
            endif
          end do ! mm

        end do ! nn

!       Subtract element sums from nodal values

        m = ie
        if(prt) write (iow,2001) m

        if(m.gt.0 .and. m.le.numel) then
          do l = 1,nen

!           Reduce node sums by element weights

            n = abs(ix(l,m))
            if(n.gt.0) then
              if(nnid(n).gt.0) then
                ndw(n) = ndw(n) - msum(m)
                call front(nfrnt,nd,n,numb,1)

!               Check if equation is to be numbered

                if(ndw(n).eq.0) then
                  node = node + 1
                  nd(n) = node
                  call front(nfrnt,nd,n,numb,2)
                  if(prt) write (iow,2002) n
                endif

              endif
            endif
          end do ! l

        endif

!       Remove element from system

        nume = nume + 1
        if(m.gt.0) then
          msum(m) = - nume
        endif

!       Check if front has been reduced to zero

        if(numb.eq.0) go to 100
        go to 110
      endif

!     Put in final order - add inactive nodes

      do n = 1,numnp

        if(nd(n).eq.0) then
          node = node + 1
          nfrnt(node) = n
          if(prt) write (iow,2003) n
        else
          nn = nd(n)
          nfrnt(nn) = n
        endif

      end do ! n

!     Formats

 2000 format(/1x,'Calculation of Minimum Front'/)
 2001 format( 5x,'Next element on front=',i6)
 2002 format( 5x,'Next node numbered =',i6)
 2003 format( 5x,'Node with no element attached =',i7)
 2004 format( 5x,'Starting node number =',i7/)

      end subroutine opnum

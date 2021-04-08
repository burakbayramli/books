!$Id:$
      subroutine ptiend(ie,ix,ib,ip,x,ma,nie,nen,nen1,ndm,numel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Connect line elements to boundaries of 2-d solids

!      Inputs:
!         ie(nie,*) - Assembly information for material sets
!         ib(*)     - Node indicators??
!         x(ndm,*)  - Nodal coordinates of mesh
!         ma        - Material set number of line element to tie
!         nie       - Dimension of ie array
!         nen       - Number of nodes on element
!         nen1      - Dimension of ix array
!         ndm       - Spatial dimension of problem
!         numel     - Number of elements in mesh

!      Outputs:
!         ix(nen1,*)- Element nodal connection list
!         ip(*)     - Nodal number list
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer       :: ma,nie,nen,nen1,ndm,numel

      integer       :: n,m, ii,i1,i2,ij1, j1,j2,ij2, n1,n2,nn, m1,m2
      real (kind=8) :: dx,dy, dx1,dx2, dy1,dy2, tol

      integer       :: ie(nie,*),ix(nen1,*),ib(*),ip(*)
      integer       :: iord1(50),iord2(50)
      real (kind=8) :: x(ndm,*)

      save

      data      tol/ 1.d-08 /

!     Loop over elements: search for line type 'ma'

      do n = 1,numel
        if(ix(nen1,n).eq.ma) then

!         Find edge on boundary of line: must have non-zero length

          call pltord(ix(1,n),ie(nie-1,ix(nen1,n)),ij1,iord1)
          do i1 = 1,ij1
            i2 = mod(i1,ij1) + 1
            n1 = ix(iord1(i1),n)
            n2 = ix(iord1(i2),n)
            if(ib(n1).ne.0 .and. ib(n2).ne.0) then
              dx1 = x(1,n2) - x(1,n1)
              dy1 = x(2,n2) - x(2,n1)
              if(abs(dx1) + abs(dy1) .gt. tol) then

!               Search other types of elements for contiguous edge

                do m = 1,numel
                  if(ix(nen1,m).ne.ma) then
                    call pltord(ix(1,m),ie(nie-1,ix(nen1,m)),ij2,iord2)
                    do j1 = 1,ij2
                      j2 = mod(j1,ij2) + 1
                      m1 = ix(iord2(j1),m)
                      m2 = ix(iord2(j2),m)
                      if(ib(m1).ne.0 .and. ib(m2).ne.0) then

!                       Check that node pairs have same coordinates

                        dx  = abs(x(1,m1)-x(1,n2))+abs(x(1,m2)-x(1,n1))
                        dy  = abs(x(2,m1)-x(2,n2))+abs(x(2,m2)-x(2,n1))
                        if(dx + dy .lt. tol) then

!                         Check side has non-zero length

                          dx2 = x(1,m2) - x(1,m1)
                          dy2 = x(2,m2) - x(2,m1)
                          if(abs(dx2) + abs(dy2) .gt. tol) then

!                           Contiguous edge has opposite normals

                            if(abs(dx1+dx2)+abs(dy1+dy2).lt.tol) then
                              ix(iord2(i1),n) = m2
                              ix(iord2(i2),n) = m1
                              ib(m1)          = -1
                              ib(m2)          = -1
                              ib(n1)          = -1
                              ib(n2)          = -1
                              ip(n1)          = m2
                              ip(n2)          = m1
                              do nn = 1,numel
                                do ii = 1,nen
                                  if(ix(ii,nn).eq.n1) ix(ii,nn) = m2
                                  if(ix(ii,nn).eq.n2) ix(ii,nn) = m1
                                end do
                              end do
                            endif
                          endif
                        endif
                      endif
                    end do
                  endif
                end do
              endif
            endif
          end do
        endif
      end do

      end subroutine ptiend

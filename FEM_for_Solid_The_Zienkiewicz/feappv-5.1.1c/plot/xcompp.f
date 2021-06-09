!$Id:$
      subroutine xcompp(ie,ix,id,nie,nen1,nen,numnp,numel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Determine number of elements connected to each node
!               for use in mesh plot/outline routine xpline.

!      Inputs:
!         ie(nie,*)   - Material set assembly data
!         ix(nen1,*)  - Element nodal connection list
!         nie         - Dimension of ie array
!         nen1        - Dimension of ix array
!         nen         - Number of nodes connected to element
!         numnp       - Number of nodes in mesh
!         numel       - Number of elements in mesh

!      Outputs:
!         id(*)       - Connection data counter data
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer      :: nie,nen1,nen,numnp,numel
      integer      :: i,iu,i1,i2,j,ma,n, nel,pstyp
      integer      :: ie(nie,*),ix(nen1,*),id(*),iplt(30)

      save

      do n = 1,numnp+1
        id(n) = 0
      end do

      do n = 1,numel
        if(ix(nen1,n).gt.0) then  ! Test for visible elements
          pstyp = ie(1,ix(nen1,n))
          if(pstyp.ne.0) then
            ma = ie(nie-1,ix(nen1,n))
            do i = nen,1,-1
              if(ix(i,n).gt.0) then
                nel = i
                exit
              endif
            end do ! i
            if(ix(nen+7,n).eq.-22) then  ! 2-d VEM
              call vem_compp(ix(nen+8,n), iplt, nel, iu)
            elseif(ix(nen+7,n).eq.-23) then
              write(*,*) ' ERROR: VEM 3D not implemented'
            else
              call plftyp(pstyp,nel,ma)
              call pltord(ix(1,n),ma,iu,iplt)
            endif

            do i = 1,iu-1
              i1 = iplt(i)
              if( i1.gt.0 .and. i1.le.nen ) then
                do j = i+1,iu
                  i2 = iplt(j)
                  if( i2.gt.0 .and. i2.le.nen ) then
                   i1     = min(ix(i1,n),ix(i2,n))
                   id(i1) = id(i1) + 1
                   go to 100
                  end if
                end do ! j
              end if
100           continue
            end do ! i
          endif
        endif
      end do ! n

      j     = 1
      i     = id(1)
      id(1) = 1
      do n = 2,numnp+1
        j     = j + i
        i     = id(n)
        id(n) = j
      end do ! n

      end subroutine xcompp

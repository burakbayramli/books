c$Id:$
      subroutine rstprf(jp,idl,id,ix,ndf,nen1,nen,neq,numel)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Reset profile for elements

c      Inputs:
c         id(ndf,*)     - Equation numbers for nodal dof
c         ix(nen1,*)    - Element nodal connections
c         ndf           - Number dof/node
c         nen1          - Dimension for ix array
c         nen           - Number nodes connected to an element
c         neq           - Number of equations active
c         numel         - Number of elements in mesh

c      Scratch:
c         idl(*)        - Store element active equations, etc.

c      Outputs:
c         jp(*)         - Row/column lengths for each equation
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pointer.h'
      include  'comblk.h'

      integer   ndf,nen1,nen,neq,numel
      integer   i,j,ii, ma, mm, n,nad

      integer   jp(*),idl(*),id(ndf,*),ix(nen1,*)

c     Zero pointer array

      do n = 1,neq
        jp(n) = 0
      end do ! n

c     Compute column heights

      do n = 1,numel

c       Test for active element

        ma  = ix(nen1-1,n)
        if(ma.ge.0) then
          mm  = 0
          nad = 0
          do i = 1,nen
            ii = ix(i,n)

c           Set element profile

            if(ii.gt.0) then
              do j = 1,ndf
                if(id(j,ii).gt.0 ) then
                  if(mm.eq.0) mm = id(j,ii)
                  mm       = min(mm,id(j,ii))
                  nad      = nad + 1
                  idl(nad) = id(j,ii)
                end if
              end do
            endif
          end do

c         Compute column heights

          do i = 1,nad
            ii = idl(i)
            jp(ii) = max(jp(ii),ii-mm)
          end do
        end if
      end do

      end

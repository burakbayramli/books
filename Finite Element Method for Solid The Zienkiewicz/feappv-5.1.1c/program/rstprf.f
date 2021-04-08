!$Id:$
      subroutine rstprf(jp,idl,eq,ix,ie,ndl,ndf,nen1,nen,neq,
     &                  numel,nummat)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Reset profile for elements

!      Inputs:
!         eq(ndf,*)     - Equation numbers for nodal dof
!         ix(nen1,*)    - Element nodal connections
!         ie(nie,*)     - Element properties
!         ndl           - Maximum number of element equations
!         ndf           - Number dof/node
!         nen1          - Dimension for ix array
!         nen           - Number nodes connected to an element
!         neq           - Number of equations active
!         numel         - Number of elements in mesh
!         nummat        - Number of material sets

!      Scratch:
!         idl(*)        - Store element active equations, etc.

!      Outputs:
!         jp(*)         - Row/column lengths for each equation
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdat1.h'
      include  'p_int.h'
      include  'pointer.h'
      include  'comblk.h'

      integer       :: ndl,ndf,nen1,nen,neq,numel,nummat
      integer       :: i,j,ii, m, ma, ml, mm, n,nad, rn
      integer       :: jp(*),idl(*),eq(ndf,*),ix(nen1,*),ie(nie,*)

!     Zero pointer array

      do n = 1,neq
        jp(n) = 0
      end do ! n

!     Compute column heights

      do n = 1,numel

!       Test for active element
        rn  = ix(nen1-1,n)
        if(rn.ge.0) then
          mm  = 0
          nad = 0
          do i = 1,nen
            ii = ix(i,n)

!           Set element profile
            if(ii.gt.0) then
              do j = 1,ndf
                if(eq(j,ii).gt.0 ) then
                  if(mm.eq.0) mm = eq(j,ii)
                  mm       = min(mm,eq(j,ii))
                  nad      = nad + 1
                  idl(nad) = eq(j,ii)
                end if
              end do
            endif
          end do

!         Add any internal element equations
          ma = ix(nen1,n)
          do m = 1,nummat
            if(ie(nie-2,m).eq.ma) then
!             Add element internal equations
              if(ie(nie-8,m).gt.0) then
                ml = ix(nen+4,n)   ! Element number
                if(ml.gt.0) then
                  fp(1) = np(210)+ndl*(ml-1) - 1
                  do j = 1,ie(nie-8,m)
                    if(mr(fp(1)+j).gt.0) then
                      nad      = nad + 1
                      idl(nad) = mr(fp(1)+j)
                      if(mm.eq.0) mm = idl(nad)
                      mm       = min(mm,idl(nad))
                    endif
                  end do ! j
                endif
              endif
            endif
          end do ! m

!         Compute column heights
          do i = 1,nad
            ii = idl(i)
            jp(ii) = max(jp(ii),ii-mm)
          end do
        end if
      end do

      end subroutine rstprf

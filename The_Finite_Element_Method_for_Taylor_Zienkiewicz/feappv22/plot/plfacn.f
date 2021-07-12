c$Id:$
      subroutine plfacn(ix,ia,nen,numel,nface,ie,nie)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Determines which exterior faces are directed toward
c               view point.

c      Inputs:
c         ix(nen1,*)- Element nodal connection lists
c         ia(*)     - Active element plots based on materials
c         nen       - Number nodes/element
c         numel     - Number of elements/faces
c         ie(nie,*) - Material set assembly data
c         nie       - Dimension of ie array

c      Outputs:
c         nface     - Number of faces
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'pdata5.h'
      include  'pdata6.h'
      include  'plclip.h'
      include  'sdata.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   lclip,addfac
      integer   nen,numel,nface,nie, i,j,m,n
      integer   iel,iiel

      integer   ix(nen1,numel), ia(*), ie(nie,*)
      integer   iq(4,6), it(3,4)

      save

c     8-node brick faces

      data iq/3,2,1,4, 1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8, 5,6,7,8/

c     4-node tet faces

      data it/1,2,4, 2,3,4, 3,1,4, 1,3,2/

c     Compute location of boundary faces

      nface = 0
      do n = 1,numel
        if(ix(nen1-1,n).ge.0 .and. ia(n).ge.0) then
         iel = ie(nie-1,ix(nen1,n))
         if(iel.gt.0) then
           iiel = inord(iel)
         else
           iiel = exord(-iel)
         endif

c        No face if inord < 0

         if     (iiel.lt.0) then

c        Set for tetrahedral element faces

         elseif (iiel .eq. 9 ) then

          if( lclip(ix(1,n),4,hr(np(43)),ndm) ) then
            do m = 1,4
              addfac = .true.
              do j = 1,3
                i = ix(it(j,m),n) - 1
                if(mr(np(47)+i).eq.0) then
                  addfac = .false.
                endif
              end do ! j
              if(addfac) then
                nface = nface + 1
              endif
            end do ! m
          end if

c        Set for brick element faces

         elseif (iiel .gt. 10 ) then

          if( lclip(ix(1,n),8,hr(np(43)),ndm) ) then
            do m = 1,6
              addfac = .true.
              do j = 1,4
                i = ix(iq(j,m),n) - 1
                if(mr(np(47)+i).eq.0) then
                  addfac = .false.
                endif
              end do ! j
              if(addfac) then
                nface = nface + 1
              endif
            end do ! m
          end if

c        Set space for line elements

         elseif( iiel.gt.0 .and. iiel.le.3 ) then
          nface = nface + 1

c        Set space for top and bottom shell faces

         elseif( lclip(ix(1,n),min(4,nen),hr(np(43)),ndm) ) then

          nface = nface + 2

         end if

        end if
      end do

      end

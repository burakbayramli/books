!$Id:$
      subroutine plfacn(ix,ia,nen,numel,nface,ie,nie)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Determines which exterior faces are directed toward
!               view point.

!      Inputs:
!         ix(nen1,*)- Element nodal connection lists
!         ia(*)     - Active element plots based on materials
!         nen       - Number nodes/element
!         numel     - Number of elements/faces
!         ie(nie,*) - Material set assembly data
!         nie       - Dimension of ie array

!      Outputs:
!         nface     - Number of faces
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'pbody.h'
      include  'pdata5.h'
      include  'pdata6.h'
      include  'plclip.h'
      include  'qudshp.h'
      include  'sdata.h'

      include  'pointer.h'
      include  'comblk.h'

      logical       :: lclip,addfac
      integer       :: nen,numel,nface,nie, i,j,m,n, iel,iiel, ien,nel
      integer       :: ufac,pstyp
      integer       :: ix(nen1,numel),ia(*),ie(nie,*),iq(4,6),it(3,4)
      integer       :: it2(3,16)

      save

!     8-node brick faces

      data iq/3,2,1,4, 1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8, 5,6,7,8/

!     4-node tet faces

      data it/1,2,4, 2,3,4, 3,1,4, 1,3,2/

!     10-node tet faces

      data it2 / 1, 5, 8,  5, 2, 9,  5, 9, 8,  8, 9, 4,
     &           2, 6, 9,  6, 3,10,  6,10, 9,  9,10, 4,
     &           3, 7,10,  7, 1, 8,  7, 8,10, 10, 8, 4,
     &           1, 7, 5,  7, 3, 6,  7, 6, 5,  5, 6, 2 /

!     Compute location of boundary faces

      nface = 0
      do n = 1,numel
        if(ix(nen1-1,n).ge.0 .and. ia(n).ge.0) then
          pstyp = ie(1,ix(nen1,n))
          if(pstyp.gt.0) then
            iel = ie(nie-1,ix(nen1,n))
            do j = nen,1,-1
              if(ix(j,n).gt.0) then
                nel = j
                exit
              endif
            end do ! j

!           Get plot type

            call plftyp(pstyp,nel,iel)

            if(iel.gt.0) then
              iiel = inord(iel)
            else
              iiel = exord(-iel)
            endif

!           6-node triangle

            if(iiel.eq.7) then
              ien = 3
            else
              ien = nen
            endif

!           No face if iiel < 0

            if     (iiel.lt.0) then

!           1-d elements

            elseif(pstyp .eq. 1) then

!             Set space for line elements

              if( iiel.gt.0 .and. iiel.le.3 ) then

                if( lclip(ix(1,n),2,hr(npxx),ndm) ) then
                  nface = nface + 1
                endif
              endif ! iiel > 0

!           2-d elements

            elseif(pstyp .eq. 2) then

!             Set space for top and bottom faces

              if( lclip(ix(1,n),min(4,ien),hr(npxx),ndm) ) then

                nface = nface + 2

              end if ! iiel

!           3-d element plots

            elseif(pstyp .eq. 3) then

!             Set for linear tetrahedral element faces

              if (iiel .eq. 9) then

                if( lclip(ix(1,n),4,hr(npxx),ndm) ) then
                  do m = 1,4
                    addfac = .true.
                    do j = 1,3
                      i = ix(it(j,m),n) - 1
                      if(mr(nprn+i).eq.0) then
                        addfac = .false.
                      endif
                    end do ! j
                    if(addfac) then
                      nface = nface + 1
                    endif
                  end do ! m
                end if

!             Set for quadratic tetrahedral element faces

              elseif (iiel .eq. 15) then

                if( lclip(ix(1,n),4,hr(npxx),ndm) ) then
                  do m = 1,16
                    addfac = .true.
                    do j = 1,3
                      i = ix(it2(j,m),n) - 1
                      if(mr(nprn+i).eq.0) then
                        addfac = .false.
                      endif
                    end do ! j
                    if(addfac) then
                      nface = nface + 1
                    endif
                  end do ! m
                end if

!             64 node cubic brick

              elseif (iiel .eq. 46 .and. .not.nurbfl) then

                if( lclip(ix(1,n),nen,hr(npxx),ndm) ) then
                  call pfacepqr( 3,ipu,ufac)
                  do m = 1,ufac
                    addfac = .true.
                    do j = 1,4
                      i = ix(ipu(j,m),n) - 1
                      if(mr(nprn+i).eq.0) then
                        addfac = .false.
                      endif
                    end do ! j
                    if(addfac) then
                      nface = nface + 1
                    endif
                  end do ! m
                end if

!             Set for 8 to 27 node brick element faces

              elseif (iiel .gt. 10 ) then

                if( lclip(ix(1,n),8,hr(npxx),ndm) ) then
                  do m = 1,6
                    addfac = .true.
                    do j = 1,4
                      i = ix(iq(j,m),n) - 1
                      if(mr(nprn+i).eq.0) then
                        addfac = .false.
                      endif
                    end do ! j
                    if(addfac) then
                      nface = nface + 1
                    endif
                  end do ! m
                endif

              endif ! iiel

            endif ! pstyp > 0

!         User tests

          elseif(pstyp.lt.0) then

            if( lclip(ix(1,n),nen,hr(npxx),ndm) ) then
              call ufacelib(pstyp,nel,ipu,ufac)
              do m = 1,ufac
                addfac = .true.
                do j = 1,4
                  i = ix(ipu(j,m),n) - 1
                  if(mr(nprn+i).eq.0) then
                    addfac = .false.
                  endif
                end do ! j
                if(addfac) then
                  nface = nface + 1
                endif
              end do ! m
            end if

          end if ! pstyp
        end if ! pty
      end do ! n

      end subroutine plfacn

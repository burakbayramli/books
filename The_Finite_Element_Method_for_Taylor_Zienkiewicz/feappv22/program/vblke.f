c$Id:$
      subroutine vblke(nr,ns,nt,ix,ni,ne,nf,nen1,mat,ntyp,dlayer,ilr)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Generate a block of 3-d 8-node brick elements

c      Inputs:
c         nr        - Number elements in 1-local coordinate dir.
c         ns        - Number elements in 2-local coordinate dir.
c         nt        - Number elements in 3-local coordinate dir.
c         ni        - Initial node number for block
c         ne        - Initial element number for block
c         nf        - Final   element number for block
c         nen1      - Dimension of ix array
c         mat       - Material set number for block
c         ntyp      - Element type for generations
c                     10: 8-node hexahedral  elements
c                     11: 4-node tetrahedral elements

c      Outputs:
c         ix(*)     - Element nodal connection list for block
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'cdat2.h'
      include  'iofile.h'
      include  'trdata.h'

      integer   ni,nf,ne,nen1,mat,ma,ntyp
      integer   nr,ns,nt,nrs,i,j,k,l,m,n,dlayer

      integer   ix(nen1,*),iq(8),it(4),ilr(*),nd(8),itc(4,6)

      save

      data      itc / 1,2,4,5, 2,3,4,8, 2,4,5,8,
     &                2,6,3,8, 3,6,7,8, 5,6,2,8 /

c     Check generation order

      do i = 1,4
        it(i) = i
      end do ! i
      if(trdet.gt.0.0d0) then
        do i = 1,8
          iq(i  ) = i
        end do ! i
      else
        do i = 1,4
          iq(i+4) = i
          iq(i  ) = i+4
        end do ! i
        i     = it(2)
        it(2) = it(3)
        it(3) = i
      endif

      nrs   = nr*ns
      nd(1) = -1
      nd(2) =  0
      nd(3) =  nr
      nd(4) =  nr - 1
      nd(5) =  nrs - 1
      nd(6) =  nrs
      nd(7) =  nrs + nr
      nd(8) =  nrs + nr - 1

c     Compute element connections

      if(dlayer.ge.0) then
        ma = mat
      endif
      nf = ne - 1
      do k = 1,nt-1
        if(dlayer.eq.3) then
          ma = ilr(k)
        endif
        do j = 1,ns-1
          if(dlayer.eq.2) then
            ma = ilr(j)
          endif
          n = nr*(j-1 + ns*(k-1)) + ni
          do i = 1,nr-1
            if(dlayer.eq.1) then
              ma = ilr(i)
            endif
            n = n + 1

c           Hexahedral  elements

            if(ntyp.eq.10) then
              nf = nf + 1
              ix(nen1,nf) = ma
              do m = 1,8
                ix(iq(m),nf) = n + nd(m)
              end do ! m

c           Tetrahedral elements

            elseif(ntyp.eq.11) then
              do l = 1,6
                nf = nf + 1
                ix(nen1,nf) = ma
                do m = 1,4
                  ix(it(m),nf) = n + nd(itc(m,l))
                end do ! m
              end do ! l
            endif

          end do ! i
        end do ! j
      end do ! k

      end

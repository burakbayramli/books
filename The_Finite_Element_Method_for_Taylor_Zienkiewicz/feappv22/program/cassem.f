c$Id:$
      subroutine cassem(ad, au, al, s, ir, jc, ld, nst, alfl,
     &                  bycol,diagin,all)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Assemble a sparse matrix A into a compressed array
c                'ad', 'au', 'al'

c      Inputs:
c         s(*,*)  - Element matrix to assemble
c         ir(*)   - Location of non-zero entries in A by profile col/rows
c         jc(*)   - Pointer array to find entries for equations
c         ld(*)   - Local/global array to map 's' into 'A'.
c         nst     - Size of 's' and 'ld'
c         alfl    - Flag: if true assemble both upper and lower parts
c                         if false assemble symmetric array in upper part
c         bycol   - Sparse storage scheme by column if .true.
c         diagin  - Includes diagonal in sparse assembly if .true.
c         all     - Stores all terms in row/column if .true.

c      Outputs:
c         ad(*)   - Diagonal part of A
c         au(*)   - Sparse store of upper part of A
c         al(*)   - Sparse store of lower part of A
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'constant.h'

      logical   alfl,bycol,diagin,all
      integer   nst, i,j,k,n
      integer   inz,inza, jc(*),ir(*), ld(*)
      real*8    ad(*), au(*), al(*), s(nst,nst)

      save

c     Compact assembly of profile matrix

      do i = 1,nst
        n = ld(i)
        if( n.ge.1) then

c         Assemble total array

          if(all) then

c           Assemble by columns

            if(bycol) then
              do j = 1,nst
                k = ld(j)
                if(k.gt.0 .and. n.gt.1) then
                  inz     = inza(jc(n-1)+1, jc(n), ir, k, n)
                  ad(inz) = ad(inz) + s(j,i)
                elseif(k.gt.0 .and. n.eq.1) then
                  inz     = inza(      one, jc(n), ir, k, n)
                  ad(inz) = ad(inz) + s(j,i)
                endif
              end do ! j

c           Assemble by rows (no reordering)

            else
              do j = 1,nst
                k = ld(j)
                if(k.gt.0 .and. n.gt.1) then
                  inz     = inza(jc(n-1)+1, jc(n), ir, k, n)
                  ad(inz) = ad(inz) + s(i,j)
                elseif(k.gt.0 .and. n.eq.1) then
                  inz     = inza(      one, jc(n), ir, k, n)
                  ad(inz) = ad(inz) + s(i,j)
                endif
              end do ! j

            endif

c         Assemble upper/lower parts by columns

          elseif(bycol) then
            do j = 1,nst
              k = ld(j)
c             Assemble including diagonal
              if(diagin) then
                if(k.gt.0 .and. k.le.n ) then
                  if(n.eq.1) then
                    ad(1) = ad(1) + s(j,i)
                    if(alfl) al(1) = al(1) + s(i,j)
                  else
                    inz     = inza(jc(n-1)+1, jc(n), ir, k, n)
                    ad(inz) = ad(inz) + s(j,i)
                    if(alfl) al(inz) = al(inz) + s(i,j)
                  endif
                endif
c             Assemble excluding diagonal
              else
                if(k.gt.0 .and. k.lt.n ) then
                  inz     = inza(jc(n-1)+1, jc(n), ir, k, n)
                  au(inz) = au(inz) + s(j,i)
                  if(alfl) al(inz) = al(inz) + s(i,j)
                endif
              endif
            end do ! j

c         Assemble upper/lower parts by rows

          else
            do j = 1,nst
              k = ld(j)
              if(k.gt.0) then
c               Assemble including diagonal
                if(diagin) then
                  if( k.ge.n ) then
                    inz     = inza(jc(n+neq), jc(n+neq+1)-1, ir, k, n)
                    ad(inz) = ad(inz) + s(j,i)
                    if(alfl) al(inz) = al(inz) + s(i,j)
                  endif
c               Assemble excluding diagonal
                else
                  if( k.gt.n ) then
                    inz     = inza(jc(n+neq), jc(n+neq+1)-1, ir, k, n)
                    au(inz) = au(inz) + s(j,i)
                    if(alfl) al(inz) = al(inz) + s(i,j)
                  endif
                endif
              endif
            end do ! j
          endif

c         Assemble diagonal for .not.diagin cases

          if(.not.diagin) then
            do j = 1,nst
              if(ld(j).eq.n) ad(n) = ad(n) + s(i,j)
            end do ! j
          endif

        endif

      end do ! i

      end

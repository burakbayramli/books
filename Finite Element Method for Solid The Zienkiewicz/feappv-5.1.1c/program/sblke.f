!$Id:$
      subroutine sblke(nr,ns,x,ix,ni,ne,n,ndm,nen1,nodinc,ntyp,nm,mat,
     &                 dlayer,ilr,ctype)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: elements for 2-d problems

!         nm < 4 : Generates a line of elements
!           ns = 1     2-node line
!           ns = 2     3-node line
!         nm > 3 : Generates a block of elements
!           ntyp = 1   4-node quadrilaterals
!           ntyp = 2   3-node triangles - diags ll to ur
!           ntyp = 3   3-node triangles - diags ul to lr
!           ntyp = 4   3-node triangles - diags
!           ntyp = 5   3-node triangles - diags
!           ntyp = 6   3-node triangles - diags union jack
!           ntyp = 7   6-node triangles - diags ll to ur
!           ntyp = 8   8-node quadrilaterals
!           ntyp = 9   9-node quadrilaterals

!           ntyp =-1   3-node triangles - crossed pattern
!           ntyp =-7   7-node triangles - diags ll to ur (bubble node)

!      Inputs:
!         nr        - Number nodes in 1-local coordinate dir.
!         ns        - Number nodes in 2-local coordinate dir.
!         ni        - Initial node number for block
!         ne        - Initial element number for block
!         n         - Number of previous last node on block
!         ndm       - Spatial dimension of mesh
!         nen1      - Dimension of ix array
!         nodinc    - Increment to node numbers at end of each line
!         ntyp      - Block type
!         nm        - Number of nodes on block
!         mat       - Material set number for block
!         ctype     - Type of block coordinates

!      Outputs:
!         n         - Number of last node on block (added by 7-node tris).
!         x(ndm,*)  - Nodal coordinates for block
!         ix(*)     - Element nodal connection list for block
!-----[--.----+----.----+----.-----------------------------------------]

      implicit   none

      include   'cdata.h'
      include   'cdat2.h'
      include   'iofile.h'
      include   'trdata.h'

      character (len=15) :: ctype

      logical       :: ityp, pcomp
      integer       :: i,j,n, me, np,nm,nn,n8, inc
      integer       :: nr,ns,ni,ne,ndm,nen1,nodinc,ntyp,mat,ma,dlayer
      real (kind=8) :: rr,sn,cn, o3

      integer       :: ix(nen1,*),ilr(*)
      integer       :: iq(9),it(6)
      real (kind=8) :: x(ndm,*)

      save

      data       o3 /0.333333333333333d0/

!     Generate elements

      if(dlayer.ge.0) then
        ma = mat
      endif
      if(ne.gt.0) then
        do i = 1,9
          iq(i) = i
        end do ! i
        do i = 1,6
          it(i) = i
        end do ! i
        if(trdet.lt.0.0d0) then
          iq(1) = 4
          iq(2) = 3
          iq(3) = 2
          iq(4) = 1
          iq(5) = 7
          iq(7) = 5
          it(1) = 2
          it(2) = 1
          it(5) = 6
          it(6) = 5
        endif
        me = ne - 1

!       Line generations

        if(nm.lt.4) then
          inc = max(1,min(2,ns))
          nn  = ni
          do i = 1,nr-1,inc
            if(dlayer.eq.1) then
              ma = ilr(inc*i-inc+1)
            endif
            nn = nn + 1
            me = me + 1
            ix(nen1,me) = ma
            ix(nen+7,me) = netyp
            ix(nen+8,me) = 1
            ix(1,me)    = nn - 1
            if(inc.eq.2) then
              ix(3,me) = nn
              nn       = nn + 1
            endif
            ix(2,me) = nn
          end do

!       Block generations

        elseif(ntyp.ge.0 .or. ntyp .eq. -7) then
          inc = 1
          if(abs(ntyp).ge.7) inc = 2
          do j = 1,ns-1,inc
            if(dlayer.eq.2) then
              ma = ilr(inc*j-inc+1)
            endif
            if(ntyp.eq.8) then
              nn = (nr + nodinc*2 + (nr+1)/2)*(j-1)/2 + ni
              n8 =  nn
            else
              nn = (nr + nodinc)*(j-1) + ni
            endif
            do i = 1,nr-1,inc
              if(dlayer.eq.1) then
                ma = ilr(inc*i-inc+1)
              endif
              nn = nn + 1
              me = me + 1
              ix(nen1,me) = ma
              ix(nen+7,me) = netyp
              ix(nen+8,me) = 1
              if(ntyp.eq.0) then
                if(ndm.eq.1) then
                  ix(1,me)     = nn - 1
                  ix(2,me)     = nn
                else
                  ix(iq(1),me) = nn - 1
                  ix(iq(2),me) = nn
                  ix(iq(3),me) = nn + nr + nodinc
                  ix(iq(4),me) = nn + nr - 1 + nodinc
                endif
              elseif(abs(ntyp).eq.7) then
                ix(it(1),me) = nn - 1
                ix(it(4),me) = nn
                ix(it(2),me) = nn + 1
                ix(it(6),me) = nr+nodinc + nn
                ix(it(5),me) = nr+nodinc + nn + 1
                ix(it(3),me) = 2*(nr+nodinc) + nn + 1
                ix(nen+7,me) = netyp
                ix(nen+8,me) = 2
                if(ntyp.eq.-7) then
                  ix(7,me) = n
                  x(1,n)   = (x(1,nn-1)+x(1,nn+1)+x(1,ix(3,me)))*o3
                  x(2,n)   = (x(2,nn-1)+x(2,nn+1)+x(2,ix(3,me)))*o3
                  if(ndm.ge.2 .and. pcomp(ctype,'pola',4)) then
                    call pdegree(x(2,n), sn,cn)
                    rr     = x(1,n)
                    x(1,n) = x0(1) + rr*cn
                    x(2,n) = x0(2) + rr*sn
                  endif
                  n        = n + 1
                endif
                me           = me + 1
                ix(it(1),me) = nn - 1
                ix(it(6),me) = nr+nodinc + nn - 1
                ix(it(4),me) = nr+nodinc + nn
                ix(it(3),me) = 2*(nr+nodinc) + nn - 1
                ix(it(5),me) = 2*(nr+nodinc) + nn
                ix(it(2),me) = 2*(nr+nodinc) + nn + 1
                ix(nen1,me)  = ma
                ix(nen+7,me) = netyp
                ix(nen+8,me) = 2
                if(ntyp.eq.-7) then
                  ix(7,me) = n
                  x(1,n)   = (x(1,nn-1)+x(1,ix(2,me))+x(1,ix(3,me)))*o3
                  x(2,n)   = (x(2,nn-1)+x(2,ix(2,me))+x(2,ix(3,me)))*o3
                  if(ndm.ge.2 .and. pcomp(ctype,'pola',4)) then
                    call pdegree(x(2,n), sn,cn)
                    rr     = x(1,n)
                    x(1,n) = x0(1) + rr*cn
                    x(2,n) = x0(2) + rr*sn
                  endif
                  n        = n + 1
                endif
                nn = nn + 1
              elseif(ntyp.eq.8) then
                ix(iq(1),me) = nn - 1
                ix(iq(2),me) = nn + 1
                ix(iq(3),me) = nr + nodinc + (nr+1)/2 + nodinc + nn + 1
                ix(iq(4),me) = nr + nodinc + (nr+1)/2 + nodinc + nn - 1
                ix(iq(5),me) = nn
                ix(iq(6),me) = nr + nodinc +  n8 + 1
                ix(iq(7),me) = nr + nodinc + (nr+1)/2 + nodinc + nn
                ix(iq(8),me) = nr + nodinc +  n8
                ix(nen+8,me) = 2
                nn = nn + 1
                n8 = n8 + 1
              elseif(ntyp.eq.9) then
                ix(iq(1),me) = nn - 1
                ix(iq(2),me) = nn + 1
                ix(iq(3),me) = 2*(nr+nodinc) + nn + 1
                ix(iq(4),me) = 2*(nr+nodinc) + nn - 1
                ix(iq(5),me) = nn
                ix(iq(6),me) = nr+nodinc + nn + 1
                ix(iq(7),me) = 2*(nr+nodinc) + nn
                ix(iq(8),me) = nr+nodinc + nn - 1
                ix(iq(9),me) = nr+nodinc + nn
                ix(nen+8,me) = 2
                nn = nn + 1
              else
                ityp = (ntyp.eq.1)  .or.
     &                 (ntyp.eq.3.and.mod(j,2)  .eq.1) .or.
     &                 (ntyp.eq.4.and.mod(j,2)  .eq.0) .or.
     &                 (ntyp.eq.5.and.mod(i+j,2).eq.0) .or.
     &                 (ntyp.eq.6.and.mod(i+j,2).eq.1)
                if(ityp) then
                  ix(it(1),me) = nn - 1
                  ix(it(2),me) = nn + nr + nodinc
                  ix(it(3),me) = nn + nr + nodinc - 1
                  me = me + 1
                  ix(it(1),me) = nn - 1
                  ix(it(2),me) = nn
                  ix(it(3),me) = nn + nr + nodinc
                  ix(nen1,me)  = ma
                  ix(nen+7,me) = netyp
                  ix(nen+8,me) = 1
                else
                  ix(it(1),me) = nn - 1
                  ix(it(2),me) = nn
                  ix(it(3),me) = nn + nr + nodinc - 1
                  me = me + 1
                  ix(it(1),me) = nn
                  ix(it(2),me) = nn + nr + nodinc
                  ix(it(3),me) = nn + nr + nodinc - 1
                  ix(nen1,me)  = ma
                  ix(nen+7,me) = netyp
                  ix(nen+8,me) = 1
                endif
              endif
            end do
          end do
        elseif(ntyp.eq. -1) then
          do j = 1,ns-1
            if(dlayer.eq.2) then
              ma = ilr(j)
            endif
            n  = (2*nr+nodinc-1)*(j-1) + ni
            nn = n + 2*nr + nodinc - 1
            inc = 2
            if(j.eq.ns-1) inc = 1
            do i = 1,nr-1
              if(dlayer.eq.1) then
                ma = ilr(i)
              endif
              n            = n  + 1
              np           = nn + inc
              me           = me + 1
              ix(nen1,me)  = ma
              ix(it(1),me) = n - 1
              ix(it(2),me) = n + 1
              ix(it(3),me) = n
              me           = me + 1
              ix(nen1,me)  = ma
              ix(it(1),me) = n + 1
              ix(it(2),me) = np
              ix(it(3),me) = n
              me           = me + 1
              ix(nen1,me)  = ma
              ix(it(1),me) = np
              ix(it(2),me) = nn
              ix(it(3),me) = n
              me           = me + 1
              ix(nen1,me)  = ma
              ix(it(1),me) = nn
              ix(it(2),me) = n - 1
              ix(it(3),me) = n
              n            = n  + 1
              nn           = nn + inc
            end do
          end do
        endif
      endif

!     Set final element number

      n = me

      end subroutine sblke

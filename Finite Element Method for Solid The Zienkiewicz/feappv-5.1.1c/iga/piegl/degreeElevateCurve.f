!$Id:$
      subroutine degreeElevateCurve(np,pp,Up,Pw, t,Qw,Uq,pq, nd1)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: This function degree elevates by t a given NURBS curve
!               and refines control points Pw to Qw such that the
!               curve does not change shape.

!      Reference: L. Piegl & W. Tiler, "The NURBS Book".
!                 Springer-Verlag: Berlin 1995; p. 206.

!      Inputs:
!        np           - Order [n+pp]
!        pp           - Old polynomial order
!        Up(0:*)      - Old knot vector
!        Pw(nd1,0:*)  - Old NURB control points
!        t            - Elevation factor
!        nd1          - Space dimension + 1

!      Outputs:
!        Qw(nd1,0:*)  - New NURB control points
!        Uq(0:*)      - New knot vector [np+(d_knot-1)*t]
!        pq           - New polynomial order [pp + t]
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer    np, nq, pp, t, pq, nd1
      integer    i, j, k,kj, a,b,c,d, ppi, pb, qh, s, lbz,rbz
      integer    cind, kind, bi, rp, rb, first, last
      real*8     binom, alf, bet, gam, inv, ua, ub

      real*8     Up(0:*), Pw(nd1,0:*), Uq(0:*), Qw(nd1,0:*)
      real*8     bpts(nd1,0:pp), ebpts(nd1,0:pp+t), Nextbpts(nd1,0:pp)
      real*8     alphas(pp), bezalfs(0:pp+t,0:pp)

      nq = np + pp
      pq = pp + t
      qh = pq/2

!     Compute Bezier degree elevation coefficients

      bezalfs(0,0)   = 1.0d0
      bezalfs(pq,pp) = 1.0d0

      do i = 1,qh
        inv = 1.0d0/binom(pq,i)
        ppi = min(pp,i)
        do j = max(0,i-t),ppi
          bezalfs(i,j) = inv*binom(pp,j)*binom(t,i-j)
        enddo ! j
      enddo ! i

      do i = qh+1,pq-1
        ppi = min(pp,i)
        do j = max(0,i-t),ppi
          bezalfs(i,j) = bezalfs(pq-i,pp-j)
        enddo ! j
      enddo ! i

      pb      =  pq
      kind    =  pq + 1
      rb      = -1
      a       =  pp
      b       =  pp + 1
      cind    =  1
      ua      =  Up(0)
      do d = 1,nd1
        Qw(d,0) = Pw(d,0)
      end do ! d

      do i = 0,pq
        Uq(i) = ua
      enddo ! i

!     Initialize first Bezier segment

      do i = 0,pp
        do d = 1,nd1
          bpts(d,i) = Pw(d,i)
        enddo ! d
      enddo ! i

!     Big loop through knot vector

      do while (b.lt.nq)
        i = b

!       Skip repeated knots

        do while ((b.lt.nq).and.(Up(b).eq.Up(b+1)))
          b = b + 1
        enddo ! while
        bi = b  - i  + 1
        pb = pb + bi + t
        ub = Up(b)
        rp = rb
        rb = pp - bi

!       Insert knot u(b) r times

        if (rp.gt.0) then
          lbz = (rp + 2)/2
        else
          lbz = 1
        endif
        if (rb.gt.0) then
          rbz = pq - (rb + 1)/2
        else
          rbz = pq
        endif
        if (rb.gt.0) then

!         Insert knot to get Bezier segment

          do k = pp,bi+1,-1
            alphas(k-bi) = (ub - ua)/(Up(a+k) - ua)
          enddo ! k
          do j = 1,rb
            s = bi + j
            do k = pp,s,-1
              do d = 1,nd1
                bpts(d,k) = alphas(k-s+1)*(bpts(d,k) - bpts(d,k-1))
     &                    + bpts(d,k-1)
              end do ! d
            enddo ! k
            do d = 1,nd1
              Nextbpts(d,rb-j) = bpts(d,pp)
            end do ! d
          enddo ! j
        endif            ! End of insert knot

!       Degree elevate Bezier (Only points lbz,,,pq are used below)

        do i = lbz,pq
          do d = 1,nd1
            ebpts(d,i) = 0.0d0
          end do ! k
          ppi = min(pp,i)
          do j = max(0,i-t),ppi
            do d = 1,nd1
              ebpts(d,i) = ebpts(d,i) + bezalfs(i,j)*bpts(d,j)
            enddo ! d
          enddo ! j
        enddo ! i        ! End of degree elevating Bezier

!       Must remove knot u = Uq(a) rp times

        if (rp.gt.1) then
          first = kind - 2
          last  = kind
          bet   = (ub - Uq(kind-1))/(ub - ua)

!         Knot removal loop

          do c = 1,rp-1
            i = first
            j = last
            kj = j - kind + 1

!           Loop to compute new control points for one removal step

            do while((j-i).gt.c)
              if (i.lt.cind) then
                alf = (ub - Uq(i))/(ua - Uq(i))
                do d = 1,nd1
                  Qw(d,i) = alf*(Qw(d,i) - Qw(d,i-1)) + Qw(d,i-1)
                end do ! d
              endif
              if (j.ge.lbz) then
                if ((j-c).le.(kind-pq+rp)) then
                  gam = (ub - Uq(j-c))/(ub - ua)
                else
                  gam = bet
                endif
                do d = 1,nd1
                  ebpts(d,kj) = gam*(ebpts(d,kj) - ebpts(d,kj+1))
     &                        + ebpts(d,kj+1)
                end do ! d
              endif
              i  = i  + 1
              j  = j  - 1
              kj = kj - 1
            enddo ! while
            first = first - 1
            last  = last  + 1
          enddo ! c
         endif            ! End of removing knot, u = Uq(a)

!        Load knot ua

         if (a.ne.pp) then
           do i = 0,(pq-rp-1)
             Uq(kind) = ua
             kind     = kind + 1
           enddo ! i
         endif

!        Load control points into Qw

         do j = lbz,rbz
           do d = 1,nd1
             Qw(d,cind) =  ebpts(d,j)
           end do ! d
           cind = cind + 1
         enddo ! j

!        Set up for next pass through loop

         if (b.lt.nq) then
           do j = 0,rb-1
             do d = 1,nd1
               bpts(d,j) = Nextbpts(d,j)
             end do ! d
           enddo ! j
           do j = rb,pp
             do d = 1,nd1
               bpts(d,j) = Pw(d,b-pp+j)
             end do ! d
           enddo ! j
           a  = b
           b  = b + 1
           ua = ub

!        End knots

         else

           do i = 0,pq
             Uq(kind+i) = ub
           enddo ! i
         endif
      enddo ! End of while-loop (b < m)

      end

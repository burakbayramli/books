!$Id:$
      subroutine degree_elevate_curve(np,pp,Up,Pw, t,Qw,Uq,pq,nd1)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: This function degree elevates by t a given NURBS curve
!               and refines projective control points Pw such that the
!               curve does not change shape.

!      Adapted from algorithm in Piegl, Les. "The NURBS Book".
!                                Springer-Verlag: Berlin 1995; p. 206.

!      October 3, 2003
!      J. Austin Cottrell
!      CES Graduate Student
!      Texas Institute for Computational Engineering Science
!      University of Texas at Austin
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer    np, nq, pp, t, pq, nd1
      integer    i, j, k,kj, a,b,c,d, ppi, pb, qh, s, lbz,rbz
      integer    cind, kind, bi, rb, rp, first, last
      real*8     binom, alf, bet, gam, inv, ua, ub

      real*8     Up(*), Pw(nd1,*), Uq(*), Qw(nd1,*)
      real*8     bpts(nd1,pp+1), ebpts(nd1,pp+t+1)
      real*8     Nextbpts(nd1,pp-1)
      real*8     alphas(pp-1), bezalfs(pp+t+1,pp+1)

      nq = np + pp
      pq = pp + t
      qh = pq/2                ! note that this takes the floor(pq/2)

!     Compute Bezier degree elevation coefficients

      bezalfs(1,1)       = 1.0d0
      bezalfs(pq+1,pp+1) = 1.0d0

      do i = 1,qh
        inv = 1.0/binom(pq,i)
        ppi = min(pp,i)
        do j = max(0,i-t),ppi
          bezalfs(i+1,j+1) = inv*binom(pp,j)*binom(t,i-j)
        enddo ! j
      enddo ! i

      do i = qh+1,pq-1
        ppi = min(pp,i)
        do j = max(0,i-t),ppi
          bezalfs(i+1,j+1) = bezalfs(pq-i+1,pp-j+1)
        enddo ! j
      enddo ! i

      call mprint(bezalfs,pp+t+1,pp+1,pp+t+1,'BEZALFS_o')

      pb      =  pq
      kind    =  pq+1
      rb      = -1
      a       =  pp
      b       =  pp + 1
      cind    =  1
      ua      =  Up(1)
      do d = 1,nd1
        Qw(d,1) = Pw(d,1)
      end do ! d

      do i = 0,pq
        Uq(i+1) = ua
      enddo ! i

!     Initialize first Bezier segment

      do i = 0,pp
        do d = 1,nd1
          bpts(d,i+1) = Pw(d,i+1)
        end do ! d
      enddo ! i

!     Big loop through knot vector

      do while (b.lt.nq)
        i = b

!       Skip repeated knots

        do while ((b.lt.nq).and.(Up(b+1).eq.Up(b+2)))
          b = b+1
        enddo ! while
        bi = b  - i  + 1
        pb = pb + bi + t
        ub = Up(b+1)
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
            alphas(k-bi) = (ub - ua)/(Up(a+k+1) - ua)
          enddo ! k
          do j = 1,rb
            s = bi+j
            do k = pp,s,-1
              do d = 1,nd1
                bpts(d,k+1) = alphas(k-s+1)*(bpts(d,k+1) - bpts(d,k))
     &                      + bpts(d,k)
              end do ! d
            enddo ! k
            do d = 1,nd1
              Nextbpts(d,rb-j+1) = bpts(d,pp+1)
            end do ! d
          enddo ! j

          call mprint(alphas,1,pp-1,1,'ALPHA_o')
          call mprint(bpts,nd1,pp+1,nd1,'BPTS_o')
          call mprint(Nextbpts,nd1,pp-1,nd1,'Nextbpts_o')
        endif               ! End of insert knot

!       Degree elevate Bezier (olnly points lbz,,pq are used below)

        do i = lbz,pq
          do d = 1,nd1
            ebpts(d,i+1) = 0.0d0
          end do ! d
          ppi          = min(pp,i)
          do j = max(0,i-t),ppi
            do d = 1,nd1
              ebpts(d,i+1) = ebpts(d,i+1) + bezalfs(i+1,j+1)*bpts(d,j+1)
            end do ! d
          enddo ! j
        enddo ! i          ! End of degree elevating Bezier

        call mprint(ebpts,nd1,pp+t+1,nd1,'EBPTS_o')
!       Must remove knot u = Uq(a) rp times

        if (rp.gt.1) then
          first = kind - 2
          last  = kind
          bet   = (ub - Uq(kind))/(ub - ua)
          do c = 1,rp-1
            i = first
            j = last
            kj = j-kind+1

!           Loop to compute new control points for one removal step

            do while((j-i).gt.c)
              if (i.lt.cind) then
                alf = (ub - Uq(i+1))/(ua - Uq(i+1))
                do d = 1,nd1
                  Qw(d,i+1) = alf*(Qw(d,i+1) - Qw(d,i)) + Qw(d,i)
                end do ! d
              endif
              if (j.ge.lbz) then
                if ((j-c).le.(kind-pq+rp)) then
                  gam = (ub - Uq(j-c+1))/(ub - ua)
                else
                  gam = bet
                endif
                do d = 1,nd1
                  ebpts(d,kj+1) = gam*(ebpts(d,kj+1) - ebpts(d,kj+2))
     &                          + ebpts(d,kj+2)
                end do ! d
              endif
              i  = i  + 1
              j  = j  - 1
              kj = kj - 1
            enddo ! while
            call mprint(Qw,nd1,cind,nd1,'QW_o')
            call mprint(ebpts,nd1,pp+t+1,nd1,'EBPTS_o1')

            first = first - 1
            last  = last  + 1
          enddo ! c
         endif            ! End of removing knot, u = Uq(a)

!        Load knot ua

         if (a.ne.pp) then
           do i = 0,(pq-rp-1)
             Uq(kind+1) = ua
             kind       = kind + 1
           enddo ! i
         endif

!        Load control points into Qw

         do j = lbz,rbz
           do d = 1,nd1
             Qw(d,cind+1) =  ebpts(d,j+1)
           end do ! d
           cind = cind +1
         enddo ! j

!        Set up for next pass through loop

         if (b.lt.nq) then
           do j = 0,rb-1
             do d = 1,nd1
               bpts(d,j+1) = Nextbpts(d,j+1)
             end do ! d
           enddo ! j
           do j = rb,pp
             do d = 1,nd1
               bpts(d,j+1) = Pw(d,b-pp+j+1)
             end do ! d
           enddo ! j
           a  = b
           b  = b + 1
           ua = ub

!        End knots

         else
           do i = 0,pq
             Uq(kind+i+1) = ub
           enddo ! i
         endif
      enddo ! End of while-loop (b < m)

      end

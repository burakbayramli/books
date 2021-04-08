!$Id:$
      subroutine CurveKnotIns(np,p, Up,Pw, uu, k,s,r,nq, Uq,Qw, ndm)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Compute new curve from knot insertion at uu for r times.
!               u_k .le. uu .lt. u_k+1 or uu = [u_k,u_k+1)

!      Inputs :
!        np         - Number control points  (original)
!        p          - Order                  (unchanged)
!        Up(*)      - Knot vector            (original)
!        Pw(ndm,*)  - Control points         (original)
!        uu         - Insertion value
!        k          - Insertion location     (k .ge. 0)
!        s          - Initial multiplicity at location "k"
!        r          - Number of repeats      (r + s .le. p)

!      Outputs:
!        nq         - Number control points  (revised)
!        Uq(*)      - Knot vector            (revised)
!        Qw(ndm,*)  - Control point          (revised)
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      integer   np,p, k,s,r,nq, ndm
      real*8    uu, alpha
      real*8    Up(0:*),Uq(0:*), Pw(ndm+1,0:*), Qw(ndm+1,0:*)

      integer   mp, i,j,l,nd
      real*8    Rw(ndm+1,0:p)

!     Set parameters

      mp = np + p + 1  ! Number of original knots
      nq = np + r      ! Final number of control points

!     Load new knot vector

      do i = 0,k
        Uq(i) = Up(i)
      end do ! i

      do i = 1,r
        Uq(i+k) = uu
      end do ! i

      do i = k+1,mp
        Uq(i+r) = Up(i)
      end do ! i

!     Save unaltered control points in new vector

      do i = 0,np+1 ! WAS 1,np+1
        do nd = 1,ndm+1
          Qw(nd,i) = 0.0d0
        end do ! nd
      end do ! i

      do i = 0,k-p
        do nd = 1,ndm+1
          Qw(nd,i) = Pw(nd,i)
        end do ! nd
      end do ! i
      do i = k-s,np
        do nd = 1,ndm+1
          Qw(nd,i+r) = Pw(nd,i)
        end do ! nd
      end do ! i
      do i = 0,p-s
        do nd = 1,ndm+1
          Rw(nd,i) = Pw(nd,k-p+i)
        end do ! nd
      end do ! i

!     Insert new knot r times

      do j = 1,r
        l = k-p+j
        do i = 0,p-j-s
          alpha = (uu - Up(l+i))/(Up(i+k+1) - Up(l+i))
          do nd = 1,ndm+1
            Rw(nd,i) = alpha*Rw(nd,i+1) + (1.0d0 - alpha)*Rw(nd,i)
          end do ! nd
        end do ! i
        do nd = 1,ndm+1
          Qw(nd,l)       = Rw(nd,0)
          Qw(nd,k+r-j-s) = Rw(nd,p-j-s)
        end do ! nd
      end do ! j

!     Load remaining control points

      if(r.gt.0) then
        do i = l+1,k-s-1
          do nd = 1,ndm+1
             Qw(nd,i) = Rw(nd,i-l)
          end do ! nd
        end do ! i
      endif

      end

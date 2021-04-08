!$Id:$
      subroutine DecomposeCurve(n, p, U, Pw, nb, Qw, ndm)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Decompose curve into Bezier Segments

!     Inputs:
!       n             - Number control points
!       p             - Degree of polynomial
!       U(*)          - Knot vector
!       Pw(ndm+1,*)   - Control array

!     Outputs:
!       nb            - Number Bezier points
!       Qw(ndm+1,*,*) - Bezier Control points
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer    n,p, nb, ndm
      real*8     U(0:*), Pw(4,0:*), Qw(4,0:5,*)

      integer    m, a,b, i,j,k, r,s, mult,save
      real*8     numer, alpha, alphas(0:p)

      m  = n + p + 1
      a  = p
      b  = p + 1
      nb = 1

      do i = 0,p
        do j = 1,ndm+1
         Qw(j,i,nb) = Pw(j,i)
        end do ! j
      end do ! i

      do while (b.lt.m)
        i = b
        do while (b .lt. m .and. U(b+1) .eq. U(b) )
          b = b + 1
        end do ! while
        mult = b - i + 1

        if(mult .lt. p) then
          numer = U(b) - U(a)  ! Numerator of alpha

!         Compute and store alphas

          do j = p,mult+1,-1
            alphas(j-mult-1) = numer/(U(a+j) - U(a))
          end do ! j

!         Insert knot r times

          r = p - mult
          do j = 1,r
            save = r - j
            s    = mult + j ! This many new points
            do k = p,s,-1
              alpha = alphas(k-s)
              do i = 1,ndm+1
                Qw(i,k,nb) = alpha*Qw(i,k,nb)
     &                     + (1.0d0 - alpha)*Qw(i,k-1,nb)
              end do ! i
            end do ! k

!           Control points of next segment

            if(b .lt. m) then
              do i = 1,ndm+1
                Qw(i,save,nb+1) = Qw(i,p,nb)
              end do ! i
            endif
          end do ! j

!         Bezier segment completed`

!         Initialize for next segment

          if(b .lt. m) then
            nb = nb + 1
            do i = p-mult,p
              do j = 1,ndm+1
                Qw(j,i,nb) = Pw(j,b-p+i)
              end do ! j
            end do ! i
            a = b
            b = b + 1
          endif
        endif

      end do ! while

!     Return number of segments found

      nb = nb - 1

      end

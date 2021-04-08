!$Id:$
      subroutine ExtractCurve(n, p, U, nb, Ce)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!     Purpose: Decompose curve into Bezier Segments

!     Inputs:
!       n             - Number control points
!       p             - Degree of polynomial
!       U(*)          - Knot vector

!     Outputs:
!       nb            - Number Bezier points
!       Ce(0:p,0:p,*) - Extraction operator
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer    n,p, nb
      real*8     U(0:*), Ce(0:p,0:p,*)

      integer    m, a,b, i,j,k, r,s, mult,save
      real*8     numer, alpha, alphas(0:p)

      m  = n + p + 1
      a  = p
      b  = p + 1
      nb = 1

      do i = 0,p
        do j = 0,p
         Ce(j,i,nb) = 0.0d0
        end do ! j
        Ce(i,i,nb) = 1.0d0
      end do ! i

      do while (b.lt.m)

        do i = 0,p
          do j = 0,p
           Ce(j,i,nb+1) = 0.0d0
          end do ! j
          Ce(i,i,nb+1) = 1.0d0
        end do ! i

        i = b
!       do while (b .lt. m .and. U(b+1) .eq. U(b) )
        do while (U(b+1) .eq. U(b) )
          b = b + 1
          if(b.ge.m-1) exit
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
              do i = 0,p
                Ce(k,i,nb) = alpha*Ce(k,i,nb)
     &                     + (1.0d0 - alpha)*Ce(k-1,i,nb)
              end do ! i
            end do ! k

!           Control points of next segment

            if(b .lt. m) then
              s = p - j - save
              do i = save,j+save
                Ce(save,i,nb+1) = Ce(p,i+s,nb)
              end do ! i
            endif
          end do ! j

        endif
!       Initialize for next segment

        nb = nb + 1
        a = b
        b = b + 1

      end do ! while

!     Return number of segments found

      nb = nb - 1

      end

!$Id:$
      subroutine DersOneBasisFun(p,m,U,i,uu,n, ders)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: compute derivatives of basis funciton N_i,p

!      Algorithm A2.5: The NURBS Book, Page 76

!      Inputs :
!         p         - Order of basis function
!         m         - Highest knot position in U(0:m)
!         U(0:m)    - Knot vector positions
!         i         - Function to compute
!         uu        - Position on knot interval
!         n         - Order of highest derivative (n <= p)

!      Outputs:
!         ders(k)   - Derivatives
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer    p,m,i,n
      real*8     U(0:m), uu, ders(0:m)

      integer    j,jj,k
      real*8     Uleft,Uright, saved, temp
      real*8     Nb(0:p,0:p), Nd(0:p)

!     Local property

      if(uu .lt. U(i) .or. uu .gt. U(i+p+1)) then
        do k = 0,n
          ders(k) = 0.0d0
        end do ! k
        return
      endif

!     Initialize zeroth-degree functions

      do j = 0,p
        if(uu.ge. U(i+j) .and. uu .lt. U(i+j+1)) then
          Nb(j,0) = 1.0d0
        else
          Nb(j,0) = 0.0d0
        endif
      end do ! j

!     Compute full triangular table

      do k = 1,p
        if(Nb(0,k-1) .eq. 0.0d0) then
          saved = 0.0d0
        else
          saved = ((uu-U(i))*Nb(0,k-1))/(U(i+k) - U(i))
        endif
        do j = 0,p-k
          Uleft  = U(i+j+1)
          Uright = U(i+j+k+1)
          if(Nb(j+1,k-1).eq.0.0d0) then
            Nb(j,k) = saved
            saved   = 0.0d0
          else
            temp    = Nb(j+1,k-1)/(Uright - Uleft)
            Nb(j,k) = saved + (Uright - uu)*temp
            saved   = (uu - Uleft)*temp
          endif
        end do ! j
      end do ! k

!     The function values

      ders(0) = Nb(0,p)

!     Compute the derivatives

      do k = 1,n
        do j = 0,k
          Nd(j) = Nb(j,p-k)
        end do ! j
!       Compute table of width k
        do jj = 1,k
          if(Nd(0) .eq. 0.0d0) then
            saved = 0.0d0
          else
            saved = Nd(0)/(U(i+p-k+jj) - U(i))
          end if
          do j = 0,k-jj
            Uleft   = U(i+j+1)
!           Uright  = U(i+j+p+jj+1)
            Uright  = U(i+j+p+jj)
            if(Nd(j+1) .eq. 0.0d0) then
              Nd(j) = dble(p-k+jj)*saved
              saved  = 0.0d0
            else
              temp   = Nd(j+1)/(Uright - Uleft)
              Nd(j)  = dble(p-k+jj)*(saved - temp)
              saved  = temp
            endif
          end do ! j
        end do ! jj

!       k-th derivative

        ders(k) = Nd(0)
      end do ! end do ! k

      end

!$Id:$
      subroutine dlbspline(p,U,uu, ders)

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: compute local basis function N_0,p and its derivative

!      Adapted from Algorithm A2.5: The NURBS Book, Page 76

!      Inputs :
!         p         - Order of basis function
!         U(0:p+1)  - Knot vector positions
!         uu        - Position on knot interval

!      Outputs:
!         ders(0:1) - Function and derivative
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      integer    p
      real*8     U(0:*), uu, ders(0:1)

      integer    j,k
      real*8     Uleft,Uright, saved, temp
      real*8     N(0:p,0:p), Nd(0:p)

!     Local property

      if(uu .lt. U(0) .or. uu .gt. U(p+1)) then
        ders(0) = 0.0d0
        ders(1) = 0.0d0
        return
      endif

!     Initialize zeroth-degree functions

      do j = 0,p
        if(uu.ge. U(j) .and. uu .lt. U(j+1)) then
          N(j,0) = 1.0d0
        else
          N(j,0) = 0.0d0
        endif
      end do ! j

!     Compute triangular table

      do k = 1,p
        if(N(0,k-1) .eq. 0.0d0) then
          saved = 0.0d0
        else
          saved = ((uu-U(0))*N(0,k-1))/(U(k) - U(0))
        endif
        do j = 0,p-k
          Uleft  = U(j+1)
          Uright = U(j+k+1)
          if(N(j+1,k-1).eq.0.0d0) then
            N(j,k) = saved
            saved   = 0.0d0
          else
            temp    = N(j+1,k-1)/(Uright - Uleft)
            N(j,k) = saved + (Uright - uu)*temp
            saved   = (uu - Uleft)*temp
          endif
        end do ! j
      end do ! k

!     Function value

      ders(0) = N(0,p)

!     Compute derivative

      do j = 0,1
        Nd(j) = N(j,p-1)
      end do ! j

!     Compute table of width 1

      if(Nd(0) .ne. 0.0d0) then
        Nd(0) = Nd(0)/(U(p) - U(0))
      end if

!     Derivative

      if(Nd(1) .eq. 0.0d0) then
        ders(1) = dble(p)*Nd(0)
      else
        ders(1) = dble(p)*(Nd(0) - Nd(1)/(U(p+1) - U(1)))
      endif

      end

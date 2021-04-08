!$Id:$
      subroutine chlfwd(u,g,s,nn)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Use Cholesky factors to project onto a standard eigenproblem

!      Inputs:
!         g(*)  - Symmetric projected matrix
!         u(*)  - Upper factor for projection
!         nn    - Size of arrays

!      Outputs:
!         s(*,*) - Projected array

!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer       :: i,j,id,im,jd,nn
      real (kind=8) :: u(*),g(*),s(nn,nn), dot

!     Choleski factorization of a symmetric, positive definite matrix

      u(1) = 1.d0/sqrt(abs(u(1)))
      jd   = 1
      do j = 2,nn
        id = 0
        do i = 1,j-1
          if(i.gt.1) u(jd+i) = u(jd+i) - dot(u(id+1),u(jd+1),i-1)
          id = id + i
          u(jd+i) = u(jd+i)*u(id)
        end do
        u(jd+j) = 1.d0/sqrt(abs(u(jd+j) - dot(u(jd+1),u(jd+1),j-1)))
        jd = jd + j
      end do

!     Perform forward solutions to get projected matrix

      s(1,1) = g(1)*u(1)
      id = 1
      do i = 2,nn
        s(1,i) = g(id+1)*u(1)
        im = i - 1
        jd = 0
        do j = 1,im
         s(i,j) = (g(id+j) - dot(u(id+1),s(1,j),im))*u(id+i)
         if(j.gt.1) s(j,i) = (g(id+j)-dot(u(jd+1),s(1,i),j-1))*u(jd+j)
         jd = jd + j
        end do
        id = id + i
        s(i,i) = (g(id) - dot(u(id-im),s(1,i),im))*u(id)
      end do

!     Complete projection

      g(1) = s(1,1)*u(1)
      jd = 2
      do j = 2,nn
        g(jd) = s(j,1)*u(1)
        id = 2
        do i = 2,j
          im = i - 1
          g(jd+im) = (s(j,i) - dot(u(id),g(jd),im))*u(id+im)
          id = id + i
        end do
        jd = jd + j
      end do

      end subroutine chlfwd

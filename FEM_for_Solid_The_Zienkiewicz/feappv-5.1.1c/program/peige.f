!$Id:$
      subroutine peige(s,nst, dr,vflg)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Compute the eigenvalues and vectors for last
!                computed element array (numel array)

!      Inputs:
!         s(nst,*) - Last element array
!         nst      - Dimension of element array
!         vflg     - Flag, compute vectors if true

!      Outputs:
!         dr(*)    - Array of element eigenvalues and vectors
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'p_point.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: vflg
      integer       :: nst, i, j, j1, n1,n2,n3
      real (kind=8) :: s(nst,nst), dr(*)

      save

!     Set pointers

      n1 =  1 +  nst
      n2 = n1 + (nst*(nst+1))/2
      n3 = n2 +  nst*nst

!     Load stiffness terms into triangular matrix

      j1 = -1
      do j = 1,nst
        do i = 1,j
          dr(n1+j1+i) = s(i,j)
        end do
        j1 = j1 + j
      end do

!     Compute eigenpairs for last element computed

      call eisql(dr(n1),dr(1),dr(n3),dr(n2),nst,j1)

!     Move eigenvalues and vectors to 'EIGE' storage for plots.

      do i = 0,nst*nst-1
        hr(np(75)+i) = dr(n2+i)
      end do
      point = np(75) + nst*nst - 1
      do i = 1,nst
        hr(point+i) = dr(i)
      end do

!     Output eigenpairs for last element

      call mprint (  dr,       1, nst,  1,'Eigenvalue')
      if(vflg) then
        call mprint (dr(n2), nst, nst,nst,'Eigenvect.')
      endif

      end subroutine peige

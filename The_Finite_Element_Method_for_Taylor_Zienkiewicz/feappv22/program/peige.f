c$Id:$
      subroutine peige(s,nst, dr,vflg)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Compute the eigenvalues and vectors for last
c                computed element array (numel array)

c      Inputs:
c         s(nst,*) - Last element array
c         nst      - Dimension of element array
c         vflg     - Flag, compute vectors if true

c      Outputs:
c         dr(*)    - Array of element eigenvalues and vectors
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'pointer.h'
      include  'comblk.h'


      logical   vflg
      integer   nst, i, j, j1, n1,n2,n3
      integer   nty
      real*8    s(nst,nst), dr(*)

      save

c     Set pointers

      n1 =  1 +  nst
      n2 = n1 + (nst*(nst+1))/2
      n3 = n2 +  nst*nst

c     Load stiffness terms into triangular matrix

      j1 = -1
      do j = 1,nst
        do i = 1,j
          dr(n1+j1+i) = s(i,j)
        end do
        j1 = j1 + j
      end do

c     Compute eigenpairs for last element computed

      call eisql(dr(n1),dr(1),dr(n3),dr(n2),nst,j1)

c     Move eigenvalues and vectors to 'EIGE' storage for plots.

      do i = 0,nst*nst-1
        hr(np(75)+i) = dr(n2+i)
      end do
      nty = np(75) + nst*nst - 1
      do i = 1,nst
        hr(nty+i) = dr(i)
      end do

c     Output eigenpairs for last element

      call mprint (  dr,       1, nst,  1,'Eigenvalue')
      if(vflg) then
        call mprint (dr(n2), nst, nst,nst,'Eigenvect.')
      endif

      end

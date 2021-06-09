!$Id:$
      subroutine paddv(vk,ve,nneq,tau,id)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Add scaled eigenvector to displacement vector

!      Inputs:
!         vk(*)   - Original displacement vector
!         ve(*)   - Eigenvector
!         nneq    - Size of displacement and equation number arrays
!         tau     - Modification factor
!         id(*)   - Equation number array

!      Outputs:
!         vk(*)   - Modified displacement vector
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer       :: nneq,i,j, id(*)
      real (kind=8) :: tau,vknorm,venorm,xsi, vk(*),ve(*), dot

      save

      vknorm = sqrt(dot(vk,vk,nneq))
      venorm = sqrt(dot(ve,ve,nneq))
      xsi = vknorm / (venorm * tau)
      do i = 1,nneq
        j = id(i)
        if (j.gt.0) vk(i) = vk(i) + xsi * ve(j)
      end do

      write(iow,2000) vknorm,venorm,xsi
      if(ior.lt.0) write(*,2000) vknorm,venorm,xsi

!     Format

2000  format(/,3x,'Norm displ. vector  = ',g12.5,/,
     &         3x,'Norm eigenvector    = ',g12.5,/,
     &         3x,'Scaling factor      = ',g12.5,/)

      end subroutine paddv

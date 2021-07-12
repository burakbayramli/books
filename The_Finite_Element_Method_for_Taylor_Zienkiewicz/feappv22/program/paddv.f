c$Id:$
      subroutine paddv(vk,ve,nneq,tau,id)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Add scaled eigenvector to displacement vector

c      Inputs:
c         vk(*)   - Original displacement vector
c         ve(*)   - Eigenvector
c         nneq    - Size of displacement and equation number arrays
c         tau     - Modification factor
c         id(*)   - Equation number array

c      Outputs:
c         vk(*)   - Modified displacement vector
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer   nneq,i,j, id(*)
      real*8    tau,vknorm,venorm,xsi, vk(*),ve(*), dot

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

c     Format

2000  format(/,3x,'Norm displ. vector  = ',g12.5,/,
     &         3x,'Norm eigenvector    = ',g12.5,/,
     &         3x,'Scaling factor      = ',g12.5,/)

      end

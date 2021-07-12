c$Id:$
      subroutine ptrans(ia,angl,ul,p,s,nel,ndf,nst,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set transformation data for element computations
c               with sloping boundary conditions

c      Inputs:
c         ia(*)     - Degrees of freedom to rotate
c         angl(*)   - Array of element nodal angles
c         nel       - Number of nodes on element
c         ndf       - Number dof/node
c         nst       - Dimension of element arrays
c         isw       - Switch: rotate ul if isw=1; otherwise element
c                     arrays

c      Outputs:
c         ul(*)     - Element solution variables
c         p(*)      - Element vector
c         s(*,*)    - Element matrix
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   nel,ndf,nst,isw, i1,ij1,ij2, i, j
      real*8    cs,sn,tm

      integer   ia(2)
      real*8    angl(*),ul(nst,4),p(ndf,*),s(nst,nst)

      save

c     Subroutine to make two-dimesional rotations

      ij1 = ia(1)
      ij2 = ia(2)

      if(ndf.le.1) return

c     Transform displacement quantities to element coordinates

      if(isw.eq.1) then
        do i = 1,nel
          if(angl(i).ne.0.0d0) then
            call pdegree(angl(i), sn,cs)
            do j = 1,6
              tm        = cs*ul(ij1,j) - sn*ul(ij2,j)
              ul(ij2,j) = sn*ul(ij1,j) + cs*ul(ij2,j)
              ul(ij1,j) = tm
            end do
          endif
          ij1 = ij1 + ndf
          ij2 = ij2 + ndf
        end do

c     Transform element arrays to global coordinates

      else
        i1 = 0
        do i = 1,nel
          if(angl(i).ne.0.0d0) then
            call pdegree(angl(i), sn,cs)

c           Transform load vector

            tm       = cs*p(ij1,i) + sn*p(ij2,i)
            p(ij2,i) =-sn*p(ij1,i) + cs*p(ij2,i)
            p(ij1,i) = tm
            if(isw.eq.2) then

c             Postmultiply s by transformation

              do j = 1,nst
                tm         = s(j,i1+ij1)*cs + s(j,i1+ij2)*sn
                s(j,i1+ij2)=-s(j,i1+ij1)*sn + s(j,i1+ij2)*cs
                s(j,i1+ij1)= tm
              end do

c             Premultiply s by transformation

              do j = 1,nst
                tm         = cs*s(i1+ij1,j) + sn*s(i1+ij2,j)
                s(i1+ij2,j)=-sn*s(i1+ij1,j) + cs*s(i1+ij2,j)
                s(i1+ij1,j)= tm
              end do
            endif
          endif
          i1 = i1 + ndf
        end do
      endif

      end

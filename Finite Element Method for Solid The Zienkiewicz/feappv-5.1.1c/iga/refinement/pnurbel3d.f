!$Id:$
      subroutine pnurbel3d(knotdiv,ix,
     &                     knots,nsides,lknot,nblk,ktnum,nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose:  Construct 3-d block of elements from nurb sides

!     Inputs:

!     Outputs:
!       knotdiv(4,*,*) - Knot division array
!       ix(nen1,*)     - Element array for NURBS
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'print.h'
      include   'p_ptname.h'
      include   'sdata.h'

      real*8     knots(dknotig,*)
      integer    knotdiv(4,kdiv,*), ix(nen1,*)
      integer    nsides(dsideig,*),lknot(0:4,*),nblk(14,*)
      integer    ktnum(6,*),nblksd(dblokig,*)

!     Local variables
      integer    i,j,k, i1,j1,k1,l1,l2, nb,ns, e, node

!     Loop through block numbers

      e   = estart
      do nb = 1,nurbk

        if(nblk(1,nb).eq.3) then

!         Determine knot spacing

          ptelm(1,nb) = e + 1
          ptelm(3,nb) = 0
          call pknotel(knots,lknot,ktnum(1,nb),
     &                 ktnum(4,nb),knotdiv(1,1,nb),3)

!         Loop through control points assigning global node to IX array

          l2 = knotdiv(4,ktnum(4,nb),nb)
          do k1 = ktnum(5,nb)+1,ktnum(6,nb)
            do j1 = ktnum(4,nb)+1,ktnum(5,nb)
              do i1 = 1,ktnum(4,nb)
                e            = e + 1
                ix(nen1 ,e)  = nblk(2,nb)
                ix(nen1-1,e) = nblk(3,nb)
                ix(nen+7,e)  = nb + 500*i1         ! NURBblk
                ix(nen+8,e)  = j1
                ix(nen+9,e)  = k1
                node         = 0
                do j = knotdiv(3,j1,nb),knotdiv(4,j1,nb)
                  l1 = l2 * (j-1)
                  do i = knotdiv(3,i1,nb),knotdiv(4,i1,nb)
                    ns = nblksd(l1+i,nb)
                    do k = knotdiv(3,k1,nb),knotdiv(4,k1,nb)
                      node       = node + 1
                      ix(node,e) = nsides(k,ns)
                    end do ! i
                  end do ! j
                  ptelm(3,nb) = max(ptelm(3,nb),node)
                end do ! k
              end do ! i1
            end do ! j1
          end do ! k1
          ptelm(2,nb) = e
        endif
      end do ! nb

!     Output IX to output file

      if(prt) then
        write(iow,2001) (i,i=1,nen)
        do i = estart+1,e
          j1 = 0
          do j = 1,nen
            if(ix(j,i).gt.0) j1 = j
          end do ! j
          write(iow,2002) i,ix(nen1,i),ix(nen1-1,i),(ix(j,i),j=1,j1)
        end do ! i
      endif
      estart = e

!     Formats

2001  format(/5x,
     &   'E l e m e n t   G l o b a l   N o d e   N u m b e r s'//
     &        4x,'Elmt Mat Reg',8(i3,'-node':)/(16x,8(i3,'-node':)))

2002  format(1i8,2i4,8i8:/(16x,8i8:))

      end

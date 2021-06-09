!$Id:$
      subroutine pnurbel2d(knotdiv, ix,
     &                     knots,nsides,lknot,nblk,ktnum,nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose:  Construct block of 2-d elements from nurb sides

!     Inputs

!     Outputs:
!       knotdiv(4,kdiv,*) - Knot division array
!       ix(nen1,*)        - Element array for NURBS
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'print.h'
      include   'p_ptname.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      real*8     knots(dknotig,*)
      integer    knotdiv(4,kdiv,*), ix(nen1,*)
      integer    nsides(dsideig,*),lknot(0:4,*),nblk(14,*)
      integer    ktnum(6,*),nblksd(dblokig,*)

!     Local variables
      integer    i,j, j1, nb, e
      integer    nenu,nenv,nenp

!     Compute nurb element size

!     write(*,*) 'KNOT1',(knots(i,1),i=1,dknotig)
!     write(*,*) 'KNOT2',(knots(i,2),i=1,dknotig)
!     write(*,*) 'KNOT3',(knots(i,3),i=1,dknotig)
!     write(*,*) 'KNOT4',(knots(i,4),i=1,dknotig)
!     write(*,*) 'LKNOT1',(lknot(i,1),i=0,4)
!     write(*,*) 'LKNOT2',(lknot(i,2),i=0,4)
!     write(*,*) 'LKNOT3',(lknot(i,3),i=0,4)
!     write(*,*) 'LKNOT4',(lknot(i,4),i=0,4)
!     write(*,*) 'NBLK1',(nblk(i,1),i=1,14)
!     write(*,*) 'NBLK2',(nblk(i,2),i=1,14)
!     write(*,*) 'KNOTDIV1',((knotdiv(i,j,1),i=1,4),j=1,kdiv)
!     write(*,*) 'KNOTDIV2',((knotdiv(i,j,2),i=1,4),j=1,kdiv)
!     write(*,*) 'KTNUM1',(ktnum(i,1),i=1,6)
!     write(*,*) 'KTNUM2',(ktnum(i,2),i=1,6)
!     write(*,*) 'NBLKSD1',(nblksd(i,1),i=1,12)
!     write(*,*) 'NBLKSD2',(nblksd(i,2),i=1,12)
!     call iprint(nsides,dsideig,10,dsideig,'NSIDES')

      nenp = 0
      do nb = 1,nurbk
        if(nblk(1,nb).eq.2) then
          call pknotel(knots,lknot,ktnum(1,nb),
     &                 ktnum(4,nb),knotdiv(1,1,nb),2)
          j1 = ktnum(4,nb)+1
          nenu = knotdiv(4,j1,nb) - knotdiv(3,j1,nb) + 1
          nenv = knotdiv(4, 1,nb) - knotdiv(3, 1,nb) + 1
          nenp = max(nenp,nenu*nenv)
        endif
      end do ! nb

!     write(*,*) 'PNURBEL2D:NENP',nenp

!     Loop through block numbers

      e   = estart
      do nb = 1,nurbk

        if(nblk(1,nb).eq.2) then

!         Determine knot spacing

          call pknotel(knots,lknot,ktnum(1,nb),
     &                 ktnum(4,nb),knotdiv(1,1,nb),2)

!         Loop through control points assigning global node to IX array

          ptelm(1,nb) = e + 1
          call pknotix2(knotdiv,nb,e,ptelm(3,nb),ix,nsides,nblk,ktnum,
     &                  nblksd)

          ptelm(2,nb) = e
        endif
      end do ! nb

!     Output IX to output file

      if(prt) then
        write(iow,2001) (i,i=1,nenp)
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

2001  format(/5x
     &   'E l e m e n t   G l o b a l   N o d e   N u m b e r s'//
     &        4x,'Elmt Mat Reg',8(i3,'-node':)/(16x,8(i3,'-node':)))

2002  format(1i8,2i4,8i8:/(16x,8i8:))

      end

      subroutine pknotix2(knotdiv,nb,e,nen2d,ix,nsides,nblk,ktnum,
     &                    nblksd)

      implicit   none

      include   'cdata.h'
      include   'cnurb.h'
      include   'igdata.h'
      include   'sdata.h'

      integer    knotdiv(4,kdiv,*), nb,ix(nen1,*),nblk(14,*),ktnum(6,*)
      integer    nsides(dsideig,*),nblksd(dblokig,*)

      integer    i,j, i1,j1, e, nen2d, node,ns
!     Loop through control points assigning global node to IX array

      nen2d = 0
      do j1 = ktnum(4,nb)+1,ktnum(5,nb)
        do i1 = 1,ktnum(4,nb)
          e            = e + 1
          ix(nen1 ,e)  = nblk(2,nb)
          ix(nen1-1,e) = nblk(3,nb)
          ix(nen+7,e)  = nb + 500*i1              ! NURB block
          ix(nen+8,e)  = j1
          node         = 0

          do j = knotdiv(3,j1,nb),knotdiv(4,j1,nb)
            ns = nblksd(j,nb)
            do i = knotdiv(3,i1,nb),knotdiv(4,i1,nb)
              node       = node + 1
              ix(node,e) = nsides(i,ns)
            end do ! i
          end do ! j
          nen2d = max(nen2d,node)
        end do ! i1
      end do ! j1

      end

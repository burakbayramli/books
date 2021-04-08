!$Id:$
      subroutine pelvout3d(blk,x,wt,knots,nsides,lknot,lside,nblk,
     &                     nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Outputs unmodified NURBS blocks and patches to file

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]

      implicit   none

      include   'cnurb.h'
      include   'igdata.h'
      include   'iodata.h'
      include   'p_ptname.h'
      include   'sdata.h'

      integer    blk, i,j,k,l,n, nod, cnurnp
      integer    sid3, kno1,kno2,kno3, len1,len2,len3, inside
      real*8     x(ndm,*), wt(*), knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*),nblk(14,*)
      integer    nblksd(dblokig,*)

      sid3 = nblksd(1,blk)
      kno1 = nblk(6,blk)
      kno2 = nblk(7,blk)
      kno3 = lside(2,sid3)

      len1 = lknot(3,kno1)
      len2 = lknot(3,kno2)
      len3 = lside(1,sid3)

      write(ios,'(/a)') 'KNOTs'
      write(ios,2001) kno1,lknot(1,kno1),
     &               (knots(n,kno1),n=1,lknot(1,kno1))
      write(ios,2001) kno2,lknot(1,kno2),
     &               (knots(n,kno2),n=1,lknot(1,kno2))
      kno3  = kno2 + 1
      write(ios,2001) kno3,lknot(1,kno3),
     &               (knots(n,kno3),n=1,lknot(1,kno3))

      inside = nnside + 1
      cnurnp = nnurnp

      write(ios,'(/a,i8)') 'NURBS',nnurnp
      l = 0
      do j = 1,len2
        do i = 1,len1
          l   = l + 1
          do k = 1,len3
            nod = nsides(k,nblksd(l,blk))
            write(ios,2003) nnurnp+k,0,(x(n,nod),n=1,ndm),wt(nod)
          end do ! k
          nnurnp = nnurnp + len3
        end do ! i
      end do ! j

!     Output sides of control points

      if(blockfl) then
        write(ios,'(/a)') 'NSIDes PELVOUT3D'
        nod    = cnurnp
        do j = 1,len2
          do i = 1,len1
            nnside = nnside + 1
            write(ios,2002) nnside,len3,kno3,(nod+n,n=1,len3)
            nod    = nod + len3
          end do ! i
        end do ! j
      endif

!     Output region number

      if(nblk(3,blk).gt.0) then
        write(ios,'(/a,i5)') 'REGIon',nblk(3,blk)
      endif

!     Output block data

      if(blockfl) then

        write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
        write(ios,2004)   3,nblk(2,blk),kno1,kno2
        write(ios,2005) (n,n=inside,nnside)

!     Output patch data

      else

        write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
        write(ios,2006)   nblk(2,blk),len1,len2,len3,kno1,kno2,kno3
        nod    = cnurnp
        do j = 1,len2
          do i = 1,len1
            nnside = nnside + 1
            write(ios,2005) (nod+n,n=1,len3)
            nod    = nod + len3
          end do ! i
        end do ! j

      endif

      write(ios,'(a)') ' '

!     Formats

2001  format('  open',2i4,1p,13e16.8:/(1p,16e16.8:))
2002  format('  side',3i4,12i6:/(16i6:))
2003  format(i8,i3,1p,4e16.8)
2004  format('  block',4i6)
2005  format(16i6:)
2006  format('  solid',7i6)

      end

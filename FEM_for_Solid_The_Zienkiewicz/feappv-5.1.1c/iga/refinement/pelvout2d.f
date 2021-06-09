!$Id:$
      subroutine pelvout2d(blk,x,wt,knots,nsides,lknot,lside,nblk,
     &                     nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Outputs unmodified NURBS curves to file

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters
!         prt       - Flag, output if true

!      Outputs:
!         N.B.  Users are responsible for command actions.  See
!               programmers manual for example.
!-----[--.----+----.----+----.-----------------------------------------]

      implicit   none

      include   'cnurb.h'
      include   'igdata.h'
      include   'iodata.h'
      include   'p_ptname.h'
      include   'sdata.h'

      integer    blk, j,l,n, sid1,sid2, kno1,kno2, nod, cnurnp
      real*8     x(ndm,*), wt(*), knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*),nblk(14,*)
      integer    nblksd(dblokig,*)

!     Set block parameters

      sid1 = nblksd(1,blk)
      kno1 = lside(2,sid1)

      sid2 = nblksd(nblk(4,blk)+1,blk)
      kno2 = lside(2,sid2)

!     Output knot vectors

      write(ios,'(/a)') 'KNOTs'
      write(ios,2001) kno1,lknot(1,kno1),
     &               (knots(n,kno1),n=1,lknot(1,kno1))
      write(ios,2001) kno2,lknot(1,kno2),
     &               (knots(n,kno2),n=1,lknot(1,kno2))

!     Output control points for NURBS

      cnurnp = nnurnp     ! Save for side outputs
!     nod    = nnurnp
      write(ios,'(/a)') 'NURBS'
      do j = 1,nblk(4,blk)
        do l = 1,lside(1,sid1)
          nod = nsides(l,nblksd(j,blk))
          write(ios,2003) nnurnp+l,0,(x(n,nod),n=1,ndm),wt(nod)
        end do ! l
        nnurnp = nnurnp + lside(1,sid1)
      end do ! j

!     Output side data for nblocks

      if(blockfl) then

!       Store edge side numbers

        nblk(8,blk) = nnside + 1  ! eside(1,blk)

        write(ios,'(/a)') 'NSIDes'
        nod    = cnurnp
        do l = 1,nblk(4,blk)
          nnside = nnside + 1
          write(ios,2002) nnside,lside(1,sid1),kno1,
     &                   (nod+n,n=1,lside(1,sid1))
          nod    = nod + lside(1,sid1)
        end do ! l
        nblk( 9,blk) = nnside
        nblk(10,blk) = nnside + 1
        nblk(11,blk) = nnside + 2

!       Write bottom list

        nnside = nnside + 1
        nod    = cnurnp + 1
        write(ios,2002) nnside,nblk(4,blk),kno2,
     &                 (nod+lside(1,sid1)*(n-1),n=1,nblk(4,blk))

!       Write top list

        nnside = nnside + 1
        nod    = cnurnp
        write(ios,2002) nnside,nblk(4,blk),kno2,
     &                 (nod+lside(1,sid1)*n,n=1,nblk(4,blk))
      endif

!     Output region number

      if(nblk(3,blk).gt.0) then
        write(ios,'(/a,i5)') 'REGIon',nblk(3,blk)
      endif

!     Output block data

      if(blockfl) then

        write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
        write(ios,2004) 2,nblk(2,blk),nnside-1
        write(ios,2005) (nblk(n,blk),n=8,11)   ! esides

!     Output patch data

      else

        write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
        write(ios,2006) nblk(2,blk),lside(1,sid1),nblk(4,blk),kno1,kno2
        nod = cnurnp
        do l = 1,nblk(4,blk)
          write(ios,2007) (nod+n,n=1,lside(1,sid1))
          nod    = nod + lside(1,sid1)
        end do ! l

      endif

      write(ios,'(a)') ' '

!     Formats

2001  format('  open',2i4,1p,13e16.8:/(1p,16e16.8:))
2002  format('  side',3i4,12i6:/(16i6:))
2003  format(i8,i3,1p,4e16.8)
2004  format('  block',3i6)
2005  format('  eside',4i6)
2006  format('  surface',5i6)
2007  format(16i6:)

      end

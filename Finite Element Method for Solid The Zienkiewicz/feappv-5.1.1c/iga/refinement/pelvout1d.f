!$Id:$
      subroutine pelvout1d(blk,x,wt,knots,nsides,lknot,lside,nblk,
     &                     nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  1-d Output of unmodified NURBS curve to file

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

      integer    blk, l,n, sid, kno, nod
      real*8     x(ndm,*), wt(*), knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*),nblk(14,*)
      integer    nblksd(dblokig,*)

      sid = nblksd(1,blk)
      kno = lside(2,sid)       ! kside(sid)

!     Output knot vector

      write(ios,'(/a)') 'KNOTs'
      write(ios,2001) kno,lknot(1,kno),(knots(n,kno),n=1,lknot(1,kno))

!     Output control points

      write(ios,'(/a)') 'NURBS'
      do l = 1,lside(1,sid)
        nod = nsides(l,nblksd(1,blk))
        write(ios,2003) nnurnp+l,0,(x(n,nod),n=1,ndm),wt(nod)
      end do ! l

!     Side outputs for NBLOck

      if(blockfl) then
        nnside = nnside + 1
        write(ios,'(/a)') 'NSIDes'
        write(ios,2002) nnside,lside(1,sid),kno,
     &                 (nnurnp+n,n=1,lside(1,sid))

      endif

!     Output region description

      if(nblk(3,blk).gt.0) then
        write(ios,'(/a,i5)') 'REGIon',nblk(3,blk) ! nuregn(blk)
      endif

!     Output block description

      if(blockfl) then

        write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
        write(ios,2004)   1,nblk(2,blk),nnside

!     Output patch description

      else
         write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
         write(ios,2005) nblk(2,blk),lside(1,sid),lside(2,sid),
     &                  (nnurnp+n,n=1,lside(1,sid))

      endif

!     Increase number of control points

      nnurnp = nnurnp + lside(1,sid)

!     Formats

2001  format('  open',2i4,1p,13e16.8:/(1p,16e16.8:))
2002  format('  side',3i4,12i6:/(16i6:))
2003  format(i8,i3,1p,4e16.8)
2004  format('  block',3i6)
2005  format('  line',3i6/(16i6:))

      end

!$Id:$
      subroutine pnrdrd(x,wt,ndtyp,nnurb,prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input all NURBS coordinates and weights

!      Inputs:
!         prt      - Print results if true
!         nnurb    - Number of records to read

!      Outputs:
!         x(ndm,*) - Nodal coordinates
!         wt(*)    - Weigth
!         ndtyp(*) - Active node identifier
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdata.h'
      include   'chdata.h'
      include   'comfil.h'
      include   'sdata.h'
      include   'iofile.h'
      include   'ioincl.h'
      include   'trdata.h'

      logical    prt, pcomp, incfl
      character  inam*4,fnam*15,gnam*4
      integer    i,j,n,noge, ibc, ndtyp(*), nnurb,jmin,jmax
      real*8     xx(3),ww, x(ndm,numnp), wt(numnp)

!     Check for an include or nogeneration record
!     [include filename <noge> - Input from filename
!     [nogeneration            - Omit generation field

      read(ior,1000) record
      call pstrip(xxx,record,1)
      call acheck(xxx,yyy,15,80,80)
      read(yyy,1002)  inam,fnam,gnam
      if(pcomp(inam,'incl',4)) then
        call pincld(fnam)
        noge  = 1
        incfl = .true.
        if(pcomp(gnam,'noge',4)) then
          noge = 0
        endif
      elseif(pcomp(inam,'noge',4)) then
        noge  = 0
        incfl = .false.
      else
        backspace(ior)
        noge  = 1
        incfl = .false.
      endif

!     List directed input of nnurb records all the data

      jmin = 0
      jmax = 0
      do n = 1,nnurb
        if(noge.eq.0) then
          read(ior,*) j,    (xx(i),i=1,ndm),ww
        else
          read(ior,*) j,ibc,(xx(i),i=1,ndm),ww
        endif
        if(jmin.eq.0) then
          jmin = j
        endif
        jmin = min(j,jmin)
        jmax = max(j,jmax)
        do i = 1,ndm
          x(i,j) = xx(i)
        end do ! i
        wt(j)    = ww
        if(wt(j).le.0.0d0) then
          write(iow,3001) j
          write(  *,3001) j
          wt(j) = 1.0d0
        endif
        ndtyp(j) = 0     ! Activate node
      end do ! n

!     Transform if necessary

      if((tr(1,1)+tr(2,2)+tr(3,3) .ne. 3.d0) .or.
     &   (max(abs(xr(1)),abs(xr(2)),abs(xr(3))).ne.0.0d0) ) then
        do n = jmin,jmax
          do i = 1,ndm
            xx(i) = x(i,n)
          end do ! i
          do i = 1,ndm
            x(i,n) = xr(i)
            do j = 1,ndm
              x(i,n) = x(i,n) + tr(i,j)*xx(j)
            end do ! j
          end do ! i
        end do ! n
      endif

!     Output computed data

      if(prt) then
        call prtitl(prt)
        if(ndm.eq.1) then
          write(iow,2001)  (i,i=1,ndm)
        elseif(ndm.eq.2) then
          write(iow,2002)  (i,i=1,ndm)
        else
          write(iow,2003)  (i,i=1,ndm)
        endif
        do n = jmin,jmax
          write(iow,2004) n,(x(i,n),i=1,ndm),wt(n)
        end do ! n
      endif

      irecrd(isf) = irecrd(isf) + nnurb

!     Clear the include file if necessary

      if(incfl) then
        call pincld('end')
      endif

!     Formats

1000  format(a)
1002  format(a4,11x,a15,a4)

2001  format(5x,'Nodal Coordinates'//6x,'Node',1(i9,'-Coord':),
     &       9x,'Weight')
2002  format(5x,'Nodal Coordinates'//6x,'Node',2(i9,'-Coord':),
     &       9x,'Weight')
2003  format(5x,'Nodal Coordinates'//6x,'Node',3(i9,'-Coord':),
     &       9x,'Weight')
2004  format(i10,1p,4e16.8)

3001  format(' *WARNING* PNRDRD: Node',i6,' has zero value. Set = 1.0')

      end

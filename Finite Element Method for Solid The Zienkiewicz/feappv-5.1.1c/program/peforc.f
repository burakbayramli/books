!$Id:$
      subroutine peforc(x,f,ndm,ndf,numnp,vtype,prt,prth,fname)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set force or displacement values based on specified
!               edge coordinates.

!      Inputs:
!         x(ndm,*)   - Nodal coordinates of mesh
!         ndm        - Spatial dimension of mesh
!         ndf        - Number dof/node
!         numnp      - Number of nodes in mesh
!         prt        - Output generated results if true
!         prth       - Output title/header data if true
!         fname      - File name for inputs

!      Outputs:
!         f(ndf,*)   - Nodal force or displacement values
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'pointer.h'
      include  'comblk.h'

      character (len=15) :: text
      character (len=5)  :: fname
      character (len=4)  :: vtype

      logical       :: prt,prth, errck, tinput, vinput, pcomp, gapfl
      integer       :: ndm,ndf,numnp,i,j,n
      real (kind=8) :: pdiff, x0,dx, gap
      real (kind=8) :: x(ndm,numnp),f(ndf,numnp),td(16)

      save

!     Read input of boundary edge forces

      gap   = 1.d-3/sqrt(dble(max(1,numnp)))
      gapfl = .false.
100   if(ior.lt.0) write(*,3001) ndf
      errck = tinput(text,1,td(2),ndf+1)
      if(errck) go to 100
      if(pcomp(text,'gap',3)) then
        gap   = td(2)
        gapfl = .true.
        if(prt) then
          write(iow,2002) gap
          if(ior.lt.0) then
            write(*,2002) gap
          endif
        endif
        go to 100
      else
        errck = vinput(text,15,td(1),1)
        i  = abs(nint(td(1)))
      endif
      if(i.le.0.or.i.gt.ndm) go to 4
      x0 = td(2)

!     Locate nodes on which to apply loading

      if(gapfl) then
        dx = gap
      else
        dx  = pdiff(x,i,ndm,numnp)*1.d-3
      endif
      do n = 1,numnp
        if(mr(np(190)-1+n).ge.0.and.abs(x(i,n)-x0).le.dx) then
          if(pcomp(vtype,'add',3)) then
            do j = 1,ndf
              f(j,n) = f(j,n) + td(j+2)
            end do
          else
            do j = 1,ndf
              f(j,n) = td(j+2)
            end do
          endif
        endif
      end do
      go to 100

4     call prtitl(prth)
      if(prt) write(iow,2000) (i,fname,i=1,ndf)
      if(prt.and.ior.lt.0) write(*,2000) (i,fname,i=1,ndf)
      do n = 1,numnp
        do i = 1,ndf
          if(f(i,n).ne.0.0d0) go to 400
        end do
        go to 410
400     if(prt) write(iow,2001) n,(f(i,n),i=1,ndf)
        if(prt.and.ior.lt.0) write(*,2001) n,(f(i,n),i=1,ndf)
410     continue
      end do

!     Formats

2000  format('  E d g e    N o d a l    V a l u e'/
     &       /4x,'Node',6(i6,'-',a5):/(8x,6(i6,'-',a5)))

2001  format(i8,1p,6e12.4:/(8x,1p,6e12.4))

2002  format(/'     Search gap = ',1p,1e12.4/)

3001  format(' Input: ndir,x(ndir),(fl(i),i=1,',i2,')','   >',$)

      end subroutine peforc

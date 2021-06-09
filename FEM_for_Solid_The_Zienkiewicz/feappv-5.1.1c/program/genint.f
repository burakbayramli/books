!$Id:$
      subroutine genint(nd,ix,nix,num,cname,carg,prt,prth,err,type)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Generate integer data arrays

!      Inputs:
!         nd        - Number of integer items/node to generate
!         nix       - Dimension of integer array ix
!         num       - Number of generated items
!         cname     - Header identification for outputs
!         carg      - Value identifier for outputs
!         prt       - Output generated data if true
!         prth      - Output title/header if true
!         type      - Generation type: 1=node, 2=element

!      Outputs:
!         ix(nix,*) - Integer data generated
!         err       - True if error occurred
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'dstars.h'
      include  'iofile.h'

      character (len=12) :: cd
      character          :: cname*(*), carg*(*)

      logical       :: prt,prth,err,errck, pinput, oflg, norec
      integer       :: i,j,l,n,nn,lg,ng,nd,nix,num,mct,type
      integer       :: ix(nix,*),ixl(14)
      real (kind=8) :: td(16)

      save

      norec = .true.
      err = .false.
      cd  = carg
      mct = 0
      n   = 0
      ng  = 0
100   l   = n
      lg  = ng

!     Call input routine - values returned in td and then moved

101   if(ior.lt.0) then
        write(*,2010)
      endif
      errck = pinput(td,2+nd)
      if(errck) go to 101
      nn = nint(td(1))
      if(type.eq.1) then
        n = nn + starnd
      elseif(type.eq.2) then
        n = nn + starel
      else
        write(*,*) ' *ERROR* Wrong call to GENINT'
        call plstop(.true.)
      endif
      if(n.gt.num) then
        write(iow,3001) n,cname
        if(ior.lt.0) write(*,3001) n,cname
      endif
      if(n.le.0.or.n.gt.num) go to 104
      norec = .false.

      ng  = nint(td(2))
      do i = 1,nd
        ixl(i)  = nint(td(i+2))
        ix(i,n) = ixl(i)
      end do ! i
      if(lg.ne.0) then
        lg = sign(lg,n-l)
102     l = l + lg
        if((n-l)*lg.le.0) go to 100
        if(l.le.0.or.l.gt.num) go to 103
        do i = 1,nd
          ix(i,l) = ix(i,l-lg)
        end do ! i
        go to 102
103     write(iow,3000) l,cname
        if(ior.lt.0) write(  *,3000) l,cname
        err = .true.
      endif
      go to 100

!     Output data sets

104   if(norec) then
        write(  *,4001) cd(1:5)
        write(iow,4001) cd(1:5)
        return
      endif
      if(.not.prt) return

      do j = 1,num

!       Output if non-zero

        oflg = .false.
        do l = 1,nd
          if(ix(l,j).ne.0) oflg = .true.
        enddo ! l

        if(oflg) then

!         Output header

          mct = mct - 1
          if(mct.le.0) then
            mct = 50
            call prtitl(prth)
            if(type.eq.1) then
              write(iow,2001) cname,(l,cd,l=1,nd)
              if(ior.lt.0) then
                write(*,2001) cname,(l,cd,l=1,nd)
              endif
            else
              write(iow,2002) cname,(l,cd,l=1,nd)
              if(ior.lt.0) then
                write(*,2002) cname,(l,cd,l=1,nd)
              endif
            endif
          endif

!         Output values

          write(iow,2009) j,(ix(l,j),l=1,nd)
          if(ior.lt.0) then
            write(*,2009) j,(ix(l,j),l=1,nd)
          endif
        endif

      end do ! j

!     Formats

2001  format(5x,a//'      Node',6(i5,a6):/(10x,6(i5,a6):))
2002  format(5x,a//'   Element',6(i5,a6):/(10x,6(i5,a6):))

2009  format(i10,6i11:/(10x,6i11):)

2010  format(' Input: item#, inc., values'/3x,'>',$)

3000  format(' *ERROR* GENINT: Attempt to generate item',i5,' in ',a)

3001  format(' *ERROR* GENINT: Attempt to input item',i5,', terminate',
     & ' input in ',a)

4001  format(' *WARNING* GENINT: No data found for a ->',a5,
     &       ' <- data set.')

      end subroutine genint

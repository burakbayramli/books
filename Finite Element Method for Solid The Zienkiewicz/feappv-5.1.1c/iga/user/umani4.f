!$Id:$
      subroutine umani4

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Line edge boundary condition set

!      Use:     ledge
!                 boun d1 x1 d2 x2 (id(i),i=1,ndf)
!                 disp d1 x1 d2 x2 (ud(i),i=1,ndf)

!      Inputs:

!      Outputs:
!        id(ndf,*,2) Boundary condition values
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'
      include   'umac1.h'
      include   'pointer.h'
      include   'comblk.h'

      logical    pcomp, errck,tinput
      character  text(1)*15
      real*8     td(15)

      if (pcomp(uct,'man4',4)) then
          uct = 'ledg'
      else

        text = 'start'
        do while (.not.pcomp(text,'    ',4))
          errck = tinput(text,1,td,15)
          if(pcomp(text,'boun',4)) then
            write(iow,2000)
            call uman4id(mr(np(190)),mr(np(31)),hr(np(43)),td)
          elseif(pcomp(text,'disp',4)) then
            write(iow,2001)
            call uman4ud(mr(np(190)),hr(np(27)),hr(np(43)),td)
          endif
        end do ! while

      end if

!     formats

2000  format(5x,'L i n e   B o u n d a r y   C o n d i t i o n s'/)
2001  format(5x,
     &   'L i n e   D i s p l a c e m e n t   C o n d i t i o n s'/)

      end

      subroutine uman4id(ndtyp,id,x,td)

      implicit   none

      include   'cdata.h'
      include   'sdata.h'
      include   'print.h'
      include   'iofile.h'

      integer    id(ndf,numnp,2), ndtyp(*)
      real*8     x(ndm,*), td(15)

      logical    prflg
      integer    d1,d2, n, i
      real*8     x1,x2

      real*8     gap

      prflg = prt

      d1  = nint(td(1))
      d2  = nint(td(3))
      x1  = td(2)
      x2  = td(4)
      gap = 1.0d-03/sqrt(dble(max(1,numnp)))
      if(min(d1,d2).gt.0 .and. max(d1,d2).le.ndm) then

        write(iow,2002) d1,x1,d2,x2

        do n = 1,numnp
          if(max(abs(x(d1,n)-x1),abs(x(d2,n)-x2)).lt.gap) then
            if(prflg) then
             write(iow,2000) (i,i=1,ndf)
             prflg = .false.
            endif
            do i = 1,ndf
              if(id(i,n,2).eq.0) then
                id(i,n,2) = nint(td(i+4))
              endif
            end do ! i
            if(prt .and. ndtyp(n).ge.0) then
              write(iow,2001) n,(id(i,n,2),i=1,ndf)
            endif
          endif
        end do ! n

      else
        write(iow,3000) d1,x1,d2,x2
      endif

!     Formats

2000  format(6x,'node',7(i3,'-B.C.':)/(10x,7(i3,'-B.C.')))
2001  format(i10,7i8/(10x,7i8))
2002  format(i10,'-Direction x =',1p,1e12.4/
     &       i10,'-Direction x =',1p,1e12.4)
3000  format(' *ERROR* LBOU: d1 = ',i3,' x1 =', 1p,1e12.4/
     &       '               d2 = ',i3,' x2 =', 1p,1e12.4/)

      end

      subroutine uman4ud(ndtyp,f,x,td)

      implicit   none

      include   'cdata.h'
      include   'sdata.h'
      include   'print.h'
      include   'iofile.h'

      integer    ndtyp(*)
      real*8     x(ndm,*), f(ndf,numnp,2), td(15)

      logical    prflg
      integer    d1,d2, n, i
      real*8     x1,x2

      real*8     gap

      prflg = prt

      d1  = nint(td(1))
      d2  = nint(td(3))
      x1  = td(2)
      x2  = td(4)
      gap = 1.0d-03/sqrt(dble(max(1,numnp)))
      if(min(d1,d2).gt.0 .and. max(d1,d2).le.ndm) then

        write(iow,2002) d1,x1,d2,x2

        do n = 1,numnp
          if(max(abs(x(d1,n)-x1),abs(x(d2,n)-x2)).lt.gap) then
            if(prflg) then
             write(iow,2000) (i,i=1,ndf)
             prflg = .false.
            endif
            do i = 1,ndf
              f(i,n,2) = td(i+4)
            end do ! i
            if(prt .and. ndtyp(n).ge.0) then
              write(iow,2001) n,(f(i,n,2),i=1,ndf)
            endif
          endif
        end do ! n

      else
        write(iow,3000) d1,x1,d2,x2
      endif

!     Formats

2000  format(6x,'node',5(i6,'-Disp.':)/(10x,5(i6,'-Disp.')))
2001  format(i10,1p,5e12.4/(10x,1p,5e12.4))
2002  format(i10,'-Direction x =',1p,1e12.4/
     &       i10,'-Direction x =',1p,1e12.4)
3000  format(' *ERROR* LEDG: d1 = ',i3,' x1 =', 1p,1e12.4/
     &       '               d2 = ',i3,' x2 =', 1p,1e12.4/)

      end

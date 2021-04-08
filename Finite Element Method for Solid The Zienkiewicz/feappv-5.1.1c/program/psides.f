!$Id:$
      subroutine psides(is,side,nside,prt,prth,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Input sides for blending functions

!     Input:
!       nside   - Total number of sides
!       prt     - Output if true
!       prth    - Output title if true
!       isw     - 1=sides; 2=faces

!     Output:
!       is(*,*) - List of side nodes/face sides
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      character (len=15) :: vtype

      logical       :: prt,prth, pcomp
      integer       :: isw,nside
      integer       :: is(nside,*)

      logical       :: setvar,tinput
      integer       :: i,n,side
      real (kind=8) :: td(16)

      save

      if(prt) then
        call prtitl(prth)
        if(isw.eq.1) write(iow,2000)
        if(isw.eq.2) write(iow,2001)
        if(ior.lt.0) then
          if(isw.eq.1) write(*,2000)
          if(isw.eq.2) write(*,2001)
        endif
      endif

!     Input Sides for blending inputs

      vtype = 'initial'
      do while(.not.pcomp(vtype,'    ',4))

        if(ior.lt.0 .and. isw.eq.1) write(*,3000)
        if(ior.lt.0 .and. isw.eq.2) write(*,3001)

        setvar = tinput(vtype,1,td,nside-1)

        if(.not. pcomp(vtype,'    ',4)) then
          side   = side + 1
          if(pcomp(vtype,'pola',4)) then
           is(1,side) = 1
          elseif(pcomp(vtype,'cart',4)) then
           is(1,side) = 0
          elseif(pcomp(vtype,'segm',4)) then
           is(1,side) = 2
          elseif(pcomp(vtype,'elip',4)) then
           is(1,side) = 3
          else
           is(1,side) = -1
          endif

          if(is(1,side).ge.0) then
            do n = 2,nside
              is(n,side) = nint(td(n-1))
            end do
            do n = nside,1,-1
              if(is(n,side).ne.0) go to 100
            end do

100         if(prt) then
              write(iow,2002) side,vtype,(is(i,side),i=2,n)
              if(ior.lt.0) then
                write(*,2002) side,vtype,(is(i,side),i=2,n)
              endif
            endif
          else
            side = side - 1
          endif

        endif

      end do ! while

3000  format('   Input: side, is(side,snode),snode=2,nside'/'   >',$)

3001  format('   Input: face, is(face,side),side=1,nface'/'   >',$)

2000  format('   S i d e   S u p e r N o d e   C o n n e c t i o n s'//
     &'       Side Type 1-Nd 2-Nd 3-Nd 4-Nd 5-Nd 6-Nd 7-Nd 8-Nd 9-Nd'/)

2001  format('   F a c e    S i d e    C o n n e c t i o n s'//
     &'       Face 1-Sd 2-Sd 3-Sd 4-Sd 5-Sd 6-Sd 7-Sd 8-Sd 9-Sd'/)

2002  format(5x,i4,1x,a4,1x,9i5:/(15x,9i5))

      end subroutine psides

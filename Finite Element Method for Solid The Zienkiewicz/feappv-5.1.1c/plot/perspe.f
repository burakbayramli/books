!$Id:$
      subroutine perspe(flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Input of perspective parameters
!               kpers: flag for perspective projection (1=perspective)

!      Inputs:
!         flag      - Flag, if true and not default user to provide
!                           data

!      Outputs:
!         none      - Outputs through common blocks
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'pdata0.h'
      include  'ppers.h'
      include  'prmptd.h'
      include  'pdatps.h'
      include  'sdata.h'

      logical       :: flag, hdcpyo, errck, pinput
      integer       :: iused,ifrfl, i, j
      real (kind=8) :: t(3,3), tgold(3), v(3), vnorm

      save

 1    if(flag) then
        if(defalt) then
          iused = 0
        else
          if(ior.lt.0) write(*,2000)
          errck = pinput(e,1)
          iused = nint(e(1))
        endif
        if(iused.eq.0) then
          if(eold(1)+eold(2)+eold(3).eq.0.0d0) then
            eold(1) = 3.0d0*vmax(1)
            eold(2) = 2.0d0*vmax(2)
            if(ndm.le.2) then
              eold(3) = 1.5d0*max(eold(1),eold(2))
            else
              eold(3) = 1.5d0*vmax(3)
            endif
            if(eold(1).eq.0.0d0) eold(1) = 0.4d0*max(eold(2),eold(3))
            if(eold(2).eq.0.0d0) eold(2) = 0.4d0*max(eold(3),eold(1))
            if(eold(3).eq.0.0d0) then
              if(ndm.le.2) then
                eold(3) = 1.4d0*max(eold(1),eold(2))
              else
                eold(3) = 0.4d0*max(eold(1),eold(2))
              endif
            endif
          endif

          if(defalt) then
            call pzero(e,3)
          else
            if(ior.lt.0) write(  *,2001) vmin,vmax,eold
            errck = pinput(e,3)
          endif

          if(e(1)+e(2)+e(3).eq.0.0d0) then
            e(1)    = eold(1)
            e(2)    = eold(2)
            e(3)    = eold(3)
          else
            eold(1) = e(1)
            eold(2) = e(2)
            eold(3) = e(3)
          endif

          write(iow,2002) vmin,vmax,e

          tg(1)    = 0.5d0*(vmax(1) + vmin(1))
          tg(2)    = 0.5d0*(vmax(2) + vmin(2))
          tg(3)    = 0.5d0*(vmax(3) + vmin(3))
          tgold(1) = tg(1)
          tgold(2) = tg(2)
          tgold(3) = tg(3)

          if(defalt) then
            call pzero(v,3)
          else
            if(ior.lt.0) write(  *,2003) vold
            errck = pinput(v,3)
          endif

          if(v(1)+v(2)+v(3).eq.0.0d0) then
            v(1)    = vold(1)
            v(2)    = vold(2)
            v(3)    = vold(3)
          else
            vold(1) = v(1)
            vold(2) = v(2)
            vold(3) = v(3)
          endif

          write(iow,2004) v

        endif
      endif

      if(.not.flag .or. iused.ne.0) then
        e(1)  = eold(1)
        e(2)  = eold(2)
        e(3)  = eold(3)
        tg(1) = tgold(1)
        tg(2) = tgold(2)
        tg(3) = tgold(3)

        v(1)  = vold(1)
        v(2)  = vold(2)
        v(3)  = vold(3)
      endif

!     Projection matrix t

      q(1,1) = e(1) - tg(1)
      q(2,1) = e(2) - tg(2)
      q(3,1) = e(3) - tg(3)
      enorm = sqrt(q(1,1)*q(1,1)+q(2,1)*q(2,1)+q(3,1)*q(3,1))
      if(enorm.le.0.0d0) go to 901

!     PostScript

      hdcpyo = hdcpy
      if ( hdlogo .and. hdcpy) then
        hdcpy = .false.
        ifrfl = 1
      endif
      call plview(q,vmin,vmax)
      if (ifrfl .eq. 1) then
        hdcpy = hdcpyo
        ifrfl = 0
      endif

      do i = 1,3
        q(i,3) = q(i,1) / enorm
      end do
      do i = 1,3
        do j = 1,3
          t(i,j) = - q(i,3) * q(j,3)
        end do
        t(i,i) = t(i,i) + 1.d0
      end do

!     Find projection of v

      do i = 1,3
        q(i,2) = t(i,1)*v(1) + t(i,2)*v(2) + t(i,3)*v(3)
      end do
      vnorm = sqrt(q(1,2)*q(1,2)+q(2,2)*q(2,2)+q(3,2)*q(3,2))
      if(vnorm.le.0.0d0) go to 901
      do i = 1,3
        q(i,2) = q(i,2) / vnorm
      end do

!     Compute normal vector

      call vecp(q(1,2),q(1,3),q(1,1))

!     Compute rotation matrix

      do i = 1,3
        do j = 1,3
          xlbda(i,j) = q(1,i)*t(1,j) + q(2,i)*t(2,j) + q(3,i)*t(3,j)
        end do
      end do
      xlbda(3,1) = xlbda(1,2)*xlbda(2,3) - xlbda(1,3)*xlbda(2,2)
      xlbda(3,2) = xlbda(1,3)*xlbda(2,1) - xlbda(1,1)*xlbda(2,3)
      xlbda(3,3) = xlbda(1,1)*xlbda(2,2) - xlbda(1,2)*xlbda(2,1)
      return

  901 if(ior.lt.0) write(  *,3000)
      if(ior.gt.0) write(iow,3000)
      call errclr('PERSPE')
      if(flag) goto 1

!     Formats

 2000 format(' Use old parameters? (0 = new or 1 = old) :',$)

 2001 format(' Body occupies the space with:'/
     &  '                 X',13x,'Y',13x,'Z'/
     &  '   Minimum ',1p3e14.5/'   Maximum ',1p3e14.5/
     &  ' Enter coordinates of view point   (X,Y,Z).'/
     &  ' Default: X=',1p,1e9.2,', Y=',1p,1e9.2,', Z=',1p,1e9.2/
     &  '  >',$)

 2002 format(' Body occupies the space with:'/
     &  '                 X',13x,'Y',13x,'Z'/
     &  '   Minimum ',1p3e14.5/'   Maximum ',1p3e14.5/
     &  '          X=',1p,1e9.2,', Y=',1p,1e9.2,', Z=',1p,1e9.2/)

 2003 format(' Enter comps. of vertical vector   (X,Y,Z).'/
     &  ' Default: X=',1p,1e9.2,', Y=',1p,1e9.2,', Z=',1p,1e9.2/
     &  '  >',$)

 2004 format('   Components of vertical vector   (X,Y,Z).'/
     &  '          X=',1p,1e9.2,', Y=',1p,1e9.2,', Z=',1p,1e9.2/)

 3000 format(' *ERROR* Improper view specified')

      end subroutine perspe

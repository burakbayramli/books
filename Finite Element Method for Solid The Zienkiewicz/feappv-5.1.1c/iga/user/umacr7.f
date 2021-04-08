!$Id:$
      subroutine umacr7(lct,ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Curve: Elevation and knot insertion

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'umac1.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    pcomp
      character  lct*15
      real*8     ctl(3)

      save

!     Set command word

      if(pcomp(uct,'mac7',4)) then      ! Usual    form
        uct = 'curv'                    ! Specify 'curve' function
      elseif(urest.eq.1) then           ! Read  restart data

      elseif(urest.eq.2) then           ! Write restart data

      else                              ! Perform user operation

        call u7curve(lct,ctl,
     &               hr(np(298)),mr(np(299)),mr(np(308)),mr(np(309)))

      endif

      end

      subroutine u7curve(lct,ctl,
     &                   knots,nsides,lknot,lside)

      implicit   none

      include   'sdata.h'
      include   'cnurb.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'umac1.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    pcomp
      character  lct*15
      real*8     ctl(3),knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*)

      integer    iss,deg, les,lek,leq,leu, l, kno,ord, pq,nd1
      integer    dknot

      integer    k,s,r

      real*8     uu

      save

!     Set values of original knot can control vectors

      iss = nint(ctl(1))
      les = lside(1,iss)

      call usetPw(les, ndm, nsides(1,iss),hr(np(43)),hr(np(263)),PP1)

      kno = lside(2,iss)
      lek = lknot(1,kno)
      ord = lknot(2,kno)

      do l = 1,lek
        UU1(l) = knots(l,kno)
      end do ! l
      dknot = 0
      dknot = 1
      do l = 1,lek-1
        if(UU1(l+1).ne.UU1(l)) then
          dknot = dknot + 1
        endif
      end do ! l

      nd1 = ndm + 1

      if(pcomp(lct,'elev',4)) then
        deg = nint(ctl(2))
        if(nint(ctl(3)).eq.0) then
          call degreeElevateCurve(les,ord,UU1,PP1,
     &                            deg,QQ2,UU2,pq, nd1)
        else
          call degree_elevate_curve(les,ord,UU1,PP1,
     &                              deg,QQ2,UU2,pq, nd1)
        endif

        leq = les + (dknot-1)*deg
        leu = les + ord + dknot*deg + 1

!     Insert a knot

      elseif(pcomp(lct,'inse',4)) then

        uu = ctl(2)
        r  = nint(ctl(3))

        call setUw_ks(lek,UU1, uu, k,s)

        call curveknotins(les-1,ord, UU1,PP1, uu, k,s,r,
     &                    leq,UU2,QQ2, ndm)

        leu = les + ord + r + 1
        leq = leq + 1
      endif

!     Copy back to nurbs side

!     lknot(1,kno) = leu
!     do l = 1,leu
!       knots(l,kno) = UU2(l)
!     end do ! l

      end

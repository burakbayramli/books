!$Id:$
      subroutine pointnd(d,ul,s,r,ndf,nst,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose:  Point Stiffness & Mass ELEMENT

!     Input data: (isw = 1)

!       Record ('mass',m)
!         m - Mass / dof

!       Record ('spri',k)
!         k - Spring / dof

!     Outputs: (isw = 4)
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'iofile.h'

      character (len=8) :: vtype

      logical       :: errck, tinput, pcomp, spring, masses
      integer       :: ndf, nst, isw, i
      real (kind=8) :: d(*),ul(ndf,nen,*),s(nst,*),r(ndf,*),td(6)

      save

!     INPUT MATERIAL PROPERTIES

      if(isw.eq.1) then

!       Record:

        spring = .false.
        masses = .false.

        vtype = 'start'
        do while(.not.pcomp(vtype,'    ',4))

          errck = tinput(vtype,1,td,6)

!         Mass parameter

          if    (pcomp(vtype,'mass',4)) then

            do i = 1,ndf
              d(i) = td(i)
            end do ! i
            masses = .true.

!         Spring parameter

          elseif(pcomp(vtype,'spri',4)) then

            do i = 1,ndf
              d(i+10) = td(i)
            end do ! i
            spring = .true.

!         Inadmissible data

          elseif(.not.pcomp(vtype,'    ',4)) then
            write(  *,4000) vtype
            write(iow,4000) vtype

          end if

        end do ! while

!       Output material properties

        write(iow,2000)
        if(masses) then
          write(iow,2001) (i,d(i),i=1,ndf)
        endif
        if(spring) then
          write(iow,2002) (i,d(i+10),i=1,ndf)
        endif

!       Set for no plots

        pstyp = 0

!     CHECK ELEMENTS

      elseif(isw.eq.2) then

!     COMPUTE ELEMENT STIFFNESS AND RESIDUAL ARRAYS

      elseif(isw.eq.3  .or. isw.eq.6 ) then

        do i = 1,ndf
          r(i,1) = -d(i)*ul(i,1,5) - d(i+10)*ul(i,1,1)
          s(i,i) =  d(i)*ctan(3)   + d(i+10)*ctan(1)
        end do ! i

!     COMPUTE MASS MATRIX

      elseif(isw.eq.5) then

        do i = 1,ndf
          r(i,1) = d(i)
          s(i,i) = d(i)
        end do ! i

      endif

!     I/O Formats

2000  format(9x,'P o i n t    E l e m e n t'/)
2001  format(14x,'MASS(',i1,')     :',1p,1e12.4)
2002  format(14x,'SPRING(',i1,')   :',1p,1e12.4)

4000  format(' *WARNING* Property: ',a,' is not valid')

      end subroutine pointnd

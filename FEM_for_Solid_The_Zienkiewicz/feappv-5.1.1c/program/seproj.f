!$Id:$
      subroutine seproj(p,s,se,dt,st,ser,ix,nel,nen,numnp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    21/04/2018
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Assemble element projection quantities into global ones

!      Inputs:
!        p(*)        - Element nodal weights
!        s(*)        - Element projected quantities
!        se(*)       - Element error quantities
!        ix*)        - Element node numbers
!        nel         - Number element nodes
!        nen         - Number element nodes max
!        numnp       - Number global nodes

!      Outputs:
!        dt(*)       - Global nodal weights
!        st(*)       - Global projected quanties
!        ser(*)      - Global error quanties
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'sdata.h'
      include   'strnum.h'
      include   'iofile.h'

      integer       :: nel,nen,numnp, nd,nn,i
      integer       :: ix(*)
      real (kind=8) :: p(nen),s(nen,*),se(*)
      real (kind=8) :: dt(numnp),st(numnp,*),ser(*)

      save

!     Check for error on projection size

      if(iste.gt.istv) then
        write(  *,3000) iste,istv,iste
        write(iow,3000) iste,istv,iste
        call plstop(.true.)
      endif

!     Projection assembly over element nodes

      do nd = 1,nel
        nn = ix(nd)
        if(nn.gt.0) then
          dt(nn)  = dt(nn) + p(nd)
!         Assemble error indicators
          ser(nn) = ser(nn) + se(nd)
!         Assemble integated stress
          do i = 1,iste
            st(nn,i) = st(nn,i) + s(nd,i)
          end do ! i
        endif
      end do ! nd

!     Formats

3000  format(' --> *ERROR* in User Element Projections:'//
     &   13x,'Number  projected  items = ',i3/
     &   13x,'Current number specified = ',i3//
     &   13x,'Correct by adding include file strnum.h & statement:'/
     &   17x,'istv = max(istv,'i3,')'/
     &   13x,'in isw.eq.1 part of user element.'/
     &   13x,'See FEAP Programmer Manual for details.'/)

      end subroutine seproj

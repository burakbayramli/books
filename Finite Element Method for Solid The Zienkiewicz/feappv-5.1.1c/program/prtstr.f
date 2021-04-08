!$Id:$
      subroutine prtstr(x,dp,ds,ndm,numnp,n1,n2,n3,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Output nodal projected stresses and principal values

!      Inputs:
!         x(ndm,*)    - Nodal coordinates of mesh
!         dp(numnp,*) - Principal values at nodes
!         ds(numnp,*) - Stress values at nodes
!         ndm         - Spatial dimension of mesh
!         numnp       - Number of nodes in mesh
!         n1          - Number of first node to output
!         n2          - Number of last  node to output
!         n3          - Increment to node from n1
!         prth        - Output title/header data if true

!      Outputs:
!         None        - Outputs to file/screen
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'
      include  'fdata.h'
      include  'strnum.h'
      include  'xtout.h'

      logical       :: cknon0,vnon0,prth
      integer       :: ndm,numnp,n1,n2,n3, i, n, ista, n_count, nxt1
      real (kind=8) :: x(ndm,*),dp(numnp,*),ds(numnp,*)

      save

!     Determine how many non-zero nodal values there are

      ista = 0
      do n = 1,abs(istv)
        if(cknon0(ds(1,n),numnp) ) ista = n
      end do

      if(ista.gt.0) then
        n_count = 0
        nxt1  = max(1,nxt)
        do n = n1,n2,n3
          if( nxt.eq.0 .or. abs(x(nxt1,n)-xt).le.xtol ) then
            vnon0 = .false.
            do i = 1,ista
              if(ds(n,i).ne.0.0d0) vnon0 = .true.
            end do
            if(vnon0) then
              n_count = n_count - 1
              if(n_count.le.0) then
                call prtitl(prth)
                write(iow,2000) (i,i=1,3),(i,i=1,ista)
                if(ior.lt.0.and.pfr) then
                  write(*,2000) (i,i=1,3),(i,i=1,ista)
                endif
                n_count = 50
              endif
              write(iow,2001) n,(dp(n,i),i=1,7),(ds(n,i),i=1,ista)
              if(ior.lt.0.and.pfr) then
                write(*,2001) n,(dp(n,i),i=1,7),(ds(n,i),i=1,ista)
              endif
            endif
          endif
        end do
      else
        if(ior.lt.0.and.pfr) write(*,*) 'All values zero'
        write(iow,*) 'All values zero'
      endif

!     Formats

2000  format('   N o d a l   P r o j e c t i o n s'//'   Node',
     & 3(i3,'-Pr.Value'),'  1-Pr.Angle'/
     & 7x,'   I_1 Value Mises Value   J_3 Value'/(7x,6(i6,' Value')))

2001  format(/i7,1p,4e12.4/7x,1p,3e12.4/(7x,1p,6e12.4))

      end subroutine prtstr

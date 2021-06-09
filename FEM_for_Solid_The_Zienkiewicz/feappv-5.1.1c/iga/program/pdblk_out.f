!$Id:$
      subroutine pdblk_out(ndisp)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: NURBS displacement outputs

!      Inputs:
!        ndisp(9)   -

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'sdata.h'
      include   'pointer.h'
      include   'comblk.h'

      integer    iload,itype
      real*8     ndisp(9)

      iload = nint(ndisp(8))
      itype = nint(ndisp(9))
      if(iload.eq.3) then
        call pnurbdiout(itype,ndisp,hr(np(43)),hr(np(27)+nneq))
      endif

      end

      subroutine pnurbdiout(itype,ndisp,x,u)

      implicit   none

      include   'iofile.h'
      include   'cdata.h'
      include   'sdata.h'

      logical    pflag
      integer    itype, i,n, nod
      real*8     ndisp(8), x(ndm,numnp),u(ndf,numnp), ux,uy
      real*8     Unorm, Edisp

      Unorm = 0.0d0
      Edisp = 0.0d0

      write(iow,2000)
      nod = 0
      do n = 1,numnp
        pflag = .false.
        do i = 1,ndm
          if(u(i,n).ne.0.0d0) then
            pflag = .true.
            exit
          endif
        end do ! i
        if(pflag) then
          if(itype.eq.1) then
            call pcircle (x(1,n),ndisp, ux,uy, .false.)
          elseif(itype.eq.2) then
            call pellipse(x(1,n),ndisp, ux,uy, .false.)
          endif
          write(iow,2001) n,(u(i,n),i=1,ndm),ux,uy
          Unorm = Unorm + u(1,n)**2 + u(2,n)**2
          Edisp = Edisp + (u(1,n) - ux)**2 + (u(2,n) - uy)**2
          nod   = nod + 1
        endif
      end do

      Unorm = sqrt(Unorm)/dble(nod)
      Edisp = sqrt(Edisp)/dble(nod)
      write(  *,*) ' U_norm   =',Unorm,' U_error ',Edisp
      write(iow,*) ' U_norm   =',Unorm,' U_error ',Edisp

!     Formats

2000  format(4x,'Node',6x,'u_approx',6x,'v_approx'/
     &                14x,'u_exact ',6x,'v_exact ')
2001  format(i8,1p,2e24.15/8x,1p,2e24.15)

      end

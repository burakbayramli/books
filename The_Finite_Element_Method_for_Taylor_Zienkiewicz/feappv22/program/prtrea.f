c$Id:$
      subroutine prtrea(r,x,ndm,ndf,n1,n2,n3,prth)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Output nodal reactions for current solution

c      Inputs:
c         x(*)      - Current value of reactions
c         x(ndm,*)  - Nodal coordinates of mesh
c         ndm       - Spatial dimension of mesh
c         ndf       - Number dof/node
c         n1        - First node to output
c         n2        - Last noed to output
c         n3        - Increment to n1
c         prth      - Output title/header data if true

c      Outputs:
c         None      - Outputs to file
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'fdata.h'
      include  'iofile.h'
      include  'xtout.h'

      logical   prth
      integer   ndm,ndf,n1,n2,n3, i,k,n,  count, nxt1

      real*8    x(ndm,*),r(ndf,*),rsum(6),asum(6),psum(6)

      save

      do k = 1,ndf
        psum(k) = 0.d0
        rsum(k) = 0.d0
        asum(k) = 0.d0
      end do
      do i = 1,numnp
        do k = 1,ndf
          rsum(k) = rsum(k) - r(k,i)
          asum(k) = asum(k) + dabs(r(k,i))
        end do
      end do
      count = 0
      nxt1  = max(1,nxt)
      do n = n1,n2,n3
        if(nxt.eq.0 .or. abs(x(nxt1,n)-xt).le.xtol ) then
          count = count - 1
          do k = 1,ndf
            psum(k) = psum(k) - r(k,n)
          end do
          if(count.le.0) then
            call prtitl(prth)
            write(iow,2000) (k,k=1,ndf)
            if(ior.lt.0.and.pfr) then
              write(*,2000) (k,k=1,ndf)
            endif
            count = 50
          endif
          if(ior.lt.0.and.pfr) then
            write(*,2001) n,(-r(k,n),k=1,ndf)
          endif
          write(iow,2001) n,(-r(k,n),k=1,ndf)
        endif
      end do

c     Print sum checks

      write(iow,2002) (psum(k),k=1,ndf)
      write(iow,2003) (rsum(k),k=1,ndf)
      write(iow,2004) (asum(k),k=1,ndf)
      if(ior.lt.0.and.pfr) then
        write(*,2002) (psum(k),k=1,ndf)
        write(*,2003) (rsum(k),k=1,ndf)
        write(*,2004) (asum(k),k=1,ndf)
      endif

c     Formats

2000  format('  N o d a l    R e a c t i o n s'//
     &  '   Node',6(i8,' dof'):/(7x,6(i8,' dof')))

2001  format(i7,1p,6e12.4:/(7x,1p,6e12.4))

2002  format(/' Pr.Sum',1p,6e12.4:/(7x,1p,6e12.4))

2003  format( '   Sum ',1p,6e12.4:/(7x,1p,6e12.4))

2004  format( '  |Sum|',1p,6e12.4:/(7x,1p,6e12.4))

      end

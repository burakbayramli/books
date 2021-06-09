!$Id:$
      subroutine stcn3d(ix,d,xl,th,ul,shp,dt,st,
     &                     ndf,ndm,nel,numnp,nhv,istrt)

!_____________________________________________________________________c
      implicit  none

      include  'hdata.h'
      include  'comblk.h'

      integer   ndf,ndm,nel,numnp,nhv,istrt
      integer   ii, l, ll, lint, nn

      real*8    xsj, xj, ta

      integer   ix(*)
      real*8    dt(numnp),st(numnp,*),xl(ndm,*),th(*),shp(4,8)
      real*8    d(*),sig(8),eps(9,3),dd(6,6,5),ul(ndf,*),sg(4,8)
      real*8    sv(5,4)

      save

!     Compute stress projections to nodes

      l    = 2
      if(nel.eq.4) then
        call tint3d(l,lint,sv)
      else
        call int3d(l,lint,sg)
      endif

!     Set initial counter for history terms in stress/strain

      nn   = 0
      do l = 1,lint
        if(nel.eq.4) then
          call tetshp(sv(1,l),xl,ndm,xsj,shp)
          xsj = xsj*sv(5,l)
        else
          call shp3d(sg(1,l),xsj,shp,xl,ndm)
          xsj = xsj*sg(4,l)
        endif

!       Compute strain at point

        call strn3d(d,ul,th,shp,ndf,nel, eps,ta)

!       Compute stress at point

        call modlsd(l,d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt,
     &              dd,sig,  8)

!       Compute projections: int ( sig * shp(i) * darea )

        do ii = 1,nel
          ll = abs(ix(ii))
          if(ll.ne.0) then
            xj     = xsj*shp(4,ii)
            dt(ll) = dt(ll) + xj
            st(ll,1) = st(ll,1) + sig(1)*xj
            st(ll,2) = st(ll,2) + sig(2)*xj
            st(ll,3) = st(ll,3) + sig(3)*xj
            st(ll,4) = st(ll,4) + sig(4)*xj
            st(ll,5) = st(ll,5) + sig(5)*xj
            st(ll,6) = st(ll,6) + sig(6)*xj
          endif
        end do
        nn = nn + nhv
      end do

      end

!$Id:$
      subroutine vem_plcon(k, nc, nel, xl, iplt, icl, v, vc, cont)

      implicit   none

      logical          :: cont
      integer          :: k, nc,nel, i
      integer          :: iplt(*), icl(*)
      real    (kind=8) :: xl(3,*), v(*), vc(*)
      real    (kind=8) :: xm(3,3), vq(4)

!     Linear VEM

      if(k.eq.1) then
        vq(3)   = 0.0d0
        xm(:,3) = 0.0d0

        do i = 1,nel
          vq(3)   = vq(3) + v(i)
          xm(:,3) = xm(:,3) + xl(:,i)
        end do ! i
        vq(3)   = vq(3)/dble(nel)
        xm(:,3) = xm(:,3)/dble(nel)

!       Plot triangular projections

        do i = 1,nel
          vq(1) = v(iplt(i))
          vq(2) = v(iplt(i+1))
          xm(:,1) = xl(:,iplt(i))
          xm(:,2) = xl(:,iplt(i+1))
          if(cont) then
            call pltecn(xm,vq,vc,nc)
          else
            call pltcor(3,icl,vq,vc,nc)
            call pltefl(3,icl,xm,vq,vc,nc)
          endif
        end do ! i

!     Quadratic VEM

      elseif(k.eq.2) then
        vq(3)   = 0.0d0
        xm(:,3) = 0.0d0

        do i = 1,nel-mod(nel,2)
          vq(3)   = vq(3) + v(i)
          xm(:,3) = xm(:,3) + xl(:,i)
        end do ! i
        vq(3)   = vq(3)/dble(nel-mod(nel,2))
        xm(:,3) = xm(:,3)/dble(nel-mod(nel,2))

!       Plot triangular projections

        do i = 1,nel-mod(nel,2),2
          vq(1) = v(iplt(i))
          vq(2) = v(iplt(i+1))
          xm(:,1) = xl(:,iplt(i))
          xm(:,2) = xl(:,iplt(i+1))
          if(cont) then
            call pltecn(xm,vq,vc,nc)
          else
            call pltcor(3,icl,vq,vc,nc)
            call pltefl(3,icl,xm,vq,vc,nc)
          endif
          vq(1) = v(iplt(i+1))
          vq(2) = v(iplt(i+2))
          xm(:,1) = xl(:,iplt(i+1))
          xm(:,2) = xl(:,iplt(i+2))
          if(cont) then
            call pltecn(xm,vq,vc,nc)
          else
            call pltcor(3,icl,vq,vc,nc)
            call pltefl(3,icl,xm,vq,vc,nc)
          endif
        end do ! i

!     k-order VEM

      else
        write(*,*) ' Not available in vFEAPpv'
      endif

      end subroutine vem_plcon

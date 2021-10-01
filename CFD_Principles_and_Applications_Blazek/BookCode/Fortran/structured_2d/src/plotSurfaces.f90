!> @file plotSurfaces.f90
!!
!! Output of flow variables at the surfaces.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: March 11, 2014
!
! *****************************************************************************
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!
! *****************************************************************************

!> Writes out selected values at walls, injection and symmetry boundaries
!! in Vis2D format. Quantities are interpolated from cell centers to grid
!! nodes by arithmetic averaging.
!!
subroutine PlotSurfaces

  use ModDataTypes
  use ModControl
  use ModFiles
  use ModGeometry
  use ModNumerics
  use ModPhysics
  use ModPlotQuant
  use ModInterfaces, only : ErrorMessage
  implicit none

! local variables
  character(chrlen) :: fname
  integer     :: errFlag, itype, lb, lbeg, lend, lstep, ibeg, iend, &
                 istep, jbeg, jend, jstep, nquant, nsurfs
  integer     :: i, j, iseg, m, m1
  real(rtype) :: rho, u, v, e, press, temp, c, ptot, ttot, mach, machis, &
                 ptloss, pratio, ptotinf, gamav, cpav, gam1, ggm1
  real(rtype) :: cf, cp, viscav, sx, sy, ds, sxn, syn, grdnx, grdny, grdnn, &
                 dvdnx, dvdny, dvdna, sgn
  real(rtype) :: varout(mxquant+2), gradv(2,2)

! *****************************************************************************

  ptotinf = 0.D0
  if (kflow == "E") then
    gam1    = gamma - 1.D0
    ggm1    = gamma/gam1
    ptotinf = pinf*(1.D0+0.5D0*gam1*machinf*machinf)**ggm1
  endif

! open plot file

  write(fname,"(A,I5.5,A)") Trim(fnSurf),iter,".v2d"
  open(unit=ifSurf, file=fname, status="unknown", action="write", iostat=errFlag)
  if (errFlag /= 0) call ErrorMessage( "cannot open plot file (surfaces)" )

! header

  nquant = 0
  do m=1,mxquant
    if (lquant(m) == "Y") nquant = nquant + 1
  enddo

  nsurfs = 0  ! no. of surfaces to output
  do iseg=1,nsegs
    itype = lbsegs(iseg,1)
    if ((itype>=300 .and. itype<600) .or. &
        (itype>=800 .and. itype<900)) nsurfs = nsurfs + 1
  enddo

  write(ifSurf,1000) Trim(title),nsurfs,nquant+2

! names of variables

  do m=1,mxquant
    if (lquant(m) == "Y") then
      write(ifSurf,"(A)") Trim(cquant(m))
    endif
  enddo

! compute quantities & write'em out

  do iseg=1,nsegs

    itype = lbsegs(iseg,1)
    lb    = lbsegs(iseg,2)
    lbeg  = lbsegs(iseg,3)
    lend  = lbsegs(iseg,4)

    if ((itype>=300 .and. itype<600) .or. &
        (itype>=800 .and. itype<900)) then

                       lstep =  1
      if (lbeg > lend) lstep = -1
      if      (lb == 1) then
        ibeg  = lbeg - Min(lstep,0)
        iend  = lend + Max(lstep,0)
        istep = lstep
        jbeg  = 2
        jend  = 2
        jstep = 1
      else if (lb == 2) then
        ibeg  = il
        iend  = il
        istep = 1
        jbeg  = lbeg - Min(lstep,0)
        jend  = lend + Max(lstep,0)
        jstep = lstep
      else if (lb == 3) then
        ibeg  = lbeg - Min(lstep,0)
        iend  = lend + Max(lstep,0)
        istep = lstep
        jbeg  = jl
        jend  = jl
        jstep = 1
      else if (lb == 4) then
        ibeg  = 2
        iend  = 2
        istep = 1
        jbeg  = lbeg - Min(lstep,0)
        jend  = lend + Max(lstep,0)
        jstep = lstep
      endif

      if (itype>=300 .and. itype<500) then
        write(ifSurf,1010) Abs(lend-lbeg)+2,"Wall"
      else if (itype>=500 .and. itype<600) then
        write(ifSurf,1010) Abs(lend-lbeg)+2,"Symmetry"
      else
        write(ifSurf,1010) Abs(lend-lbeg)+2,"Injection"
      endif

      do j=jbeg,jend,jstep
        do i=ibeg,iend,istep

          varout(1) = x(i,j)
          varout(2) = y(i,j)

          rho = 0.25D0*(cv(1,i,j)+cv(1,i-1,j)+cv(1,i-1,j-1)+cv(1,i,j-1))
          u   = 0.25D0*(cv(2,i  ,j  )/cv(1,i  ,j  )+ &
                        cv(2,i-1,j  )/cv(1,i-1,j  )+ &
                        cv(2,i-1,j-1)/cv(1,i-1,j-1)+ &
                        cv(2,i  ,j-1)/cv(1,i  ,j-1))
          v   = 0.25D0*(cv(3,i  ,j  )/cv(1,i  ,j  )+ &
                        cv(3,i-1,j  )/cv(1,i-1,j  )+ &
                        cv(3,i-1,j-1)/cv(1,i-1,j-1)+ &
                        cv(3,i  ,j-1)/cv(1,i  ,j-1))
          e   = 0.25D0*(cv(4,i  ,j  )/cv(1,i  ,j  )+ &
                        cv(4,i-1,j  )/cv(1,i-1,j  )+ &
                        cv(4,i-1,j-1)/cv(1,i-1,j-1)+ &
                        cv(4,i  ,j-1)/cv(1,i  ,j-1))
          press = 0.25D0*(dv(1,i,j)+dv(1,i-1,j)+dv(1,i-1,j-1)+dv(1,i,j-1))
          temp  = 0.25D0*(dv(2,i,j)+dv(2,i-1,j)+dv(2,i-1,j-1)+dv(2,i,j-1))
          c     = 0.25D0*(dv(3,i,j)+dv(3,i-1,j)+dv(3,i-1,j-1)+dv(3,i,j-1))
          gamav = 0.25D0*(dv(4,i,j)+dv(4,i-1,j)+dv(4,i-1,j-1)+dv(4,i,j-1))
          cpav  = 0.25D0*(dv(5,i,j)+dv(5,i-1,j)+dv(5,i-1,j-1)+dv(5,i,j-1))

          if (kequs == "N") then
            viscav = 0.25D0*(dv(6,i,j)+dv(6,i-1,j)+dv(6,i-1,j-1)+dv(6,i,j-1))
            if      (lb == 1) then
              m  = Max( 2,i  )
              m  = Min(i2,i  )
              m1 = Max( 2,i-1)
              m1 = Min(i2,i-1)
              gradv(1,1) = 0.5D0*(gradfj(1,m,2)+gradfj(1,m1,2))
              gradv(1,2) = 0.5D0*(gradfj(2,m,2)+gradfj(2,m1,2))
              gradv(2,1) = 0.5D0*(gradfj(3,m,2)+gradfj(3,m1,2))
              gradv(2,2) = 0.5D0*(gradfj(4,m,2)+gradfj(4,m1,2))
              sx         = -0.5D0*(sj(1,m,2)+sj(1,m1,2))
              sy         = -0.5D0*(sj(2,m,2)+sj(2,m1,2))
            else if (lb == 2) then
              m  = Max( 2,j  )
              m  = Min(j2,j  )
              m1 = Max( 2,j-1)
              m1 = Min(j2,j-1)
              gradv(1,1) = 0.5D0*(gradfi(1,il,m)+gradfi(1,il,m1))
              gradv(1,2) = 0.5D0*(gradfi(2,il,m)+gradfi(2,il,m1))
              gradv(2,1) = 0.5D0*(gradfi(3,il,m)+gradfi(3,il,m1))
              gradv(2,2) = 0.5D0*(gradfi(4,il,m)+gradfi(4,il,m1))
              sx         = 0.5D0*(si(1,il,m)+si(1,il,m1))
              sy         = 0.5D0*(si(2,il,m)+si(2,il,m1))
            else if (lb == 3) then
              m  = Max( 2,i  )
              m  = Min(i2,i  )
              m1 = Max( 2,i-1)
              m1 = Min(i2,i-1)
              gradv(1,1) = 0.5D0*(gradfj(1,m,jl)+gradfj(1,m1,jl))
              gradv(1,2) = 0.5D0*(gradfj(2,m,jl)+gradfj(2,m1,jl))
              gradv(2,1) = 0.5D0*(gradfj(3,m,jl)+gradfj(3,m1,jl))
              gradv(2,2) = 0.5D0*(gradfj(4,m,jl)+gradfj(4,m1,jl))
              sx         = 0.5D0*(sj(1,m,jl)+sj(1,m1,jl))
              sy         = 0.5D0*(sj(2,m,jl)+sj(2,m1,jl))
            else if (lb == 4) then
              m  = Max( 2,j  )
              m  = Min(j2,j  )
              m1 = Max( 2,j-1)
              m1 = Min(j2,j-1)
              gradv(1,1) = 0.5D0*(gradfi(1,2,m)+gradfi(1,2,m1))
              gradv(1,2) = 0.5D0*(gradfi(2,2,m)+gradfi(2,2,m1))
              gradv(2,1) = 0.5D0*(gradfi(3,2,m)+gradfi(3,2,m1))
              gradv(2,2) = 0.5D0*(gradfi(4,2,m)+gradfi(4,2,m1))
              sx         = -0.5D0*(si(1,2,m)+si(1,2,m1))
              sy         = -0.5D0*(si(2,2,m)+si(2,2,m1))
            endif
            ds    = Sqrt(sx*sx+sy*sy)
            sxn   = sx/ds
            syn   = sy/ds
            grdnx = gradv(1,1)*sxn + gradv(1,2)*syn
            grdny = gradv(2,1)*sxn + gradv(2,2)*syn
            grdnn = grdnx*sxn + grdny*syn
            dvdnx = grdnx - grdnn*sxn
            dvdny = grdny - grdnn*syn
            if (lb==1 .or. lb==3) sgn = Sign(1.D0,grdnx)
            if (lb==2 .or. lb==4) sgn = Sign(1.D0,grdny)
            dvdna = Sqrt(dvdnx*dvdnx+dvdny*dvdny)
            if (kflow == "E") then
              cf = 2.D0*sgn*viscav*dvdna/(rhoinf*qinf*qinf)
            else
              cf = 2.D0*sgn*viscav*dvdna/(refrho*refvel*refvel)
            endif
          else
            viscav = 0.D0
            cf     = 0.D0
          endif

          gam1  = gamav - 1.D0
          ggm1  = gamav/gam1
          mach  = Sqrt(u*u+v*v)/c
          ttot  = (e+press/rho)/cpav
          ptot  = press*(ttot/temp)**ggm1
          if (kflow == "E") then
            ptloss = 1.D0 - ptot/ptotinf
            pratio = ptotinf/press
            cp     = 2.*(pinf-press)/(rhoinf*qinf*qinf)
          else
            ptloss = 1.D0 - ptot/ptinl
            pratio = ptinl/press
            cp     = 2.*((p12rat*pout)-press)/(refrho*refvel*refvel)
          endif
          machis = (pratio**(1.D0/ggm1)-1.D0)*2.D0/gam1
          machis = Max(machis, 0.D0)
          machis = Sqrt(machis)

! ------- store quantities in varout()
          nquant = 2
          do m=1,mxquant
            if (lquant(m) == "Y") then
              nquant = nquant + 1

! ----------- density
              if (m == 1) then
                varout(nquant) = rho

! ----------- u-velocity
              else if (m == 2) then
                varout(nquant) = u

! ----------- v-velocity
              else if (m == 3) then
                varout(nquant) = v

! ----------- static pressure
              else if (m == 4) then
                varout(nquant) = press

! ----------- total pressure
              else if (m == 5) then
                varout(nquant) = ptot

! ----------- static temperature
              else if (m == 6) then
                varout(nquant) = temp

! ----------- total temperature
              else if (m == 7) then
                varout(nquant) = ttot

! ----------- local Mach number
              else if (m == 8) then
                varout(nquant) = mach

! ----------- isentropic Mach number
              else if (m == 9) then
                varout(nquant) = machis

! ----------- total pressure loss
              else if (m == 10) then
                varout(nquant) = ptloss

! ----------- laminar viscosity coefficient
              else if (m == 11) then
                varout(nquant) = viscav

! ----------- skin friction coefficient
              else if (m == 12) then
                varout(nquant) = cf

! ----------- pressure coefficient
              else if (m == 13) then
                varout(nquant) = cp
              endif
            endif
          enddo

          write(ifSurf,1020) (varout(m), m=1,nquant)

        enddo ! i
      enddo   ! j

    endif ! itype

  enddo ! iseg

  close(unit=ifSurf)

1000  format(A,/,"1",/,"Boundaries",/,I3,I3,/,"x [m]",/,"y [m]")
1010  format(I6," 0",/,"0 0 0",/,A)
1020  format(1P,20E16.8)

end subroutine PlotSurfaces

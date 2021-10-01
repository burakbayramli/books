!> @file plotSurfaces.f90
!!
!! Output of flow variables at the surfaces.
!
! *****************************************************************************
!
!  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
!  Created February 25, 2014
!  Last modification: June 4, 2014
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

!> Writes out selected values at walls and symmetry boundaries in Vis2D format.
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
  integer     :: errFlag, itype, ibegf, iendf, ibegn, iendn, ibf1, ibf2, &
                 nquant, nsurfs
  integer     :: i, ib, ibf, ibn, m
  real(rtype) :: rrho, u, v, e, press, temp, c, ptot, ttot, mach, machis, &
                 ptloss, pratio, ptotinf, gam1, ggm1
  real(rtype) :: cf, cp, visc, sx, sy, ds, sxn, syn, grdnx, grdny, grdnn, &
                 dvdnx, dvdny, dvdna, sgn
  real(rtype) :: varout(mxquant+2)

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
  do ib=1,nsegs
    itype = btype(ib)
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

  ibegf = 1
  ibegn = 1

  do ib=1,nsegs

    iendf = ibound(1,ib)
    iendn = ibound(2,ib)
    itype = btype(ib)

    if ((itype>=300 .and. itype<600) .or. &
        (itype>=800 .and. itype<900)) then

      write(ifSurf,1010) iendn-ibegn+1,Trim(bname(ib))

      do ibn=ibegn,iendn
        i = bnode(1,ibn)

        varout(1) = x(i)
        varout(2) = y(i)

        rrho  = 1.D0/cv(1,i)
        u     = cv(2,i)*rrho
        v     = cv(3,i)*rrho
        e     = cv(4,i)*rrho
        press = dv(1,i)
        temp  = dv(2,i)
        c     = dv(3,i)
        gam1  = dv(4,i) - 1.D0
        ggm1  = dv(4,i)/gam1

        if (kequs == "N") then
          ibf1 = -1
          ibf2 = -1
          do ibf=ibegf,iendf
            if (bface(1,ibf) == i) then
              if (ibf1 < 0) then
                ibf1 = ibf
              else
                ibf2 = ibf
              endif
            endif
            if (bface(2,ibf) == i) then
              if (ibf1 < 0) then
                ibf1 = ibf
              else
                ibf2 = ibf
              endif
            endif
          enddo
          if (ibf2 < 0) ibf2 = ibf1
          visc  = dv(6,i)
          sx    = -0.5D0*(sbf(1,ibf1)+sbf(1,ibf2))   ! to point inside
          sy    = -0.5D0*(sbf(2,ibf1)+sbf(2,ibf2))
          ds    = Sqrt(sx*sx+sy*sy)
          sxn   = sx/ds
          syn   = sy/ds
          grdnx = gradx(2,i)*sxn + grady(2,i)*syn
          grdny = gradx(3,i)*sxn + grady(3,i)*syn
          grdnn = grdnx*sxn + grdny*syn
          dvdnx = grdnx - grdnn*sxn
          dvdny = grdny - grdnn*syn
          if (grdnx > grdny) then    ! to get somehow the main flow
            sgn = Sign(1.D0,grdnx)
          else
            sgn = Sign(1.D0,grdny)
          endif
          dvdna = Sqrt(dvdnx*dvdnx+dvdny*dvdny)
          if (kflow == "E") then
            cf = 2.D0*sgn*visc*dvdna/(rhoinf*qinf*qinf)
          else
            cf = 2.D0*sgn*visc*dvdna/(refrho*refvel*refvel)
          endif
        else
          visc = 0.D0
          cf   = 0.D0
        endif

        mach  = Sqrt(u*u+v*v)/c
        ttot  = (e+press*rrho)/dv(5,i)
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

! ----- store quantities in varout()
        nquant = 2
        do m=1,mxquant
          if (lquant(m) == "Y") then
            nquant = nquant + 1

! --------- density
            if (m == 1) then
              varout(nquant) = cv(1,i)

! --------- u-velocity
            else if (m == 2) then
              varout(nquant) = u

! --------- v-velocity
            else if (m == 3) then
              varout(nquant) = v

! --------- static pressure
            else if (m == 4) then
              varout(nquant) = press

! --------- total pressure
            else if (m == 5) then
              varout(nquant) = ptot

! --------- static temperature
            else if (m == 6) then
              varout(nquant) = temp

! --------- total temperature
            else if (m == 7) then
              varout(nquant) = ttot

! --------- local Mach number
            else if (m == 8) then
              varout(nquant) = mach

! --------- isentropic Mach number
            else if (m == 9) then
              varout(nquant) = machis

! --------- total pressure loss
            else if (m == 10) then
              varout(nquant) = ptloss

! --------- laminar viscosity coefficient
            else if (m == 11) then
              varout(nquant) = visc

! --------- skin friction coefficient
            else if (m == 12) then
              varout(nquant) = cf

! --------- pressure coefficient
            else if (m == 13) then
              varout(nquant) = cp
            endif
          endif
        enddo

        write(ifSurf,1020) (varout(m), m=1,nquant)

      enddo ! node

    endif ! itype

    ibegf = iendf + 1
    ibegn = iendn + 1

  enddo ! ib

  close(unit=ifSurf)

1000  format(A,/,"1",/,"Boundaries",/,I3,I3,/,"x [m]",/,"y [m]")
1010  format(I6," 0",/,"0 0 0",/,A)
1020  format(1P,20E16.8)

end subroutine PlotSurfaces


!     Mechanical

      real (kind=8)::  gradum,      graduvol     , gradubar
      common /fe2varr/ gradum(3,3), graduvol(3,3), gradubar(3,3)

      real (kind=8)::  sigvol   , udmulti
      common /fe2varr/ sigvol(6), udmulti(6,3,125)

!     Thermal

      real (kind=8)::  gradth,    gradthvol   , gradthbar
      common /fet2arr/ gradth(3), gradthvol(3), gradthbar(3)

      real (kind=8)::  fluxvol   , tmulti
      common /fet2arr/ fluxvol(3), tmulti(3,125)

!     Coupled

      real (kind=8)::   dtmulti,        tdmulti
      common /fet2arr/  dtmulti(6,125), tdmulti(3,3,125)


!     General

      real (kind=8)::  volr, vols, vcur, vref
      common /fe2varr/ volr, vols, vcur, vref

      logical          fbarfe, ebarfe, epsfe, iksolv
      common /fe2varl/ fbarfe, ebarfe, epsfe, iksolv

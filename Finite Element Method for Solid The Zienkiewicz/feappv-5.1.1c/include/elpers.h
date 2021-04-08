
      real (kind=8):: gradu     ,epsc     ! Gradient of displacement
      common /elpers/ gradu(3,3),epsc(9)

      real (kind=8):: ptau   ,pctau       ! Recovered stress & tangent
      common /elpers/ ptau(6),pctau(6,6)

      real (kind=8):: gradt               ! Thermal gradient
      common /elpers/ gradt(3)

      real (kind=8):: pflux   ,pcflux     ! Recovered flux & tangent
      common /elpers/ pflux(3),pcflux(3,3)

      real (kind=8):: xc   ,xc0   ,volm0  ! RVE Size
      common /elpers/ xc(3),xc0(3),volm0

      real (kind=8):: fdet, ttemp, opar
      common /elpers/ fdet, ttemp, opar(2)

      integer         dsend, drecv
      common /elpers/ dsend, drecv

      integer         prtype, prpropu, prpropt  ! Problem type
      common /elpers/ prtype, prpropu, prpropt

      logical         finflg              ! Finite deformation (true)
      common /elpers/ finflg

      logical         filflg              ! File inputs if     (true)
      common /elpers/ filflg

      logical         fluxfl, stflag      ! Output control
      common /elpers/ fluxfl, stflag

      logical         hillfl, kirkfl      ! Multiscale type
      common /elpers/ hillfl, kirkfl

      logical         iflxfl, istrfl      ! Multiscale gradient type
      common /elpers/ iflxfl, istrfl

      character       perifile*128        ! Periodic file
      common /elperc/ perifile

      real (kind=8):: gradu0              ! Periodic inputs     
      common /elperg/ gradu0(3,3)

      real (kind=8):: gradt0   , temp0    ! Periodic inputs     
      common /elperg/ gradt0(3), temp0

      real (kind=8):: epsbar0   ,grdbar0     ,gdtbar0
      common /elperg/ epsbar0(6),grdbar0(3,3),gdtbar0(3)

      real (kind=8):: epsbar   ,grdbar     ,gdtbar
      common /elperg/ epsbar(6),grdbar(3,3),gdtbar(3)

      real (kind=8):: epsbarn   ,grdbarn     ,gdtbarn
      common /elperg/ epsbarn(6),grdbarn(3,3),gdtbarn(3)



      integer          ::  k_order         ! Loop counter
      integer          ::  ltot            ! Total number of GP's
      integer          ::  nk, nkm1, nkm2  ! Loop counter
      common /vem_datai/   k_order,ltot,nk,nkm1,nkm2

      integer          ::  qtype
      common /vem_datai/   qtype

      real    (kind=8) ::  vol,volhm       !
      real    (kind=8) ::  hVm1            ! 
      common /vem_datar/   vol,volhm,hVm1

      real    (kind=8) ::  Dmat(65,10)     ! Stabilization matrix
      real    (kind=8) ::  Pdmat(6,2,65)   ! Derivative projection
      real    (kind=8) ::  Plmat(2,2,65)   ! Linking deriv. projection
      real    (kind=8) ::  P0mat(10,65)    ! Function   projection
      common /vem_datar/   Dmat, Pdmat, Plmat, P0mat

      real    (kind=8) ::  xp(2,64)        ! GP Cartesian coordinates
      real    (kind=8) ::  xc(2)           ! Centroid coordinate 
      common /vem_datar/   xp,xc

      real    (kind=8) ::  mm(10,64)       ! Interpolation at GP
      real    (kind=8) ::  dd(10,10)       ! Projection matrices
      real    (kind=8) ::  dDmat(10)       ! Projection matrices
      real    (kind=8) ::  ss(65,65)       ! Stabilization matrix
      common /vem_datar/   mm,dd,dDmat,ss

      real    (kind=8) ::  m0, m1,m2       ! Parameters for mid-sides
      common /vem_datar/   m0, m1,m2

!     Reissner-Mindlin Plates

      real    (kind=8) ::  Hmat(5,5)
      real    (kind=8) ::  Gmat(5,66)
      real    (kind=8) ::  Rmat(5)
      common /vem_datarm/  Hmat, Gmat, Rmat

      real    (kind=8) ::  elv(4,125)
      integer          ::             lintv(20)
      common /vem_dataq/   elv       ,lintv

      real    (kind=8) ::  elq(4,3),elc(4,7)
      common /vem_datac/   elq     ,elc

!     Plasticity values

      real    (kind=8) ::  ss_pl(2), ep_pl(2), trSfac, trDfac
      common /vem_datas/   ss_pl   , ep_pl   , trSfac, trDfac

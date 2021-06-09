
      logical          quad, ttfl, nurbfl, vemfl
      common /qudshpa/ quad, ttfl, nurbfl, vemfl

      real*8           jac
      integer                    lint, npm, nvn
      common /qudshp0/ jac(125), lint, npm, nvn

      real*8           sg1      , shp1
      common /qudshp1/ sg1(2,20), shp1(2,20,20)

      real*8           sg2      , el2      , shp2
      common /qudshp2/ sg2(3,64), el2(4,16), shp2(3,64,64)

      real*8           sg3       , el3      , shp3
      common /qudshp3/ sg3(4,125), el3(5,16), shp3(4,125,125)

      real*8           shps2
      common /qudshp4/ shps2(3,64,64)

      real*8           s1w      , s2w      , s3w
      common /qudshpn/ s1w(2,20), s2w(2,20), s3w(2,20)

      real*8           shpm
      common /qudshpm/ shpm(125,125)

      real*8           hsize   ,hksize   ,xx0   ,dist_min
      common /qudhsiz/ hsize(2),hksize(2),xx0(3),dist_min

      real (kind=8) :: sigp        , epsp
      common /qudshpp/ sigp(10,125), epsp(6,125)

      integer          sa     , ga, la, lint1, lint2, lint3
      common /qudshpi/ sa(125), ga, la, lint1, lint2, lint3

      real (kind=8) :: phi        , press      , dvol
      common /qudshp5/ phi(10,125), press(125), dvol(125)

!     Projection arrays for local least-squares

      real (kind=8) :: matnp         ,prjnp
      common /qudshp6/ matnp(125,125),prjnp(125)

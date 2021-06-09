c-----[--.----+----.----+----.-----------------------------------------]
c      Modifications
c      1. Change nope1 and nope2 to nope_s and nope_m        04/05/2013
c         Change neps1 and neps2 to neps_s and neps_m
c         Change cndm  to  ndmc
c      2. Add gapmax                                         05/11/2013
c-----[--.----+----.----+----.-----------------------------------------]

c      Coded/Modified by: Rossana Dimitri
c      Date             : February, 2012
c      Release          : 1.0

c-----[--.----+----.----+----.-----------------------------------------]
!     Integer values

      integer             nknot1   ,nknot2
      common /i3dnurbrec/ nknot1(2),nknot2(2)

      integer             k_order1   ,k_order2
      common /i3dnurbrec/ k_order1(2),k_order2(2)

      integer             nope_s   ,nope_m
      common /i3dnurbrec/ nope_s(2),nope_m(2)

      integer             nbas1,nbas2
      common /i3dnurbrec/ nbas1,nbas2

      integer             neps_s   ,neps_m
      common /i3dnurbrec/ neps_s(2),neps_m(2)

      integer             nneps1,nneps2
      common /i3dnurbrec/ nneps1,nneps2

      integer             ndmc
      common /i3dnurbrec/ ndmc

      integer             knot_len1   ,knot_len2
      common /i3dnurbrec/ knot_len1(2),knot_len2(2)

      integer             lk1,lk2
      common /i3dnurbrec/ lk1,lk2

      integer             ncp1   ,ncp2
      common /i3dnurbrec/ ncp1(2),ncp2(2)

      integer             nncp1,nncp2
      common /i3dnurbrec/ nncp1,nncp2

      integer             ngauss,nngaus
      common /i3dnurbrec/ ngauss,nngaus

      integer             setbc
      common /i3dnurbrec/ setbc

!     Real values

      real*8            Npen, Tpen, muF
      common /rnurbrec/ Npen, Tpen, muF

      real*8            gapmin,gapmax
      common /rnurbrec/ gapmin,gapmax


!     Real values

      real*8         UU1     ,UU2     ,PP1     ,QQ2
      common /rnurb/ UU1(300),UU2(300),PP1(600),QQ2(600)

      real*8         dxdxi     ,dxidx
      common /rnurb/ dxdxi(3,3),dxidx(3,3)

!     Integer values

      integer        nurnp
      common /inurb/ nurnp

      integer        iq1, iq2, iq3
      common /inurb/ iq1, iq2, iq3

      integer        numnsd
      common /inurb/ numnsd

      integer        nurbk
      common /inurb/ nurbk

      integer        ns2
      common /inurb/ ns2(300)

      integer        kdiv, lenkt 
      common /inurb/ kdiv, lenkt

      integer        lkdim   , estart
      common /inurb/ lkdim(3), estart

      integer        mxcp,mxel,mxce,mxee
      common /inurb/ mxcp,mxel,mxce,mxee

      integer        onumnp,onumel,onen1,onen,onneq,ovneq, npl_int,t_ma
      common /inurb/ onumnp,onumel,onen1,onen,onneq,ovneq, npl_int,t_ma

      integer        cetype,sdim
      common /inurb/ cetype,sdim

!     Edges

      integer        nurstr
      common /inurb/ nurstr

!     Logical  values

      logical        knotfl,nsidfl,nblkfl,nlodfl, gmvfl, ndisfl
      common /lnurb/ knotfl,nsidfl,nblkfl,nlodfl, gmvfl, ndisfl

      logical        nactb   , oldtf, npatfl, blockfl
      common /lnurb/ nactb(3), oldtf, npatfl, blockfl

!     Plot     values

      integer        nd_lin, ne_lin
      common /pnurb/ nd_lin, ne_lin

!     NEW MESH VALUES

!     Integer values

      integer         nnurnp,nnside
      common /inurbn/ nnurnp,nnside

!     integer         nmcp
!     common /inurbn/ nmcp(120)

      integer         tsplnp, tsplel, tfile, tsplce
      common /inurbn/ tsplnp, tsplel, tfile, tsplce(10)

      integer         tsplee,      tsplty
      common /inurbn/ tsplee(200), tsplty(200)

!     PLOT MESH COORDINATES

      integer         numpln,nnpl
      common /inurbp/ numpln,nnpl

!$Id:$
      subroutine pmacr (initf)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Command language instruction subprogram.
!               Controls problem solution and output algorithms by
!               order of specifying macro commands in array wd.

!      Inputs:
!         initf     - Flag, Initialize solution data if true

!      Outputs:
!         none      - Routine may be called several times
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'arclel.h'
      include  'arclei.h'
      include  'arcler.h'
      include  'augdat.h'
      include  'cdata.h'
      include  'cdat1.h'
      include  'chdata.h'
      include  'comfil.h'
      include  'comnds.h'
      include  'compas.h'
      include  'counts.h'
      include  'ddata.h'
      include  'debugs.h'
      include  'dyndat.h'
      include  'eltran.h'
      include  'endata.h'
      include  'evdata.h'
      include  'fdata.h'
      include  'gltran.h'
      include  'hdata.h'
      include  'hdatam.h'
      include  'hlpdat.h'
      include  'iofile.h'
      include  'ldata.h'
      include  'modcon.h'
      include  'ndata.h'
      include  'pconstant.h'
      include  'pdata2.h'
      include  'pdata3.h'
      include  'plflag.h'
      include  'plist.h'
      include  'pmod2d.h'
      include  'prflag.h'
      include  'print.h'
      include  'prld1.h'
      include  'prlod.h'
      include  'ptdat1.h'
      include  'ptdat2.h'
      include  'ptdat3.h'
      include  'ptdat4.h'
      include  'ptdat5.h'
      include  'ptdat6.h'
      include  'ptdat7.h'
      include  'ptdat8.h'
      include  'rdata.h'
      include  'rdat0.h'
      include  'region.h'
      include  'sdata.h'
      include  'tdata.h'
      include  'tdatb.h'
      include  'tdato.h'
      include  'umac1.h'

      include  'pointer.h'
      include  'comblk.h'

      integer        :: ncomd
      parameter        (ncomd = 73)

      character (len=128) :: fint
      character (len=15)  :: lct(200)
      character (len=4)   :: wd(ncomd)

      logical       :: initf, errck,vinput,pcomp,setvar,palloc
      integer       :: nlp,nif, i, j, k, ll,last
      integer       :: nwd1,nwd2,nwd3,nwd4,nwd5,nwd6,nwd7,nwd8,nwd9,nwdp
      integer       :: nw1,nw2,nw3,nw4,nw5,nw6,nw7,nw8,nw9,nwp

      integer       :: ed(ncomd),  jct(200)
      real (kind=4) :: tary(2), etime , tt
      real (kind=8) :: td(4)

      save

!     List of entries in array wd for macro commands.
!     N.B.  The continue label 'n' indicates which pmacr'n'
!           subprogram contains macro command statements.

      data wd/'stre','utan','tang','form','mass','reac','chec','damp',
     1        'augm','geom','dire','iter','hill',

     2        'tol ','dt  ','loop','next','prop','data','time','prin',
     2        'nopr','tran','init','iden','newf','back','debu','if  ',
     2        'else','endi','echo',

     3        'disp','solv','mesh','plot','subs','writ','read','rest',
     3        'velo','acce','bfgs','arcl','save','eige','epri','eigv',
     3        'show','tplo','dsol','opti',

     4        'mac1','mac2','mac3','mac4','mac5','mac6','mac7','mac8',
     4        'mac9','ma10','ma11','ma12',

     5        'outm','comm',

     6        'para',

     7        'grap',

     8        'elev','inse',

     9        'fe2 ','rve ',

     p        'manu' /

      data ed/    0,     0,     0,     0,     0,     0,     0,     1,
     1            1,     1,     3,     3,     1,

     2            0,     0,     0,     0,     0,     1,     0,     0,
     2            0,     0,     0,     1,     1,     1,     0,     3,
     2            3,     3,     0,

     3            0,     0,     0,     0,     0,     1,     1,     1,
     3            0,     0,     1,     1,     1,     0,     0,     1,
     3            0,     1,     0,     0,

     4            5,     5,     5,     5,     5,     5,     5,     5,
     4            5,     5,     5,     5,

     5            5,     0,

     5            5,

     5            5,

     5            5,     5,

     5            5,     5,

     p            4 /

      data nwd1,nwd2,nwd3,nwd4,nwd5,nwd6,nwd7,nwd8,nwd9,nwdp
     &    /  13,  19,  20,  12,   2,   1,   1,   2,   2,   1 /

      if(initf) then

!       Set counter values

        nstep  = 0
        niter  = 0
        naugm  = 0
        titer  = 0
        taugm  = 0
        iaugm  = 0

!       Set initial values of parameters

        enzer  = 0.0d0
        aengy  = 0.0d0
        aold   = 0.0d0
        augf   = 1.0d0
        rnmax  = 0.0d0
        shift  = 0.0d0
        tol    = 1.d-16
        dt     = 0.0d0
        dtold  = 0.0d0
        prop   = 1.0d0
        propo  = 1.0d0
        ttim   = 0.0d0

        call pzero(prldv,50)

!       Arc-length method

        rlnew  =  0.0d0
        timold = -1.0d0
        kflag  =  0
        nastep =  0
        noi    =  0
        nreg   = -1

!       Dynamic parameters

        call dparam(bpr,'init')
        cc1    = 1.d0
        cc2    = 1.d0
        cc3    = 1.d0
        do i = 1,3
          bpr(i)  = 0.0d0
          ctan(i) = 0.0d0
          gtan(i) = 0.0d0
        end do
        ctan(1)    = 1.0d0
        gtan(1)    = 1.0d0
        dynflg     = .false.
        floop(1:2) = .false.
        gflag      = .true.
        modfl      = .true.
        ndebug     = 0
        numint     = 5
        rayla0     = 0.d0
        rayla1     = 0.d0

!       Save room for history variable storage

        if(nhmax.gt.0) then
          setvar = palloc( 50,'NH1  ', nhmax,  2)
          setvar = palloc( 51,'NH2  ', nhmax,  2)
        endif
        if(nh3max.gt.0) then
          setvar = palloc( 52,'NH3  ', nh3max, 2)
        endif
        theta(1) = 0.0d0
        theta(2) = 0.0d0
        theta(3) = 1.0d0

!       Set default solution flags

        arcf   = .false.
        compfl = .false.
        compms = .true.
        cadamp = .true.
        castif = .true.
        debug  = .false.
        fl( 1) = .false.
        fl( 2) = .false.
        fl( 3) = .true.
        fl( 4) = .true.
        fl( 5) = .true.
        fl( 6) = .true.
        fl( 7) = .true.
        fl( 8) = .false.
        fl( 9) = .false.
        fl(10) = .true.
        fl(11) = .false.
        fl(12) = .true.
        fops   = .true.
        hadd   = .true.
        hflgu  = .true.
        h3flgu = .true.
        echo   = .false.
        linear = .false.
        pfl    = .false.
        pfr    = .true.
        plfl   = .true.
        prnt   = .true.
        refl   = .false.
        rfl    = .false.
        screfl = .true.

!       Set integer parameters

        ittyp  = -1   ! default is incore profile solver
        li     = 0
        lvcn   = -1
        maxbl  = 0
        md     = 0
        npld   = 0
        naplts = 0
        ncplts = 0
        ndplts = 0
        neplts = 0
        nlplts = 0
        nrplts = 0
        nsplts = 0
        nuplts = 0
        nvplts = 0
        ntstep = 0
        nc     = 1
        nv     = 1
        nw     = 1
        niols(1) = 0
        niols(2) = 0
        niols(3) = 0
        npstr    = max(11,npstr+1)

        call pzero(epl,200)

!       Set initf to prevent reinitializing parameters

        initf  = .false.

!       Initialize history database items

        call formfe(np(40),np(26),np(26),np(26),
     &             .false.,.false.,.false.,14,1,numel,1)

!       Set umacro names for default values

        nw4 = nwd1 + nwd2 + nwd3
        do j = 1,12
          i = nw4 + j
          if(.not.pcomp(umacc(j),wd(i),4)) then
            wd(i) = umacc(j)
            ed(i) = 0
          endif
        end do ! j

      endif

!     Set pointers to macro subprograms

      nlp    = nwd1 + 3
      nif    = nwd1 + 16
      nw1    = nwd1
      nw2    = nwd2 + nw1
      nw3    = nwd3 + nw2
      nw4    = nwd4 + nw3
      nw5    = nwd5 + nw4
      nw6    = nwd6 + nw5
      nw7    = nwd7 + nw6
      nw8    = nwd8 + nw7
      nw9    = nwd9 + nw8
      nwp    = nwdp + nw9

!     Input the macro commands


100   call pmacio (jct,lct,ct,wd,ed,nwp,nlp,nif,ll,prth)
      if(ll.le.0) go to 300

!     Execute macro instruction program

      nh1 = np(50)
      nh2 = np(51)
      nh3 = np(52)
      lv = 0
      l = 1
200   j = jct(l)
      i = l - 1
      tt = etime(tary)
      if(j.ne.nlp .and. j.ne.nlp+1) then
        errck = vinput(lzz(l),80,td,4)
        ct(1,l) = td(1)
        ct(2,l) = td(2)
        ct(3,l) = td(3)
        macd    = td(4)
        maci    = nint(td(4))
        write(yyy,2003) wd(j),lct(l),(ct(k,l),k=1,3)

!       Strip leading blanks and comments

        call pstrip(xxx,yyy,3)

!       Set the yyy value

        call acheck(xxx,yyy,15,75,75)

2003    format (a4,',',a4,',',3(1p,1e14.7,','))

      endif
      if((l.ne.1.and.l.ne.ll).and.pfr) then
        if(prnt) write(iow,2001) i,wd(j),lct(l),(ct(k,l),k=1,3),tary
        if((ior.lt.0.and.prnt) .or. echo) then
          write(*,2001) i,wd(j),lct(l),(ct(k,l),k=1,3),tary
        endif
      endif

!     Transfer to correct subprogram to execute macro

      last  = ll
      if(j.le.nw1) then
        call pmacr1(lct,ct,j)
      elseif(j.le.nw2) then
        call pmacr2(lct,ct,j-nw1)
      elseif(j.le.nw3) then
        call pmacr3(lct,ct,j-nw2)
      elseif(j.le.nw4) then
        uct = wd(j)
        call pmacr4(ct(1,l),lct(l),j-nw3)
      elseif(j.le.nw5) then
        call pmacr5(lct(l),j-nw4)
      elseif(j.le.nw6) then
        call pmacr6(lct(l),ct(1,l),j-nw5)
      elseif(j.le.nw7) then
        call pmacr7(j-nw6)
      elseif(j.le.nw8) then
        call pmacr8(lct(l),ct(1,l),j-nw7)
      elseif(j.le.nw9) then
        call pmacr9(lct(l),ct(1,l),j-nw8)
      elseif(j.eq.nwp) then
        hlplev = max(-1,min(3,int(ct(1,l))))
      endif
      l = l + 1
      if(l.le.last) go to 200
      if (ior.lt.0) go to 100
300   tt = etime(tary)
      write(iow,2000) tary
      if(ior.lt.0) write(*,2000) tary

!     Save restart information

      if(fl(7)) return
      if(ll.eq. -1) then
        fint = fsav
        call restrt(fint,hr(np(40)),ndm,ndf,nneq,2)
        if(ior.lt.0) then
          write(*,2002) fint
        endif
        write(iow,2002) fint
      endif

!     Formats

2000  format(' *End of Solution Execution*',31x,'t=',2f9.2)
2001  format(' *Command ',i3,' * ',a4,1x,a15,
     &   'v:',3g11.3/59x,'t=',2f9.2)
2002  format(/'           Saved  Restart  File: ',a)

      end subroutine pmacr

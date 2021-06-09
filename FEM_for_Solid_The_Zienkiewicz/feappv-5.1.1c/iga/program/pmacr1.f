!$Id:$
      subroutine pmacr1(lct,ct,j)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Command language instruction subprogram.  Mostly for
!               FEM arrays

!      Inputs:
!         lct(*)     - Command option
!         ct(3,*)    - Command parameters
!         j          - Command number in this routine

!      Outputs:
!         Depends on command number j
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include   'allotd.h'
      include   'arclel.h'
      include   'arclei.h'
      include   'arcler.h'
      include   'augdat.h'
      include   'cdata.h'
      include   'cdat1.h'
      include   'comfil.h'
      include   'compas.h'
      include   'counts.h'
      include   'ddata.h'
      include   'debugs.h'
      include   'eltran.h'
      include   'endata.h'
      include   'eqslv.h'
      include   'eqsym.h'
      include   'errind.h'
      include   'evdata.h'
      include   'fdata.h'
      include   'gltran.h'
      include   'hdatam.h'
      include   'iodata.h'
      include   'iofile.h'
      include   'ldata.h'
      include   'modreg.h'
      include   'mxsiz.h'
      include   'ndata.h'
      include   'pdata3.h'
      include   'p_int.h'
      include   'p_point.h'
      include   'plist.h'
      include   'pmod2d.h'
      include   'pointer.h'
      include   'print.h'
      include   'prflag.h'
      include   'prlod.h'
      include   'prstrs.h'
      include   'qudshp.h'
      include   'rdata.h'
      include   'rdat0.h'
      include   'rdat1.h'
      include   'sdata.h'
      include   'ssolve.h'
      include   'strnum.h'
      include   'tdata.h'
      include   'xtout.h'
      include   'comblk.h'

      logical   fa,tr,cfr,pcomp,f8o,exst,tfl,setvar,palloc
      logical   nomass,factor,compsv
      character cmtyp*4
      integer   iops,mops,i,j, neqms, k1,k2,k3,k4
      real*8    tops,ur,rel0,reln,step, aengysv

      character lct(*)*15
      real*8    ct(3,*)
      real*4    tary(2), etime , tt

      logical   cknon0
      real*8    dot

      save

      data fa,tr/.false.,.true./

!     Set history update flag to false (no updates)

      hflgu  = .false.
      h3flgu = .false.
      nomass = .false.

!     Set transient parameters for current

      if(fl(9)) call dsetci
      do i = 1,3
        ctan(i) = gtan(i)
      end do

!     Transfer to correct process

      go to (1,2,2,4,5,6,7,8,9,10,11,12,13), j

!     Print stress values

!     [stre,,k1,k2,k3]        - output elmt values for k1 to k2 inc k3
!     [stre,all]              - output all element values
!     [stre,node,k1,k2,k3]    - output nodal stresses k1 to k3 inc k3
!                               (lumped projection)
!     [stre,coor,nxt,xt,xtol] - output nodal stresses at x-nxt=xt+-xtol

1     k1 = nint(ct(1,l))
      k2 = nint(ct(2,l))
      if(k2.eq.0) k2 = k1
      k3 = nint(ct(3,l))
      if(k3.eq.0) k3 = 1
      nxt = 0
      if (pcomp(lct(l),'node',4)  .or. pcomp(lct(l),'coor',4)) then

!       Allocate array storage

        if (plfl) then
          if(nurbfl) then
            call pbezlin()
          endif
          setvar = palloc ( 58,'NDNP',numnp*npstr,2)
          setvar = palloc ( 57,'NDER',numnp*8    ,2)
          setvar = palloc (207,'NSCR',numel      ,2)
          nper   = np(57)
          npnp   = np(58)
          plfl   = .false.
        endif
        ner = nper
        nph = npnp

!       Set output limits

        if (pcomp(lct(l),'node',4)) then
          k1 = max(1,min(numnp,k1))
          k2 = max(1,min(numnp,k2))
          if(k2-k1.ne.0) k3 = isign(k3,k2-k1)
        elseif(pcomp(lct(l),'coor',4)) then
          nxt = max(1,min(k1,ndm))
          xt  = ct(2,l)
          if(ct(3,l).eq.0.0d0) then
            xtol = 0.01
          else
            xtol = abs(ct(3,l))
          endif
          k1 = 1
          k2 = numnp
          k3 = 1
        endif

!       Projections to nodes

        point = npnp + numnp
        if(.not.fl(11)) then
          istv = npstr - 1
          call pzero (hr(np(207)),numel)
          call pzero (hr(npnp), npstr*numnp)
          call pzero (hr(nper),     8*numnp)

          pltmfl = .true.
          call formfe(np(40),np(26),np(26),np(26),fa,fa,fa,8,
     &                1,numel,1)
          pltmfl = .false.
          call pltstr(hr(npnp),hr(nper+numnp),hr(point),numnp,ndm)
        endif

!       Output sets

        call prtstr(hr(np(43)),hr(nper+numnp),hr(point),
     &              ndm,numnp,k1,k2,k3,prth)
        fl(11) = .true.

!     Output values as specified by elements

      else
        if (pcomp(lct(l),'all ',4)) then
          k1 = 1
          k2 = numel
          k3 = 1
        else
          k1 = max(1,min(numel,k1))
          k2 = max(1,min(numel,k2))
          if(k2-k1.ne.0) k3 = isign(k3,k2-k1)
        endif
        call formfe(np(40),np(26),np(26),np(26),fa,fa,fa,4,k1,k2,k3)
      endif

      return

!     Form tangent stiffness: j= 2 (utan); j=3 (tang)
!     [utan]                     - form unsymmetric tangent
!     [tang]                     - form symmetric tangent

!     [utan,,1]                  -   " + form rhs and solve.
!     [tang,,1]                  -   " + form rhs and solve.

!     [utan,line,1,shift,value]  -   " with line search on value
!     [tang,line,1,shift,value]  -   " with line search on value

!     [tang,eigv,0,shift]        -   " with no mass/damping added

2     castif = .true.
      cadamp = .false.
      camass = .false.
      if( pcomp(lct(l),'nume',4) ) then
        ndflg = .true.
      else
        ndflg = .false.
      endif


      if(neq.gt.0) then
        cfr = j.eq.2
        call presol(cfr, exst)
        if(exst) return

!       Form residual for ct(l,1) > 0

        f8o = .false.
        if(ct(1,l).gt.0.0d0) then
          f8o    = .true.

!         Form interpolated load vector - include nodal/surface parts

          call ploa1(ttim,dt)
          call pload(mr(np(31)),hr(np(30)),hr(np(26)),prop*rlnew,tr)
        endif

!       Construct shifts

        rfl   = .false.
        shflg = .false.
        shift = ct(2,l)
        if( pcomp(lct(l),'eigv',4)
     &      .or. (.not.fl(9).and.shift.ne.0.0d0)) then
          ctan(2) = 0.0d0
          if(prnt) write(iow,2000) shift
          if(ior.lt.0.and.prnt) write(*,2000) shift
          if(idenf) then
            nxu = np(nx) + neq
            if(compfl) then
              call colred(hr(np(nx)),shift,nxl, hr(na))
              if(cfr.and.nxl.gt.neq)then
                call colred(hr(nxu),shift,nxl-neq,hr(np(5)))
              endif
            else
              call colred(hr(np(nx)),shift,neq, hr(na))
              call cashift(hr(nau),hr(nxu),shift,mr(np(21)),
     &                     mr(np(90)),mr(np(91)),neq)
              if(cfr.and.nxl.gt.neq)then
                call cashift(hr(nal),hr(nxu),shift,mr(np(21)),
     &                       mr(np(90)),mr(np(91)),neq)
              endif
            endif
          else
            ctan(3) = -shift*ctan(1)
            shflg   = .true.
          endif

!       Specified shifts or dynamics/static cases

        elseif(shift.ne.0.0d0) then
          ctan(3) = -shift*ctan(1)
          shflg   = .true.
        else
          shift   = -ctan(3)
        endif

!       Compute flexible FE contributions to arrays

        hflgu  = f8o
        h3flgu = f8o
        call formfe(np(40),np(26),na,nal,tr,f8o,fa,3,1,numel,1)
        ndflg = .false.

!       Output residual norm

        tt = etime(tary)
        if(f8o) then
          rnorm = sqrt(dot(hr(np(26)),hr(np(26)),neq))
          if(rnmax.eq.0.0d0) then
            reln = 1.d0
            rel0 = rnorm
          else
            if(rel0.eq.0.d0) rel0 = 1.d0
            reln = rnorm/rel0
          endif
          if(prnt) write(iow,2001) rnorm,reln,tary
          if(ior.lt.0.and.prnt) write(*,2001) rnorm,reln,tary
          fl(7) = .false.
          fl(8) = .false.
          if(abs(aengy).lt.aold) aold = abs(aengy)
        else
          if(prnt) write(iow,2002) tary
          if(ior.lt.0.and.prnt) write(*,2002) tary
        endif

!       Set pointers then factor and solve equations

        fp(1)  = na
        fp(2)  = nau
        fp(3)  = nal
        fp(4)  = np(21)
        factor = ct(1,l).ge.0.0d0

        call psolve(hr(np(26)),fp,factor,f8o,cfr, prnt)
        if(factor) then

!         Timing for factorization

          if(tdiff .gt.0.0d0 .and. prnt .and. pfr
     &                       .and. ittyp.eq.-1) then
            call datric(iops,mops,mr(fp(4)),neq)
            tops  = (dble(mops) + dble(iops)*1.d-6)/tdiff
            if(cfr) tops = 2.d0*tops
            if(ior.lt.0) then
              write(*,2019) tops,tdiff
            endif
            write(iow,2019) tops,tdiff
          endif

        else
          write(iow,3002)
          if(ior.lt.0) write(*,3002)
        endif

!       Update solutions using line search or arc length method

        if(f8o) then

!         Update iteration counter

          niter  = niter  + 1
          iaugm  = iaugm  + 1

!         Set initial values for a line search (conservative to
!         check initital iterate solution for possible line search)

          if(ct(3,l).le.0.0d0) ct(3,l) = 0.8d0
          if (rnmax.eq.0.0d0) then
            reln  = 1.d0
            rnmax = abs(aengy)
            aold  = rnmax/0.9999d0/ct(3,l)
          else
            reln  = (aengy)/rnmax
          endif
          if(pfr) write(iow,2004) rnmax,aengy,reln,tol
          if(pfr.and.ior.lt.0) write(*,2004) rnmax,aengy,reln,tol
          if(abs(aengy).le.tol*rnmax .or. linear
     &                              .or. abs(aengy).lt.enzer) then
            if(lv.gt.1) then
              ct(1,lve(lv)) = ct(1,lvs(lv))
              l = lve(lv) - 1
              floop(1) = .false.
            endif
          elseif(.not.linear) then

!           Line search - linear search along solution direction

            if(pcomp(lct(l),'line',4)) then
              if(abs(aengy).gt.ct(3,l)*aold) then
                setvar = palloc(111,'TEMP1',nneq        , 2)
                setvar = palloc(112,'TEMP2',nneq*(nrt+4), 2)

                step = 1.0d0
                call serchl(aold,mr(np(31)),np(111),np(40),
     &                      hr(np(26)),ct(3,l),hr(np(112)),neq,step)

                setvar = palloc(112,'TEMP2',0, 2)
                setvar = palloc(111,'TEMP1',0, 2)
              endif
            endif
          endif

!         Perform arc length control

          if(arcf) then
            call arclen(hr(np(26)),hr(np(84)),hr(np(85)),hr(np(27)),
     &                  hr(nal),hr(nau),hr(na),mr(np(21)),mr(np(31)),
     &                  ndf,numnp,neq,ttim)
          endif

!         Update solution

          call pupdate(mr(np(31)),hr(np(30)),hr(np(40)),hr(np(42)),
     &                 hr(np(26)),fl(9),2)

        endif

      else

!       No active equation update case

        if(prnt .and. ior.lt.0) write(*,3006)
        call ploa1( ttim,dt)
        call pload( mr(np(31)),hr(np(30)),hr(np(26)),prop*rlnew,fa)
        call pupdate(mr(np(31)),hr(np(30)),hr(np(40)),hr(np(42)),
     &               hr(np(26)),fl(9),2)
!       Update stresses

        hflgu  = .true.
        h3flgu = .true.
        rnmax  = 1.d0
        niter  = niter  + 1
        call formfe(np(40),np(26),na,nal,fa,fa,fa,6,1,numel,1)

      endif
      return

!     Compute residual for time step/iteration

!     [form]       - form rhs residual
!     [form,acce]  -    " + get initial acceleration if needed
!     [form,expl]  -    " + do explicit solution with lumped mass

4     if(fl(8)) return
      rfl = .false.
      call ploa1(ttim,dt)
      call pload(mr(np(31)),hr(np(30)),hr(np(26)),prop*rlnew,tr)
      hflgu  = .true.
      h3flgu = .true.

!     Compute current residual

      call formfe(np(40),np(26),np(26),np(26),fa,tr,fa,6,1,numel,1)

      rnorm = sqrt(dot(hr(np(26)),hr(np(26)),neq))

!     Set residual array

      if(rnmax.eq.0.0d0) then
        reln  = 1.d0
        rel0  = rnorm
      else
        if(rel0.eq.0.d0) rel0 = 1.d0
        reln  = rnorm/rel0
      endif

!     Output current residual norm

      if(prnt) then
        write(iow,2003) rnorm,reln
        if(ior.lt.0) then
          write(*,2003) rnorm,reln
        endif
      endif
      fl(8) = .true.

!     Compute initial acceleration

      if(pcomp(lct(l),'acce',4)) then
        if(ior.lt.0) write(*,*) ' Forming initial acceleration'

!       Initialize for consistent mass

        if(fl(1).and.fl(9)) then
          setvar = palloc(1,'TANG1',mr(np(21)+neq-1)+neq,2)
          na = np(1)
          call pzero(hr(na),int(mr(np(21)+neq-1)+neq))
          call pmove(hr(nm),hr(na),int(mr(np(21)+neq-1)+neq))
          call pmove(hr(np(26)),hr(np(42)+nneq),neq)
          nau = na + neq
          call datri(hr(nau),hr(nau),hr(na),mr(np(21)),
     &               neq,neq)
          call dasol(hr(nau),hr(nau),hr(na),hr(np(42)+nneq),
     &               mr(np(21)),neq,neq,rnorm)
          call pexpd(hr(np(42)+nneq),hr(na),mr(np(31)),neq,nneq)

!       Initialize for lumped mass

        elseif(fl(9)) then
          if(fl(2)) then
            fp(8) = nl
          else
            setvar = palloc(112,'TEMP2',neq, 2)
            fp(8)   = np(112)
            imtyp = 1
            call formfe(np(40),fp(8),fp(8),fp(8),tr,fa,fa,5,
     &                  1,numel,1)
          endif

          if(cknon0(hr(fp(8)),neq)) then
            setvar = palloc(111,'TEMP1',neq, 2)
            call piacel(hr(fp(8)),hr(np(26)),hr(np(42)+nneq),neq)
            call pexpd(hr(np(42)+nneq),hr(np(111)),mr(np(31)),
     &                 neq,nneq)
            setvar = palloc(111,'TEMP1',0, 2)
          else
            if(ior.lt.0) then
              write(  *,3008)
            else
              write(iow,3008)
              call plstop(.true.)
            endif
          endif

          if(np(112).ne.0) setvar = palloc(112,'TEMP2',0, 2)

!       Write error

        else
          write(iow,3000)
          if(ior.lt.0) write(*,3000)
        endif

      endif

!     Explicit solution

41    if(pcomp(lct(l),'expl',4) .or. nomass) then

!       Perform solution and update

        nomass = .false.
        if(fl(2).and.fl(9)) then

          neqms = neq
          call piacel(hr(nl),hr(np(26)),hr(np(26)),neqms)

          call pupdate(mr(np(31)),hr(np(30)),hr(np(40)),hr(np(42)),
     &                 hr(np(26)),fl(9),2)

!       Write error

        else
          write(iow,3004)
          if(ior.lt.0) write(*,3004)
          nomass = .true.
          cmtyp  = 'lump'
          go to 51
        endif

      endif

      return

!     [mass],lump : Lumped mass matrix used
!     [mass],cons : Consistent mass matrix used
!     [mass]      : Same as 'cons'istent

!     Form lumped mass approximation

5     imtyp  = 1
      cmtyp  = lct(l)(1:4)
      nomass = .false.
51    compsv =  compfl
      if(pcomp(cmtyp,'lump',4)) then
        setvar = palloc(13,'LMAS1',neq,2)
        nl = np(13)
        call pzero(hr(nl),neq)
        imtyp = 1
        nx    = 13
        nxl   = neq
        fl(1) = .false.
        fl(2) = .true.
        fl(5) = .false.
        if(prnt .and. ior.lt.0) write(*,2016)

!     Form consistent mass approximation

      else
        if(compms) then
          k1 = 0
          call iters(k1,2)
          compms = .false.
        endif
        compfl = .true.
        nm = np(9)
        call pzero (hr(nm),nnm)
        nx  = 9
        nxl = nnm
        neqs = neq
        fl(1) = .true.
        fl(2) = .false.
        fl(6) = .false.
        if(prnt .and. ior.lt.0) then
          if(imtyp.eq.1) then
            write(*,2017)
          elseif(imtyp.eq.2) then
            write(*,2018)
          endif
        endif
      endif

      castif = .false.
      cadamp = .false.
      camass = .true.
      idenf  = .false.
      call formfe(np(40),nl,nm,nm,fl(1),fl(2),fa,5,1,numel,1)
      compfl =  compsv

!     Check that mass matrix has non-zero diagonal entries

      if(imtyp.eq.1 .and. .not.cknon0(hr(np(nx)),neq)) then
        if(ior.lt.0) then
          write(  *,3008)
        else
          write(iow,3008)
          call plstop(.true.)
        endif
      endif
      if(nomass) then
        nomass = .false.
        go to 41
      endif
      return

!     Compute reactions and print

!     [reac,,k1,k2,k3]        - print reactions at nodes k1 to k2 inc k3
!     [reac,all]              - print all reactions
!     [reac,coor,nxt,xt,xtol] - print reactions at nodes x-nt=xt+-xtol
!     [reac,imag,k1,k2,k3]    - print complex imaginary reactions
!                               at nodes k1 to k2 inc k3
!     [reac,list,k1]          - print reactions in list k1
!     [reac,regi,k1,k2]       - compute reactions for region k1
!                               assign to proportional load  k2
!     [reac,file]             - compute reactions for active regions
!                               save to file: fsav.rea

6     nxt = 0

!     Set output limits

      tfl = pfr
      k4  = 0
      if(.not.pcomp(lct(l),'list',4)) then
        if (pcomp(lct(l),'all ',4)) then
          k1 = 1
          k2 = numnp
          k3 = 1
        elseif(pcomp(lct(l),'coor',4)) then
          k1 = nint(abs(ct(1,l)))
          nxt = max(1,min(k1,ndm))
          xt  = ct(2,l)
          if(ct(3,l).eq.0.0d0) then
            xtol = 0.01
          else
            xtol = abs(ct(3,l))
          endif
          k1 = 1
          k2 = numnp
          k3 = 1
        elseif(pcomp(lct(l),'file',4)) then
          k1 = 0
          rfl = .false.
          ct(1,l) = -1.d0
        else
          k1 = nint(abs(ct(1,l)))
          k1 = max(1,min(numnp,k1))
          k2 = nint(ct(2,l))
          if(k2.eq.0) k2 = k1
          k2 = max(1,min(numnp,k2))
          k3 = nint(ct(3,l))
          if(k3.eq.0) k3 = 1
          if(k2-k1.ne.0) k3 = isign(k3,k2-k1)
          pfr = .true.
        endif
      endif

!     Compute new reactions

      if(.not.rfl) then
        call pzero(hr(np(26)),nneq)
        if(ct(1,l).lt.0.0d0) then
          call ploa1(ttim,dt)
          call pload(mr(np(31)),hr(np(30)),hr(np(26)),prop*rlnew,fa)
        endif
        pltmfl = .true.
        call formfe(np(40),np(26),np(26),np(26),fa,tr,tr,6,1,numel,1)
        pltmfl = .false.
      endif

!     Selected nodal outputs

      if(k1.gt.0) then
        call prtrea(hr(np(26)+k4),hr(np(43)),ndm,ndf,k1,k2,k3,prth)
      endif

!     Set flag to prevent recomputation of reactions for same state

      rfl = .true.
      pfr = tfl

!     Compute/output work: Energy = U x R

      if(k1.gt.0) then
        ur = -dot(hr(np(26)),hr(np(40)),nneq)
        write(iow,2005) ur
        if(ior.lt.0) write(*,2005) ur
      endif

      return

!     Check mesh for input errors

!     [chec]      - check mesh for errors
!     [chec,init] - check mesh for initialization of history data base
!                   changes

7     if(pcomp(lct(l),'init',4)) then
        k1     = 14
        hflgu  = .true.                ! Permit update on data base
        h3flgu = .true.
      else
        k1 = 2
      endif
      call formfe(np(40),np(26),np(26),np(26),fa,fa,fa,k1,1,numel,1)
      return

! --- [damp] form consistent damping matrix (isw=9)

8     setvar = palloc(17,'DAMP1',mr(np(21)+neq-1)+neq,2)
      nc = np(17)
      k1 = neq + mr(np(21)+neq-1)
      call pzero (hr(nc),k1)
      call formfe (np(40),nc,nc,nc,tr,fa,fa,9,1,numel,1)
      return

! --- [augm,,value] perform nested update for augmented lagrangian
!                   'value' is used only for first iteration in
!                   each time step. (default value is 1.0 and normally
!                   should be used.)

! --- [augm,pena,value] reset augmented penalty parameter only

9     if(rnmax.eq.0.0d0) then

!       New time step

        if(ct(1,l).gt.0.0d0) then
          augf = ct(1,l)
        else
          augf = 1.0d0
        endif
        if(prnt) write(iow,2006) augf
        if(ior.lt.0.and.prnt) write(*,2006) augf
      endif

!     Augment element values

      hflgu  = .true.
      h3flgu = .true.
      call formfe(np(40),np(26),np(26),np(26),fa,fa,fa,10,
     &            1,numel,1)

!     Continue with current time step

      aold  = rnmax
      aengy = rnmax
      naugm = naugm + 1
      iaugm = 0
      return

! --- [geom]        - Geometric stiffness formulation for eigenvalues
!     [geom,on/off] - Control to add element geometric stiffness terms

10    if    (pcomp(lct(l),'on', 2)) then
        gflag = .true.
        write(iow,2022)
        if(ior.lt.0) then
          write(*,2022)
        endif
      elseif(pcomp(lct(l),'off',3)) then
        gflag = .false.
        write(iow,2023)
        if(ior.lt.0) then
          write(*,2023)
        endif
      else
        imtyp = 2
        cmtyp = 'cons'
        go to 51
      endif
      return

! --- [dire]ct solution option
!     [direct,sparse] ! N.B. Users must furnish their own

11    if(pcomp(lct(l),'spar',4)) then
        ittyp  = -2
        k1     =  0
        call iters(k1,1)

!     Profile solution

      else
        compfl = .false.
        ittyp  = -1
        if(ior.lt.0) write(*,2015)
        write(iow,2015)
!       k1 = (mr(np(21)+neq-1)+neq)*ipr
      endif
      return

! --- [iter]ation solution option

12    ittyp = 1
      if(ior.lt.0) write(*,2011)
      write(iow,2011)
      k1 = 0
      call iters(k1,1)
      return

!     [hill]-mandel computations
!     [hill tang]  - Compute tangent and stress
!     [hill stre]  - Compute stress only

!     Compute Hill-Mandel projection of stress and moduli

13    aengysv = aengy   ! Save current energy of solution
      call phillmandel(lct(l),ct)
      aengy   = aengysv
      return

!     Formats

2000  format('   Shift to tangent matrix = ',1p,e12.5)
2001  format('   Residual norm = ',1p,2e17.7,6x,'t=',0p,2f9.2)
2002  format(59x,'t=',0p,2f9.2)
2003  format('   Residual norm = ',1p,2e17.7)
2004  format('   Energy convergence test'/
     &       '    Maximum   =',1p,e25.15,' Current   =',1p,e25.15/
     &       '    Relative  =',1p,e25.15,' Tolerance =',1p,e25.15)
2005  format('   Energy: Displacements * Reactions = ',1p,e17.7/1x)
2006  format('   Current Augmented Lagrangian Factor =',1p,e13.5)
2011  format('   Iterative Solution: Diagonal Preconditioner'/)
2015  format('   Direct Solution: Profile'/)
2016  format('   Mass Type DIAGONAL (LUMP) Specified'/)
2017  format('   Mass Type CONSISTENT Specified'/)
2018  format('   Geometric Stiffness Specified'/)
2019  format('--> SOLVE AT',f9.2,' Mflops. Time=',f12.2)
2022  format('   Geometric stiffness ON')
2023  format('   Geometric stiffness OFF')

3000  format(' *ERROR* Unable to compute starting acceleration,'
     &      ,' Static problem or density zero')

3002  format(' *WARNING* Unfactored tangent produced do not try'
     &      ,' normal solution.')

3004  format(' *WARNING* No mass defined: Default to diagonal mass.')

3006  format(' *WARNING* No active equations: Solution updated')

3008  format(' *ERROR* No non-zero terms in mass matrix:',
     &       ' Check density value for materials')

      end

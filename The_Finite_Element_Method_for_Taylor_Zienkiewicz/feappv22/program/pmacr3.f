c$Id:$
      subroutine pmacr3(lct,ct,j)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Command language instruction subprogram mostly for
c               FEM arrays

c      Inputs:
c         lct(*)     - Command option
c         ct(3,*)    - Command parameters
c         j          - Command number in this routine

c      Outputs:
c         Depends on command number j
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'allotd.h'
      include  'arclel.h'
      include  'arclei.h'
      include  'arcler.h'
      include  'augdat.h'
      include  'cdata.h'
      include  'cdat1.h'
c     include  'chdata.h'
      include  'comfil.h'
      include  'compac.h'
      include  'compas.h'
      include  'corset.h'
      include  'corfil.h'
      include  'cornum.h'
      include  'counts.h'
      include  'ddata.h'
      include  'debugs.h'
      include  'edgdat.h'
      include  'endata.h'
      include  'eqsym.h'
      include  'evdata.h'
      include  'fdata.h'
      include  'hdatam.h'
      include  'iofile.h'
      include  'ldata.h'
      include  'modcon.h'
      include  'modreg.h'
      include  'mxsiz.h'
      include  'ndata.h'
      include  'pointer.h'
      include  'prflag.h'
      include  'print.h'
      include  'prlod.h'
      include  'psize.h'
      include  'ptdat1.h'
      include  'ptdat2.h'
      include  'ptdat3.h'
      include  'ptdat5.h'
      include  'rdata.h'
      include  'rdat0.h'
      include  'rdat1.h'
      include  'region.h'
      include  'sdata.h'
      include  'setups.h'
      include  'tdata.h'
      include  'xtout.h'
      include  'comblk.h'

      logical   pcomp,sfl,accrcy,tfl,cknon0
      logical   pfro,setvar,palloc,pinput,tinput
      character type(1)*4,yyy(1)*15,fint*17
      integer   i,ii,imas, j,jj, k,kk,k1,k2,k3, lflag, ml1
      integer   mad, n,nn, npr,nnp,nbfgs,nnn
      real*8    dd, dist
      real*8    step,stol,etol, ee,phi2,vphi, chec, tau, reln

      integer   ndfeig(20)
      integer   mb, ml2,nty,jty, fp(5)
      real*8    td(20),xc(3)
      real*8    dot, dotid, dotx

      character lct(*)*15
      real*8    ct(3,*)

      save

      integer   npmx, nsmx
      data      npmx, nsmx /200, 200/

c     Transfer to correct process

      go to (1,2,3,4,5,6,7,8,1,1,11,12,13,14,15,16,17,18), j

c     Print displacements

c     [disp,all]             - print all displ.
c     [disp,,k1,k2,k3]       - print displ. k1 to k3 step k3
c     [disp,coor,k1,xt,xtol] - print displ. for all nodes where x-k1=xt

1     k1   = ct(1,l)
      pfro = pfr

      if(j.eq.1) then
        ml1 = 1
        ml2 = np(40)
        setvar = .true.
      elseif(j.eq.9) then
        ml1 = 2
        ml2 = np(42)
        setvar = fl(9)
      elseif(j.eq.10) then
        ml1 = 3
        ml2 = np(42) + nneq
        setvar = fl(9)
      endif

c     Set output limits

      k2 = ct(2,l)
      if(k2.eq.0) k2 = k1
      k3 = ct(3,l)
      if(k3.eq.0) k3 = 1
      nxt = 0

c     Set for all

      if(pcomp(lct(l),'all ',4)) then
        k1 = 1
        k2 = numnp
        k3 = 1

c     Set for specified coordinate

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
      else
        k1 = max(1,min(k1,numnp))
        k2 = max(1,min(numnp,k2))
        if(k2-k1.ne.0) k3 = sign(k3,k2-k1)
        pfr = .true.
      endif

c     Perform displacement, velocity, acceleration outputs:

      if(setvar) then
        call prtdis(hr(np(43)),hr(ml2),ttim,prop,ndm,ndf,
     &              k1,k2,k3,ml1,prth)
      else
        write(iow,3001)
        if(ior.lt.0) then
          write(*,3001)
        endif
      endif

      pfr = pfro
      return

c     Solve equations
c     [solv]
c     [solv,line,value] - use line search for energy ratios > value

2     if(neq.gt.0) then
        if(solver .and. fl(4) .and. .not.compfl) then
          if(ior.lt.0) then
            write(iow,3002)
            return
          else
            write(iow,3002)
            call plstop()
          endif
        endif

c       Test for RHS (form)

        if(.not.fl(8)) return

c       Set up solver call for a resolution

        floop(2) = .true.
        fl(7)    = .false.
        fl(8)    = .false.
        if(abs(aengy).lt.aold) aold = abs(aengy)

        fp(1)  = na
        fp(2)  = nau
        fp(3)  = nal
        fp(4)  = np(26)
        fp(5)  = np(21)
        call psolve(fp,.false.,.true.,.true.,prnt)

c       Update iteration counter

        niter  = niter  + 1
        iaugm  = iaugm  + 1

c       Check convergence

        if (rnmax.eq.0.0d0) then
          rnmax  = abs(aengy)
          reln   = 1.d0
          if(ct(1,l).le.0.0d0) ct(1,l) = 0.8d0
          aold   = rnmax/ct(1,l)/0.9999d0
        else
          reln   = (aengy)/rnmax
        endif
        if(pfr) write(iow,2000) rnmax,aengy,reln,tol
        if(pfr.and.ior.lt.0) write(*,2000) rnmax,aengy,reln,tol
        if(abs(aengy).le.tol*rnmax .or. linear
     +                             .or. abs(aengy).lt.enzer ) then
          if(lv.gt.1) then
            ct(1,lve(lv)) = ct(1,lvs(lv))
            l = lve(lv) - 1
          endif
        elseif(.not.linear) then

c         Line search

          if(pcomp(lct(l),'line',4).and.abs(aengy).gt.ct(1,l)*aold) then
            setvar = palloc(81,'TEMP1',nneq        , 2)
            setvar = palloc(82,'TEMP2',nneq*(nrt+4), 2)
            step = 1.0

            call serchl(aold,mr(np(31)),np(81),np(40),
     &                  hr(np(26)),ct(1,l),hr(np(82)),neq,step)

            setvar = palloc(82,'TEMP2', 0, 2)
            setvar = palloc(81,'TEMP1', 0, 2)
          endif
          if(arcf) then
            call arclen(hr(np(26)),hr(np(79)),hr(np(80)),hr(np(27)),
     &                  hr(nal),hr(nau),hr(na),mr(np(21)),mr(np(31)),
     &                  ndf,numnp,neq,ttim)
          endif
        endif
      else
        call ploa1(ttim,dt )
        call pload(mr(np(31)),hr(np(30)),hr(np(26)),prop*rlnew,.false.)
      endif

      call update(mr(np(31)),hr(np(30)),hr(np(40)),hr(np(42)),
     &            hr(np(26)),fl(9),2)
      fl(8) = .false.
      return

c     Modify mesh data
c     [mesh]           -  Reenter mesh generation phase
c     [mesh,filename]  -  Read the data from 'filename'

c     Reset boundary codes to represent correct b.c. - can change

3     eanfl = .false.
      ebcfl = .false.
      edifl = .false.
      efcfl = .false.

      neang = 0
      nebcs = 0
      nedis = 0
      nefrc = 0

c     Set coordinate input flags all false

      surfl = .false.
      boufl = .false.
      disfl = .false.
      forfl = .false.
      angfl = .false.
      reafl = .false.
      intfl = .false.

      nsurf = 0
      nbouf = 0
      ndisf = 0
      nforf = 0
      nangf = 0
      nintf = 0

c     Regenerate mesh data

      if(.not.pcomp(lct(l),' ',1)) then

c       Set inputs from file specified in second field

        fint = lct(l)
        if(ior.lt.0) write(*,2001) fint
        call pincld(fint)
      endif

      i     = -1
      chflg = .true.
      call pmesh(i,prt,prth)
      chflg = .false.

      if(.not.pcomp(lct(l),' ',1)) then
        call pincld('end')
      endif

c     Set edge boundary codes, forces, displacements, and angles

      if(eanfl.or.ebcfl.or.edifl.or.efcfl) then
        call pedgin()
      endif

c     Add surface loads and boundary codes

      if(surfl .or. boufl .or. forfl .or. angfl
     &         .or. disfl .or. reafl .or. intfl ) then
        call ploadc()
      endif

c     Set ID to current boundary condition array

      do k2 = 0,nneq-1
        mr(np(31)+k2) = mr(np(31)+k2+nneq)
      end do

c     Set region indicator so all active region elements are assembled

      nreg = -1

c     Set new current profile

      call profil(mr(np(21)),mr(np(34)),mr(np(31)),mr(np(33)),1,pfr)
      call profil(mr(np(21)),mr(np(34)),mr(np(31)),mr(np(33)),2,pfr)
      return

c     Plot outputs
c     [plot] - enter plot mode for interactive
c     [plot,optn] - see plot manual for optn's

4     rfl = .false.
      call pplotf(lct(l),ct(1,l),prop)
      return

c     Subspace eigencomputations (for: mass,iden,geom)
c     [subs,,k1,k2]     - subspace for k1 eigenpairs
c     [subs,prin,k1,k2] - subspace for k1 eigenpairs - print matrices
c                       - k2 used to overwrite default no. guard vects.

5     if(fl(4) .and. .not.compfl) then
        write(iow,3002)
        if(ior.lt.0) write(*,3002)
        return
      endif
      if(fl(5).and.fl(6)) then
        write(iow,3003)
        if(ior.lt.0) then
          write(*,3003)
          return
        endif
        call plstop()
      endif
      if(fl(1)) then
        mb = np(9)
        imas = 1
      else
        mb = np(13)
        imas = 2
      endif
      mf  = ct(1,l)
      mad = ct(2,l)
      stol= ct(3,l)

c     Mask tolerance if it is too small

      if(stol.eq.0.d0) then
        stol = max(tol, 1.d-12)
      endif

      if(mad.le.0) mad = max(mf,8)
      mf = min(neq,max(1,mf))
      mq = min(mf+mad,neq)
      call numass(hr(mb),neq,mq)
      if(mq.lt.mf.and.ior.gt.0) write(iow,2002) mq
      if(mq.lt.mf.and.ior.lt.0) write(  *,2002) mq
      mf = min(mf,mq)
      setvar = palloc(76,'EVAL',mq,    2)
      setvar = palloc(77,'EVEC',mq*neq,2)
      setvar = palloc(81,'TEMP1',neq        ,2)
      setvar = palloc(82,'TEMP2',mq*(mq+1)/2,2)
      setvar = palloc(83,'TEMP3',mq*(mq+1)/2,2)
      setvar = palloc(84,'TEMP4',mq         ,2)
      setvar = palloc(85,'TEMP5',mq         ,2)
      setvar = palloc(86,'TEMP6',mq*mq      ,2)

      sfl    = pcomp(lct(l),'prin',4)
      setvar = pfr
      pfr    = sfl
      call subsp(hr(np(1))   ,hr(mb)    ,hr(np(77)),hr(np(81)),
     &           hr(np(82))  ,hr(np(83)),hr(np(76)),hr(np(84)),
     &           hr(np(85))  ,hr(np(86)),mf,mq,neq,imas,shift,stol,
     &           sfl,25)
      pfr    = setvar

      setvar = palloc(86,'TEMP6', 0,2)
      setvar = palloc(85,'TEMP5', 0,2)
      setvar = palloc(84,'TEMP4', 0,2)
      setvar = palloc(83,'TEMP3', 0,2)
      setvar = palloc(82,'TEMP2', 0,2)
      setvar = palloc(81,'TEMP1', 0,2)

      return

c     Write a file
c     [writ,file]  - open write file named 'file'
c     [writ,disp]  - write displacements to 'file'
c     [writ,stre]  - write nodal streses to 'file'
c     [writ,eige]  - write eigenpairs to 'file'
c     [writ,wind]  - rewind 'file'
c     [writ,clos]  - close 'file'

6     call writer(lct(l),hr(np(40)),nneq)
      return

c     Read a file
c     [read,file]  - open read file named 'file'
c     [read,disp]  - read displacements from 'file'
c     [read,stre]  - read nodal streses from 'file'
c     [read,eige]  - read eigenpairs from 'file'
c     [read,wind]  - rewind 'file'
c     [read,clos]  - close 'file'

7     call reader(lct(l),hr(np(40)),nneq)
      if(pcomp(lct(l),'disp',4)) fl(11) = .false.
      return

c     Restart previously run problem
c     [rest,ext_name]

8     fint = fres
      if(.not.pcomp(lct(l),'    ',4)) then
        call addext(fint,lct(l),17,4)
      endif
      if(ior.lt.0) write(*,2003) fint
      write(iow,2003) fint
      call restrt(fint,hr(np(40)),ndm,ndf,nneq,1)
      return

c     BFGS algorithm
c     [bfgs,,k1,k2,k3] - BFGS soln k1 = no. steps; k2 = line search tol;
c                                  k3 = bfgs energy tol

11    if (fl(12)) then
        setvar = palloc( 69,'BFGD',neq,   2)
        setvar = palloc( 70,'BFGO',neq,   2)
        setvar = palloc( 73,'BFGV',neq,   2)
        setvar = palloc( 74,'BFGW',neq,   2)
        setvar = palloc( 72,'BFGT',nneq*i, 2)
c       setvar = palloc( 71,'BFGS',neq*30,2)
        fl(12) = .false.
      endif

c     Call BFGS routine
c        Max. of of iterations (nbfgs) = 15
c        Default stol for line search  = .8
c        Default etol for bfgs energy  = tol

      nbfgs = ct(1,l)
      stol  = ct(2,l)
      etol  = ct(3,l)
      if (nbfgs.eq.0)    nbfgs = 15
      if (stol.eq.0.0d0) stol  = 0.8d0
      if (etol.le.0.0d0) etol  = tol
      stol  = min(stol,0.9d0)

c     Assign storage for BFGS V and W vectors

      setvar = palloc( 71,'BFGS',neq*nbfgs*2,2)

      call iterat(hr(na),mr(np(21)),np(40),np(26),
     &            hr(np(70)),hr(np(69)),hr(np(72)),accrcy,hr(np(73)),
     &            hr(np(74)),prt,mr(np(31)),nbfgs,stol,etol)
      return

c     Arc-length method
c     [arcl,,kflag,lflag]   - set arc length parameters
c     [arcl,add,k1,tau]     - add eigvenvector k1, amount = tau
c     [arcl,chec,k1]        - check for bifurcation using eigv. k1
c     [arcl,off]            - set arclength to off

12    if(pcomp(lct(l),'off' ,3)) then
        if(ior.lt.0) write(*,2004)
        write(iow,2004)
        arcf  = .false.
        rlnew = 1.d0
        return
      else
        if(ior.lt.0) write(*,2005)
        write(iow,2005)
      endif
      if(pcomp(lct(l),'add' ,3)) go to 121
      if(pcomp(lct(l),'chec',4)) go to 122
      if(kflag.eq.0) kflag = ct(1,l)
      if(kflag.eq.0) kflag = 2
      lflag  = ct(2,l)
      if(lflag.eq.0) then
        k1 = max(neq,nneq)
        setvar = palloc( 79,'MU1  ',k1,2)
        if(kflag .eq. 2 .or. kflag .eq. 3 .or. kflag .eq. 5) then
          setvar = palloc( 80,'MU2   ',k1,2)
        endif
        arcf = .true.
      endif
      call dicont(mr(np(31)),numnp,ndf,lflag)
      return

c     Add scaled eigenvector to displacement vector

121   tau = ct(2,l)
      k1  = ct(1,l)
      k1 = max(min(mf,k1),1)
      if(np(77).eq.0.and.ior.lt.0) then
        write(*,3004)
        return
      endif
      kk = (k1 - 1) * neq
      if(tau.eq.0.0d0) then
        vphi = dotid(hr(np(40)),hr(np(77)+kk),mr(np(31)),nneq)
        phi2 = dot(hr(np(77)+kk),hr(np(77)+kk),neq)
        ee   = dot(hr(np(40)),hr(np(40)),nneq)
        tau  = 100.d0 * vphi / sqrt(ee*phi2) + 1.d0
        if(ior.lt.0) write(*,2006) tau,k1
        write(iow,2006) tau,k1
      endif
      call paddv(hr(np(40)),hr(np(77)+kk),nneq,tau,mr(np(31)))
      return

c     Check for bifurcation or limit point

122   k1 = ct(1,l)
      if(k1.eq.0) k1 = 1
      if(np(77).eq.0.and.ior.lt.0) then
        write(*,3004)
        return
      endif
      kk = (k1 - 1)*neq
      chec = dotid(hr(np(27)),hr(np(77)+kk),mr(np(31)),nneq)
      if(ior.lt.0) write(*,2007) k1,chec
      write(iow,2007) k1,chec
      return

c     Save restart information for intermediate points
c     [save,ext_name]

13    fint = fres
      if(.not.pcomp(lct(l),'    ',4)) then
        call addext(fint,lct(l),17,4)
      endif
      if(ior.lt.0) write(*,2008) fint
      write(iow,2008) fint
      call restrt(fint,hr(np(40)),ndm,ndf,nneq,2)
      return

c     [eige,<vect>] Compute last element eigenpairs

14    if(cknon0(hr(np(36)),nst*nst)) then
        tfl = pcomp(lct(l),'vect',4)
        setvar = palloc(75,'EIGE ',  nst*nst+  nst, 2)
        setvar = palloc(81,'TEMP1',2*nst*nst+5*nst, 2)
        call peige(hr(np(36)),nst,hr(np(81)),tfl)
        setvar = palloc(81,'TEMP1', 0, 2)
      else
        if(ior.lt.0) then
          write(*,*) '   *ERROR* Perform TANG or CMAS First'
        endif
      endif
      return

c     [epri]nt - Output last element stiffness and residual

15    call mprint(hr(np(36)),nst,nst,nst,'Last Element S-Matrix')
      call mprint(hr(np(35)),  1,nst,  1,'Last Element P-Vector')
      return

c     [eigv],dofs,list      : Set DOFS for eigen computation (1= active; 0=not)
c     [eigv],all,nnn        : Output eigenvector nnn (all)
c     [eigv],coor,k1,xt,nnn : Output eigenvector nnn for x_k1 = xt
c     [eigv],nnn,k1,k2,k3   : Output eigenvector nnn nodes k1 - k2 @ inc k3

16    if(pcomp(lct(l),'dofs',4)) then

        if(ior.lt.0) write(*,4000)
        setvar = pinput(td,ndf)
        do i = 1,ndf
          ndfeig(i) = td(i)
        end do ! i
        write(iow,2009) (i,ndfeig(i),i=1,ndf)
        if(ior.lt.0) then
          write(*,2009) (i,ndfeig(i),i=1,ndf)
        endif

      elseif(np(77).gt.0) then

        call pzero(hr(np(26)),nneq)
        tfl = pfr

c       Set for all

        if(pcomp(lct(l),'all ',4)) then

          nxt = 0
          nnn = max(1,min(mq,int(ct(1,l)))) - 1
          k1  = 1
          k2  = numnp
          k3  = 1

c       Set for specified coordinate

        elseif(pcomp(lct(l),'coor',4)) then
          nxt = max(1,min(int(ct(1,l)),ndm))
          xt  = ct(2,l)
          xtol= 0.01
          k1  = 1
          k2  = numnp
          k3  = 1
          nnn = max(1,min(mq,int(ct(3,l)))) - 1

c       Output vector nnn nodal values

        else
          call setval(lct(l),15,tau)
          nnn = max(1,min(mq,int(tau))) - 1
          nxt = 0
          k1  = int(ct(1,l))
          k2  = int(ct(2,l))
          k3  = int(ct(3,l))
          if(k2.eq.0) k2 = k1
          if(k3.eq.0) k3 = 1
          k1  = max(1,min(k1,numnp))
          k2  = max(1,min(numnp,k2))
          if(k2-k1.ne.0) k3 = sign(k3,k2-k1)
          pfr = .true.
        endif

c       Print eigenvectors

        call pmovec(mr(np(31)),hr(np(77)+nnn*neq),hr(np(26)),nneq)
        call prtdis(hr(np(43)),hr(np(26)),ttim,hr(np(76)+nnn),ndm,ndf,
     &              k1,k2,k3,4,prth)
        pfr = tfl

c     Warn must compute eigen problem first

      else
        write(iow,3005)
        if(ior.lt.0) then
          write(*,3005)
        endif
      endif
      return

c     [show]      show current solution parameters
c     [show,cont] show user contact types and variables
c     [show,dict] show dictionary of program array allocation
c     [show,elem] show user element types

c       Output loaded user element descriptors

17    if(pcomp(lct(l),'elem',4)) then
        write(*,2010)
        do nn = 1,50,20
          do i = nn,min(nn+19,50)
            call elmlib(hr,hr,hr,mr,hr,hr,hr,ndf,ndm,ndm,i,0)
          end do
        end do

c     Dictionary prints

      elseif(pcomp(lct(l),'dict',4)) then

        call pprtd

c     Output array values

      elseif(.not.pcomp(lct(l),'    ',4)) then

        call outary(lct(l),ct(1,l))

c     Show problem sizes

      else
        if(neq.gt.0) then
          npr = mr(np(21)+neq-1)
          nnp = npr/neq
        else
          npr = 0
          nnp = 0
        endif
        if(ior.lt.0) then
          write(  *,2011) numnp,numel,nummat,neq,npr,nnp,
     &                    ttim,rnmax,dt,aengy,tol,augf,
     &                    prop,noi,nstep,titer
        else
          write(iow,2011) numnp,numel,nummat,neq,npr,nnp,
     &                    ttim,rnmax,dt,aengy,tol,augf,
     &                    prop,noi,nstep,titer
        endif
      endif
      return

c     [tplo]t,,<interval> - time history plots for displacements / stresses
c      Options : disp,n1,n2,x,y,z
c                velo,n1,n2,x,y,z
c                acce,n1,n2,x,y,z
c                reac,n1,n2,x,y,z
c                stre,n1,n2,x,y,z
c                show
c     Set output incrment

18    ntincr = max(1,int(ct(1,l)))

      if(prt) then
        write(iow,2012) ntincr
        if(ior.lt.0) then
          write(*,2012) ntincr
        endif
      endif

c     Input list of time history output quantities

      if(ior.lt.0) write(*,3006)
181   if(ior.lt.0) write(*,3007)
      setvar = tinput(yyy,1,td,5)
      if(setvar) go to 181
      type(1) = yyy(1)
      n       = td(1)
      i       = td(2)

c     Find the location for unspecified node/elements

      if(n.eq.0) then

c       Locate a node

        if(pcomp(yyy,'disp',4) .or.
     &     pcomp(yyy,'velo',4) .or.
     &     pcomp(yyy,'acce',4) .or.
     &     pcomp(yyy,'reac',4)) then

          call pgetd('X    ',nx,nl,nn, setvar)
          dist = 0.d+0
          do nty = nx,nx+nl-1
            dist = max(dist,abs(hr(nty)))
          end do

          nn = 0
          do nty = nx,nx+nl-1,ndm
            nn = nn + 1
            if(mr(np(49)+nn-1).ge.0) then
              dd = dotx(hr(nty),td(3),ndm)
              dd = sqrt(dd)
              if(dd.lt.dist) then
                dist = dd
                n    = nn
              endif
            endif
          end do

c       Locate an element

        elseif(pcomp(yyy,'stre',4)) then

          call pgetd('X    ',nx,nl,nn, setvar)
          dist = 0.d+0
          do n = nx,nx+nl-1
            dist = max(dist,abs(hr(n)))
          end do

          call pgetd('IX   ',nnn,nl,nn,setvar)
          nn = 0
          do jty = nnn,nnn+nl-1,nen1
            nn  = nn + 1
            do jj = 1,ndm
              xc(jj) = 0.0d0
            end do

            kk = 0
            do jj = 0,nen-1
              k = mr(jty+jj)
              if(k.gt.0) then
                kk  = kk + 1
                nty = nx + k*ndm - ndm -1
                do ii = 1,ndm
                  xc(ii) = xc(ii) + hr(nty+ii)
                end do
              endif
            end do
            if(kk.gt.0) then
              do ii = 1,ndm
                xc(ii) = xc(ii) / kk
              end do

              dd = dotx(xc(1),td(3),ndm)
              dd = sqrt(dd)
              if(dd.lt.dist) then
                dist = dd
                n    = nn
              endif
            endif
          end do

        endif
      endif

      if(pcomp(type,'    ',4)) return

c     Displacements

      if(pcomp(type,'disp',4)) then
        ndplts         = min(npmx,ndplts + 1)
        idpl(1,ndplts) = ndf*(n-1)+i
        idpl(2,ndplts) = n

c     Velocities

      elseif(pcomp(type,'velo',4)) then
        nvplts         = min(npmx,nvplts + 1)
        ivpl(1,nvplts) = ndf*(n-1)+i
        ivpl(2,nvplts) = n

c     Accelerations

      elseif(pcomp(type,'acce',4)) then
        naplts         = min(npmx,naplts + 1)
        iapl(1,naplts) = ndf*(n-1)+i
        iapl(2,naplts) = n

c     Stresses

      elseif(pcomp(type,'stre',4)) then
        nsplts         = min(nsmx,nsplts + 1)
        ispl(1,nsplts) = n
        ispl(2,nsplts) = i

c     Reactions

      elseif(pcomp(type,'reac',4)) then
        nrplts         = min(npmx,nrplts + 1)
        irpl(1,nrplts) = ndf*(n-1)+i
        irpl(2,nrplts) = n

c     Show: Active outputs

      elseif(pcomp(type,'show',4)) then

        do n = 1,ndplts
          i = idpl(1,n) - ndf*(idpl(2,n) -1)
          if(ior.lt.0) write(*,3008) n,idpl(2,n),i
          write(iow,3008) n,idpl(2,n),i
        end do

        do n = 1,nvplts
          i = ivpl(1,n) - ndf*(ivpl(2,n) -1)
          if(ior.lt.0) write(*,3009) n,ivpl(2,n),i
          write(iow,3009) n,ivpl(2,n),i
        end do

        do n = 1,naplts
          i = iapl(1,n) - ndf*(iapl(2,n) -1)
          if(ior.lt.0) write(*,3010) n,iapl(2,n),i
          write(iow,3010) n,iapl(2,n),i
        end do

        do n = 1,nrplts
          i = irpl(1,n) - ndf*(irpl(2,n) -1)
          if(ior.lt.0) write(*,3012) n,irpl(2,n),i
          write(iow,3012) n,irpl(2,n),i
        end do

        do n = 1,nsplts
          if(ior.lt.0) write(*,3012) n,ispl(1,n),ispl(2,n)
          write(iow,3012) n,ispl(1,n),ispl(2,n)
        end do

        return

      endif
      go to 181

c     Output formats

2000  format('   Energy convergence test'/
     &       '    Maximum   =',1p,e25.15,' Current   =',1p,e25.15/
     &       '    Relative  =',1p,e25.15,' Tolerance =',1p,e25.15)
2001  format('   Read new mesh data from file : ',a)
2002  format('   Number of eigenpairs reduced to',i4,' by number of',
     &       ' nonzero lumped mass terms')
2003  format('   Restart from : ',a)
2004  format('   Arc length set to OFF.')
2005  format('   Arc length set to ON.')
2006  format('   Scaling factor tau = ',1p,1e15.5,' using phi',i2)
2007  format('   Bifurcation check: f*phi',i2,' = ',1p,1e15.5)
2008  format('   Save to file : ',a)
2009  format(/'   Eigenpair active DOF (1 = active; 0 = inactive)'/
     &      (7x,6(i3,'-dof =',i3)))
2010  format('   A v a i l a b l e    E l e m e n t    T y p e s',/)
2011  format(/,
     &  '   C u r r e n t    S o l u t i o n    P a r a m e t e r s',/
     &  /,'     Number nodes  =',i8,4x,' :  Number elements  =',i8,/
     &    '     Number matls  =',i8,4x,' :  Number equations =',i8,/
     &    '     Profile terms =',i8,4x,' :  Avg. column      =',i8,/
     &    '     Time          =',e12.4,' :  Max. energy norm =',e12.4,/
     &    '     Dt            =',e12.4,' :  Energy norm      =',e12.4,/
     &    '     Tol           =',e12.4,' :  Augment factor   =',e12.4,/
     &    '     Prop load     =',e12.4,' :  Time integration =',i8,/
     &    '     No. Steps     =',i8,4x,' :  No. Iterations   =',i8)
2012  format(/'   Output interval for time history data =',i4)

c     Warnings and errors

3000  format(' *WARNING* Unable to compute incremental acceleration,'
     &      ,' No mass matrix defined')
3001  format(' *ERROR* Problem not dynamic - no output produced')
3002  format(' *ERROR* No stiffness matrix, use TANG or UTAN')
3003  format(' *ERROR* No mass matrix, use MASS or IDEN')
3004  format(' *ERROR* Compute eigenvectors first')
3005  format(' *ERROR* Must use SUBS command first')
3006  format(' Input: Type (disp:velo:acce:stres:reac);'/
     &       '        Node/Elmt; dof/no.')
3007  format(10x,'>',$)
3008  format(1x,'Plot',i3,' Displ. : Node  =',i4,' DOF =',i3)
3009  format(1x,'Plot',i3,' Veloc. : Node  =',i4,' DOF =',i3)
3010  format(1x,'Plot',i3,' Accel. : Node  =',i4,' DOF =',i3)
3011  format(1x,'Plot',i3,' React. : Node  =',i4,' DOF =',i3)
3012  format(1x,'Plot',i3,' Stress : Elmt  =',i4,' No. =',i3)

c     Prompts

4000  format(' Input DOF for eigen computations'/5x,'>',$)

      end

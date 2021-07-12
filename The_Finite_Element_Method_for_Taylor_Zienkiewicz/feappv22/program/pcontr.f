c$Id:$
      subroutine pcontr()

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Control program for FEAPpv problem input and solution.

c      Inputs:
c        none

c      Outputs:
c        none
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'allotd.h'
      include  'bdata.h'
      include  'cblend.h'
      include  'cdata.h'
      include  'cdat1.h'
      include  'chdata.h'
      include  'codat.h'
      include  'corset.h'
      include  'cornum.h'
      include  'comfil.h'
      include  'compac.h'
      include  'conval.h'
      include  'crotas.h'
      include  'debugs.h'
      include  'edgdat.h'
      include  'errchk.h'
      include  'hlpdat.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'ioincl.h'
      include  'iosave.h'
      include  'linka.h'
      include  'mdata.h'
      include  'mxsiz.h'
      include  'pdata2.h'
      include  'pdata5.h'
      include  'pdata6.h'
      include  'pdatps.h'
      include  'pointer.h'
      include  'plflag.h'
      include  'prflag.h'
      include  'print.h'
      include  'psize.h'
      include  'refng.h'
      include  'region.h'
      include  'sdata.h'
      include  'umac1.h'
      include  'vdata.h'
      include  'comblk.h'

      logical   errs,incf,initf,intr,intx,tfl,tief,tiefl
      logical   setvar,palloc,errc,pinput,tinput,nocount,pcomp,evint
      logical   cprt,oprt,oprth,mulprob,newprob,usetfl(5)
      character titl*80,dnam*15, fnamr*21, cdate*24, fext*4
      character uset(5)*4, type*4, usub*4, fnamp*128
      integer   i, iii,iorsv, irdef, j, l1,l2,l3,l4, nad,npd
      integer   usetno(5)
      real*8    td(12)

      save

c     Default names for manipulation sets

      data      uset / 'man1', 'man2', 'man3', 'man4', 'man5' /

c     Destroy old output file if it exists

      inquire(file=fout,exist=initf)
      if(initf) then
        open (unit=iow,file=fout,status='old')
        close(unit=iow,          status='delete')
      endif

c     Open files for input and output

      open(unit=ior,file=finp,status='old')
      open(unit=iow,file=fout,status='new')

c     Initial values for include options

      chflg   = .false.
      cprt    = .true.
      everon  = .false.
      evint   = .false.
      hdcpy   = .false.
      incf    = .false.
      intr    = .false.
      intx    = .false.
      newprob = .false.
      mulprob = .false.
      nocount = .true.
      debug   = .false.
      lread   = .false.
      lsave   = .false.
      eofile  = .false.
      ucount  = .false.
      lfile   = ios
      icf     = icl
      isf     = 1
      irdef   = ior
      fincld(1) = finp
      irecrd(1) = 0

c     Set default to print headers

      prth   = .true.

c     Flags for user manipulation commands

      do j = 1,5
        usetfl(j) = .false.
        usetno(j) = 0
      end do ! j

c     Install user functions

c     Set umesh input names

      do j = 1,5
        write(usub,'(a3,i1)') 'mes',j
        uct = usub
        call umshlib(j,prt)
        umshc(j) = uct
      end do ! j

c     Set umacro input names

      do j = 1,5
        write(usub,'(a3,i1)') 'mac',j
        uct   = usub
        fnamp = ' '
        call umaclib(j,fnamp,td,.false.)
        umacc(j) = uct
      end do ! j

c     Set umati model names

      do j = 1,5
        write(usub,'(a3,i1)') 'mat',j
        uct = 'mate'
        call uconst(usub,td,td,td,l1,l2,l3)
      end do ! j

c     Set uplot input names

      do j = 1,5
        write(usub,'(a3,i1)') 'plt',j
        uct = usub
        call upltlib(j,td,.false.)
        upltc(j) = uct
      end do ! j

c     Set umanipulation names

      do j = 1,5
        uct     = uset(j)
        call usetlib(j,prt)
        uset(j) = uct
      end do ! j

c     Input with interactive interactive statements

1     if(intx) then
        if(cprt) write(*,2009)
        errck = tinput(dnam,1,td,0)

c       Read command interactively

        if(pcomp(dnam,'y',1)) then
          write(*,2010)
          read (*,1000,err=900,end=910) yyy
          cprt = .true.

c       Read command from current file and turn off intx flag

        else
          cprt = .false.
          intx = .false.
          ior  =  abs(ior)
          read(ior,1000,err=900,end=910) yyy
        endif

c     Input from current file

      else
        ior = abs(ior)
        read(ior,1000,err=900,end=910) yyy
      endif

c     Compare with command list

      call pstrip(xxx,yyy,1)
      l1   = len(xxx)
      titl = xxx(1:l1)

c     Start solution of new problem

      if(pcomp(titl(1:4),'feap',4)) then
        go to 100

c     Set count/nocount mode

      elseif(pcomp(titl(1:4),'noco',4)) then
        nocount = .false.

      elseif(pcomp(titl(1:4),'coun',4)) then
        nocount = .true.

c     User command sets

      elseif(pcomp(titl(1:4),uset(1),4)) then
        usetno(1) = usetno(1) + 1
        usetfl(1) = .true.
        fext  = 'u1a'
        go to 300
      elseif(pcomp(titl(1:4),uset(2),4)) then
        usetno(2) = usetno(2) + 1
        usetfl(2) = .true.
        fext  = 'u2a'
        go to 300
      elseif(pcomp(titl(1:4),uset(3),4)) then
        usetno(3) = usetno(3) + 1
        usetfl(3) = .true.
        fext  = 'u3a'
        go to 300
      elseif(pcomp(titl(1:4),uset(4),4)) then
        usetno(4) = usetno(4) + 1
        usetfl(4) = .true.
        fext  = 'u4a'
        go to 300
      elseif(pcomp(titl(1:4),uset(5),4)) then
        usetno(5) = usetno(5) + 1
        usetfl(5) = .true.
        fext  = 'u5a'
        go to 300

c     Perform inputs from an include file

      elseif(pcomp(titl(1:4),'incl',4)) then
        call acheck(titl,yyy,15,80,80)
        read(yyy,1002,err=900,end=900) titl(1:4),dnam
        if(pcomp(dnam,'end',3)) then
          call pincld(dnam)
          if(evint) then
            write(*,2005) fnamr
          endif
          write(iow,2005) fnamr
        else
          fnamr =  dnam
          call pincld(dnam)
        endif
        incf = .true.
        cprt = .false.

c     Perform inputs for initial conditions

      elseif(pcomp(titl(1:4),'init',4)) then
        call acheck(titl,yyy,15,80,80)
        read(yyy,1001,err=900,end=900) titl(1:4),dnam(1:4)
        call pinitl(dnam,errs)
        if(errs) return

c     Solution mode

      elseif(pcomp(titl(1:4),'inte',4)) then
        ior   = -abs(ior)
        evint = .true.
        intr  = .true.
        intx  = .true.
        cprt  = .true.
        call pltcur()
        go to 400

      elseif(pcomp(titl(1:4),'batc',4)) then
        intr = .false.
        go to 400

c     Manual level set: 0 = basic; 1 = advanced; 2 = expert

      elseif(pcomp(titl(1:4),'manu',4)) then
        call acheck(titl,yyy,15,80,80)
        read(yyy,1003,err=900,end=911) titl(1:4),hlplev
        hlplev = max(-1,min(3,hlplev))

c     Mesh manipulations: Link and tie

c     Reset id list to link dof's on different nodes - set by node #

      elseif(pcomp(titl(1:4),'link',4)) then
        call plinka('lnk ','set')
        lkflg = .true.

      elseif(pcomp(titl(1:4),'tie' ,3)) then
        go to 500

c     Parameter sets

      elseif(pcomp(titl(1:4),'para',4) .or.
     &       pcomp(titl(1:4),'cons',4)) then
        coflg = .true.
        call pconst(prt)

c     Remarks to output file

      elseif(pcomp(titl(1:4),'rema',4)) then
        write(*,2008) titl(1:78)

c     Stop execution

      elseif(pcomp(titl(1:4),'stop',4)) then
        call pdelfl()
        if(evint) write(*,2004) fout
        if(ior.eq.irdef) return

      endif

c     Read again

      go to 1

c     Start Problem: Read and print control information

100   newprob = .true.
      do i = 1,20
        l2      = 4*i
        l1      = l2 - 3
        head(i) = titl(l1:l2)
      end do
      incf   = .false.
      intr   = .false.
      intx   = .false.
      call fdate( cdate )
      errc   = pinput(td,9)
      numnp  = td(1)
      numel  = td(2)
      nummat = td(3)
      ndm    = td(4)
      ndf    = td(5)
      nen    = td(6)
      npd    = td(7)
      nud    = td(8)
      nad    = td(9)

c     Adjust storage for material parameters

      npd    = max(npd,200)
      nud    = max(nud, 50)
      ndd    = npd + nud + 1

c     Blending function initialization

      numsn  = 0
      numsd  = 0
      numbd  = 0

c     Set filename for plot/tplot outputs

      if(idev.eq.2) then
        inquire(unit=ior,name=fnamr,exist=errs)

        if(errs) then
          if(fplt(2:12).eq.finp(2:12) .or. mulprob) then
            fnamr(1:1) = 'P'
            fplt       = fnamr
            mulprob    = .true.
          endif
        endif
      endif

c     If number of nodes, or elements is zero compute number from data

      if(nocount) then
        ucount      = .true.
        call pnums()
        irecrd(isf) = 2
        ucount      = .false.
      endif

c     Output problem size data

200   write(iow,2000) head,cdate,versn,fincld(isf),
     &               numnp,numel, ndm,ndf,nen, nummat,npd,nud

c     Set parameters for page eject and rotation dof

      o   = '    '
      errck = .false.
      lsave = .false.
      lkflg = .false.
      initf = .true.
      eanfl = .false.
      ebcfl = .false.
      edifl = .false.
      efcfl = .false.
      eprfl = .false.
      surfl = .false.
      boufl = .false.
      cprfl = .false.
      disfl = .false.
      forfl = .false.
      angfl = .false.
      tiefl = .true.
      tief  = .false.

      do i = 1,50
        ia(1,i) = 1
        ia(2,i) = 2
        ir(1,i) = 0
        ir(2,i) = 0
        ea(1,i) = 1
        ea(2,i) = 2
        er(1,i) = 0
        er(2,i) = 0
        inord(i) = 0
        exord(i) = 0
        do j = 1,30
          ipord(j,i) = 0
          epord(j,i) = 0
        end do
      end do
      nprof  = 0
      nsurf  = 0
      nbouf  = 0
      ndisf  = 0
      nforf  = 0
      nangf  = 0
      nintf  = 0
      neang  = 0
      nebcs  = 0
      nedis  = 0
      nefrc  = 0
      nepro  = 0

c     Zero global parameters

      gref   = 0
      do i = 1,3
        grefx(i) = 0.0d0
        gtref(i) = 0.0d0
      end do ! i
      do i = 1,2
        gray(i) = 0.0d0
      end do ! i
      do i = 1,14
        gfac(i) = 0.0d0
      end do ! i

c     Zero pointer array

      do i = 1,num_nps
        np(i) = 0
      end do
      setvar = palloc( 0 ,'START', 0 , 0 )

c     Set pointers for allocation of mesh arrays

      nen1      = nen + 5
      nie       = ndf + 8
      nst       = nen*ndf + nad
      mmax      = 1
      ndict     = 0
      ipoint(1) = mmax
      nneq      = ndf*numnp

c     Allocate size for arrays for mesh and solution vecors

      l1 = ndm*numnp
      l2 = ndf*numnp
      l3 = max(nen+1,nst,21)
      l4 = numnp*max(ndf,ndm)

c     Allocate and zero arrays

      setvar = palloc(26,'DR   ',l4        ,  2)
      setvar = palloc(34,'LD   ',l3        ,  1)
      setvar = palloc(35,'P    ',nst*2     ,  2)
      setvar = palloc(36,'S    ',nst*nst*2 ,  2)
      setvar = palloc(39,'TL   ',nen       ,  2)
      setvar = palloc(41,'UL   ',nst*14    ,  2)
      setvar = palloc(44,'XL   ',max(4,nen)*3,2)
      setvar = palloc(25,'D    ',nummat*ndd,  2)
      setvar = palloc(32,'IE   ',nummat*nie,  1)
      setvar = palloc(31,'ID   ',l2*2      ,  1)
      setvar = palloc(33,'IX   ',nen1*numel,  1)
      setvar = palloc(49,'NDTYP',numnp     ,  1)
      setvar = palloc(43,'X    ',l1        ,  2)
      setvar = palloc(45,'ANG  ',numnp,       2)
      setvar = palloc(46,'ANGL ',nen       ,  2)
      setvar = palloc(27,'F    ',2*l2      ,  2)
      setvar = palloc(28,'F0   ',4*l2,        2)
      setvar = palloc(29,'FPRO ',2*l2      ,  1)
      setvar = palloc(30,'FTN  ',4*l2      ,  2)
      setvar = palloc(38,'T    ',numnp     ,  2)
      setvar = palloc(40,'U    ',3*l2      ,  2)
      setvar = palloc(47,'NREN ',numnp     ,  1)

c     Set initial numbering in renumber vector and mark nodes as unused.

      do i = 0,numnp-1
        mr(np(47)+i) = i+1
        mr(np(49)+i) = 0
      end do

c     Input mesh data from file

      iii   =  0
      call pmesh(iii,prt,prth)

c     Set edge boundary codes, forces, displacements, and angles

      if(eanfl.or.ebcfl.or.edifl.or.efcfl.or.eprfl) then
        call pedgin()
      endif

c     Set cordinate angles, boundary codes, forces, displacements,
c         proportional load types and surface loads

      if(boufl .or. surfl .or. angfl .or.
     &   disfl .or. cprfl .or. forfl) then
        call ploadc()
      endif

c     Perform simple check on mesh to ensure basic data was input

      setvar = palloc(81,'TEMP1',numnp*ndf, 1)
      call meshck(mr(np(81)),mr(np(32)),mr(np(31)+nneq),mr(np(49)),
     &            mr(np(33)),nie,nen,nen1,ndf,numnp,
     &            numel,nummat,errs)
      setvar = palloc(81,'TEMP1',0, 1)
      if(errs) then
        call pdelfl()
        return
      endif

c     Set user mesh manipulation input names

      do j = 1,5
        uct = uset(j)
        call usetlib(j,prt)
        uset(j) = uct
      end do

c     Compute boundary nodes (before ties)

      if(tiefl) then
        setvar = palloc( 78,'EXTND',numnp   ,1)
        call pextnd()
        tiefl = .false.
      endif

      tfl = .true.
      go to 1

c     Perform user manipulation commands

300   errs  = .true.
      j     = 0
      do while(errs .and. j.lt.26)
        j     = j + 1
        fnamr = fsav
        write(fext(3:3),'(a1)') char(96+j)
        call addext(fnamr,fext,18,4)
        inquire(file = fnamr, exist = errs)
      end do !
      call plinka(fext,'set')
      go to 1

c     Establish profile of resulting equations for stiffness, mass, etc
c     [batc]h execution

400   if(.not.newprob) then
        write(*,3001)
        call plstop()
      elseif(intx .and. .not.intr .and. .not.incf) then
        write(*,3002)
        go to 1
      endif

      if(tfl) then

c       If ties have occurred merge boundary conditions, forces & contact

        if(tief) then
          call tiefor(mr(np(31)+nneq),hr(np(27)),mr(np(48)),ndf,numnp)
        endif

c       Compute boundary nodes (after ties)

        call pextnd()

c       Allocate memory to store all possible equations

        neq = numnp*ndf
        setvar = palloc( 21, 'JPROF', neq, 1)

c       Set user commands

        do j = 1,5
          fext = 'u1a'
          write(fext(2:2),'(i1)') j
            if(usetfl(j)) then
            do l3 = 1,26
              write(fext(3:3),'(a1)') char(96+l3)
              fnamr =  fsav
              call addext(fnamr,fext,18,4)
              inquire(file = fnamr, exist = errs)
              if(errs) then
                call opnfil(fext,fnamr,-1,ios,prt)

c               Read data from file

                iorsv = ior
                ior   = ios

                do l1 = 0,36
                  do l2 = 1,26
                    vvsave(l2,l1) = vvv(l2,l1)
                  end do
                end do
                oprt  = prt
                oprth = prth

                read(ior,1004) type,fincld(isf),irecrd(isf),prt,prth
                read(ior,1005) vvv

                call usetlib(j,prt)

                close(ior,status='delete')
                ior   = iorsv

                do l1 = 0,36
                  do l2 = 1,26
                    vvv(l2,l1) = vvsave(l2,l1)
                  end do
                end do
                prt  = oprt
                prth = oprth

              endif
            end do ! l3
          endif
        end do ! j

c       Determine current profile

        do j = 0,nneq-1
          mr(np(31)+j) = mr(np(31)+j+nneq)
        end do

        mxpro = 0
        mxneq = 0

c       Set current profile

        if(ior.lt.0) write(*,*) ' '
        call profil(mr(np(21)),mr(np(34)),mr(np(31)),
     &              mr(np(33)),1,prt)
        call profil(mr(np(21)),mr(np(34)),mr(np(31)),
     &              mr(np(33)),2,prt)
        mxpro = max(mxpro,(mr(np(21)+neq-1)))
        mxneq = max(mxneq,neq)

c       Set up stress history addresses

        call sethis(mr(np(32)),mr(np(33)),nie,nen,nen1,numel,nummat,prt)

        tfl = .false.

      endif

c     Macro module for establishing solution algorithm

      call pmacr(initf)
      go to 1

c     Tie nodes within tolerance of one another
c     [tie ] - merge regions with common coordinates

500   call acheck(titl,yyy,15,80,80)
      read(yyy,1001,err=900,end=911) titl(1:4),titl(16:19),(td(j),j=1,3)

c     Retrieve current boundary connection status

      if(.not.tief) then
        setvar = palloc( 48,'IPOS ',numnp,  1)
        call pseqn(mr(np(48)),numnp)
        tief = .true.
      endif

c     Tie line elements to regions

      if(pcomp(titl(16:19),'line',4)) then
        l2 = max(    1,min(nummat,int(td(1))))
        call ptiend(mr(np(32)),mr(np(33)),mr(np(78)),mr(np(48)),
     &              hr(np(43)),l2,nie,nen,nen1,ndm,numel)
      else

        if(pcomp(titl(16:19),'node',4)) then
          l1 = max(    1,int(td(1)))
          l2 = min(numnp,int(td(2)))
          j     = 0
          td(2) = 0.0d0
          write(iow,2011) l1,l2
        elseif(pcomp(titl(16:19),'regi',4)) then
          l1 = 1
          l2 = numnp
          l3 = max(    0,int(td(1)))
          l4 = min(mxreg,int(td(2)))
          j     = -1
          write(iow,2012) l3,l4
        elseif(pcomp(titl(16:19),'mate',4)) then
          l1 = 1
          l2 = numnp
          l3 = max(     1,int(td(1)))
          l4 = min(nummat,max(1,int(td(2))))
          j     = -2
          write(iow,2013) l3,l4
        else
          j  = td(1)
          l1 = 1
          l2 = numnp
          l3 = 0
          l4 = 0
          if(j.gt.0) then
            write(iow,2014) j,td(2)
          else
            write(iow,2015)
          endif
        endif
        setvar = palloc(81,'TEMP1',numnp, 1)
        setvar = palloc(82,'TEMP2',numnp, 1)

        call tienod(mr(np(33)),hr(np(43)),mr(np(48)),mr(np(81)),
     &              mr(np(82)),mr(np(78)),ndm,nen,nen1,
     &              numnp,numel,l1,l2,l3,l4,j,td(2))

        setvar = palloc(82,'TEMP2',0, 1)
        setvar = palloc(81,'TEMP1',0, 1)
      endif
      setvar = palloc(81,'TEMP1',numnp, 1)
      call poutie(mr(np(81)),mr(np(33)),mr(np(49)),nen,nen1,
     &            numnp,numel,prt)
      setvar = palloc(81,'TEMP1',0, 1)

      tfl = .true.
      go to 1

c     Error treatments

900   call  errclr ('PCONTR')
      call pdelfl()
      return

910   if(ior.eq.icf) then
        call pincld('end')
        incf = .false.
        intx = evint
        cprt = evint
        go to 1
      endif

911   call  endclr ('PCONTR',titl)
      call pdelfl()
      return

c     Input formats

1000  format(a)
1001  format(2(a4,11x),3f15.0)
1002  format(a4,11x,a)
1003  format(a4,11x,3i15)
1004  format(a4,2x,a12,i8,2l5)
1005  format(4f20.0)

c     Output formats

2000  format(1x,19a4,a3//5x,'Solution date: ',a//14x,a/14x,a/
     &                /5x,'Input Data Filename: ',a40/
     &                /5x,'Number of Nodal Points  - - - - - - :',i9
     &                /5x,'Number of Elements  - - - - - - - - :',i9/
     &                /5x,'Spatial Dimension of Mesh - - - - - :',i9
     &                /5x,'Degrees-of-Freedom/Node (Maximum) - :',i9
     &                /5x,'Number Element Nodes    (Maximum) - :',i9/
     &                /5x,'Number of Material Sets - - - - - - :',i9
     &                /5x,'Number Parameters/Set   (Program) - :',i9
     &                /5x,'Number Parameters/Set   (Users  ) - :',i9)


2004  format(/' *End of <FEAPpv> solution,  File: ',a/1x)
2005  format(/' *End of INCLUDE solution, File: ',a/1x)
2008  format(/' ',a/)
2009  format(/1x,'Continue with interactive input options for control?',
     &          '  <y or n> :',$)
2010  format(1x,'Specify command (INTEractive, INCLude, etc.)'/' > ',$)
2011  format(/5x,'Tie nodes from',i8,' to ',i8/1x)
2012  format(/5x,'Tie from region',i4,' to region',i4/1x)
2013  format(/5x,'Tie from material',i4,' to material',i4/1x)
2014  format(/5x,'Tie: direction =',i3,' X =',1p,1e12.5/1x)
2015  format(/5x,'Tie all nodes with common coordinates'/1x)

c     Error Messages

3001  format(/' *ERROR* Attempt to solve problem before mesh input.'/
     &        '         Check for error on FEAPpv record.'/1x)
3002  format(/' *ERROR* Can not do BATCH execution from this mode.'/
     &        '         Do INTERACTIVE or put in INCLUDE file.'/1x)

      end

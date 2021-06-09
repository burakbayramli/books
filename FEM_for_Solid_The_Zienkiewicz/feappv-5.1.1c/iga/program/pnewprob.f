!$Id:$
      subroutine pnewprob(isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Start a new problem

!      Note:    Statements in this routine were removed from pcontr.f
!               to permit better control on starting new problems.

!      Inputs:
!        isw    -  Switch control on actions

!      Outputs:
!        Problem control parameters through common blocks.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'allotd.h'
      include   'allotn.h'
      include   'augdat.h'
      include   'bdata.h'
      include   'cblend.h'
      include   'cdata.h'
      include   'cdat1.h'
      include   'cnurb.h'
      include   'comfil.h'
      include   'compac.h'
      include   'comsav.h'
      include   'contrl.h'
      include   'cornum.h'
      include   'corset.h'
      include   'ddata.h'
      include   'dstars.h'
      include   'edgdat.h'
      include   'elpers.h'
      include   'errchk.h'
      include   'gltran.h'
      include   'idata1.h'
      include   'idptr.h'
      include   'iodata.h'
      include   'iofile.h'
      include   'ioincl.h'
      include   'iosave.h'
      include   'mdata.h'
      include   'mxsiz.h'
      include   'nblend.h'
      include   'pdata5.h'
      include   'pdata6.h'
      include   'pfeapb.h'
      include   'pglob1.h'
      include   'pointer.h'
      include   'pmod2d.h'
      include   'print.h'
      include   'qudshp.h'
      include   'refng.h'
      include   'sdata.h'
      include   'setups.h'
      include   'umac1.h'
      include   'vdata.h'
      include   'comblk.h'

      character  fileck*128
      logical    errs,oprt,setvar,palloc,tinput,vinput,pcomp
      logical    contrfl,lopen
      character  cdate*24, ctext*15
      integer    isw,iii, i,j, l1,l2,l3,l4,l5,l6
      real*8     td(12)

      save

!     Jump to outputs

      if(isw.eq.2) then
        call fdate( cdate )
        go to 200
      endif

!     Close any open multiple problem file before starting new problem

      if(prob_on) then
        call pendprob

!       Remove any existing files from last problem

        call pdelfl()

!       Delete memory use

        do i = ndict,1,-1
          setvar = palloc(dlist(i),dict(i),0,iprec(i))
        end do ! i

      endif

!     Start Problem: Read and print control information

      bflg   = .false.
      gapfl  = .false.
      hisfl  = .true.
      gmvfl  = .false.
      incf   = .false.
      intr   = .false.
      intx   = .false.
      nurbfl = .false.
      vemfl  = .false.
      blockfl= .false.   ! Set for NPATCch refinements
      cdate  = '  '
      call fdate( cdate )
      ctext   = 'start'
      contrfl = .true.
      do while(.not.pcomp(ctext,'    ',4))
        errck  = tinput(ctext,1,td(2),8)
        if(    pcomp(ctext,'node',4) .or. pcomp(ctext,'numnp',5)) then
          numnp = nint(td(2))
          contrfl = .false.
        elseif(pcomp(ctext,'elem',4) .or. pcomp(ctext,'numel',5)) then
          numel = nint(td(2))
          contrfl = .false.
        elseif(pcomp(ctext,'mate',4) .or. pcomp(ctext,'numma',5)) then
          nummat = nint(td(2))
          contrfl = .false.
        elseif(pcomp(ctext,'dime',4) .or. pcomp(ctext,'ndm',3)) then
          ndm = nint(td(2))
          contrfl = .false.
        elseif(pcomp(ctext,'dofs',4) .or. pcomp(ctext,'ndf',3)) then
          ndf = nint(td(2))
          contrfl = .false.
        elseif(pcomp(ctext,'elno',4) .or. pcomp(ctext,'nen',3)) then
          nen = nint(td(2))
          contrfl = .false.
        elseif(pcomp(ctext,'add',3)  .or. pcomp(ctext,'nad',3)) then
          nad = nint(td(2))
          contrfl = .false.
        elseif(pcomp(ctext,'prop',4)  .or. pcomp(ctext,'npd',3)) then
          npd = nint(td(2))
          contrfl = .false.
        elseif(pcomp(ctext,'upro',4)  .or. pcomp(ctext,'nud',3)) then
          nud = nint(td(2))
          contrfl = .false.
        elseif(contrfl) then
          errck = vinput(ctext,15,td(1),1)
          if(nint(td(1)).ge.0) then
            numnp  = nint(td(1))
            numel  = nint(td(2))
            nummat = nint(td(3))
            ndm    = nint(td(4))
            ndf    = nint(td(5))
            nen    = nint(td(6))
            nad    = nint(td(7))
            npd    = nint(td(8))
            nud    = nint(td(9))
            go to 101
          endif
        endif
      end do ! while
101   nnn    = 0

!     Adjust storage for material parameters

      npd    = max(npd,300)
      nud    = max(nud,150)
      ndd    = npd + nud + 1

!     Star node/element initialization

      starnd = 0
      starel = 0

!     Blending function initialization

      numsn  = 0
      numsd  = 0
      numbd  = 0

!     NURB blending function initialization

      nurma  = 0
      nursn  = 0
      nursd  = 0
      nurbd  = 0

!     NURB subdivision interval for element plots

      npl_int = 2

!     Contact array initialization

      numcels = 0
      optflg  = .false.
      optmsh  = .false.

!     Serial & parallel solution by unblocked equations

      pfeap_blk  = .false.
      pfeap_glob = .false.

!     Set filenames for multiple problem case

      if(irdef.ne.ior) then

        inquire(unit=ior,name=fnamp,exist=errs)

        prob_on = .false.
        if(errs) then

!         Set multiple problem flag

          prob_on = .true.

!         Save master output file name and unit number

          i = index(flog,' ')
          if(nprob.eq.0) then
            if(isw.gt.0) write(iow,2017) flog(1:i-1)
            iow_sav  = iow
            fout_sav = fout
          endif

!         Extract file name

          i = index(fnamp,' ')
          if(i.eq.0) i = 128
          do j = i,1,-1
            if(pcomp(fnamp(j:j),char(47),1) .or.       ! char(47) = '/'
     &         pcomp(fnamp(j:j),char(92),1)) go to 110 ! char(92) = '\'
          end do ! j
          j = 0
110       fnamr = fnamp(j+1:j+21)

!         Set new plot file name

          fnamr(1:1) = 'P'
          fplt(1:128) = ' '
          fplt(1: 17) = fnamr
          i = index(fplt,'.')
          if(i.gt.0) then
            fplt(i: 21) = ' '
          endif
          i = min(index(fplt,' '), 16)
          if(i.eq.0) then
            i = 16
          endif

!         Increment problem counter or delete output file

          if(keepfl) then
            nprob = nprob + 1
          else
            close(unit = iow, status = 'delete')
            keepfl = .true.
            nprob  = max(1,nprob)
          endif

!         Add problem counter to name

          write(fplt(i:i+2),'(a)') '000'
          if(nprob.lt.10) then
            write(fplt(i+2:i+2),'(i1)') nprob
          elseif(nprob.lt.100) then
            write(fplt(i+1:i+2),'(i2)') nprob
          elseif(nprob.lt.1001) then
            write(fplt(  i:i+2),'(i3)') nprob
          else
            write(*,*) 'Exceeded limit of multiple files (PCONTR)'
          endif

!         Set file names for new problem

          if(isw.gt.0) then
            iow  = 8
            fout = fplt
            fout(1:1) = 'O'
            fres = fplt
            fres(1:1) = 'R'
            fsav = fplt
            fsav(1:1) = 'S'

!           Create clean output file

            inquire(file=fout,exist=initf)
            if(initf) then
              open (unit=iow,file=fout,status='old')
              close(unit=iow,          status='delete')
            endif
            open(unit=iow,file=fout,status='new')
            if(nprob.gt.1) write(iow,2019)
            write(iow,2020) nprob,fout
            inquire(unit=iow_sav,opened=lopen)
            if(lopen) write(iow_sav,2021) nprob
          endif

!       Error in file structure

        else
          write(  *,3003)
          write(iow,3003)
          call plstop(.true.)
        endif

!     Single problem solution

      else
        prob_on = .false.
      endif

!     Zero pointer array

      setvar = palloc( 0 ,'START', 0 , 0 )

!     Zero number of dictionary entries

      ndict     = 0

!     If number of nodes, or elements is zero compute number from data

      if(nocount) then
        oprt        =  prt
        prt         = .false.
        ucount      = .true.
        nurnp       = 0
        call pnums()
        irecrd(isf) =  2
        prt         =  oprt
        ucount      = .false.

!       Star node/element re-initialization

        starnd = 0
        starel = 0
      endif

!     Output problem size data

200   write(iow,2000) head,cdate,versn,fincld(isf),
     &               numnp,numel, ndm,ndf,nad,nen, nummat,npd,nud

!     Check that problem has nodes elements, etc.

      if(min(numnp,numel,nummat, ndm,ndf,nen).eq.0) then
        call plstop(.true.)
      endif

!     Initialize clock

      call stime()

!     Set parameters for page eject and rotation dof

      o   = '    '
      errck = .false.
      lsave = .false.
      lkflg = .false.
      initf = .true.
      cxifl = .false.
      eanfl = .false.
      ebcfl = .false.
      ebsfl = .false.
      curfl = .false.
      edifl = .false.
      efcfl = .false.
      eprfl = .false.
      espfl = .false.
      finflg= .false.
      surfl = .false.
      boufl = .false.
      cprfl = .false.
      disfl = .false.
      forfl = .false.
      angfl = .false.
      reafl = .false.
      intfl = .false.
      tiefl = .true.
      tief  = .false.
      stifl = .false.

!     Rotation parameters

      do i = 1,50
        ia(1,i)  = 1
        ia(2,i)  = 2
        ir(1,i)  = 0
        ir(2,i)  = 0
        ea(1,i)  = 1
        ea(2,i)  = 2
        er(1,i)  = 0
        er(2,i)  = 0
        inord(i) = 0
        exord(i) = 0
        do j = 1,30
          ipord(j,i) = 0
          epord(j,i) = 0
        end do ! j
      end do ! i
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
      ncurv  = 0
      nespi  = 0

!     Zero global parameters

      if(    ndm.le.2) then
        g2type = 2           ! default plane strain
      elseif(ndm.eq.3) then
        g2type = 7           ! default 3-d
      else
        g2type = 9           ! unspecified
      endif
      gdtype = 1
      gtdof  = 0
      gref   = 0
      do i = 1,3
        grefx(i)  = 0.0d0
        gtref(i)  = 0.0d0
      end do ! i
      do i = 1,2
        gray(i) = 0.0d0
      end do ! i
      do i = 1,14
        gfac(i) = 0.0d0
      end do ! i
      augf   =  1.0d0        ! Augmenting factor multiplier

!     Set pointers for allocation of mesh arrays

      nen1      = nen + 11  ! For NURBS element dimensioning
      nie       = 13 ! 1,2 defined; others are nie, nie-1, etc.
      nst       = max(nen*ndf + nad,1)
      nneq      = ndf*numnp

!     Allocate size for arrays for mesh and solution vecors

      l1   = ndm*numnp
      l2   = max(ndf*numnp,1)
      l3   = max(nen+1,7*nst,21)
      l4   = numnp*max(ndf,ndm)
      l5   = ndf*nen
      l6   = max(1,numel)

!     Allocate and zero arrays

      setvar = palloc( 26,'DR   ',l4          ,  2)
      setvar = palloc( 34,'LD   ',l3          ,  1)
      setvar = palloc( 35,'P    ',nst*3       ,  2)
      setvar = palloc( 36,'S    ',nst*nst*2   ,  2)
      setvar = palloc( 39,'TL   ',nen         ,  2)
      setvar = palloc( 41,'UL   ',nst*14      ,  2)
      setvar = palloc( 44,'XL   ',max(4,nen)*3,  2)
      setvar = palloc( 25,'D    ',nummat*ndd  ,  2)
      setvar = palloc( 32,'IE   ',nummat*nie  ,  1)
      setvar = palloc(240,'IEDOF',nummat*l5   ,  1)
      setvar = palloc( 31,'ID   ',l2*2        ,  1)
      setvar = palloc( 33,'IX   ',nen1*l6     ,  1)
      setvar = palloc(190,'NDTYP',numnp       ,  1)
      setvar = palloc(100,'RIXT ',numnp       ,  1)
      setvar = palloc(181,'RBEN ',l6          ,  1)
      setvar = palloc( 43,'X    ',l1          ,  2)
      setvar = palloc( 45,'ANG  ',numnp       ,  2)
      setvar = palloc( 46,'ANGL ',nen         ,  2)
      setvar = palloc( 27,'F    ',2*l2        ,  2)
      setvar = palloc( 28,'F0   ',4*l2        ,  2)
      setvar = palloc( 29,'FPRO ',2*l2        ,  1)
      setvar = palloc( 30,'FTN  ',4*l2        ,  2)
      setvar = palloc( 38,'T    ',numnp       ,  2)
      setvar = palloc( 40,'U    ',4*l2        ,  2)
      setvar = palloc( 89,'NREN ',numnp*2     ,  1)

!     Set ID address pointers

      id31    = np(31)
      idpt(1) = np(31)

!     Set pointers

      npid    = np(31)         ! ID
      npix    = np(33)         ! IX
      npuu    = np(40)         ! U
      npxx    = np(43)         ! X
      nprn    = np(89)         ! NREN
      npty    = np(190)        ! NDTYP

!     Set initial numbering in renumber vector and mark nodes as unused.

      do i = 0,numnp-1
        mr(np( 89)+i      ) = i+1  ! Remap list
        mr(np( 89)+i+numnp) = i+1  ! Reverse list
        mr(np(190)+i      ) = 0
      end do ! i

!     Mark all elements as unused

      do i = 1,numel
        mr(np(33)+nen1*i-1) = -99
      end do ! i

!     Set element assembly array

      sa(1) = 0
      do j = 2,nen
        sa(j) = sa(j-1) + ndf
      end do ! j
      la = ndf*nen   ! Location of element equations in element array
      ga = la + nad  ! Location of global  equations in element array

!     Open file to store material data

      inquire(unit=iwd,name=fileck, opened=errs)

!     Input a mesh from binary file (if it exists)

      iii   =  0

!     Input mesh data from file

      call pmesh(iii,prt,prth)

!     Set edge boundary codes, forces, displacements, and angles

      if(eanfl.or.ebcfl.or.edifl.or.efcfl.or.eprfl) then
        call pedgin()
      endif

!     Set cordinate angles, boundary codes, forces, displacements,
!         proportional load types and surface loads

      if(boufl .or. surfl .or. angfl .or.
     &   disfl .or. cprfl .or. forfl) then
        call ploadc()
      endif

!     Perform simple check on mesh to ensure basic data was input

      setvar = palloc(111,'TEMP1',numnp*ndf, 1)
      call meshck(mr(np(111)),mr(np(32)),mr(np(240)),mr(np(31)+nneq),
     &            mr(np(190)),mr(np(33)),nie,nen,nen1,ndf,
     &            numnp,numel,nummat,errs)
      setvar = palloc(111,'TEMP1',0, 1)
      if(errs) then
        call plstop(.true.)
      endif

!     Compute boundary nodes (before ties)

      if(tiefl) then
        setvar = palloc( 78,'EXTND',numnp ,1)
        call pextnd()
        tiefl  = .false.
      endif

      tfl = .true.

!     Input/output formats

2000  format(1x,19a4,a3//5x,'Solution date: ',a//14x,a/14x,a/
     &                /5x,'Input Data Filename: ',a/
     &                /5x,'Number of Nodal Points  - - - - - - :',i9
     &                /5x,'Number of Elements  - - - - - - - - :',i9/
     &                /5x,'Spatial Dimension of Mesh - - - - - :',i9
     &                /5x,'Degrees-of-Freedom/Node (Maximum) - :',i9
     &                /5x,'Equations/Element       (Maximum) - :',i9
     &                /5x,'Number Element Nodes    (Maximum) - :',i9/
     &                /5x,'Number of Material Sets - - - - - - :',i9
     &                /5x,'Number Parameters/Set   (Program) - :',i9
     &                /5x,'Number Parameters/Set   (Users  ) - :',i9)

2017  format(/'  Problem definitions are specified by include files.'
     &      //'  Output for each problem is written to separate files.'
     &      //'  Check file ',a,' for problem list and errors.')

2019  format(/'  ',70('-'))

2020  format(/'  --> Problem',i4,': Output in file: ',a)

2021  format(/'  --> End Problem',i4)

3003  format(/' *ERROR* PCONTR: File name error')

      end

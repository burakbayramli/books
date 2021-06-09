!$Id:$
      subroutine pcontr()

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Control program for FEAPpv problem input and solution.

!      Inputs:
!        none

!      Outputs:
!        none
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'allotd.h'
      include  'bdata.h'
      include  'cblend.h'
      include  'cdata.h'
      include  'cdat1.h'
      include  'chdata.h'
      include  'codat.h'
      include  'contrl.h'
      include  'corset.h'
      include  'cornum.h'
      include  'comfil.h'
      include  'compac.h'
      include  'conval.h'
      include  'crotas.h'
      include  'debugs.h'
      include  'dstars.h'
      include  'edgdat.h'
      include  'elname.h'
      include  'errchk.h'
      include  'hdatam.h'
      include  'hlpdat.h'
      include  'iodata.h'
      include  'iofile.h'
      include  'ioincl.h'
      include  'iosave.h'
      include  'linka.h'
      include  'lmdata.h'
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
      include  'qudshp.h'
      include  'refng.h'
      include  'region.h'
      include  'sdata.h'
      include  'umac1.h'
      include  'vdata.h'
      include  'comblk.h'

      character (len=80) :: titl
      character (len=15) :: dnam, usub, tx(8)
      character (len=4)  :: fext, uset(12), vtype

      logical       :: errs,setvar,palloc,tinput,pcomp,evint,lp_in
      logical       :: cprt,oprt,oprth,mulprob,newprob,usetfl(12)
      logical       :: cinput, pinput, setlagf
      integer       :: i, iorsv, j,jj, l1,l2,l3,l4
      integer       :: usetno(12), itd(1)
      real (kind=8) :: td(12)

      save

!     Default names for manipulation sets

      data      uset / 'man1', 'man2', 'man3', 'man4', 'man5' , 'man6',
     &                 'man7', 'man8', 'man9', 'ma10', 'ma11' , 'ma12'/

!     Destroy old output file if it exists
      inquire(file=fout,exist=initf)
      if(initf) then
        open (unit=iow,file=fout,status='old')
        close(unit=iow,          status='delete')
      endif

!     Open files for input and output
      open(unit=ior,file=finp,status='old')
      open(unit=iow,file=fout,status='new')

!     Initial values for include options
      chflg   = .false.
      cprt    = .true.
      everon  = .false.
      evint   = .false.
      hdcpy   = .false.
      incf    = .false.
      intr    = .false.
      intx    = .false.
      keepfl  = .true.
      lagrfl  = .false.
      lp_in   = .true.
      newprob = .false.
      mulprob = .false.
      nocount = .true.
      pltmfl  = .false.
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
      ndfl(:) = 0

!     Set default to print headers
      prth   = .true.

!     Flags for user manipulation commands
      do j = 1,12
        usetfl(j) = .false.
        usetno(j) = 0
      end do ! j

!     Install user functions

!     Set user element names
      td(1)  = 0.0d0
      itd(1) = 0
      do j = 1,15
        utx(1) = 'user'
        jj  = j
        call elmlib(td(1),td(1),td(1),itd(1),td(1),td(1),td(1),
     &              1,1,1,jj,-1)  ! Can assign a name for element
        umatn(j) = utx(1)
      end do ! j

!     Set user mesh input names
      do j = 1,12
        if(j.lt.10) then
          write(usub,'(a3,i1)') 'mes',j
        else
          write(usub,'(a2,i2)') 'me',j
        endif
        uct = usub(1:4)
        call umshlib(j,tx,prt)
        umshc(j) = uct
      end do ! j

!     Set user macro input names
      do j = 1,12
        if(j.lt.10) then
          write(usub,'(a3,i1)') 'mac',j
        else
          write(usub,'(a2,i2)') 'ma',j
        endif
        uct   = usub(1:4)
        fnamp = ' '
        call umaclib(j,fnamp,td)
        umacc(j) = uct
      end do ! j

!     Set umati model names
      do j = 1,5
        write(usub,'(a3,i1)') 'mat',j
        uct = 'mate'
        call uconst(usub,td,td,td,l1,l2,l3)
      end do ! j
      uct       = 'mate'
      usub(1:4) = 'mat0'
      call uconst(usub,td,td,td,l1,l2,l3)

!     Set uplot input names
      do j = 1,5
        write(usub,'(a3,i1)') 'plt',j
        uct = usub(1:4)
        call upltlib(j,td)
        upltc(j) = uct
      end do ! j

!     Set umanipulation names
      do j = 1,12
        uct     = uset(j)
        call usetlib(j)
        uset(j) = uct
      end do ! j

!     Input with interactive interactive statements
1     if(intx) then
        if(cprt) then
          write(*,2009)
          ior = -abs(ior)
        endif
        errck = tinput(dnam,1,td,0)

!       Read command interactively
        if(pcomp(dnam,'y',1)) then
          write(*,2010)
!         read (*,1000,err=900,end=910) yyy
          if(.not.cinput()) then
            goto 910
          end if
          yyy = record
          cprt = .true.

!       Read command from current file and turn off intx flag
        else
          cprt  = .false.
          intr  = .false.
          intx  = .false.
          ior   =  abs(ior)
          read(ior,1000,err=900,end=910) yyy
        endif

!     Input from current file
      else
        ior = abs(ior)
        read(ior,1000,err=900,end=910) yyy
      endif

!     Compare with command list
      call pstrip(xxx,yyy,1)
      l1   = len(xxx)
      titl = xxx(1:l1)

!     Start solution of new problem
      if(pcomp(titl(1:4),'feap',4)) then
        go to 100

!     Set count/nocount mode
      elseif(pcomp(titl(1:4),'noco',4)) then
        nocount = .false.

      elseif(pcomp(titl(1:4),'coun',4)) then
        nocount = .true.

!     Set keep/nokeep flags for output file retension
      elseif(pcomp(titl(1:4),'keep',4)) then
        keepfl = .true.

      elseif(pcomp(titl(1:4),'noke',4)) then
        keepfl = .false.

!     User command sets
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

!     User problem selection
      elseif(pcomp(titl(1:5),'ufeap',5)      .or.
     &       pcomp(titl(1:7),'fe2feap',7) ) then

        if(titl(1:1).eq.'u') then
          l3 = 1
        else
          l3 = 0
        endif

        newprob = .true.
        do i = 1,20
          l2 = 4*i + l3
          l1 = l2 - 3
          head(i) = titl(l1:l2)
        end do ! i
        call uprob

!     Perform inputs from an include file
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

!     Perform inputs for initial conditions
      elseif(pcomp(titl(1:4),'init',4)) then
        call acheck(titl,yyy,15,80,80)
        read(yyy,1001,err=900,end=900) titl(1:4),dnam(1:4)
        call pinitl(dnam,errs)
        if(errs) return

!     Solution mode
      elseif(pcomp(titl(1:4),'inte',4)) then
        ior   = -abs(ior)
        evint = .true.
        intr  = .true.
        intx  = .true.
        cprt  = .true.
        call pltcur()
        go to 400

      elseif(pcomp(titl(1:4),'batc',4)) then
        evint = .false.
        cprt  = .false.
        intr  = .false.
        intx  = .false.
        go to 400

!     Manual level set: 0 = basic; 1 = advanced; 2 = expert
      elseif(pcomp(titl(1:4),'manu',4)) then
        call acheck(titl,yyy,15,80,80)
        read(yyy,1003,err=900,end=911) titl(1:4),hlplev
        hlplev = max(-1,min(3,hlplev))

!     Mesh manipulations: Link and tie

!     Reset id list to link dof's on different nodes - set by node #
      elseif(pcomp(titl(1:4),'link',4)) then
        call plinka('lnk ','set')
        lkflg = .true.

      elseif(pcomp(titl(1:4),'tie' ,3)) then
        go to 500

!     Parameter sets
      elseif(pcomp(titl(1:4),'para',4) .or.
     &       pcomp(titl(1:4),'cons',4)) then
        coflg = .true.
        call pconst(prt)

!     Loop start
      elseif(pcomp(titl(1:4),'loop',4)) then
        call acheck(titl,yyy,15,80,80)
        read(yyy,1002,err=900,end=911) titl(1:4),dnam
        call ploops(lp_in,dnam,1)

!     Loop end
      elseif(pcomp(titl(1:4),'next',4)) then
        call acheck(titl,yyy,15,80,80)
        read(yyy,1002,err=900,end=911) titl(1:4),dnam
        call ploops(lp_in,dnam,2)

!     Set list of Lagrange multiplier nodal variables
      elseif(pcomp(titl(1:4),'lagr',4)) then
        do i = 1,ndf,16
          errck = pinput(td(1),min(16,ndf-i+1))
          jj = 0
          do j = i,min(i+15,ndf)
            jj           = jj + 1
            ndfl(i) = nint(td(jj))
          end do ! j
        end do ! i
        write(iow,2024) (i,ndfl(i),i=1,ndf)
        lagrfl = .true.

!     Remarks to output file
      elseif(pcomp(titl(1:4),'rema',4)) then
        write(*,2008) titl(1:78)

!     Stop execution
      elseif(pcomp(titl(1:4),'stop',4)) then
        if(evint) write(*,2004) fout
        if(abs(ior).eq.irdef) then
          call pdelfl()
          call plstop(.false.)
        endif

      endif

!     Read again
      go to 1

!     Start Problem: Read and print control information

100   newprob = .true.
      do i = 1,20
        l2      = 4*i
        l1      = l2 - 3
        head(i) = titl(l1:l2)
      end do
      call pnewprob(1)
      go to 1

!     [mani] - Perform user manipulation commands

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

!     Establish profile of resulting equations for stiffness, mass, etc
!     [batc]h execution

400   if(.not.newprob) then
        write(*,3001)
        call plstop(.true.)
      elseif(intx .and. .not.intr .and. .not.incf) then
        write(*,3002)
        go to 1
      endif

      if(tfl) then

!       If ties have occurred merge boundary conditions, forces & contact
        if(tief) then
          call tiefor(mr(np(31)+nneq),hr(np(27)),mr(np(79)),ndf,numnp)
        endif

!       Compute boundary nodes (after ties)
        call pextnd()

!       Allocate memory to store all possible equations

!       Nodal equations
        neq = numnp*ndf
!       Element equations
        if(setlagf(mr(np(33)),mr(np(32)))) then
          neq = neq + ndl*numel
        endif
        setvar = palloc( 21, 'JP1  ', neq, 1)

!       Set user commands
        do j = 1,12
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

!               Read data from file

                iorsv = ior
                ior   = ios

                do l1 = 0,36
                  do l2 = 1,26
                    vvsave(l2,l1) = vvv(l2,l1)
                  end do
                end do
                oprt  = prt
                oprth = prth

                read(ior,1004) vtype,fincld(isf),irecrd(isf),prt,prth
                read(ior,1005) vvv

                call usetlib(j)

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

!       Determine current profile

        do j = 0,nneq-1
          mr(np(31)+j) = mr(np(31)+j+nneq)
        end do

        mxpro = 0
        mxneq = 0

!       Set current profile

        if(ior.lt.0) write(*,*) ' '
        call profil(mr(np(21)),mr(np(34)),mr(np(31)),
     &              mr(np(33)),1,prt)
        call profil(mr(np(21)),mr(np(34)),mr(np(31)),
     &              mr(np(33)),2,prt)
        mxpro = max(mxpro,(mr(np(21)+neq-1)))
        mxneq = max(mxneq,neq)

!       Set up stress history addresses

        call sethis(mr(np(32)),mr(np(33)),nie,nen,nen1,numel,nummat,prt)

        tfl = .false.

      endif

!     Macro module for establishing solution algorithm

      call pmacr(initf)
      go to 1

!     Tie nodes within tolerance of one another
!     [tie ] - merge regions with common coordinates

500   call acheck(titl,yyy,15,80,80)
      read(yyy,1001,err=900,end=911) titl(1:4),titl(16:19),(td(j),j=1,3)

!     Retrieve current boundary connection status

      if(.not.tief) then
        setvar = palloc( 79,'IPOS ',numnp,  1)
        call pseqn(mr(np(79)),numnp)
        tief = .true.
      endif

!     Tie line elements to regions

      if(pcomp(titl(16:19),'line',4)) then
        l2 = max(    1,min(nummat,int(td(1))))
        call ptiend(mr(np(32)),mr(np(33)),mr(np(78)),mr(np(79)),
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
          j  = nint(td(1))
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
        setvar = palloc(111,'TEMP1',numnp, 1)
        setvar = palloc(112,'TEMP2',numnp, 1)

        call tienod(mr(np(33)),hr(np(43)),mr(np(79)),mr(np(111)),
     &              mr(np(112)),mr(np(78)),ndm,nen,nen1,
     &              numnp,numel,l1,l2,l3,l4,j,td(2))

        setvar = palloc(112,'TEMP2',0, 1)
        setvar = palloc(111,'TEMP1',0, 1)
      endif
      setvar = palloc(111,'TEMP1',numnp, 1)
      call poutie(mr(np(111)),mr(np(33)),mr(np(190)),nen,nen1,
     &            numnp,numel,prt)
      setvar = palloc(111,'TEMP1',0, 1)

      tfl = .true.
      go to 1

!     Error treatments

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

!     Input formats

1000  format(a)
1001  format(2(a4,11x),3f15.0)
1002  format(a4,11x,a)
1003  format(a4,11x,3i15)
1004  format(a4,2x,a12,i8,2l5)
1005  format(4f20.0)

!     Output formats

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

2024  format(/'   N o d a l   L a g r a n g e   M u l t p l i e r'/
     &        '      ndf  Multiplier'/'             Number'/(i8,i12))

!     Error Messages

3001  format(/' *ERROR* Attempt to solve problem before mesh input.'/
     &        '         Check for error on FEAPpv record.'/1x)
3002  format(/' *ERROR* Can not do BATCH execution from this mode.'/
     &        '         Do INTERACTIVE or put in INCLUDE file.'/1x)

      end subroutine pcontr

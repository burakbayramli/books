c  *********************************************************************
c  *                                                                   *
c  *          vortex lattice aerodynamic computation                   *
c  *            Nasa-LaRC program no. a2794                            *
c  *                                                                   *
c  *          see NASA TN D-7921 by j.e. lamar and b.b. gloss          *
c  *                                                                   *
c  *  this program modified by p.r. keidel for w.h. mason              *
c  *  on october 22, 1989, to be used on a personal computer.          *
c  *  note:  there are ten subroutines to be linked to this program    *
c  *  Curtis Mitchel modified for WATFOR and 80 col output: Spr. 92    *
c  *  Bob Narducci modified to include center of gravity and fix bugs  *
c  *  for the AOE 4114 class, Spring 1993                              *
c  *  Valery Razgonyaev fixed more bugs, Spring 1994                   *
c  *  
C     Andy Ko added directory change section 11/14/02                  *
c  *  Last mod: April 2, 1994                                          *
c  *  Andy Ko last mod: 11/14/02
c  *                                                                   *
c  *********************************************************************
c
      USE DFPORT
      common /all/ bot,m,beta,ptest,qtest,tblscw(50),q(200),pn(200),
     1             pv(200),alp(200),s(200),psi(200),phi(50),zh(50)
      common /tothre/ cir(200,2)
      common /threfor/ ccav(2,50),clt,clnt,nssw,alpd
      common /onethre/ twist(2),cref,sref,cave,cldes,strue,ar,artrue,
     1                 rtcdht(2),config,nsswsv(2),msv(2),kbot,plan,
     2                 iplan,mach,sswwa(50),xl(2),xt(2),
     3                 clwb,cmcl,cla(2),blair(50),clamar(2)
      common /mainone/ icodeo,total,aan(2),xs(2),ys(2),kfcts(2),
     1                 xreg(25,2),yreg(25,2),areg(25,2),dih(25,2),
     2                 mcd(25,2),xx(25,2),yy(25,2),as(25,2),
     3                 ttwd(25,2),mmcd(25,2),an(2),zz(25,2),itpcod,
     3                 xrego(25,2),cg
      common /ccrrdd/  tspan,tspana,kbit
      real mach
      character*15 infile, outfil
      character*72 newdir

C  Title screen - Andy Ko 3/14/01

      WRITE (*,*) '****************************************************'
	WRITE (*,*) '     VT Aerospace Aircraft Design Software Series'
	WRITE (*,*) '****************************************************'
	WRITE (*,*)
      WRITE (*,*) 'VLMpc v2'
      WRITE (*,*)
	WRITE (*,*)
      WRITE (*,*) '      Vortex Lattice Aerodynamic Computation        '
      WRITE (*,*) '           Nasa-LaRC program no. a2794              '
      WRITE (*,*)
      WRITE (*,*) '    NASA TN D-7921 by J.E. Lamar and B.B. Gloss     '
      WRITE (*,*)
      WRITE (*,*) 'this program modified by P.R. Keidel for W.H. Mason '
      WRITE (*,*) 'on october 22, 1989, to be used on a personal       '
	WRITE (*,*) 'computer.                                           '
      WRITE (*,*) '- Curtis Mitchel modified for WATFOR and 80 col 
     &output'
      WRITE (*,*) '- Bob Narducci modified to include center of gravity'
	WRITE (*,*) '  and fix bugs for the AOE 4114 class, Spring 1993  '
      WRITE (*,*) '- Valery Razgonyaev fixed more bugs, Spring 1994    '
      WRITE (*,*) '- Andy Ko added directory change section 11/14/02   '
      WRITE (*,*)
      WRITE (*,*) 'Department of Aerospace and Ocean Engineering'
      WRITE (*,*) 'Virginia Tech, Blacksburg, VA 24061'
      WRITE (*,*) 'whmason@vt.edu'
      WRITE (*,*) 
	WRITE (*,*) '****************************************************'
      WRITE (*,*)

C  Set the input/output directory. Default is C:\ - Andy Ko 11/14/02
      
      istatus = CHDIR('C:\')
	IF (istatus .eq. enoent) THEN
	     WRITE(*,*) 'Cannot change directory to C:\'
	     WRITE(*,*) 'Please log into the network'
	     Pause 'Press the ENTER key to exit'
	     STOP
      ENDIF

      WRITE (*,*) 'Default directory is C:\'
	WRITE (*,*) 'Please enter a new directory name. '
	WRITE (*,*) 'Type "default" for the default directory' 
	READ (*,*) newdir
	IF (newdir .eq. 'default') THEN
	   WRITE (*,*) 'Default directory selected'
      ELSE
	   istatus = CHDIR(newdir)
         IF (istatus .eq. enoent) THEN
            WRITE(*,*) 'The directory does not exist'
	      WRITE(*,*) 'The directory is set to the default'
         ELSEIF (istatus .eq. enotdir) THEN
            WRITE(*,*) 'Directory could not be changed'
	      WRITE(*,*) 'The directory is set to the default'
         ELSE
	      WRITE(*,*) 'directory has been changed to ',newdir
         ENDIF
      ENDIF

      write(*,901) 'enter name of data set: '
      read (*,'(a)') infile
      write(*,901) 'enter name of output file: '
      read (*,'(a)') outfil
  901 format(1x,a,$)

      write(*,910)
  910 format(/3x,'all output is routed to disk file'/
     1        3x,'computing may take quite some time'/)

      open(5, file = infile, status = 'old')
c      open(6, file = outfil, status = 'new')
c     Added this so that outfile can be overwritten - Andy Ko 11/14/02
      open(6, file = outfil) 
      open(10,status='scratch',form='unformatted')
c
c     initialize all variables
c
c     Added below to the output file - Andy Ko 11/14/02
      WRITE (6,*) '****************************************************'
	WRITE (6,*) '     VT Aerospace Aircraft Design Software Series'
	WRITE (6,*) '****************************************************'
	WRITE (6,*)
      WRITE (6,*) 'VLMpc v2'
      WRITE (6,*)
	WRITE (6,*)
      WRITE (6,*) '      Vortex Lattice Aerodynamic Computation        '
      WRITE (6,*) '           Nasa-LaRC program no. a2794              '
      WRITE (6,*)
      WRITE (6,*) '    NASA TN D-7921 by J.E. Lamar and B.B. Gloss     '
      WRITE (6,*)
      WRITE (6,*) 'This program modified by P.R. Keidel for W.H. Mason '
      WRITE (6,*) 'on october 22, 1989, to be used on a personal       '
	WRITE (6,*) 'computer.                                           '
      WRITE (6,*) '- Curtis Mitchel modified for WATFOR and 80 col 
     &output'
      WRITE (6,*) '- Bob Narducci modified to include center of gravity'
	WRITE (6,*) '  and fix bugs for the AOE 4114 class, Spring 1993  '
      WRITE (6,*) '- Valery Razgonyaev fixed more bugs, Spring 1994    '
      WRITE (6,*) '- Andy Ko added directory change section 11/14/02   '
      WRITE (6,*)
      WRITE (6,*) 'Department of Aerospace and Ocean Engineering'
      WRITE (6,*) 'Virginia Tech, Blacksburg, VA 24061'
      WRITE (6,*) 'whmason@vt.edu'
      WRITE (6,*) 
	WRITE (6,*) '****************************************************'
      WRITE (6,*)

      icodeo=0
      total=0
      do 4 i = 1,2
      do 4 j = 1,25
  4   mmcd(j,i) = 0
 10   call geomtr
      if (icodeo.gt.0) go to 110
      if (m.gt.200) go to 40
      nsw=nsswsv(1)+nsswsv(2)
      if (nsw.gt.50) go to 30
      itsv=0
      do 20 it=1,iplan
      if (an(it).le.25.) go to 20
      write (6,100) it,an(it)
      itsv=1
 20   continue
      if (itsv.gt.0) go to 60
      go to 50
 30   write(6,90) nsw
      go to 60
 40   write(6,80) m
      go to 60
 50   call matxso
      call aerody(cg)
      if (ptest.eq.1..or.qtest.eq.1.) go to 60
      call cdragn
      if (itpcod.eq.1) call tipsuc(cg)
 60   total=total-1.
      go to 10
c
 80   format (1h1//2x,i6,'horseshoe vortices laidout, this is ',
     1'more than the 200 maximum.'/' this configuration is aborted.')
 90   format (1h1//2x,i6,' rows of horseshoe vortices laidout. this',
     1' is more than the 50 maximum.'/' this configuration is aborted.')
 100  format (1h1//2x,8hplanform,i6,4h has,i6,/,' breakpoints.',
     1'the maximum dimensioned is 25. the configuration is aborted.')
 110  stop
      end

c
c  *********************************************************************
c  *  subroutine called by main to determine the reference planform    *
c  *  breakdown and list the chordwise horseshoe vorticies             *
c  *********************************************************************
c
      subroutine geomtr
      dimension xref(25), yref(25), sar(25), a(25), rsar(25), x(25), 
     1          y(25), botsv(2), sa(2), vbord(51), spy(50,2), kfx(2), 
     2          iyl(50,2), iyt(50,2)
      common /all/ bot,m,beta,ptest,qtest,tblscw(50),q(200),pn(200),
     1             pv(200),alp(200),s(200),psi(200),phi(50),zh(50)
      common /onethre/ twist(2),cref,sref,cave,cldes,strue,ar,artrue,
     1                 rtcdht(2),config,nsswsv(2),msv(2),kbot,plan,
     2                 iplan,mach,sswwa(50),xl(2),xt(2),clwb,cmcl,
     3                 cla(2),blair(50),clamar(2)
      common /mainone/ icodeo,total,aan(2),xs(2),ys(2),kfcts(2),
     1                 xreg(25,2),yreg(25,2),areg(25,2),dih(25,2),
     2                 mcd(25,2),xx(25,2),yy(25,2),as(25,2),
     3                 ttwd(25,2),mmcd(25,2),an(2),zz(25,2),itpcod,
     4                 xrego(25,2),cg
      common /ccrrdd/  tspan,tspana,kbit
      real mach
      character*10 prtcon
      character*70 title
c
c     part one - geometry computation
c
c     section one - input of reference wing position
c
c
      if (total.eq.0.0) then
                        xl(2)     = 0.0
                        xt(2)     = 0.0
                        rtcdht(1) = 0.0
                        rtcdht(2) = 0.0
                        end if
      ytol   =  1.e-10
      azy    =  1.e+13
      pit    =  1.5707963
      rad    = 57.29578
      if (total.gt.0.) go to 110
c
c     set plan equal to 1. for a wing alone computation - even for a
c     variable sweep wing
c     set plan equal to 2. for a wing - tail combination
c
c     set total equal to the number of sets
c     of group two data provided
c
      read (5,885,end=830)title
      read (5,879) plan,total,cref,sref,cg
c      if (endfile 5) 830,10
10    iplan=plan
c
c
c     set aan(it) equal to the maximum number of curves required to
c     define the planform perimeter of the (it) planform,
c
c     set rtcdh(it) equal to the root chord height of the lifting
c     surface (it), whose perimeter points are being read in, with
c     respect to the wing root chord height
c
      write (6,875)
      write (6,885)title
      write (6,860)
      do 60 it = 1,iplan
      read (5,880) aan(it),xs(it),ys(it),rtcdht(it)
      xs(it) = xs(it) - cg
      n      = aan(it)
      n1     = n + 1
      mak    = 0

      if (iplan.eq.1) prtcon='          '
      if (iplan.eq.2.and.it.eq.1) prtcon='   first  '
      if (iplan.eq.2.and.it.eq.2) prtcon='  second  '
      write (6,870) prtcon,n,cg,rtcdht(it),xs(it)+cg,ys(it)
      write (6,990)
      do 50 i = 1,n1
      read (5,880) xrego(i,it),yreg(i,it),dih(i,it),amcd
c
c     make coordinate transformation to c.g.
c
      xreg(i,it) = xrego(i,it) - cg
c
      mcd(i,it)=amcd
      if (i.eq.1) go to 50
      if (mak.ne.0.or.mcd(i-1,it).ne.2) go to 20
      mak=i-1
   20 if (abs(yreg(i-1,it)-yreg(i,it)).lt.ytol) go to 30
      areg(i-1,it)=(xreg(i-1,it)-xreg(i,it))/(yreg(i-1,it)-yreg(i,it))
      aswp=atan(areg(i-1,it))*rad
      go to 40
   30 yreg(i,it)=yreg(i-1,it)
      areg(i-1,it)=azy
      aswp=90.
   40 j=i-1
c
c     write planform perimeter points and angles
c
      write (6,960) j,xrego(j,it),yreg(j,it),
     .                aswp,dih(j,it),mcd(j,it)
       dih(j,it)=tan(dih(j,it)/rad)
50    continue
      kfcts(it)=mak
      write (6,961) n1,xrego(n1,it),yreg(n1,it)
60    continue
c
c     part 1 - section 2
c     read group 2 data and conpute desired wing position
c
c
c     set sa(1),sa(2) equal to the sweep angle,in degrees, for the first
c     curve(s) that can change sweep for each planform
c
c     if a particular value of cl is desired at which the loadings are
c     to be computed, set cldes equal to this value
c     set cldes equal to 11. for a drag polar at cl values of-.1 to 1.0
c
c     if ptest is set equal to oe the program will compute clp
c     if qtest is set equal to one the program will compute cmq and clq
c     do not set both ptest and qtest to one for a single configuration
c
c     set twist(1) or twist(2) equal to 0. for a flat planform and to 1.
c     for a planform that has twist and/or camber
c
c     set atpcod to one if the contributions to lift,drag,and moment
c     from separated flow around the leading and/or side edges is
c     desired. otherwise set atpcod to zero.
c
70    read (5,950) config,scw,vic,mach,cldes,ptest,qtest,twist(1),sa(1),
     1             twist(2),sa(2),atpcod
      itpcod=atpcod
      if (itpcod.ne.1) go to 110
      do 100 it=1,iplan
      nbbg=aan(it)
      do 90 ibbg=2,nbbg
      if (yreg(ibbg,it).eq.yreg(ibbg+1,it)) go to 80
      go to 90
80    if (yreg(ibbg+2,it).lt.yreg(ibbg+1,it)) go to 90
      if (yreg(ibbg-1,it).lt.yreg(ibbg,it)) go to 90
      xl(it)=xreg(ibbg,it)
      xt(it)=xreg(ibbg+1,it)
      go to 100
90    continue
      xl(it)=0.0
      xt(it)=0.0
100   continue
110   continue
      write (6,890) config
c      if (endfile 5) 830,120
120   if (ptest.ne.0..and.qtest.ne.0.) go to 850
      if (scw.eq.0.) go to 140
      do 130 i=1,50
130   tblscw(i)=scw
      go to 150
140   read (5,880) sta
      nsta=sta
c
      do 123 i=1,nsta,8
      read (5,882) tblscw(i),tblscw(i+1),tblscw(i+2),tblscw(i+3),tblscw
     1(i+4),tblscw(i+5),tblscw(i+6),tblscw(i+7)
 123  continue
c
150   do 410 it=1,iplan
      n=aan(it)
      n1=n+1
      do 160 i=1,n
      xref(i)=xreg(i,it)
      yref(i)=yreg(i,it)
      a(i)=areg(i,it)
      rsar(i)=atan(a(i))
      if (a(i).eq.azy) rsar(i)=pit
160   continue
      xref(n1)=xreg(n1,it)
      yref(n1)=yreg(n1,it)
      if (kfcts(it).gt.0) go to 170
      k=1
      sa(it)=rsar(1)*rad
      go to 180
170   k=kfcts(it)
180   write (6,920) k,sa(it),it
      sb=sa(it)/rad
      if (abs(sb-rsar(k)).gt.(.1/rad)) go to 210
c
c    reference planform coordinates are stored unchanged for wings
c    without change in sweep.
c
      do 200 i=1,n
      x(i)=xref(i)
      y(i)=yref(i)
      if (rsar(i).eq.pit) go to 190
      a(i)=tan(rsar(i))
      go to 200
190   a(i)=azy
200   sar(i)=rsar(i)
      x(n1)=xref(n1)
      y(n1)=yref(n1)
      go to 390
c
c     changes in wing sweep are made here.
c
210   if(mcd(k,it).ne.2) go to 840
      ka=k-1
      do 220 i=1,ka
      x(i)=xref(i)
      y(i)=yref(i)
220   sar(i)=rsar(i)
c
c    determine leading edge intersection between fixed and variable
c         sweep wing sections
c
      sar(k)=sb
      a(k)=tan(sb)
      sai=sb-rsar(k)
      x(k+1)=xs(it)+(xref(k+1)-xs(it))*cos(sai)+(yref(k+1)-ys(it))*
     1sin(sai)
      y(k+1)=ys(it)+(yref(k+1)-ys(it))*cos(sai)-(xref(k+1)-xs(it))*
     1sin(sai)
      if (abs(sb-sar(k-1)).lt.(.1/rad)) go to 230
      y(k)=x(k+1)-x(k-1)-a(k)*y(k+1)+a(k-1)*y(k-1)
      y(k)=y(k)/(a(k-1)-a(k))
      x(k)=a(k)*x(k-1)-a(k-1)*x(k+1)+a(k-1)*a(k)*(y(k+1)-y(k-1))
      x(k)=x(k)/(a(k)-a(k-1))
      go to 240
c
c     eliminate extraneous breakpoints
c
  230 x(k)=xref(k-1)
      y(k)=yref(k-1)
      sar(k)=sar(k-1)
  240 k=k+1
c
c     sweep the breakpoints on the variable sweep panel
c     (it also keeps sweep angles in first or fourth quadrants)
c
  250 k=k+1
      sar(k-1)=sai+rsar(k-1)
  260 if (sar(k-1).le.pit) go to 270
      sar(k-1)=sar(k-1)-3.1415927
      go to 260
  270 if (sar(k-1).ge.(-pit)) go to 280
      sar(k-1)=sar(k-1)+3.1415927
      go to 270
  280 if (sar(k-1).lt.0.) go to 290
      if(sar(k-1)-pit) 320,300,300
  290 if(sar(k-1)+pit) 310,310,320
  300 a(k-1)=azy
      go to 330
  310 a(k-1)=-azy
      go to 330
  320 a(k-1)=tan(sar(k-1))
  330 kk=mcd(k,it)
      go to (350,340), kk
  340 y(k)=ys(it)+(yref(k)-ys(it))*cos(sai)-(xref(k)-xs(it))*sin(sai)
      x(k)=xs(it)+(xref(k)-xs(it))*cos(sai)+(yref(k)-ys(it))*sin(sai)
      go to 250
c
c     determine the trailing edge intersection
c     between fixed and variable sweep wing sections
c
  350 if (abs(rsar(k)-sar(k-1)).lt.(.1/rad)) go to 360
      y(k) = xref(k+1)-x(k-1)-a(k)*yref(k+1)+a(k-1)*y(k-1)
      y(k) = y(k)/(a(k-1)-a(k))
      x(k) = a(k)*x(k-1)-a(k-1)*xref(k+1)+a(k-1)*a(k)*(yref(k+1)-y(k-1))
      x(k) = x(k)/(a(k)-a(k-1))
      go to 370
  360 x(k) = xref(k+1)
      y(k) = yref(k+1)
  370 k    = k + 1
c
c     store reference planform coordinates on inboard fixed trailing
c     edge
c
      do 380   i = k,n1
      x(i)       = xref(i)
      y(i)       = yref(i)
  380 sar(i-1)   = rsar(i-1)
  390 do 400   i = 1,n
      xx(i,it)   = x(i)
      yy(i,it)   = y(i)
      mmcd(i,it) = mcd(i,it)
      ttwd(i,it) = dih(i,it)
  400 as(i,it)   = a(i)
      xx(n1,it)  = x(n1)
      yy(n1,it)  = y(n1)
      an(it)     = aan(it)
  410 continue
c
c     line up breakpoints among planforms
c
      botsv(1)   = 0.
      botsv(2)   = 0.
      write (6,980)
      do 530  it = 1,iplan
        nit        = an(it)+1
        do 470 itt = 1,iplan
          if (itt.eq.it) go to 470
          nitt       = an(itt)+1
          do 460   i = 1,nitt
            jpsv       = 0
            do 420  jp = 1,nit
              if (yy(jp,it).eq.yy(i,itt)) go to 460
  420       continue
            do 430  jp = 1,nit
              if (yy(jp,it).lt.yy(i,itt)) go to 440
  430       continue
            go to 460
  440       jpsv      = jp
            ind       = nit - (jpsv-1)
            do 450 jp = 1,ind
              k2          = nit-jp+2
              k1          = nit-jp+1
              xx(k2,it)   = xx(k1,it)
              yy(k2,it)   = yy(k1,it)
              mmcd(k2,it) = mmcd(k1,it)
              as(k2,it)   = as(k1,it)
  450         ttwd(k2,it) = ttwd(k1,it)
            yy(jpsv,it)   = yy(i,itt)
            as(jpsv,it)   = as(jpsv-1,it)
            ttwd(jpsv,it) = ttwd(jpsv-1,it)
            xx(jpsv,it)   = (yy(jpsv,it)-yy(jpsv-1,it))*
     .                       as(jpsv-1,it)+xx(jpsv-1,it)
            mmcd(jpsv,it) = mmcd(jpsv-1,it)
            an(it)        = an(it)+1.
            nit           = nit+i
  460     continue
  470   continue
c
c     sequence wing coordinates from tip to root
c
        n1       = an(it)+1.
        do 480 i = 1,n1
          q(i)   = yy(i,it)
  480   continue
        do 520 j = 1,n1
          high      = 1.
          do 490  i = 1,n1
            if ((q(i)-high).ge.0.) go to 490
            high    = q(i)
            ih      = i
  490     continue
          if (j.ne.1) go to 500
          botsv(it) = high
          kfx(it)   = ih
  500     q(ih)=1.
          spy(j,it) = high
          if (ih.gt.kfx(it)) go to 510
          iyl(j,it) = 1
          iyt(j,it) = 0
          go to 520
  510     iyl(j,it) = 0
          iyt(j,it) = 1
  520   continue
  530 continue
c
c     select maximum b/2 as the wing semispan.      if both first and
c     second planforms have same semispan then the second planform is
c     taken to be the wing.
c
      kbot = 1
      if (botsv(1).ge.botsv(2)) kbot = 2
      bot  = botsv(kbot)
c
c     compute nominal horseshoe vortex width along wing surface
c
      tspan = 0
      isave = kfx(kbot)-1
      i     = kfx(kbot)-2
  540 if (i.eq.0) go to 550
      if (ttwd(i,kbot).eq.ttwd(isave,kbot)) go to 560
  550 ctwd=cos(atan(ttwd(isave,kbot)))
      tlgth=(yy(isave+1,kbot)-yy(i+1,kbot))/ctwd
      tspan=tspan+tlgth
      if (i.eq.0) go to 570
      isave=i
  560 i=i-1
      go to 540
  570 vi=tspan/vic
      vstol=vi/2
      tspana=0.
      kbit=2
      if (iplan.eq.1) go to 610
      if (kbot.eq.2) kbit=1
      isavea=kfx(kbit)-1
      ia=kfx(kbit)-2
  580 if (ia.eq.0) go to 590
      if (ttwd(ia,kbit).eq.ttwd(isavea,kbit)) go to 600
  590 ctwda=cos(atan(ttwd(isavea,kbit)))
      tlgtha=(yy(isavea+1,kbit)-yy(ia+1,kbit))/ctwda
      tspana=tspana+tlgtha
      if (ia.eq.0) go to 610
      isavea=ia
  600 ia=ia-1
      go to 580
  610 continue
c
c     eliminate planform breakpoints which are within (b/2)/2000 units
c     laterally
c
      do 630 it=1,iplan
       n=an(it)
       n1=n+1
       do 630 j=1,n
       aa=abs(spy(j,it)-spy(j+1,it))
       if (aa.eq.0..or.aa.gt.abs(tspan/2000.0)) go to 630
       if (aa.gt.ytol) write (6,1010) spy(j+1,it),spy(j,it)
       do 620 i=1,n1
       if (yy(i,it).ne.spy(j+1,it)) go to 620
       yy(i,it)=spy(j,it)
 620   continue
       spy(j+1,it)=spy(j,it)
 630   continue
c
c      compute z coordinates
c
      do 670 it =1,iplan
       jm=an(it)+1.
       n1=an(it)+1.
       do 640 jz=1,n1
 640   zz(jz,it)=rtcdht(it)
       jz=1
 650   jz=jz+1
       if (jz.gt.kfx(it)) go to 660
       zz(jz,it)=zz(jz-1,it)+(yy(jz,it)-yy(jz-1,it))*ttwd(jz-1,it)
       go to 650
 660   jm =jm-1
       if (jm .eq. kfx(it)) go to 670
       zz(jm,it)=zz(jm+1,it)+(yy(jm,it)-yy(jm+1,it))*ttwd(jm,it)
       go to 660
 670   continue
c
c     write planform perimeter points actually used in the computations
c
      write (6,900)
      do 690 it=1,iplan
       n=an(it)
       n1=n+1
       if (it .eq. 2) write(6,1000)
       do 680 kk = 1,n
       tout=atan(ttwd(kk,it))*rad
       aout=atan(as(kk,it))*rad
       if (as(kk,it).eq.azy) aout=90.
       write(6,910)kk,xx(kk,it)+cg,yy(kk,it),zz(kk,it),
     .             aout,tout,mmcd(kk,it)
 680   continue
       write(6,910) n1,xx(n1,it)+cg,yy(n1,it),zz(n1,it)
 690   continue
c
c     part one - section three - lay out yawed horseshoe vortices
c
      strue=0.
      nsswsv(1)=0
      nsswsv(2)=0
      msv(1)=0
      msv(2)=0
      do 780 it=1,iplan
       n1=an(it)+1.
       i=0
       j=1
       yin=botsv(it)
       ile=kfx(it)
       ite=kfx(it)
c
c      determine spanwise borders of horseshoe vortices
c
 700   ixl=0
       ixt=0
       i=i+1
       if (yin.ge.(spy(j,it)+vstol))go to 710
c
c     border is within vortex spacing tolerence (vstol) of breakpoint
c     therefore use the next breakpoint inboard for the border
c
       vbord(i)=yin
       go to 740
c
c     use nominal vortex spacing to determine the border
c
 710   vbord(i)=spy(j,it)
c
c     compute subscripts ile and ite to indicate which
c     breakpoints are adjacent and whether they are on the wing
c     leading edge or trailing edge
c
 720  if (j.ge.n1) go to 730
      if (spy(j,it).ne.spy(j+1,it))go to 730
      ixl=ixl+iyl(j,it)
      ixt=ixt+iyt(j,it)
      j=j+1
      go to 720
 730  yin=spy(j,it)
      ixl=ixl+iyl(j,it)
      ixt=ixt+iyt(j,it)
      j=j+1
 740  cphi=cos(atan(ttwd(ile,it)))
      iphi=ile-ixl
      if (j.ge.n1) iphi=1
      yin = yin-vi*cos(atan(ttwd(iphi,it)))
      if (i.ne.1) go to 760
 750  ile=ile-ixl
      ite=ite+ixt
      go to 700
c
c     compute coordinates for chordwise row of horseshoe vortices
c
 760  yq=(vbord(i-1)+vbord(i))/2.0
      hw=(vbord(i)-vbord(i-1))/2.
      im1=i-1+nsswsv(1)
      zh(im1)=zz(ile,it)+(yq-yy(ile,it))*ttwd(ile,it)
      phi(im1)=ttwd(ile,it)
      sswwa(im1)=as(ile,it)
      xle=xx(ile,it)+as(ile,it)*(yq-yy(ile,it))
      xte=xx(ite,it)+as(ite,it)*(yq-yy(ite,it))
      xlocal=(xle-xte)/tblscw(im1)
c
c     compute wing area projected to the x-y plane
c
      strue=strue+xlocal*tblscw(im1)*(hw*2.)*2.
c
      nscw=tblscw(im1)
      do 770 jcw=1,nscw
       ajcw=jcw-1
       xlel=xle-ajcw*xlocal
       nts=jcw+msv(1)+msv(2)
       pn(nts)=xlel-.25*xlocal
       pv(nts)=xlel-.75*xlocal
       psi(nts)=((xle-pn(nts))*as(ite,it)+(pn(nts)-xte)*as(ile,it))/
     1 (xle-xte)
       s(nts)=hw/cphi
       q(nts)=yq
 770   continue
      msv(it)=msv(it)+nscw
c
c     test to determine when ring root is reached
c
      if (vbord(i) .lt. yreg(1,it))go to 750
       nsswsv(it)=i-1
 780   continue
       m=msv(1)+msv(2)
c
c      compute aspect ratio and average chord
c
       bot     = -bot
       ar      = 4.*bot*bot/sref
       artrue  = 4.*bot*bot/strue
       cave    = strue/(2.*bot)
       beta    = (1.-mach*mach)**.5
       nvtwo   = 0
       istart  = 0
       iend    = 0
       ndxtbl  = 0
       do 810 it = 1, iplan
         jrange  = nsswsv(it)
         do 805 j = 1, jrange
           ndxtbl = nsswsv(it)
           nmoalp = tblscw(ndxtbl)
           istart = iend + 1
           iend   = iend + nmoalp
           if (twist(it) .eq. 0) goto 795
           read (5,882) (alp(itoe), itoe = istart, iend)
           if (twist(it) .eq. 1) goto 805
           goto 805
  795      do 800 itoe = istart, iend
             alp(itoe) = 0
  800     continue
  805    continue
  810  continue
c
       write(6,1040) m
       write(6,1050) (it,msv(it),nsswsv(it),it=1,iplan)
       if(scw.ne.0.) write(6,1020)scw
       if(scw.eq.0.0) write(6,1030) (tblscw(i),i=1,nsta)
c
c      apply prandtl-glauert correction
c
       do 820 nv=1,m
       psi(nv)=atan(psi(nv)/beta)
       pn(nv)=pn(nv)/beta
 820   pv(nv)=pv(nv)/beta
       return
 830   icodeo=1
       write(6,930) config
c     Added pause statement by Andy Ko 11/14/02
	 Pause 'Press ENTER key to terminate'  
       return
 840   icodeo=2
       write(6,940) k,it
       return
 850   icodeo=3
       write(6,970) ptest,qtest
       return
 875  format (//15x,'vortex lattice aerodynamic computation program'/
     114x,'nasa-lrc no. a2794 by j.e. lamar and b.b. gloss'//
     216x,'modified for watfor77 with 72 column output'//)
 860  format (//30x,13hgeometry data)
 870  format (///15x,a10,'reference planform has',i3,' curves'//,2x,
     .' center of gravity =',f10.5,/,2x,
     .' root chord height =',f10.5,/,2x,
     .' variable sweep pivot position',4x,
     .'x(s) =',f10.5,3x,'y(s) =',f10.5//16x,
     .'break points for the reference planform' /)
 885  format (a70)
 879  format (5f10.4)
 880  format (4f9.4)
 881  format (f9.4)
 882  format (8f10.6)
 890  format (1h1//17x,17hconfiguration no.,f8.0/)
 900  format (1x,5hpoint,6x,1hx,11x,1hy,11x,1hz,8x,5hsweep,6x,
     .8hdihedral,3x,4hmove/45x,5hangle,7x,5hangle,5x,4hcode/)
 910  format (1x,i3,3f12.5,2f12.5,i6)
 920  format (/10x,5hcurve,i3,9h is swept,f12.5,20h degrees on planform,
     1i3)
 930  format (1h1///11x,'end of file encountered after configuration',
     1        f7.0)
 940  format  (1h1///8x,45hthe first variable sweep curve specified (k=
     1,i3,/,44h ) does not have an m code of 2 for planform,i4)
 950  format (8f5.1,f10.4,f5.1,f10.4,f5.1)
 960  format (2x,i5,2f12.5,2f12.5,4x,i4)
 961  format (2x,i5,2f12.5)
 970  format (1h1//1x,38herror - program cannot process ptest =,f5.1,12
     1h and qtest =,f5.1)
 980  format (//18x,35hbreak points for this configuration //)
 990  format (2x,5hpoint,8x,1hx,11x,1hy,9x,5hsweep,6x,8hdihedral,4x,4
     1hmove/14x,3href,9x,3href,8x,5hangle,7x,5hangle,6x,4hcode/)
 1000 format (/22x,28hsecond planform breakpoints /)
 1010 format (////1x,34hthe breakpoint located spanwise at,f11.5,/,1x,
     .20hhas been adjusted to,f9.5////)
 1020 format (/13x,f5.0,41h horseshoe vortices in each chordwise row//)
 1030 format (/1x,45htable of horseshoe vortices in each chordwise,/,
     .53h row (from tip to root beginning with first planform)//
     .14f5.0/14f5.0)
 1040 format (///3x,i5,' horseshoe vortices used on the left half of',
     1' the configuration'//20x,35hplanform       total       spanwise/)
 1050 format (22x,i4,10x,i3,11x,i4)
      end
c
c  *********************************************************************
c  *  subroutine called by main to solve matricies                     *
c  *********************************************************************
c
      subroutine matxso
      dimension yy(2), fv(2), fw(2), fvn(200)
      common/all/bot,m,beta,ptest,qtest,tblscw(50),q(200),pn(200),pv(2
     100),alp(200),s(200),psi(200),phi(50),zh(50)
      common/tothre/cir(200,2)
      common/insub23/apsi,aphi,xx,yyy,zz,snn,tolc
c
c
c     part 2 - compute circulation terms
c
c
c     the tolerance set at this point in the program may need to be
c     changed for computers other than the cdc 6000 series
c
c
      tolc=(bot*15.e-05)**2
      do 10 nv=1,m
      cir(nv,1)=12.5663704*alp(nv)
      cir(nv,2)=12.5663704
      if (ptest.ne.0.) cir(nv,2)=-1.0964155*q(nv)/bot
      if (qtest.ne.0.) cir(nv,2)=-1.0964155*pv(nv)*beta
 10   continue
      izz=1
      nnv=tblscw(izz)
      rewind 10
      do 70 nv=1,m
      do 20 i=1,m
 20   fvn(i)=0.
      iz=1
      nnn=tblscw(iz)
      do 60 nn=1,m
      aphi=atan(phi(iz))
      apsi=psi(nn)
      xx=pv(nv)-pn(nn)
      yy(1)=q(nv)-q(nn)
      yy(2)=q(nv)+q(nn)
      zz=zh(izz)-zh(iz)
      snn=s(nn)
      do 30 i=1,2
      yyy=yy(i)
      call infsub(bot,fv(i),fw(i))
      aphi=-aphi
      apsi=-apsi
 30   continue
      if (ptest.ne.0.) go to 40
      fvn(nn)=fw(1)+fw(2)-(fv(1)+fv(2))*phi(izz)
      go to 50
 40   fvn(nn)=fw(1)-fw(2)-(fv(1)-fv(2))*phi(izz)
 50   if (nn.lt.nnn.or.nn.eq.m) go to 60
      iz=iz+1
      nnn=nnn+tblscw(iz)
 60   continue
      dumb=-cir(nv,1)
      dumy=-cir(nv,2)
      do 65 i=1,m
      write(10) fvn(i)
 65   continue
      write(10) dumb
      write(10) dumy
      if (nv.lt.nnv.or.nv.eq.m) go to 70
      izz=izz+1
      nnv=nnv+tblscw(izz)
 70   continue
      call ssleso (m,2)
      return
      end
c
c  *********************************************************************
c  *  subroutine called by matxso, cdragn, and tipsuc #1               *
c  *********************************************************************
c
      subroutine infsub(bot,fvi,fwi)
      common /insub23/ psii,aphii,xxx,yyy,zzz,snn,tolrnc
c
      fc=cos(psii)
      fs=sin(psii)
      ft=fs/fc
      fpc=cos(aphii)
      fps=sin(aphii)
      fpt=fps/fpc
      f1=xxx+snn*ft*fpc
      f2=yyy+snn*fpc
      f3=zzz+snn*fps
      f4=xxx-snn*ft*fpc
      f5=yyy-snn*fpc
      f6=zzz-snn*fps
      ffa=(xxx**2+(yyy*fps)**2+fpc**2*((yyy*ft)**2+(zzz/fc)**2-2.*xxx*
     1     yyy*ft)-2.*zzz*fpc*(yyy*fps+xxx*ft*fps))
      ffb=(f1*f1+f2*f2+f3*f3)**.5
      ffc=(f4*f4+f5*f5+f6*f6)**.5
      ffd=f5*f5+f6*f6
      ffe=f2*f2+f3*f3
      fff=(f1*fpc*ft+f2*fpc+f3*fps)/ffb-(f4*fpc*ft+f5*fpc+f6*fps)/ffc
c
c
c     the tolerance set at this point in the program may need to be
c     changed for computers other than the cdc 6000 series
c
c
      if (abs(ffa).lt.(bot*15.e-5)**2) go to 10
      fvone=(xxx*fps-zzz*ft*fpc)*fff/ffa
      fwone=(yyy*ft-xxx)*fff/ffa*fpc
      go to 20
10    fvone=0.
      fwone=0.
c
20    if (abs(ffd).lt.tolrnc) go to 30
      fvtwo=f6*(1.-f4/ffc)/ffd
      fwtwo=-f5*(1.-f4/ffc)/ffd
      go to 40
30    fvtwo=0.
      fwtwo=0.
c
40    if (abs(ffe).lt.tolrnc) go to 50
      fvthre=-f3*(1.-f1/ffb)/ffe
      fwthre=f2*(1.-f1/ffb)/ffe
      go to 60
50    fvthre=0.
      fwthre=0.
c
60    fvi=fvone+fvtwo+fvthre
      fwi=fwone+fwtwo+fwthre
      return
      end
c
c  *********************************************************************
c  *  subroutine called by matxso to compute lift, pitching moment,    *
c  *  and induced drag coefficient for no dihedral                     *
c  *********************************************************************
c
      subroutine ssleso (nt,ncflg)
      common/tothre/cir(200,2)
      dimension rv(205), cv(205), r(205), v(10350)
      rewind 10
c
      n1=nt+ncflg
      j=n1-1
c
      do 5 i=1,n1
      read(10) r(i)
  5   continue
c
      do 10 i=1,j
 10   v(i)=-r(i+1)/r(1)
      in=1
 20   continue
c
      do 25 i=1,n1
      read(10) r(i)
 25   continue
c
      i2=0
      do 40 i=1,j
      rv(i)=0.
      do 30 ii=1,in
      i2=i2+1
 30   rv(i)=rv(i)+r(ii)*v(i2)
      n2=in+i
 40   rv(i)=rv(i)+r(n2)
      i2=in+1
      nn=j*in+1
      kk=j*i2
      j=j-1
      do 60 i=1,j
      do 50 ii=1,in
      nn=nn-1
      kk=kk-1
 50   v(kk)=v(nn)
 60   kk=kk-1
      do 70 i=1,in
 70   r(i)=v(i)
      k=0
      do 90 i=1,j
      cc=-rv(i+1)/rv(1)
      do 80 ii=1,in
      cv(ii)=cc*r(ii)
      nn=k+ii
      i2=i2+1
 80   v(nn)=cv(ii)+v(i2)
      k=nn+1
      i2=i2+1
 90   v(k)=cc
      in=in+1
      if(j.eq.ncflg) go to 100
      go to 20
 100  k=1
      do 110 j=1,ncflg
      do 110 i=1,nt
      cir(i,j)=v(k)
      k=k+1
 110  continue
      return
      end
c
c  *********************************************************************
c  *  subroutine called by main                                        *
c  *********************************************************************
c
      subroutine aerody(cg)
      dimension ycp(2), clcc(200,2), ch(2,50), sum(2), ac(2),
     2 clcl(2,50), cp(200), p(200), smoad(2,50), sldt(50), smld(2,
     3 50)
      common /all/ bot, m, beta, ptest, qtest, tblscw(50), q(200),
     2 pn(200), pv(200), alp(200), s(200), psi(200), phi(50), zh(50)
      common /tothre/ cir(200,2)
      common /threfor/ ccav(2,50), clt, clnt, nssw, alpd
      common /onethre/ twist(2), cref, sref, cave, cldes, strue, ar,
     2 artrue, rtcdht(2), config, nsswsv(2), msv(2), kbot, plan,
     3 iplan,
     4 mach, sswwa(50), xl(2), xt(2), clwb, cmcl, cla(2), blair(50),
     5 clamar(2)
      common /threcdi/ sload(3,50)
      common /insub23/ apsi, aphi, xx, yyy, zz, snn, tolcsq
      real mach
      character*8 head
c
c
c     part 3 - compute output terms
c
c
      rad = 57.29578
      twst = twist(1) + twist(2)
      alref = 1
      qinf = 1.
      nssw = nsswsv(1) + nsswsv(2)
c
c               part 3 - section 1
c               compute lift and pitching moment here
c
      iz = 1
      nnn = tblscw(iz)
      do 10 i = 1, m
      p(i) = s(i) * cos(atan(phi(iz)))
      if (i .lt. nnn .or. i .eq. m) go to 10
      iz = iz + 1
      nnn = nnn + tblscw(iz)
 10   continue
      do 20 nv = 1, 2
      sum(nv) = 0
      do 20 i = 1, m
      sum(nv) = sum(nv) + cir(i,nv) * p(i)
      if (nv .eq. 1 .and. i .eq. msv(1)) clwngt = sum(1) * 8. / sref
      if (nv .eq. 2 .and. i .eq. msv(1)) clwing = sum(2) * 8. / sref
 20   continue
      clt = 8. * sum(1) / sref
      clnt = 8. * sum(2) / sref
      if (kbot .eq. 1) go to 30
      clwngt = clt - clwngt
      clwing = clnt - clwing
 30   crl = 0.
      do 40 i = 1, m
      crl = crl + (q(i) * cir(i,2) * 2. * p(i)) * 2.
      clcc(i,1) = cir(i,1) * 2 * p(i) / (cave * s(i))
 40   clcc(i,2) = cir(i,2) * 2 * p(i) / (cave * s(i))
c
c     compute clp
c
      clp = crl / (sref * bot * 0.08725)
      cla(2) = clnt
      do 120 ixx = 1, 2
      sa = 0.
      sb = 0.
      sc = 0.
      i = 0
      jb = nsswsv(1)
      ja = 1
 50   continue
      do 70 jssw = ja, jb
      sd = 0.
      se = 0.
      sload(ixx,jssw) = 0
      nscw = tblscw(jssw)
      do 70 jscw = 1, nscw
      if (twst .eq. 0. .and. ixx .eq. 1) go to 60
      i = i + 1
      sa = sa + cir(i,ixx) * p(i)
      sb = sb + cir(i,ixx) * q(i) * p(i)
      sc = sc + cir(i,ixx) * pn(i) * p(i) * beta
      sload(ixx,jssw) = sload(ixx,jssw) + (bot * cir(i,ixx) * p(i) /
     1s(i)) / (2. * sum(ixx))
      sd = sd + cir(i,ixx)
      se = se + cir(i,ixx) * pn(i) * beta
      if (jscw .ne. nscw) go to 70
      smoad(ixx,jssw) = se
      smld(ixx,jssw) = sd
      go to 70
 60   sload(1,jssw) =  0.0
      smoad(1,jssw) =  0.0
      smld(1,jssw)  =  0.
 70   continue
      if (jssw .ge. nssw) go to 80
      ja = nsswsv(1) + 1
      jb = nssw
      if (ixx .eq. 1) go to 50
      sc2 = sc
      sa2 = sa
      clamar(1) = sc / (sa * cref)
      go to 50
 80   continue
      if (ixx .eq. 1) go to 100
      if (iplan .eq. 1) go to 90
      sc3 = sc - sc2
      sa3 = sa - sa2
      clamar(2) = sc3 / (sa3 * cref)
      go to 100
 90   clamar(1) = sc / (sa * cref)
 100  continue
      if ((twst .eq. 0. ).and. (ixx .eq. 1)) go to 110
      ycp(ixx) = sb / (sa * bot)
      ac(ixx) = sc / (sa * cref)
      go to 120
 110  ycp(1) = 0.0
      ac(1) = 0.
 120  continue
      cmcl = ac(2)
      cmo   = (ac(1) - ac(2)) * clt
c
c                   part3 - section 2
c                   compute other- and print all final- output here
c
      do 140 ixx = 1, 2
      jn = 0
      do 140 jssw = 1, nssw
      ch(ixx,jssw) = 0
      nscw = tblscw(jssw)
      do 130 jscw = 1, nscw
      jn = jn + 1
      ch(ixx,jssw)=(-2.0)*(pv(jn)-pn(jn))*beta+ch(ixx,jssw)
  130 continue
      ccav(ixx,jssw)=ch(ixx,jssw)/cave
      clcl(ixx,jssw)=sload(ixx,jssw)/ccav(ixx,jssw)
  140 continue
      cld=cldes
      if (cldes.eq.11) cld=1.
      do 150 i=1,m
      cp(i)=(clcc(i,1)+clcc(i,2)*(cld-clt)/clnt)*cave/(2.*(pn(i)-pv(i))*
     1beta)
  150 continue
      write (6,240) config
      if (ptest.ne.0.) write (6,350)
      if (qtest.ne.0.) write (6,330)
      if (ptest.eq.0..and.qtest.eq.0.) write (6,340)
      head = 'desired '
      if (cldes.eq.11.) head='        '
      iend = 11
      if (cldes.ne.11.) iend = 1
      do 190 iutk = 1,iend
      if (iend.eq.11)  cldes = (float(iutk)-1.)/10.
      if (cldes.eq.0.) cldes = -.1
      nr = 0
c
      if (iutk .eq. 1) write (6,360)
      do 160 nv = 1,nssw
        nscw        = tblscw(nv)
        np          = nr+1
        nr          = nr+nscw
        phipr       = atan(phi(nv))*rad
        sload(3,nv) = 0.
        if (nv.eq.(nsswsv(1)+1).and.iutk.eq.1) write (6,230)
        do 160 i = np,nr
          if (iutk.gt.1) go to 160
          pnpr         = pn(i)*beta
          pvpr         = pv(i)*beta
          psipr        = atan(beta*tan(psi(i)))*rad
          write (6,370)  i,pnpr+cg,pvpr+cg,q(i),zh(nv),s(i)
  160     sload (3,nv) = sload(3,nv)+clcc(i,2)*cldes/clnt+clcc(i,1)-
     1                   clcc(i,2)*clt/clnt
c
        nr         = 0
        if (iutk .eq. 1) write (6,365)
        do 165  nv = 1,nssw
          nscw     = tblscw(nv)
          np       = nr+1
          nr       = nr+nscw
          phipr    = atan(phi(nv))*rad
          if (nv.eq.(nsswsv(1)+1).and.iutk.eq.1) write (6,230)
          do 165 i = np,nr
            if (iutk.gt.1) go to 165
            pnpr   = pn(i)*beta
            pvpr   = pv(i)*beta
            psipr  = atan(beta*tan(psi(i)))*rad
            write (6,370) i,pnpr+cg,psipr,phipr,alp(i),cp(i)
  165     continue
c

          if (iutk.gt.1) go to 170
          write (6,270)
          write (6,280) cref,cave,strue,sref
          write (6,271)
          write (6,280) bot,ar,artrue,mach
  170   continue
c
      if (ptest.ne.0.) write (6,380) clp
      if (ptest.ne.0) go to 220
c
c     compute cmq,clq
c
      cmq   = 2.0*cmcl*clnt/(0.08725*cref)
      clq   = 2.0*clnt/(0.08725*cref)
      if (qtest.ne.0.) write (6,390) cmq,clq
      if (qtest.ne.0.) go to 220
c
c     compute induced drag for flat wing-body with no dihedral
c
      nsv    = nsswsv(1)+1
      mtot   = msv(1)+1
      if (kbot.eq.1) go to 180
      nsv    = nsv+nsswsv(2)
      mtot   = mtot+msv(2)
  180 call cdicls (ar,artrue,nsswsv(kbot),mtot,nsv,cdi,cdit)
      clapd  = cla(2)/57.29578
      alpo   = -(clt/cla(2))*57.29578
      alpd   = cldes/clapd+alpo
      alpw   = 1./clapd
      clwb   = clwing*alpd/57.29578+clwngt
      cdiwb  = cdi/(clwb*clwb)
      if (iutk.eq.1) write (6,250)
      write (6,260) cldes, alpd, clwb, cdi, cdiwb
  190 continue
      write (6,290) cla(2),clapd,clt,alpo,ycp(2),cmcl,cmo
      write (6,300)
      write (6,301) clt
      nr        =0
      j         =0
      do 210 nv = 1,nssw
      bclcc     = 0.
      badlae    = 0.
      basld     = 0.
      nscw      = tblscw(nv)
      np        = nr+1
      nr        = nr+nscw
      do 200 i = np,nr
        adlae  = clcc(i,2)*clt/clnt
        bsld   = clcc(i,1)-adlae
        bclcc  = bclcc+clcc(i,1)
        badlae = badlae+adlae
        basld  = basld+bsld
  200   continue
      sldt(nv)=(smoad(1,nv)+smoad(2,nv)*(cldes-clt)/clnt)/(smld(1,nv)
     1         +smld(2,nv)*(cldes-clt)/clnt)
      j  = j + nscw
      yq = q(j)/bot
      if (nv.eq.(nsswsv(1)+1)) write (6,310)
  210 write (6,320) nv,yq,sload(2,nv),clcl(2,nv),ccav(2,nv),bclcc,
     1              badlae,basld,sload(3,nv),sldt(nv)+cg
  220 continue
      return
c
  230 format (/2x,45hsecond planform horseshoe vortex descriptions/)
  240 format (1h1///28x,'aerodynamic data'///24x,
     1                  'configuration no.',f7.0//)
  250 format(//5x,'complete configuration',/31x,'lift',4x,'induced drag
     .(far field solution)',/8x, ' cl   computed alpha   cl(wb)',3x,
     .'cdi at cl(wb)',3x,'cdi/(cl(wb)**2)'/)
c  250 format(//5x,'complete configuration',6x,'induced drag (far field
c     .solution)',/31x,'lift',/8x,' cl   computed alpha   cl(wb)',3x,
c     .'cdi at cl(wb)',3x,'cdi/(cl(wb)**2)'/)
  260 format (4x,f9.4,2x,f9.4,4x,f9.4,3x,f9.4,8x,f9.4)
  270 format (////4x,' ref. chord',6x,'c average',5x,'true area ',2x,
     .' reference area')
  271 format(//,9x,3hb/2,11x,7href. ar,8x,7htrue ar,7x,
     .11hmach number)
  280 format (4f15.5)
  290 format (///17x,'complete configuration characteristics'//4x,
     .'    cl  alpha',7x,'cl(twist)  alpha     y cp      cm/cl',
     .'      cmo'/1x,'   per rad   per deg',13x,'at cl=0'/1x,7f10.5)
  300 format (//9x,'additional loading'/5x,'with cl based on s(true)'
     1,17x,'-at cl des-')
  301 format (/33x,'load    add.  basic  span   x loc'
     ./33x,'dueto  load   load   load    of'
     ./'stat  2y/b    sl     cl     c    twist  at cl  at cl',
     .'  at cl  local'
     ./'             coef  ratio  ratio           =      =',
     .'    desir  cent of',
     ./'                                       ',f6.3,'    0',
     .'           press'/)
  310 format (/2x,'contribution of the second planform to span load',
     .' distribution'/)
  320 format (i3,8f7.3,f8.3)
  330 format (/24x,24hcmq and clq are computed//)
  340 format (/1x,'static longitudinal aerodynamic coefficients',
     .' are computed'//)
  350 format (/29x,15hclp is computed//)
  360 format (/2x,'panel',6x,'x',12x,'x',11x,'y',11x,'z',11x,'s',
     ./4x,'no.',6x,'c/4',9x,'3c/4'/)
  365 format (/2x,'panel',6x,'x',10x,'c/4',7x,'dihedral',6x,'local',7x,
     .'delta'/4x,'no.',17x,'sweep',7x,'angle',8x,'alpha',7x,'cp at'/24x,
     .'angle',20x,'in rad',7x,'cl='/52x,f8.5/)
  370 format (3x,i3,5f12.5)
  380 format (//////////26x,4hclp=,f9.5////)
  390 format (//////////22x,4hcmq=,f9.5,10x,4hclq=,f9.5////)
      end
c
c  *********************************************************************
c  *  subroutine called by aerody                                      *
c  *********************************************************************
c
      subroutine cdicls (ar,artrue,isemsp,mtot,nsv,cdi,cdit)
      dimension etan(51), gampr(51), eta(41), gamma(41), ve(41), b(41)
     1, fvn(41,41)
      common /all/ bot,m,beta,ptest,qtest,tblscw(50),q(200),pn(200),
     1             pv(200),alp(200),s(200),psi(200),phi(50),zh(50)
      common /threcdi/ sload(3,50)
c
      do 10 i    = 1,41
      do 10 j    = 1,41
10    fvn(i,j)   = 0.0
      span       = 2.*bot
      cavb       =  span/artrue
      pi         = .314159265e+01
      nst        = isemsp+1
      nn         = mtot
      do 20 n    = 1,isemsp
      nm         = nsv-n
      nscw       = tblscw(nm)
      nn         = nn-nscw
      etan(n)    = asin(-q(nn)*2./span)
      gampr(n)   = sload(3,nm)*cavb/(2.*span)
20    continue
      etan(nst)  = pi/2.
      gampr(nst) = 0
      do 30 np   = 1,41
      anp        = np
30    eta(np)    = (anp-21.)*pi/42.

      do 40 jk   = 21,41
      call ftlup (eta(jk),gamma(jk),1,nst,etan,gampr,51,51)
40    continue

      do 50 ny   =  22,41
      eta(ny)    =  sin(eta(ny))
      nr         =  42-ny
      eta(nr)    = -eta(ny)
50    gamma(nr)  = gamma(ny)
      do 90 nu   = 21,41
      anu        = nu
      do 80 n    = 1,41
      an         =n
      nnud       =iabs(n-nu)
      ve(n)      =cos(((an-21.)*pi)/42.)
      if (nnud.ne.0) go to 60
      b(n)=(42.)/(4.0*cos(((anu-21.)*pi)/42.))
      go to 80
60    if (mod(nnud,2).eq.0) go to 70
      b(n)       =ve(n)/((42.)*(eta(n)-eta(nu))**2)
      go to 80
70    b(n)=0.0
80    continue

      do 90 np=21,41
      nust=iabs(nu-21)
      if (nust.eq.0) go to 90
      if (mod(nust,2).eq.0) go to 90
      npst=iabs(np-20)
      if (mod(npst,2).eq.0) go to 90
      npnud=iabs(np-nu)
      if (npnud.eq.0) go to 90
      if (mod(npnud,2).eq.0) go to 90
       fvn(nu,np)=2.*b(np)/21.*cos((anu-21.)*pi/42.)
       it=42-nu
       itt=42-np
       fvn(nu,itt)=2.0*b(itt)/21.*cos((anu-21.)*pi/42.)
       fvn(it,np)=fvn(nu,itt)
       fvn(it,itt)=fvn(nu,np)
 90    continue
c
       ccc=0.0
       do 100 n=1,41
 100   ccc=ccc+(gamma(n)*gamma(n))
       do 110 nup=1,41
       do 110 n=1,41
       ccd=ccd-2.0*fvn(nup,n)*(gamma(nup)*gamma(n))
 110   continue
       cdi=pi*ar/4.*(ccc+ccd)
       cdit=1./(pi*ar)
       return
       end
c
c  *********************************************************************
c  *  subroutine called by cdicls and cdragn for interpolation         *
c  *********************************************************************
c
      subroutine ftlup (x,y,m,n,vari,vard,idim1,idim2)
c     ***document date 09-12-69    subroutine revised 07-07-69*********
c     modification of library interpolation subroutine ftlup
      dimension vari(idim1), vard(idim2), v(3), yy(2)
      dimension ii(43)
c
c      initialize all interval pointers to -1.0  for monotonicity check
      data (ii(j),j=1,43)/43* -1/
      ma=iabs(m)
c
c     assign interval pointers for given vari table
c     the same pointer will be used on a given vari table every time
c
c      li=mod(%loc(vari(1)),43)+1
c      i=ii(li)
      li=1
      i= -1
      if (i.ge.0) go to 60
      if (n.lt.2) go to 60
c
c     monotonicity check
c
      if(vari(2)-vari(1))20,20,40
c
c     error in monotonicity
c
c  10  k= %loc(vari(1))
   10  k= 1
       print 170, j,k,(vari(j),j=1,n),(vard(j),j=1,n)
       stop
c
c     monotonic decreasing
c
20    do 30 j=2,n
      if (vari(j)-vari(j-1))30,10,10
30    continue
      go to 60
c
c     monotonic increasing
c
40    do 50 j=2,n
      if (vari(j)-vari(j-1)) 10,10,50
50    continue
c
c     interpolation
c
60    if (i.le.0) i=1
      if (i.ge.n) i=n-1
      if (n.le.1) go to 70
      if (ma.ne.0) go to 80
c
c     zero order
c
70    y=vard(1)
      go to 160
c
c    locate i interval (x(i).le.x.lt.x(i+1))
c
80    if ((vari(i)-x)*(vari(i+1)-x)) 110,110,90
c
c     gives direction for search of intervals
c
90    in= sign(1.0,(vari(i+1)-vari(i))*(x-vari(i)))
c
c     if x outside endpoints, extrapolate from end interval
c
100   if ((i+in).le.0) go to 110
      if ((i+in).ge.n) go to 110
      i=i+in
      if ((vari(i)-x)*(vari(i+1)-x)) 110,110,100
110   if (ma.eq.2) go to 120
c
c     first order
c
      y=(vard(i)*(vari(i+1)-x)-vard(i+1)*(vari(i)-x))/(vari(i+1)-vari(i)
     1)
      go to 160
c
c     second order
c
120   if (n.eq.2) go to 10
      if (i.eq.(n-1)) go to 140
      if (i.eq.1) go to 130
c
c     pick third point
c
      sk=vari(i+1)-vari(i)
      if ((sk*(x-vari(i-1))).lt.(sk*(vari(i+2)-x))) go to 140
130   l=i
      go to 150
140   l=i-1
150   v(1)=vari(l)-x
      v(2)=vari(l+1)-x
      v(3)=vari(l+2)-x
      yy(1)=(vard(l)*v(2)-vard(l+1)*v(1))/(vari(l+1)-vari(l))
      yy(2)=(vard(l+1)*v(3)-vard(l+2)*v(2))/(vari(l+2)-vari(l+1))
      y=(yy(1)*v(3)-yy(2)*v(1))/(vari(l+2)-vari(l))
160   ii(li)=i
      return
c
170   format(1h1,' table below out of order for ftlup  at position ',
     1i5,/' x table is stored in location  ',i6//8g15.8)
      end
c
c  *********************************************************************
c  *  subroutine called by main to compute induced drag, and leading   *
c  *  edge thrust and suction                                          *
c  *********************************************************************
c
      subroutine cdragn
      dimension gam(1000),xc4(1000),yq(1000),ccr(20),fw(2),fv(2),
     1xxcc(20),ccc(200),crr(200),yb(50),cri(51),nma(2),xcc4(200),
     2chd(50),xc44(50),yy(2),pphi(50),zzh(50),z(1000),phii(1000),
     3sa(50),ssa(1000),alop(200),allp(50),alppd(1000),alo(20),
     4yc(51),yqq(50)
      common /all/ bot,m,beta,ptest,qtest,tblscw(50),q(200),pn(200),
     1 pv(200),alp(200),s(200),psi(200),phi(50),zh(50)
      common /onethre/ twist(2),cref,sref,cave,cldes,strue,ar,artrue,
     1 rtcdht(2),config,nsswsv(2),msv(2),kbot,plan,iplan,mach,sswwa(50),
     2 xl(2),xt(2),clwb,cmcl,cla(2),blair(50),clamar(2)
      common /tothre/ cir(200,2)
      common /insub23/ apsi,aphi,xx,yyy,zz,snn,tolcsq
      common /threfor/ ccav(2,50),clt,clnt,nssw,alpd
      common /ccrrdd/ tspan,tspana,kbit
      real mach
c
       write (6,250)
       apsi=0.
       tolcsq=0.
       tbls=0.
       pi=4.*atan(1.)
       fpi=4.*pi
       botl=abs(tspan)
       bol=abs(tspana)
       snn=botl/(2.*nsswsv(kbot))
       deltyb=2.*snn
       nma(kbot)=botl/deltyb
       nma(kbit)=bol/deltyb
       nmax=nma(1)+nma(2)
       do 10 i=1,m
       crr(i)=cir(i,1)+cir(i,2)*(cldes-clt)/clnt
 10    continue
       scwmin=20.
       do 20 i=1,nssw
 20    scwmin=amin1(scwmin,tblscw(i))
       nscwmi=scwmin
       mm=nscwmi*nmax
       deltxo=1./scwmin
       do 100 la=1,nssw
       chd(la)=ccav(2,la)*cave/beta
       deltxx=1./tblscw(la)
       xc=-.75*deltxx
       itbl=tblscw(la)
       do 30 lb=1,itbl
       xc=xc+deltxx
       xxcc(lb)=xc
       lc=lb+tbls
 30    alo(lb)=alp(lc)
       xle=pn(lc)+chd(la)*(1.-.75/tblscw(la))
       xoc=-.75*deltxo
       kcode=0
       lb=0
       do 90 k=1,nscwmi
       j=k+(la-1)*nscwmi
       xoc=xoc+deltxo
       xcc4(j)=-xoc*chd(la)+xle
       call ftlup (xoc,alop(j),+1,itbl,xxcc,alo,20,20)
       axmn=k*deltxo
       cat=0.
       if (kcode.eq.2) cat=ccr(lb)-cut
       kcode=0
 40    lb=lb+1
       lc=lb+tbls
       ccr(lb)=crr(lc)
       axitbl=lb*deltxx
       if (axmn-axitbl) 50,60,70
 50    cut=ccr(lb)*(axmn-(lb-1)*deltxx)/deltxx
       kcode=2
       go to 80
 60    kcode=1
 70    cut=ccr(lb)
 80    cat=cat+cut
       if (kcode .ge. 1) go to 90
       if (lb.lt.itbl)   go to 40
 90    ccc(j)=cat
       tbls=tbls+tblscw(la)
 100   continue
       ii=1
       do 150 i=1,iplan
       bott=botl
       if (i.eq.kbit) bott=bol
       iuz=nsswsv(i)
       iux=iuz+1
       ic=msv(1)+(i-1)*msv(2)
       id=ic+1
       iz=nsswsv(1)+(i-1)*nsswsv(2)
       ycat=0.
       iamm=nma(i)
       do 140 la=1,nscwmi
       yc(1)=-pi/2.
       cri(1)=0.
       do 120 j=1,iuz
       l=j+1
       lu=la+(j-1+(i-1)*nsswsv(1))*nscwmi
       allp(j)=alop(lu)
       xc44(j)=xcc4(lu)
       cri(l)=ccc(lu)
       if (la.ne.1) go to 120
       jj=j+(i-1)*nsswsv(1)
       zzh(j)=zh(jj)
       sa(j)=sswwa(jj)
       pphi(j)=phi(jj)
       yqq(j)=q(ii)
       ii=ii+tblscw(jj)
       ie=iuz-j+1
       itl=tblscw(iz)
       id=id-itl
       ia=id+itl
       if (ia.gt.ic) ycat=ycat-s(id)
       if (ia.gt.ic) go to 110
       ycat=ycat-s(id)-s(ia)
 110   iz=iz-1
       yb(ie)=ycat
 120   continue
       do 130 jp=1,iuz
       jz=jp+1
       yc(jz)=asin(yb(jp)/bott)
 130   continue
       yob=-nma(i)*2.*snn-snn
       do 140 k=1,iamm
       kp=la+(k-1+(i-1)*nma(1))*nscwmi
      yob=yob+deltyb
      yoc=asin(yob/bott)
      call ftlup (yob,yq(kp),+1,iuz,yb,yqq,50,50)
      call ftlup(yob,alppd(kp),+1,iuz,yb,allp,50,50)
      call ftlup(yob,ssa(kp),+1,iuz,yb,sa,50,50)
      call ftlup(yob,xc4(kp),+1,iuz,yb,xc44,50,50)
      call ftlup(yob,z(kp),+1,iuz,yb,zzh,50,50)
      call ftlup(yob,phii(kp),+1,iuz,yb,pphi,50,50)
      call ftlup(yoc,gam(kp),+1,iux,yc,cri,51,51)
      if (yob.gt.yb(iuz)) gam(kp)=cri(iux)
 140  continue
 150  continue
      cdrag=0.
      cthrus=0.
      csuct=0.
      const=16.*snn*bot/sref
      do 190 li=1,nmax
      la=(li-1)*nscwmi+1
      lb=li*nscwmi
      cdragi=0.
      ctt=0.
      do 180 nv=la,lb
      cpt=cos(atan(phii(nv)))
      velin=0.
      do 170 nn=1,mm
      xx=xc4(nv)-xc4(nn)
      yy(1)=yq(nv)-yq(nn)
      yy(2)=yq(nv)+yq(nn)
      zz=z(nv)-z(nn)
      aphi=atan(phii(nn))
      do 160 i=1,2
      yyy=yy(i)
      call infsub (bot,fv(i),fw(i))
      aphi=-aphi
 160  continue
      velin=((fw(1)+fw(2))-(fv(1)+fv(2))*phii(nv))*gam(nn)/fpi+velin
 170  continue
      ctt=ctt+gam(nv)*(alpd/57.29578+alppd(nv))*cpt/(2.*bot)
 180  cdragi=cdragi+velin*gam(nv)*cpt/(2.*bot)
      ctt=ctt-cdragi
      swle=atan(ssa(la))
      cst=ctt/cos(swle)
      ccc(li)=cdragi
      crr(li)=ctt
      xcc4(li)=cst
      cdrag=cdrag+cdragi*const
      cthrus=cthrus+ctt*const
      csuct=csuct+cst*const
 190  continue
      tble=0
      ii=0
      li=0
      lblr=0
      do 220 i=1,2
      iamm=nma(i)
      do 200 j=1,iamm
      jj=j+(i-1)*nma(1)
      la=1+(j-1+(i-1)*nma(1))*nscwmi
      gam(j)=ccc(jj)
      xc4(j)=crr(jj)
      z(j)=xcc4(jj)
      phii(j)=yq(la)
 200  continue
      iuz=nsswsv(i)
      do 210 lblair=1,iuz
      li=li+1
      lu=1+tble
      lblr=lblr+1
      ybb=q(lu)
      ii=ii+1
      tble=tble+tblscw(ii)
      yoob=ybb/bot
      call ftlup(ybb,cdragi,+1,iamm,phii,gam,1000,1000)
      call ftlup(ybb,ctt,+1,iamm,phii,xc4,1000,1000)
      call ftlup(ybb,cst,+1,iamm,phii,z,1000,1000)
      ll=lblair+(i-1)*nsswsv(1)
      swale=atan(sswwa(ll))*57.29578
      if(ii.eq.(nsswsv(1)+1)) write (6,240)
      write (6,260) li,yoob,swale,cdragi,ctt,cst
      blair(lblr)=cst
 210  continue
      if (nsswsv(2).eq.0) go to 230
 220  continue
 230  cdcl2=cdrag/cldes**2
      write (6,270) cdcl2,cthrus,csuct
      return
c
 240  format (/1x,'contribution of the second planform to the chord',
     .' or drag force'/)
 250  format (////1x,'induced drag,leading edge thrust , suction ',
     .'coefficient characteristics'/10x,
     .'computed at the desired cl from a near field solution'
     .//28x,'section coefficients'/22x,'l.e. sweep'/1x,
     .'station',6x,' 2y/b',5x,'angle',5x,'cdii c/2b',3x,'ct c/2b',4x,
     .'cs c/2b')
  260 format (1x,i3,5x,5f11.5)
  270 format (///26x,'total coefficients'//10x,'cdii/cl**2=',f10.5,5x,
     1'ct=',f10.5,5x,'cs=',f10.5)
      end
c
c  *********************************************************************
c  *  subroutine called by main if it is desired to determine the      *
c  *  contributions to lift and drag and moment from separated flow    *
c  *  around the leading edge and/or side edges  (see sub. geomtr)     *
c  *********************************************************************
c
      subroutine tipsuc(cg)
      dimension yy(2), wvou(60), fv(2), fw(2), xtleg(60), cirsum(50),
     1           ylegsv(50), zlegsv(50)
      common /all/ bot,m,beta,ptest,qtest,tblscw(50),q(200),pn(200),
     1             pv(200),alp(200),s(200),psi(200),phi(50),zh(50)
      common /tothre/ cir(200,2)
      common /onethre/ twist(2),cref,sref,cave,cldes,strue,ar,artrue,
     1                 rtcdht(2),config,nsswsv(2),msv(2),kbot,plan,
     2                 iplan,mach,sswwa(50),xl(2),xt(2),clwb,cmcl,
     3                 cla(2),blair(50),clamar(2)
      common /threfor/ ccav(2,50),clt,clnt,nssw,alpd
      common /insub23/ apsi,aphi,xx,yyy,zz,snn,tolcsq
      dimension xkvsew(2), centr(2)
      real mach

      xkvsew(1)=0.0
      xkvsew(2)=0.0
      centr(1)=0.0
      centr(2)=0.0
      if (iplan.eq.1.and.xl(1).eq.xt(1)) go to 540
      if (xl(1).eq.xt(1).and.xl(2).eq.xt(2)) go to 540
      blamar=1./beta
      xt(1)=xt(1)*blamar
      xt(2)=xt(2)*blamar
      xl(1)=xl(1)*blamar
      xl(2)=xl(2)*blamar
c
c     the tolerance set at this point in the program may need to be
c     changed for computers other than the cdc 6000 series
c
      tolc=.0100*bot
      tolcsq=tolc*tolc
c
      tipsu=0.
      pitch=0.
      nssw=nsswsv(1)+nsswsv(2)
c
c     geometry for tip trailing legs
c
      itt=1
      im=0
      imm=0
      nssw1=0
      ccirs=0.
      nssw2=nsswsv(1)
      nssw3=nssw2
      l=1
      nscw=msv(1)/nsswsv(1)
      go to 20
10    nssw1=nsswsv(1)
      nssw2=nssw
      nssw3=nsswsv(2)
      l=nsswsv(1)+1
      nscw=msv(2)/nsswsv(2)
      if (xl(2).eq.xt(2)) go to 500
20    i=imm+1
      j=imm+2
      iuu=2
      aphi=atan(phi(im+1))
      sa=sin(aphi)
      ca=cos(aphi)
      tlx1=pn(i)-s(i)*tan(psi(i))*ca
      tlx2=pn(j)-s(j)*tan(psi(j))*ca
      clftlg=tlx1-tlx2
      xtleg(1)=tlx1/2.+tlx2/2.
      yleg=q(i)-s(i)*ca
      if (nssw1.eq.0) ylegsv(1)=yleg
      zleg=zh(im+1)-s(i)*sa
      if (nssw1.eq.0) zlegsv(1)=zleg
      if (xl(itt).eq.xt(itt)) go to 100
      do 30 nv=2,nscw
      nvt=nv-1
30    xtleg(nv)=xtleg(nvt)-clftlg
      nctl=0
      na=1
      nb=nscw
40    do 70 nv=na,nb
c
c
c     the ratio of w/u is initialized to -1 because in the term
c     -u*alpha/u, used in this summation, alpha is set to 1 radian
c     so that the resulting tip suction can be used directly to find
c     kv side edge
c
c
      wvou(nv)=-1.
      iz=1
      nnn=tblscw(iz)
      do 60 nn=1,m
      aphi=atan(phi(iz))
      apsi=psi(nn)
      xx=xtleg(nv)-pn(nn)
      yy(1)=yleg-q(nn)
      yy(2)=yleg+q(nn)
      zz=zleg-zh(iz)
      snn=s(nn)
      do 50 i=1,2
      yyy=yy(i)
      call infsub(bot,fv(i),fw(i))
      aphi=-aphi
      apsi=-apsi
50    continue
      wvou(nv)=wvou(nv)+(fw(1)+fw(2))*cir(nn,2)/12.5663704
      if (nn.lt.nnn.or.nn.eq.m) go to 60
      iz=iz+1
      nnn=nnn+tblscw(iz)
60    continue
70    continue
      nctl=nctl+1
      if (nctl-2) 80,100,150
c
c     geometry for spanwise bound vortices
c
80    na=nscw+1
      nb=2*nscw
      ja=imm+1
      yleg=q(ja)
      zleg=zh(im+1)
      do 90 j=1,nscw
      jk=imm+j
      nv=j+nscw
90    xtleg(nv)=pn(jk)
      go to 40
c
c     geometry along right trailing legs
c
100   na=2*nscw+1
      nb=3*nscw
      ccirs=0.
      jk = imm+1
      aphi = atan(phi(im+1))
      sa = sin(aphi)
      ca = cos(aphi)
      yleg = q(jk)+s(jk)*ca
      if (nssw1 .eq. 0) ylegsv(iuu) = yleg
      zleg = zh(im+1) + s(jk) *sa
      if (nssw1 .eq. 0) zlegsv(iuu) = zleg
      if (xl(itt) .eq. xt(itt)) go to 150
      tlx1 = pn(jk) + s(jk) *tan(psi(jk))*ca
      jk  = jk+1
      tlx2 = pn(jk) + s(jk) *tan(psi(jk))*ca
      crttlg = tlx1 - tlx2
      xtleg(na) = tlx1/2. + tlx2/2.
      naa = na +1
      if (nssw1 .eq. nsswsv(1)) go to 110
      go to 130
 110  do 120 it =2,l
      iq = it -1
      if ((abs(ylegsv(it)-yleg) .lt. tolc) .and. (abs(zlegsv(it)-zleg)
     +.lt. tolc)) ccirs = cirsum(iq)
      if (ccirs .ne. 0.) go to 130
 120  continue
 130  do 140 nv = naa,nb
      nvt = nv -1
 140  xtleg(nv) = xtleg(nvt) - crttlg
      go to 40
c
 150  continue
      if (ccirs .ne. 0.) go to 160
      go to 270
 160  ij = 2*nscw+1
      xlt = xtleg(1) + clftlg/2.
      xrt = xtleg(ij) + crttlg/2.
      xll = xlt + clftlg/4.
      xrl = xrt + crttlg/4.
      if (xll .ge. xl(itt) .and. xlt .le. xt(itt)) go to 170
      if (xll .le. xl(itt) .and. xlt .ge. xt(itt)) go to 190
      if (xll .gt. xl(itt) .and. xlt .ge. xl(itt)) go to 200
      if (xll .le. xt(itt)) go to 200
      if (xll .gt. xl(itt) .and. xlt .lt. xl(itt)) go to 180
      con4 = (xt(itt) -xll)/(xlt-xll)
      go to 210
 170  con4 = (xl(itt)-xt(itt))/(xll-xlt)
      go to 210
 180  con4 = (xl(itt) - xlt)/(xll-xlt)
      go to 210
 190  con4 = 1.
      go to 210
 200  con4= 0.0
 210  continue
      if (xrl .ge. xl(itt) .and. xrt .le. xt(itt)) go to 220
      if (xrl .le. xl(itt) .and. xrt .ge. xt(itt)) go to 240
      if (xrl .gt. xl(itt) .and. xrt .ge. xl(itt)) go to 250
      if (xrl .le. xt(itt)) go to 250
      if (xrl .gt. xl(itt) .and. xrt .lt. xl(itt)) go to 230
      con5 = (xt(itt)-xrl)/(xrt-xrl)
      go to 260
 220  con5 = (xl(itt) - xt(itt))/(xrl-xrt)
      go to 260
 230  con5 = (xl(itt) - xrt)/(xrl-xrt)
      go to 260
 240  con5 = 1.
      go to 260
 250  con5 = 0.0
 260  continue
      tipsu=tipsu+ccirs*0.25*(con4*wvou(1)*clftlg-con5*wvou(ij)*crttlg)*
     +2./sref*beta
      pitch=pitch+ccirs*0.25*(-con4*wvou(1)*clftlg*xtleg(1)+con5*wvou(ij
     +)*crttlg*xtleg(ij))*2./(sref*cref)*beta**2
 270  circus = ccirs
      do 460 npos=1,nscw
      jk=imm + npos
      jn=2*nscw+npos
      npis = nscw+npos
      circus = circus + cir(jk,2)
      if (xl(itt) .eq. xt(itt)) go to 460
      xlleg = xtleg(npos)
      xrleg = xtleg(jn)
      xll = xtleg(npos) + clftlg/2.
      xlt = xtleg(npos) - clftlg/2.
      xrl = xtleg(jn) + crttlg/2.
      xrt = xtleg(jn) - crttlg/2.
      if (xll .ge. xl(itt) .and. xlt .le. xt(itt)) go to 280
      if (xll .le. xl(itt) .and. xlt .ge. xt(itt)) go to 300
      if (xll .gt. xl(itt) .and. xlt .ge. xl(itt)) go to 310
      if (xll .le. xt(itt)) go to 310
      if (xll .gt. xl(itt) .and. xlt .lt. xl(itt)) go to 290
      con1 = (xt(itt)-xll)/(xlt-xll)
      xlleg = xt(itt) + con1 * clftlg/2.
      go to 320
 280  con1 = (xl(itt) - xt(itt))/(xll-xlt)
      xlleg = (xl(itt) + xt(itt))/2.
      go to 320
 290  con1 = (xl(itt) - xlt)/(xll-xlt)
      xlleg = xlt + con1*clftlg/2.
      go to 320
 300  con1 = 1.
      go to 320
 310  con1 = 0.0
 320  continue
      if (npos .eq. nscw .and. con1 .eq. 1.) go to 360
      if (xrl .ge. xl(itt) .and. xrt .le. xt(itt)) go to 330
      if (xrl .le. xl(itt) .and. xrt .ge. xt(itt)) go to 350
      if (xrl .gt. xl(itt) .and. xrt .ge. xl(itt)) go to 370
      if (xrl .le. xt(itt)) go to 370
      if (xrl .gt. xl(itt) .and. xrt .lt. xl(itt)) go to 340
      con2 = (xt(itt)-xrl)/(xrt-xrl)
      xrleg = xt(itt) + con2*crttlg/2.
      go to 380
 330  con2 = (xl(itt) - xt(itt))/(xrl-xrt)
      xrleg = (xl(itt) + xt(itt))/2.
      go to 380
 340  con2 = (xl(itt) - xrt)/(xrl-xrt)
      xrleg = xrt + con2 * crttlg/2.
      go to 380
 350  con2 = 1.
      go to 380
 360  con1 = .75
      con2 = .75
      go to 380
 370  con2 = 0.0
 380  if (xrl .gt. xll) go to 390
      xsign = -1.0
      xbll = xll
      xblt = xrl
      go to 400
 390  xbll  = xrl
      xblt = xll
      xsign = 1.
 400  bvdlg = abs(xbll - xblt)
      if (xblt .ge. xl(itt)) go to 440
      if (xbll .le. xt(itt)) go to 440
      if (xbll .ge. xl(itt) .and. xblt .le. xt(itt)) go to 430
      if (xbll .le. xl(itt) .and. xblt .ge. xt(itt)) go to 420
      if (xbll .gt. xl(itt) .and. xblt .ge. xt(itt)) go to 410
      con3 = (xt(itt) - xbll)/(xblt-xbll)
      xtleg(npis) = xt(itt) + con3*bvdlg/2.
      con3 = con3*xsign
      go to 450
 410  con3 = (xl(itt) - xblt)/(xbll- xblt)
      xtleg(npis) = xblt + con3*bvdlg/2.
      con3 = con3*xsign
      go to 450
 420  con3 = 1.*xsign
      go to 450
 430  con3 = (xl(itt) - xt(itt))/(xbll - xblt)
      xtleg(npis) = (xl(itt) + xt(itt))/2.
      con3 = con3*xsign
      go to 450
 440  con3 = 0.0
 450  tipsu=tipsu+(circus*(wvou(npos)*clftlg*con1-con2*wvou(jn)*crttlg)+
     1      cir(jk,2)*(wvou(npis)*con3*bvdlg))*2./sref*beta
      pitch=pitch+(circus*(-wvou(npos)*clftlg*con1*xlleg+wvou(jn)*con2*
     1      crttlg*xrleg) - cir(jk,2)*
     2     (wvou(npis)*con3*bvdlg*xtleg(npis)))*2./(sref*cref)*beta**2
 460  continue
      im = im+1
      imm = imm + tblscw(im)
      if (nssw1 .eq. 0) cirsum(im) = circus
      if (nssw1 .eq. 0) iuu = im + 2
      if (im .eq. nsswsv(1)) go to 470
      if (xl(itt) .eq. xt(itt)) go to 100
      go to 480
 470  ctsw = tipsu
      cmw = pitch
      if (nssw2 .eq. nssw) go to 520
      itt = 2
      go to 10
 480  if (im .eq. nssw) go to 500
      nctl = 1
      do 490 nv = 1,nscw
      clftlg = crttlg
      ny = nv + 2*nscw
      xtleg(nv) = xtleg(ny)
 490  wvou(nv) = wvou(ny)
      go to 80
 500  xkvsew(2) = 2.*abs(tipsu-ctsw)
      if (xkvsew(2) .lt. 0.000001) go to 510
      centr(2) = (pitch - cmw)*cref/abs(tipsu-ctsw)
      go to 520
 510  centr(2) = 0.0
 520  xkvsew(1) = 2.*abs(ctsw)
      if (xkvsew(1) .lt. 0.000001) go to 530
      centr(1) = cmw*cref/abs(ctsw)
      go to 540
 530  centr(1) = 0.0
 540  call wrtran (xkvsew,centr,cg)
      return
      end
c
c  *********************************************************************
c  *  subroutine called by tipsuc                                      *
c  *********************************************************************
c
      subroutine wrtran (xkvsew,centr,cg)
      common /all/ bot,m,beta,ptest,qtest,tblscw(50),q(200),pn(200),
     1             pv(200),alp(200),s(200),psi(200),phi(50),zh(50)
      common /onethre/ twist(2),cref,sref,cave,cldes,strue,ar,artrue,
     1                 rtcdht(2),config,nsswsv(2),msv(2),kbot,plan,
     2                 iplan,mach,sswwa(50),xl(2),xt(2),
     3                 clwb,cmcl,cla(2),blair(50),clamar(2)
      common /threfor/ ccav(2,50),clt,clnt,nssw,alpd
      dimension xkv(2),xkp(2),ychlo(2),ychhi(2),centr(2),cent(2),
     1          xkvsew(2)
      real mach
c
      lch=0
      lamar = nsswsv(1)
      sumy   = 0.0
      sum    = 0.0
      conv   = 3.1415926536/180.
      cinv    = 1./(3.1415926536*ar)
      delta   = 2.*conv
      const   = 16.*bot/sref
      alpha    = alpd*conv
      s22      = alpha**2
      kbt      = 1
      if (kbot.eq.1) kbt = 2
      if (iplan.eq.1) go to 10
      xkp(kbot) = clwb/alpha
      xkp(kbt) = (cldes - clwb)/alpha
      go to 20
   10 xkp(1)   = cla(2)
   20 read(5,180) ychlo(1),ychhi(1),ychlo(2),ychhi(2)
      nch  = 1
      nc1  = nsswsv(1)
      do 40 j = 1,nc1
      if ( q(nch) .gt. ychlo(1)) go to 30
      if ( q(nch) .lt. ychhi(1)) go to 30
      sum = sum + (pn(nch)+(pn(nch) - pn(nch+1))/4.)*beta*blair(j)*
     1        s(nch)*const
      sumy = sumy + blair(j)*s(nch)*const
      if (j.eq.nc1) go to 40
   30 nch = nch + tblscw(j)
   40 continue
      xkv(1) = sumy/s22
      if (xkv(1).lt.0.000001) go to 50
      cent(1) = sum/sumy
      go to 60
   50 cent(1) = 0.0
   60 continue
      if (iplan.eq.1) go to 100
      sumy  = 0.0
      sum   = 0.0
      nch   = msv(1) + 1
      nc2   = nsswsv(2)
      do 80 j = 1,nc2
      if ( q(nch) .gt. ychlo(2)) go to 70
      if ( q(nch) .lt. ychhi(2)) go to 70
      sum = sum + (pn(nch)+(pn(nch) - pn(nch+1))/4.)*beta*blair(nc1+j)*
     1        s(nch)*const
      sumy = sumy +blair(nc1+j)*s(nch)*const
      if (j.eq.nc2) go to 80
   70 nch = nch+tblscw(j+lamar)
   80 continue
      xkv(2) = sumy/s22
      if (xkv(2).lt.0.000001) go to 90
      cent(2) = sum/sumy
      go to 100
   90 cent(2) = 0.0
  100 continue
      write(6,190)
      do 110 ik = 1, iplan
      centpm = clamar(ik)*cref
      write(6,200) ik
      write(6,210) xkp(ik),centpm+cg
      write(6,220) xkv(ik),cent(ik)+cg
  110 write(6,230) xkvsew(ik),centr(ik)+cg
  120 continue
c
      do 160 ik = 1,iplan
      if (lch.eq.1) go to 130
      write(6,240) ik
  130 write(6,250)
      alpha = 0.0
      do 160 j = 1,26
      v = sin(alpha)
      c = cos(alpha)
      c2 = c**2
      s2 = v**2
      clp = xkp(ik)*v*c2
      clvl = clp+xkv(ik)*c*s2
      clsl = clp+xkvsew(ik)*c*s2
      cltot = clvl+xkvsew(ik) *c*s2
      if (lch.eq.0) go to 140
      cmp = v*c*(xkp(2)*clamar(2)+(xkp(1)-xkp(2))*clamar(1))
      cmpl = cmp + s2/cref*(cent(2)*xkv(2)+(xkv(1)-xkv(2))*cent(1))
      cmps = cmp + s2/cref*(centr(2)*xkvsew(2) + (xkvsew(1) - xkvsew(2))
     1           * centr(1))
      cmtot = cmpl + cmps - cmp
      go to 150
  140 cmp = clamar(ik) * xkp(ik)*v*c
      cmpl = cmp + cent(ik)*xkv(ik)*s2/cref
      cmps = cmp + xkvsew(ik) * centr(ik) * s2/cref
      cmtot = cmpl + xkvsew(ik) * centr(ik) * s2/cref
  150 cdi = cltot * tan(alpha)
      cdii = (cltot**2)*cinv
      alph1 = alpha/conv
      cntt  = cltot/c
      write(6,260) alph1,cntt,clp,clvl,clsl,cltot
  160 alpha = alpha + delta
c
      do 165 ik = 1,iplan
      if (lch.eq.1) go to 135
      write(6,240) ik
  135 write(6,251)
      alpha = 0.0
      do 165 j = 1,26
      v = sin(alpha)
      c = cos(alpha)
      c2    = c**2
      s2    = v**2
      clp   = xkp(ik)*v*c2
      clvl  = clp+xkv(ik)*c*s2
      clsl  = clp+xkvsew(ik)*c*s2
      cltot = clvl+xkvsew(ik) *c*s2
      if (lch.eq.0) go to 145
      cmp   = v*c*(xkp(2)*clamar(2)+(xkp(1)-xkp(2))*clamar(1))
      cmpl  = cmp + s2/cref*(cent(2)*xkv(2)+(xkv(1)-xkv(2))*cent(1))
      cmps  = cmp + s2/cref*(centr(2)*xkvsew(2) + 
     1              (xkvsew(1) - xkvsew(2))* centr(1))
      cmtot = cmpl + cmps - cmp
      go to 155
  145 cmp   = clamar(ik) * xkp(ik)*v*c
      cmpl  = cmp + cent(ik)*xkv(ik)*s2/cref
      cmps  = cmp + xkvsew(ik) * centr(ik) * s2/cref
      cmtot = cmpl + xkvsew(ik) * centr(ik) * s2/cref
  155 cdi   = cltot * tan(alpha)
      cdii  = (cltot**2)*cinv
      alph1 = alpha/conv
      cntt  = cltot/c
      write(6,261) alph1,cmp,cmpl,cmps,cmtot,cdi,cdii
  165 alpha = alpha + delta
c
      if (iplan.eq.1) go to 170
      iplan     = 1
      lch       = 1
      write(6,280)
      xkp(1)    = xkp(1) + xkp(2)
      xkv(1)    = xkv(1) + xkv(2)
      xkvsew(1) = xkvsew(1) + xkvsew(2)
      go to 120
  170 write(6,270)
      iplan     = plan
c
  180 format(4f10.5)
  190 format(////6x,'kp , kv and respective chordwise centroids',
     1       1x,'for each planform')
  200 format(///,21x,12hplanform no.,i2/)
  210 format(10x,3hkp=,f10.5,10x,11hcentroid at,f10.5)
  220 format(7x,6hkv le=,f10.5,10x,11hcentroid at,f10.5)
  230 format(7x,6hkv se=,f10.5,10x,11hcentroid at,f10.5)
  240 format(1h1,////,13x,40hperformance characteristics for planform,
     1       i2)
  250 format(//8x,5halpha,6x,2hcn,8x,3hclp,4x,9hclp+clvle,1x,
     1       9hclp+clvse,4x,2hcl/)
  251 format(//5x,5halpha,6x,3hcmp,4x,9hcmp+cmvle,1x,9hcmp+cmvse,
     2       4x,2hcm,8x,2hcd,6x,'cl**2/'/63x,'(pi*ar)'/)
  260 format(3x,6f10.4)
  261 format(7f10.4)
  270 format(///,20x,21hThis case is finished)
  280 format(////,18x,33htotal performance characteristics)
      return
      end

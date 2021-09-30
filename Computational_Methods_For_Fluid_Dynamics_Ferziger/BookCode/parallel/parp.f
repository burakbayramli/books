c#########################################################
      program comet
c#########################################################
c
c        1994   M. Peric, E. Schreck, IfS, Hamburg
c#########################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /logic/ lwrite,lread,ltest,laxis,louts,loute,
     *       lcal(nfi),ltime
      common /contro/ iu,iv,ip,ien,ipr,jpr,nsw(nfi),sor(nfi),
     *       resor(nfi),urf(nfi),urpp,gam,beta,gravx,gravy,
     *       gds(nfi),snor(nfi),sormax,slarge,alfa,great,small
      common /var/ densit,visc,prr,flomas,flomom,tref,u(nxy),
     *       v(nxy),p(nxy),pp(nxy),t(nxy),f1(nxy),f2(nxy),
     *       dpx(nxy),dpy(nxy)
      common /geo/ x(nx),y(ny),xc(nx),yc(ny),fx(nx),fy(ny),r(ny)
      common /varold/ itim,itst,time,dt,dtr,uo(nxy),vo(nxy),
     *       to(nxy)
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      common /com/ mytid,itids(100)
      logical lwrite,lread,ltest,laxis,louts,loute,lcal,ltime
      character*20 filres,filin,filout
c
c.... PVM initialization
c
      nprx=2
      npry=2
      nproc=nprx*npry
c
      call initco
c
c......i/o file names (each process has its own files)
c
      write(filin,'(5hdata_,i1)') me
      write(filout,'(4hout_,i1)') me 
      write(filres,'(4hres_,i1)') me 
      open (unit=5,file=filin)
      open (unit=2,file=filout)
      open (unit=3,file=filres,form='unformatted')
      rewind 5
      rewind 3
      rewind 2
c
c..... input and initializations
c
      call modinp
c
c..... initial output
c
      call out1
c
c...... time steps (unsteady flow calculation)
c
      itim=0
      time=0.
      if(lread) then
      read(3) itim,time,(f1(ij),ij=1,nij),(f2(ij),ij=1,nij),
     *        (u(ij),ij=1,nij),(v(ij),ij=1,nij),(p(ij),ij=1,nij),
     *        (t(ij),ij=1,nij)
      rewind 3
      endif
c
      itims=itim+1
      itime=itim+itst
      do 500 itim=itims,itime
      time=time+dt
c
      if(ltime) then
       do ij=1,nij
        to(ij)=t(ij)
        uo(ij)=u(ij)
        vo(ij)=v(ij)
       end do
      endif
c
      write(2,*) '          time = ',time
      write(2,*) '          *****************************'
      write(2,*) '  '
c
c......boundary conditions
c
      call bcin
      if (louts) call outres
c
c......find monitoring position
c
      ijmon=li(imon)+jmon
      write(2,600) imon,jmon
c
c......simple relaxations
c
      do iter=1,maxit
       if(lcal(iu)) call calcuv
       if(lcal(ip)) call calcp
       if(lcal(ien)) call calct
       do l=1,nfi
        resor(l)=resor(l)*snor(l)
       end do
c
c..... Collect residuals to master
c
       call collect(resor,nfi,10)
c
       write(2,606) iter,resor(iu),resor(iv),resor(ip),
     *       resor(ien),u(ijmon),v(ijmon),p(ijmon),t(ijmon)
       source=amax1(resor(iu),resor(iv),resor(ip),resor(ien))
c
c..... Broadcast source to all processes
c
      call broadc(source,11)
c
       if(source.gt.slarge) go to 510
       if(source.lt.sormax) go to 250
      end do
  250 continue
c
c......output of data
c
      if(loute) call outres
      call out2
      if(lwrite) then
       write(3) itim,time,(f1(ij),ij=1,nij),(f2(ij),ij=1,nij),
     *     (u(ij),ij=1,nij),(v(ij),ij=1,nij),(p(ij),ij=1,nij),
     *     (t(ij),ij=1,nij)
      endif
  500 continue
c
c.... Finished - exit PVM
c 
      call exitnet
      stop
c
c......Divergence: print message and exit PVM 
c
  510 print *,'     *** terminated - inner iterations diverge ***'
      call exitnet
      stop
c
c......format specifications
c
  600 format(1x,'iter',3x,
     *'i-----------absolute residual source sums----------i',3x,
     *'i----field values at monitoring location(',i2,',',i3,',',i3,
     *')--i',/,1x,3('no.',2x),5x,'umom',6x,'vmom',6x,'mass',6x,'ener',
     *26x,'u',9x,'v',9x,'p',9x,'t',/)
  606 format(2x,i4,4x,1p4e10.3,10x,1p4e10.3)
      end
c
c
c#######################################################
      subroutine calcuv
c#######################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /logic/ lwrite,lread,ltest,laxis,louts,loute,
     *       lcal(nfi),ltime
      common /contro/ iu,iv,ip,ien,ipr,jpr,nsw(nfi),sor(nfi),
     *       resor(nfi),urf(nfi),urpp,gam,beta,gravx,gravy,
     *       gds(nfi),snor(nfi),sormax,slarge,alfa,great,small
      common /var/ densit,visc,prr,flomas,flomom,tref,u(nxy),
     *       v(nxy),p(nxy),pp(nxy),t(nxy),f1(nxy),f2(nxy),
     *       dpx(nxy),dpy(nxy)
      common /geo/ x(nx),y(ny),xc(nx),yc(ny),fx(nx),fy(ny),r(ny)
      common /coef/ ae(nxy),aw(nxy),an(nxy),as(nxy),ap(nxy),
     *       su(nxy),sv(nxy),apu(nxy),apv(nxy),ae1,aw1,an1,as1
      common /varold/ itim,itst,time,dt,dtr,uo(nxy),vo(nxy),
     *       to(nxy)
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      logical lwrite,lread,ltest,laxis,louts,loute,lcal,ltime
c
      urfu=1./urf(iu)
      urfv=1./urf(iv)
      gam=gds(iu)
c
c..... boundary pressure
c
      call pbound(p)
c
c..... initialize temporarily stored variables
c
      do ij=1,nij
       su(ij)=0.
       sv(ij)=0.
       apu(ij)=0.
       apv(ij)=0.
       dpx(ij)=0.
       dpy(ij)=0.
      end do
c
c..... east cv face fluxes
c
      do i=2,nim-1
      do j=2,njm
       ij=li(i)+j
       ipj=ij+nj
       call fluxe(i,j,1.)
       su(ij)=su(ij)+gam*ae1*(u(ipj)-u(ij))
       su(ipj)=su(ipj)+gam*aw1*(u(ij)-u(ipj))
       sv(ij)=sv(ij)+gam*ae1*(v(ipj)-v(ij))
       sv(ipj)=sv(ipj)+gam*aw1*(v(ij)-v(ipj))
      end do
      end do
c
c..... north cv face
c
      do i=2,nim
      do j=2,njm-1
       ij=li(i)+j
       ijp=ij+1
       call fluxn(i,j,1.)
       su(ij)=su(ij)+gam*an1*(u(ijp)-u(ij))
       su(ijp)=su(ijp)+gam*as1*(u(ij)-u(ijp))
       sv(ij)=sv(ij)+gam*an1*(v(ijp)-v(ij))
       sv(ijp)=sv(ijp)+gam*as1*(v(ij)-v(ijp))
      end do
      end do
c
c.....volume integrals (source terms)
c
      do i=2,nim
       dx=x(i)-x(i-1)
       ii=li(i)
c
      do j=2,njm
       dy=y(j)-y(j-1)
       rp=0.5*(r(j)+r(j-1))
       vol=dx*dy*rp
       ij=ii+j
c
c...... pressure source
c
       pe=p(ij+nj)*fx(i)+p(ij)*(1.-fx(i))
       pw=p(ij)*fx(i-1)+p(ij-nj)*(1.-fx(i-1))
       pn=p(ij+1)*fy(j)+p(ij)*(1.-fy(j))
       ps=p(ij)*fy(j-1)+p(ij-1)*(1.-fy(j-1))
       dpx(ij)=(pe-pw)/dx
       dpy(ij)=(pn-ps)/dy
       su(ij)=su(ij)-dpx(ij)*vol
       sv(ij)=sv(ij)-dpy(ij)*vol
c
c..... buoyancy source contribution
c
       if(lcal(ien)) then
        sb=beta*densit*vol*(t(ij)-tref)
        su(ij)=su(ij)+gravx*sb
        sv(ij)=sv(ij)+gravy*sb
       endif
c
c..... axisymmetric contribution
c
       if(laxis) then
        apv(ij)=apv(ij)+visc*vol/rp**2
       endif
c
c..... unsteady term contribution to ap and su
c
       if(ltime) then
        apt=densit*vol*dtr
        su(ij)=su(ij)+apt*uo(ij)
        sv(ij)=sv(ij)+apt*vo(ij)
        apv(ij)=apv(ij)+apt
        apu(ij)=apu(ij)+apt
       endif
      end do
      end do
c
c.....problem modifications - boundary conditions
c
      call bcuv
c
      if(ltest) then
        call print(ae,'  ae  ')
        call print(aw,'  aw  ')
        call print(as,'  as  ')
        call print(an,'  an  ')
        call print(su,'  su  ')
        call print(sv,'  sv  ')
        call print(apu,'  apu ')
        call print(apv,'  apv ')
        call print(dpx,'  dpx ')
        call print(dpy,'  dpy ')
      endif
c
c.....solving equation system for u-velocity
c
      do i=2,nim
      do ij=li(i)+2,li(i)+njm
       ap(ij)=(ae(ij)+aw(ij)+an(ij)+as(ij)+apu(ij))*urfu
       su(ij)=su(ij)+(1.-urf(iu))*ap(ij)*u(ij)
       apu(ij)=1./ap(ij)
      end do
      end do
c
      if(ksw.eq.0) then
       call solgs(u,iu)
      else
       call sipsol(u,iu)
      endif
c
c.....solving equation system for v-velocity
c
      do i=2,nim
      do ij=li(i)+2,li(i)+njm
       ap(ij)=(ae(ij)+aw(ij)+an(ij)+as(ij)+apv(ij))*urfv
       su(ij)=sv(ij)+(1.-urf(iv))*ap(ij)*v(ij)
       apv(ij)=1./ap(ij)
      end do
      end do
c
      if(ksw.eq.0) then
       call solgs(v,iv)
      else
       call sipsol(v,iv)
      endif
c
c..... exchange apu,apv,dpx & dpy along block interfaces
c
      if(nproc.gt.1) then
       call exch(apu)
       call exch(apv)
       call exch(dpx)
       call exch(dpy)
      endif
c
      if(ltest) then
        call print(apu,'  apu ')
        call print(apv,'  apv ')
        call print(dpx,'  dpx ')
        call print(dpy,'  dpy ')
      endif
      return
      end 
c
c
c########################################################## 
      subroutine calcp 
c########################################################## 
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /logic/ lwrite,lread,ltest,laxis,louts,loute,
     *       lcal(nfi),ltime
      common /contro/ iu,iv,ip,ien,ipr,jpr,nsw(nfi),sor(nfi),
     *       resor(nfi),urf(nfi),urpp,gam,beta,gravx,gravy,
     *       gds(nfi),snor(nfi),sormax,slarge,alfa,great,small
      common /var/ densit,visc,prr,flomas,flomom,tref,u(nxy),
     *       v(nxy),p(nxy),pp(nxy),t(nxy),f1(nxy),f2(nxy),
     *       dpx(nxy),dpy(nxy)
      common /geo/ x(nx),y(ny),xc(nx),yc(ny),fx(nx),fy(ny),r(ny)
      common /coef/ ae(nxy),aw(nxy),an(nxy),as(nxy),ap(nxy),
     *       su(nxy),sv(nxy),apu(nxy),apv(nxy),ae1,aw1,an1,as1
      common /varold/ itim,itst,time,dt,dtr,uo(nxy),vo(nxy),
     *       to(nxy)
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      logical lwrite,lread,ltest,laxis,louts,loute,lcal,ltime
c
c..... initialize fields
c
      sum=0.
c
      do ij=1,nij
       su(ij)=0.
       ap(ij)=0.
      end do
c
c..... east CV faces
c
      do i=2,nim-1
      do j=2,njm
       call eastp(i,j)
      end do
      end do
c
c..... north CV faces
c
      do i=2,nim
      do j=2,njm-1
       call nordp(i,j)
      end do
      end do
c
c..... boundary conditions & exchange of coeff.
c
      call bcpp
c
c..... sorce term and central coeff.
c
      fac=1./urpp
c
      do i=2,nim
      do ij=li(i)+2,li(i)+njm
       su(ij)=f1(ij-nj)-f1(ij)+f2(ij-1)-f2(ij)
       ap(ij)=(ae(ij)+aw(ij)+an(ij)+as(ij)+ap(ij))*fac
       sum=sum+su(ij)
      end do
      end do
c
      do ij=1,nij
       dpx(ij)=0.
       dpy(ij)=0.
       pp(ij)=0.
      end do
c
c
      if(ltest) then
        call print(f1,'  f1  ')
        call print(f2,'  f2  ')
      endif
      if (ltest) write(2,*) '       sum = ',sum
c
c..... solve equations system for p'
c
      if(ksw.eq.0) then
       call solgs(pp,ip)
      else
       call sipsol(pp,ip)
      endif
      if(ltest) call print(pp,'  pp  ')
c
c..... pressure correction at boundaries
c
      call pbound(pp)
      ijpref=li(ipr)+jpr
      ppo=pp(ijpref)
c
c.....Broadcast PPO to all processes
c
      call broadc(ppo,12)
c
c.....correct east mass fluxes 
c
      do i=1,nim
      do ij=li(i)+2,li(i)+njm
       f1(ij)=f1(ij)-ae(ij)*(pp(ij+nj)-pp(ij))
      end do
      end do
c
c.....correct north mass fluxes 
c
      do i=2,nim
      do ij=li(i)+1,li(i)+njm
       f2(ij)=f2(ij)-an(ij)*(pp(ij+1)-pp(ij))
      end do
      end do
c
c.....correct pressure and velocities
c
      do i=2,nim
       dx=x(i)-x(i-1)
       ii=li(i)
      do j=2,njm
       ij=ii+j
       rp=0.25*(r(j)+r(j-1))
       dy=y(j)-y(j-1)
       ppe=pp(ij+nj)*fx(i)+pp(ij)*(1.-fx(i))
       ppw=pp(ij)*fx(i-1)+pp(ij-nj)*(1.-fx(i-1))
       ppn=pp(ij+1)*fy(j)+pp(ij)*(1.-fy(j))
       pps=pp(ij)*fy(j-1)+pp(ij-1)*(1.-fy(j-1))
       u(ij)=u(ij)-(ppe-ppw)*dy*rp*apu(ij)
       v(ij)=v(ij)-(ppn-pps)*dx*rp*apv(ij)
       p(ij)=p(ij)+urf(ip)*(pp(ij)-ppo)
      end do
      end do
c
c..... exchange corrected velocities and pressure
c
      if(nproc.gt.1) then
       call exch(u)
       call exch(v)
       call exch(p)
      endif
      return
      end
c
c
c######################################################
      subroutine calct
c######################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /logic/ lwrite,lread,ltest,laxis,louts,loute,
     *       lcal(nfi),ltime
      common /contro/ iu,iv,ip,ien,ipr,jpr,nsw(nfi),sor(nfi),
     *       resor(nfi),urf(nfi),urpp,gam,beta,gravx,gravy,
     *       gds(nfi),snor(nfi),sormax,slarge,alfa,great,small
      common /var/ densit,visc,prr,flomas,flomom,tref,u(nxy),
     *       v(nxy),p(nxy),pp(nxy),t(nxy),f1(nxy),f2(nxy),
     *       dpx(nxy),dpy(nxy)
      common /geo/ x(nx),y(ny),xc(nx),yc(ny),fx(nx),fy(ny),r(ny)
      common /coef/ ae(nxy),aw(nxy),an(nxy),as(nxy),ap(nxy),
     *       su(nxy),sv(nxy),apu(nxy),apv(nxy),ae1,aw1,an1,as1
      common /varold/ itim,itst,time,dt,dtr,uo(nxy),vo(nxy),
     *       to(nxy)
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      logical lwrite,lread,ltest,laxis,louts,loute,lcal,ltime
c      
      urfi=1./urf(ien)
      gam=gds(ien)
c
c.....initialisation of temporarily stored variables
c
      do ij=1,nij
       su(ij)=0.
       ap(ij)=0.
      end do
c
c..... east cv faces
c
      do i=2,nim-1
      do j=2,njm
       ij=li(i)+j
       ipj=ij+nj
       call fluxe(i,j,prr)
       su(ij)=su(ij)+gam*ae1*(t(ipj)-t(ij))
       su(ipj)=su(ipj)+gam*aw1*(t(ij)-t(ipj))
      end do
      end do
c
c..... north cv faces
c
      do i=2,nim
      do j=2,njm-1
       ij=li(i)+j
       ijp=ij+1
       call fluxn(i,j,prr)
       su(ij)=su(ij)+gam*an1*(t(ijp)-t(ij))
       su(ijp)=su(ijp)+gam*as1*(t(ij)-t(ijp))
      end do
      end do
c
c..... volume integrals 
c
      do i=2,nim
       dx=x(i)-x(i-1)
       ii=li(i)
      do j=2,njm
       ij=ii+j
       dy=y(j)-y(j-1)
       rp=0.5*(r(j)+r(j-1))
       vol=dx*dy*rp
c
c..... unsteady term
c
       if(ltime) then
        apt=densit*vol*dtr
        su(ij)=su(ij)+apt*to(ij)
        ap(ij)=ap(ij)+apt
       endif
      end do
      end do
c
c..... boundary conditions      
c
      call modt
c
c.....assemble ap and su
c
      do i=2,nim
      do ij=li(i)+2,li(i)+njm
       ap(ij)=(aw(ij)+ae(ij)+an(ij)+as(ij)+ap(ij))*urfi
       su(ij)=su(ij)+(1.-urf(ien))*ap(ij)*t(ij)
      end do
      end do
c
c.....solving equation system for t
c
      if(ksw.eq.0) then
       call solgs(t,ien)
      else
       call sipsol(t,ien)
      endif
      return
      end
c
c
c######################################################
      subroutine fluxes
c######################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /var/ densit,visc,prr,flomas,flomom,tref,u(nxy),
     *       v(nxy),p(nxy),pp(nxy),t(nxy),f1(nxy),f2(nxy),
     *       dpx(nxy),dpy(nxy)
      common /geo/ x(nx),y(ny),xc(nx),yc(ny),fx(nx),fy(ny),r(ny)
      common /coef/ ae(nxy),aw(nxy),an(nxy),as(nxy),ap(nxy),
     *       su(nxy),sv(nxy),apu(nxy),apv(nxy),ae1,aw1,an1,as1
c
c=================================
      entry eastp(i,j)
c=================================
c
c..... geometrical quantities
c
      ij=li(i)+j
      ipj=ij+nj
      dy=y(j)-y(j-1)
      re=0.5*(r(j)+r(j-1))
      fxp=1.-fx(i)
      de=densit*dy*re
c
c..... interpolated cell face center quantities
c
      dpxel=dpx(ipj)*fx(i)+dpx(ij)*fxp
      apue=apu(ipj)*fx(i)+apu(ij)*fxp
      uel=u(ipj)*fx(i)+u(ij)*fxp
c
c..... cell face center velocities and mass fluxes
c
      ue=uel-apue*dy*re*(p(ipj)-p(ij)-dpxel*(xc(i+1)-xc(i)))
      f1(ij)=de*ue
c
c..... coefficients of p' equation
c
      ae(ij)=de*apue*dy*re
      aw(ipj)=ae(ij)
      return
c
c=================================
      entry nordp(i,j)
c=================================
c
c..... geometrical quantities
c
      ij=li(i)+j
      ijp=ij+1
      dx=x(i)-x(i-1)
      fyp=1.-fy(j)
      dn=densit*dx*r(j)
c
c..... interpolated cell face center quantities
c
      apvn=apv(ijp)*fy(j)+apv(ij)*fyp
      dpynl=dpy(ijp)*fy(j)+dpy(ij)*fyp
      vnl=v(ijp)*fy(j)+v(ij)*fyp
c
c..... cell face center velocities and mass fluxes
c
      vn=vnl-apvn*dx*r(j)*(p(ijp)-p(ij)-dpynl*(yc(j+1)-yc(j)))
      f2(ij)=dn*vn
c
c..... coefficients of p' equation
c
      an(ij)=dn*apvn*dx*r(j)
      as(ijp)=an(ij)
      return
c
c===================================
      entry fluxe(i,j,fac)
c===================================
c
c..... geometrical quantities
c
      dy=y(j)-y(j-1)
      dxe=xc(i+1)-xc(i)
      re=0.5*(r(j)+r(j-1))
c
c..... diffusion and convection coefficients
c
      de=visc*fac*dy*re/dxe
      ce=amax1(-f1(ij),0.)
      cw=amax1(f1(ij),0.)
c
c..... UDS coefficients
c
      ae(ij)=de+ce
      aw(ij+nj)=de+cw
c
c..... (CDS - UDS) coeffcicients
c
      ae1=-(ce+f1(ij)*fx(i))
      aw1=-(cw-f1(ij)*(1.-fx(i)))
      return
c
c====================================
      entry fluxn(i,j,fac)
c====================================
c
c..... geometrical quantities
c
      dx=x(i)-x(i-1)
      dyn=yc(j+1)-yc(j)
c
c..... diffusion and convection coefficients
c
      dn=visc*fac*dx*r(j)/dyn
      cn=amax1(-f2(ij),0.)
      cs=amax1(f2(ij),0.)
c
c..... UDS coefficients
c
      an(ij)=dn+cn
      as(ij+1)=dn+cs
c
c..... source term - gam * (CDS - UDS)
c
      an1=-(cn+f2(ij)*fy(j))
      as1=-(cs-f2(ij)*(1.-fy(j)))
      return
      end
c
c
c#####################################################
      subroutine print(phi,hedphi)
c#####################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      dimension phi(nxy)
      character*6 hedphi
c
      write(2,20) hedphi
      is=-11
   10 is=is+12
      ie=is+11
      ie=min0(ni,ie)
      write(2,21) (i,i=is,ie)
      write(2,22)
      do j=nj,1,-1
       write(2,23) j,(phi(li(i)+j),i=is,ie)
      end do
      if(ie.lt.ni) go to 10
c
   20 format(2x,26('*-'),5x,a6,5x,26('-*'))
   21 format(3x,'i = ',i3,11i10)
   22 format(2x,'j')
   23 format(1x,i3,1p12e10.2)
      return
      end
c
c
c########################################################
      subroutine sipsol(fi,ifi)
c########################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /logic/ lwrite,lread,ltest,laxis,louts,loute,
     *       lcal(nfi),ltime
      common /contro/ iu,iv,ip,ien,ipr,jpr,nsw(nfi),sor(nfi),
     *       resor(nfi),urf(nfi),urpp,gam,beta,gravx,gravy,
     *       gds(nfi),snor(nfi),sormax,slarge,alfa,great,small
      common /coef/ ae(nxy),aw(nxy),an(nxy),as(nxy),ap(nxy),
     *       su(nxy),sv(nxy),apu(nxy),apv(nxy),ae1,aw1,an1,as1
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      logical lwrite,lread,ltest,laxis,louts,loute,lcal,ltime
      dimension fi(nxy),be(nxy),bw(nxy),bs(nxy),bn(nxy),
     *          bp(nxy),res(nxy)
c
c..... coefficients of upper and lower matrices
c
      do i=2,nim
      do ij=li(i)+2,li(i)+njm
       bw(ij)=-aw(ij)/(1.+alfa*bn(ij-nj))
       bs(ij)=-as(ij)/(1.+alfa*be(ij-1))
       p1=alfa*bw(ij)*bn(ij-nj)
       p2=alfa*bs(ij)*be(ij-1)
       bp(ij)=1./(ap(ij)+p1+p2-bw(ij)*be(ij-nj)-bs(ij)*bn(ij-1)+small)
       bn(ij)=(-an(ij)-p1)*bp(ij)
       be(ij)=(-ae(ij)-p2)*bp(ij)
      end do
      end do
c
c.....inner iterations loop
c
      do 100 l=1,nsw(ifi)
      resab=0.
c      
c.....calculate residual and intermediate vector
c
      do i=2,nim
      do ij=li(i)+2,li(i)+njm
       res(ij)=an(ij)*fi(ij+1)+as(ij)*fi(ij-1)+ae(ij)*fi(ij+nj)+
     *         aw(ij)*fi(ij-nj)+su(ij)-ap(ij)*fi(ij)
       resab=resab+abs(res(ij))
       res(ij)=(res(ij)-bs(ij)*res(ij-1)-bw(ij)*res(ij-nj))*bp(ij)
      end do
      end do
c
      if(l.eq.1) resor(ifi)=resab
      rsm=resab/(resor(ifi)+small)
c
c..... back substitution and correction
c
      do i=nim,2,-1
      do ij=li(i)+njm,li(i)+2,-1
       res(ij)=res(ij)-bn(ij)*res(ij+1)-be(ij)*res(ij+nj)
       fi(ij)=fi(ij)+res(ij)
      end do
      end do
c
c..... exchange solution at interfaces
c
      if(nproc.gt.1) then
       call exch(fi)
      endif
c
c.....check convergence of inner iterations
c
      if (ltest) write(2,*) '    ',l,'sweep, resor = ',resab
c     if (rsm.lt.sor(ifi)) return
  100 continue
      return
      end
c
c
c######################################################
      subroutine solgs(fi,ifi)
c######################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /logic/ lwrite,lread,ltest,laxis,louts,loute,
     *       lcal(nfi),ltime
      common /contro/ iu,iv,ip,ien,ipr,jpr,nsw(nfi),sor(nfi),
     *       resor(nfi),urf(nfi),urpp,gam,beta,gravx,gravy,
     *       gds(nfi),snor(nfi),sormax,slarge,alfa,great,small
      common /coef/ ae(nxy),aw(nxy),an(nxy),as(nxy),ap(nxy),
     *       su(nxy),sv(nxy),apu(nxy),apv(nxy),ae1,aw1,an1,as1
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      logical lwrite,lread,ltest,laxis,louts,loute,lcal,ltime
      dimension fi(nxy)
c
c.....calculate residual norm
c
      resab=0.
c
      do i=2,nim
      do ij=li(i)+2,li(i)+njm
       resab=resab+abs(an(ij)*fi(ij+1)+as(ij)*fi(ij-1)+
     *       ae(ij)*fi(ij+nj)+aw(ij)*fi(ij-nj)+su(ij)-ap(ij)*fi(ij))
      end do
      end do
      resor(ifi)=resab
c
c.....inner iterations loop
c
      do 100 l=1,nsw(ifi)
       resab=0.
c      
c.....calculate residual and update variable
c
      do i=2,nim
      do ij=li(i)+2,li(i)+njm
       resko=an(ij)*fi(ij+1)+as(ij)*fi(ij-1)+ae(ij)*fi(ij+nj)+
     *       aw(ij)*fi(ij-nj)+su(ij)-ap(ij)*fi(ij)
       resab=resab+abs(resko)
       fi(ij)=fi(ij)+resko/ap(ij)
      end do
      end do
c
c..... exchange solution at interfaces
c
      if(nproc.gt.1) then
       call exch(fi)
      endif
c
c.....check convergence of inner iterations
c
      rsm=resab/(resor(ifi)+small)
      if (ltest) write(2,*) '    ',l,'sweep, resor = ',resab
c     if (rsm.lt.sor(ifi)) return
  100 continue
      return
      end
c
c
c####################################################
      subroutine pbound(fi)
c####################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /geo/ x(nx),y(ny),xc(nx),yc(ny),fx(nx),fy(ny),r(ny)
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      dimension fi(nxy)
c
c..... south and north boundaries
c
      if(jproc.eq.1) then
       do i=2,nim
        ij=li(i)+1
        fi(ij)=fi(ij+1)+(fi(ij+1)-fi(ij+2))*fy(2)
       end do
      endif
c
      if(jproc.eq.npry) then
       do i=2,nim
        ij=li(i)+nj
        fi(ij)=fi(ij-1)+(fi(ij-1)-fi(ij-2))*(1.-fy(njm-1)) 
       end do 
      endif 
c
c..... west and east boundaries
c
      if(iproc.eq.1) then
       do j=2,njm
        ij=li(1)+j
        fi(ij)=fi(ij+nj)+(fi(ij+nj)-fi(ij+nj+nj))*fx(2) 
       end do 
      endif
c
      if(iproc.eq.nprx) then
       do j=2,njm
        ij=li(ni)+j
        fi(ij)=fi(ij-nj)+(fi(ij-nj)-fi(ij-nj-nj))*(1.-fx(nim-1))
       end do
      endif
      return
      end
c
c
c########################################################
      subroutine bcond
c########################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /contro/ iu,iv,ip,ien,ipr,jpr,nsw(nfi),sor(nfi),
     *       resor(nfi),urf(nfi),urpp,gam,beta,gravx,gravy,
     *       gds(nfi),snor(nfi),sormax,slarge,alfa,great,small
      common /var/ densit,visc,prr,flomas,flomom,tref,u(nxy),
     *       v(nxy),p(nxy),pp(nxy),t(nxy),f1(nxy),f2(nxy),
     *       dpx(nxy),dpy(nxy)
      common /geo/ x(nx),y(ny),xc(nx),yc(ny),fx(nx),fy(ny),r(ny)
      common /coef/ ae(nxy),aw(nxy),an(nxy),as(nxy),ap(nxy),
     *       su(nxy),sv(nxy),apu(nxy),apv(nxy),ae1,aw1,an1,as1
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
c
c========================
      entry bcpp
c========================
c
c..... south and north boundaries
c
      if(jproc.ne.1) then
       do i=2,nim
        call nordp(i,1)
       end do
      endif
c
      if(jproc.ne.npry) then
       do i=2,nim
        call nordp(i,njm)
       end do 
      endif 
c
c..... west and east boundaries
c
      if(iproc.ne.1) then
       do j=2,njm
        call eastp(1,j)
       end do 
      endif
c
      if(iproc.ne.nprx) then
       do j=2,njm
        call eastp(nim,j)
       end do
      endif
      return
c
c========================
      entry bcuv 
c========================
c
c..... south  boundary (wall or block interface)
c
      if(jproc.eq.1) then
       do i=2,nim
        ij=li(i)+2
        ds=visc*(x(i)-x(i-1))*r(1)/(yc(2)-yc(1))
        apu(ij)=apu(ij)+ds
       end do
c
      else
       do i=2,nim
        ij=li(i)+1
        ijp=ij+1
        call fluxn(i,1,1.)
        su(ijp)=su(ijp)+gds(iu)*as1*(u(ij)-u(ijp))
        sv(ijp)=sv(ijp)+gds(iv)*as1*(v(ij)-v(ijp))
       end do
      endif
c
c..... north boundary (wall or block interface)
c
      if(jproc.eq.npry) then
       do i=2,nim
        ij=li(i)+njm
        dn=visc*(x(i)-x(i-1))*r(njm)/(yc(nj)-yc(njm))
        apu(ij)=apu(ij)+dn
        su(ij)=su(ij)+dn*u(ij+1)
       end do
c
      else
       do i=2,nim
        ij=li(i)+njm
        call fluxn(i,njm,1.)
        su(ij)=su(ij)+gds(iu)*an1*(u(ij+1)-u(ij))
        sv(ij)=sv(ij)+gds(iv)*an1*(v(ij+1)-v(ij))
       end do
      endif
c
c..... west boundary (wall or block interface)
c
      if(iproc.eq.1) then
       do j=2,njm
        ij=li(2)+j
        dw=0.5*visc*(y(j)-y(j-1))*(r(j)+r(j-1))/(xc(2)-xc(1))
        apv(ij)=apv(ij)+dw
       end do 
      else 
c
       do j=2,njm
        ij=li(1)+j
        ipj=ij+nj
        call fluxe(1,j,1.)
        su(ipj)=su(ipj)+gds(iu)*aw1*(u(ij)-u(ipj))
        sv(ipj)=sv(ipj)+gds(iv)*aw1*(v(ij)-v(ipj))
       end do
      endif
c
c..... east boundary (wall or block interface)
c
      if(iproc.eq.nprx) then
       do j=2,njm
        ij=li(nim)+j
        de=0.5*visc*(y(j)-y(j-1))*(r(j)+r(j-1))/(xc(ni)-xc(nim))
        apv(ij)=apv(ij)+de
       end do
c
      else
       do j=2,njm
        ij=li(nim)+j
        call fluxe(nim,j,1.)
        su(ij)=su(ij)+gds(iu)*ae1*(u(ij+nj)-u(ij))
        sv(ij)=sv(ij)+gds(iv)*ae1*(v(ij+nj)-v(ij))
       end do
      endif
      return
c
c===========================
      entry modt
c===========================
c
c..... south  boundary (adiabatic wall or block interface)
c
      if(jproc.ne.1) then
       do i=2,nim
        ij=li(i)+1
        ijp=ij+1
        call fluxn(i,1,prr)
        su(ijp)=su(ijp)+gds(ien)*as1*(t(ij)-t(ijp))
       end do
      endif
c
c..... north boundary (adiabatic wall or block interface)
c
      if(jproc.ne.npry) then
       do i=2,nim
        ij=li(i)+njm
        call fluxn(i,njm,prr)
        su(ij)=su(ij)+gds(ien)*an1*(t(ij+1)-t(ij))
       end do
      endif
c
c..... west boundary (isothermal wall or block interface)
c
      if(iproc.eq.1) then
       do j=2,njm
        ij=li(2)+j
        dw=0.5*visc*prr*(y(j)-y(j-1))*(r(j)+r(j-1))/(xc(2)-xc(1))
        ap(ij)=ap(ij)+dw
        su(ij)=su(ij)+dw*t(ij-nj)
       end do 
      else 
c
       do j=2,njm
        ij=li(1)+j
        ipj=ij+nj
        call fluxe(1,j,prr)
        su(ipj)=su(ipj)+gds(ien)*aw1*(t(ij)-t(ipj))
       end do
      endif
c
c..... east boundary (isothermal wall or block interface)
c
      if(iproc.eq.nprx) then
       do j=2,njm
        ij=li(nim)+j
        de=0.5*visc*prr*(y(j)-y(j-1))*(r(j)+r(j-1))/(xc(ni)-xc(nim))
        ap(ij)=ap(ij)+de
        su(ij)=su(ij)+de*t(ij+nj)
       end do
c
      else
       do j=2,njm
        ij=li(nim)+j
        call fluxe(nim,j,prr)
        su(ij)=su(ij)+gds(ien)*ae1*(t(ij+nj)-t(ij))
       end do
      endif
      return
      end
c
c###################################################
      subroutine inout
c###################################################
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /logic/ lwrite,lread,ltest,laxis,louts,loute,
     *       lcal(nfi),ltime
      common /contro/ iu,iv,ip,ien,ipr,jpr,nsw(nfi),sor(nfi),
     *       resor(nfi),urf(nfi),urpp,gam,beta,gravx,gravy,
     *       gds(nfi),snor(nfi),sormax,slarge,alfa,great,small
      common /var/ densit,visc,prr,flomas,flomom,tref,u(nxy),
     *       v(nxy),p(nxy),pp(nxy),t(nxy),f1(nxy),f2(nxy),
     *       dpx(nxy),dpy(nxy)
      common /geo/ x(nx),y(ny),xc(nx),yc(ny),fx(nx),fy(ny),r(ny)
      common /varold/ itim,itst,time,dt,dtr,uo(nxy),vo(nxy),
     *       to(nxy)
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      logical lwrite,lread,ltest,laxis,louts,loute,lcal,ltime
      character title*80
c
c==============================
      entry modinp
c==============================
c
c..... read input data
c
      read(5,6) title
    6 format(a80)
      read(5,*) lread,lwrite,ltest,louts,loute,ltime
      read(5,*) maxit,imon,jmon,ipr,jpr,sormax,slarge,alfa,urpp
      read(5,*) densit,visc,prm,gravx,gravy,beta,th,tc,tref
      read(5,*) uin,vin,pin,tin,umax,itst,dt,ksw
      read(5,*) (lcal(i),i=1,nfi)
      read(5,*) (urf(i),i=1,nfi)
      read(5,*) (sor(i),i=1,nfi)
      read(5,*) (nsw(i),i=1,nfi)
      read(5,*) (gds(i),i=1,nfi)
      read(5,*) xmin,xmax,ymin,ymax,nicv,njcv
      read(5,*) iproc,jproc
c
      iu=1
      iv=2
      ip=3
      ien=4
      small=1.e-15
      great=1.e15
      dtr=1./dt
      prr=1./prm
c
c..... generate grid data 
c
      ni=nicv+2
      nj=njcv+2
      nim=ni-1
      njm=nj-1
      nij=ni*nj
      do i=1,ni
       li(i)=(i-1)*nj
      end do
c
c..... grid coordinates
c
      dx=(xmax-xmin)/float(nprx*nicv)
      dy=(ymax-ymin)/float(npry*njcv) 
      x(1)=xmin+float((iproc-1)*nicv)*dx
      y(1)=ymin+float((jproc-1)*njcv)*dx
      do i=2,nim
       x(i)=x(i-1)+dx
      end do
      do j=2,njm
       y(j)=y(j-1)+dy
      end do
      x(ni)=x(nim)
      y(nj)=y(njm)
c
c..... x- coordinates of CV centers
c
      do i=2,nim
       xc(i)=0.5*(x(i)+x(i-1))
      end do
c
      if(iproc.eq.1) then
       xc(1)=x(1)
      else
       xc(1)=xc(2)-dx
      endif
c
      if(iproc.eq.nprx) then
       xc(ni)=x(nim)
      else
       xc(ni)=xc(nim)+dx
      endif
c
c..... y- coordinates of CV centers
c
      do j=2,njm
       yc(j)=0.5*(y(j)+y(j-1))
      end do
c
      if(jproc.eq.1) then
       yc(1)=y(1)
      else
       yc(1)=yc(2)-dy
      endif
c
      if(jproc.eq.npry) then
       yc(nj)=y(njm)
      else
       yc(nj)=yc(njm)+dy
      endif
c
c..... interpolation factors
c
      do i=1,nim
       fx(i)=(x(i)-xc(i))/(xc(i+1)-xc(i))
      end do
c
      do j=1,njm
       fy(j)=(y(j)-yc(j))/(yc(j+1)-yc(j))
      end do
c
c..... initialize varible values
c
      do i=2,nim
      do ij=li(i)+2,li(i)+njm
       u(ij)=uin
       v(ij)=vin
       t(ij)=tin
       p(ij)=pin
      end do
      end do
c
c..... set radius
c
      if(laxis) then
       do j=1,nj
        r(j)=y(j)
       end do
      else
       do j=1,nj
        r(j)=1.
       end do
      endif
      return
c
c.... set boundary values 
c
c==========================
      entry bcin
c==========================
      flomom=0.
      flomas=0.
c
c..... west and east isothermal boundaries
c
      if(iproc.eq.1) then
       do j=1,nj
        t(j)=th
       end do
      endif
c
      if(iproc.eq.nprx) then
       do j=1,nj
        t(li(ni)+j)=tc
       end do
      endif
c
c..... north wall velocity (lid-driven cavity)
c
      if(jproc.eq.npry) then
       do i=2,nim
        u(li(i)+nj)=umax
       end do
      endif
c
c..... set normalisation factors
c
      do l=1,nfi
       snor(l)=1.
       resor(l)=0.0
      end do
      if(flomas.lt.1.e-15) flomas=1.
      if(flomom.lt.1.e-15) flomom=1.
      snor(iu)=1./(flomom+small)
      snor(iv)=snor(iu)
      snor(ip)=1./(flomas+small)
      snor(ien)=1./(flomas*tin+small)
      return
c
c======================================
      entry outres
c======================================
      if(lcal(iu)) call print(u,'u vel.')
      if(lcal(iv)) call print(v,'v vel.')
      if(lcal(ip)) call print(p,'press.')
      if(lcal(ien)) call print(t,'temper')
      if(ltest.and.lcal(iu)) call print(f1,'mass_e')
      if(ltest.and.lcal(iu)) call print(f2,'mass_n')
      return
c
c=======================================
      entry out1
c=======================================
      write(2,601) title,densit,visc,alfa
  601 format(1h1,//,50x,a80,/,50x,40('*'),/,50x,
     *        'fluid density    :  ',e10.4,/,50x,
     *        'dynamic viscosity:  ',e10.4,/,50x,
     *        'alfa  parameter  :  ',f 6.2)
      if(lcal(iu)) write(2,606) iu,urf(iu) ,iu,gds(iu)
      if(lcal(iv)) write(2,606) iv,urf(iv) ,iv,gds(iv)
      if(lcal(ip)) write(2,607) ip,urf(ip)
      if(lcal(ien)) write(2,606) ien,urf(ien) ,ien,gds(ien)
  606 format(50x,'urf(',i2,' ) = ',f4.2,',  gds(',i2,' ) = ',f4.2)
  607 format(50x,'urf(',i2,' ) = ',f4.2)
      write(2,608)
  608 format(//)
  610 format(a40)
      return
c
c======================================
      entry out2
c======================================
c
c..... heat fluxes at west and east isothermal boundaries
c
      if(iproc.eq.1.and.lcal(ien)) then
       qwal=0.
       do j=2,njm
        area=0.5*(r(j)+r(j-1))*(y(j)-y(j-1))
        coef=visc*prr*area/(xc(2)-xc(1))
        qwal=qwal+coef*(t(li(2)+j)-t(li(1)+j))
       end do
       write(2,620) 'west',jproc,qwal
      endif
c
      if(iproc.eq.nprx.and.lcal(ien)) then
       qwal=0.
       do j=2,njm
        area=0.5*(r(j)+r(j-1))*(y(j)-y(j-1))
        coef=visc*prr*area/(xc(ni)-xc(nim))
        qwal=qwal+coef*(t(li(ni)+j)-t(li(nim)+j))
       end do
       write(2,620) 'east',jproc,qwal
      endif
  620 format(/,10x,'Heat flux through ',a4,' wall for processor',
     *       i3,':  ',1p1e14.5)
c
c..... wall shear stress at moving lid
c
      if(jproc.eq.npry.and.umax.ne.0.) then
       qwal=0.
       do i=2,nim
        coef=visc*(x(i)-x(i-1))*r(njm)/(yc(nj)-yc(njm))
        qwal=qwal+coef*(u(li(i)+nj)-u(li(i)-njm))
       end do
       write(2,621) iproc,qwal
      endif
  621 format(/,10x,'Wall shear stress at moving lid for processor',
     *       i3,':  ',1p1e14.5)
      return
      end
c
c
c#############################################################
      subroutine initco
c#############################################################
c     communication primitves for PVM 3.x
c     The SPMD (single program multiple data) model is used, i.e.
c     the first process creates all other necessary processes.
c**************************************************************
c      include '/soft/include/fpvm3.h'
      include '/home/local/pvm/include/fpvm3.h'
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      common /com/ mytid,itids(100)
      common /hostcomp/ hosts(100)
      character*40 pname,hosts
c
c     iproc,jproc - position of the subdomain in the array of blocks
c     nprx,npry   - dimensions of the array of blocks
c     itids - task identifiers, unique integers for each task in PVM
c     mytid - task id of the own task
c     nproc - number of hosts in the pvm, = nprx*npry 
c     info  - status variable for the pvm-routines
c     pname - name of the executable to run
c     hosts - chararacter array with names of cluster computers
c
c..... Enroll in pvm (get own task identifier)
c     
      call pvmfmytid(mytid)
c
c..... Find out if parent or child - spawned processes have parents
c
      call pvmfparent(itids(1))
      if(itids(1).lt.0) then
c
c..... I am the first process (parent), starting others
c 
        itids(1)=mytid
        me=1
c
c..... get the number of available hosts and their names
c
        open(unit=1,file='hosts')
        rewind 1
        do i=1,nproc
          read(1,1) hosts(i)
        end do
    1   format(a40)
c
c..... Find out the name (pname) of the executable program. 
c
        call getarg(0,pname)
c
c..... Start programs on other mashines
c
        if(nproc.gt.1) then
          do i=2,nproc
            call pvmfspawn(pname,PVMHOST,hosts(i),1,itids(i),info)
            print *,' Starting ',pname,' on ',hosts(i)
            if (info.lt.0) then
              call pvmfexit(info)
              stop
            endif
          end do
c
c..... Broadcast the task identifiers to all processors 
c
          call pvmfinitsend(0, info)
          call pvmfpack(integer4,itids(1),nproc,1,info)
          call pvmfmcast(nproc-1,itids(2),0,info)
        endif
      else 
c
c..... Other processes (children) receive task identifiers
c
         call pvmfrecv(itids(1),0,info)
         call pvmfunpack(integer4,itids(1),nproc,1,info)
         do i=2,nproc
           if(mytid.eq.itids(i)) me=i
         end do
c
      endif
c
c..... All mashines are now runing the same code with their data
c
      return
      end
c
c########################################################
      subroutine sendnb(fitr,ij1,nel,nbtid,mtag)
c########################################################
c     This routine sends NEL-elements of the array FITR
c     starting at index IJ1 to neighbour whose tid is NBTID
c********************************************************
c      include '/soft/include/fpvm3.h'
      include '/home/local/pvm/include/fpvm3.h'
      dimension fitr(*)
c
c.....Initialize the communication buffer
c
      call pvmfinitsend(0,info)
c
c..... Copy the message to the buffer and send it
c
      call pvmfpack(REAL8,fitr(ij1),nel,1,info)
      call pvmfsend(nbtid,mtag,info)
c
       return
       end
c
c########################################################
      subroutine recvnb(fitr,ij1,nel,nbtid,mtag)
c########################################################
c     This routine receives NEL elements of the array FITR
c     starting at index IJ1 from neighbour whose tid is NBTID
c********************************************************
c      include '/soft/include/fpvm3.h'
      include '/home/local/pvm/include/fpvm3.h'
      dimension fitr(*)
c
c.... Receive the message and unpack it
c
      call pvmfrecv(nbtid,mtag,info)
      call pvmfunpack(REAL8,fitr(ij1),nel,1,info)
c
       return
       end
c
c
c########################################################
      subroutine broadc(value,mtag)
c########################################################
c     Master broadcasts VALUE to all other processors
c     e. g. pressure-correction at monitoring location or
c     sum of absolute residuals from all subdomains
c********************************************************   
c      include '/soft/include/fpvm3.h'
      include '/home/local/pvm/include/fpvm3.h'
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      common /com/ mytid,itids(100)
c
      if(nproc.gt.1) then
c
c.... Master broadcasts
c
        if(mytid.eq.itids(1)) then
          call pvmfinitsend(0,info)
          call pvmfpack(REAL8,value,1,1,info)
          call pvmfmcast(nproc-1,itids(2),mtag,info)
c
c.... Other processors receiving
c
        else
          call pvmfrecv(itids(1),mtag,info)
          call pvmfunpack(REAL8,value,1,1,info)
        endif
      endif
c
      return
      end
c
c
c########################################################
      subroutine collect(values,nel,mtag)
c########################################################
c     Master collects NEL elements of array VALUES from all 
c     other processors (e. g. sum of absolute residuals)
c********************************************************   
c      include '/soft/include/fpvm3.h'
      include '/home/local/pvm/include/fpvm3.h'
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      common /com/ mytid,itids(100)
      dimension values(*),fitr(100)
c
      if(nproc.gt.1) then
c
c.... All slaves sending
c
        if(mytid.ne.itids(1)) then
          call pvmfinitsend(0,info)
          call pvmfpack(REAL8,values(1),nel,1,info)
          call pvmfsend(itids(1),mtag,info)
c
c.... Master receiving (receive and add to own values)
c
        else
          do i=2,nproc
            call pvmfrecv(itids(i),mtag,info)
            call pvmfunpack(REAL8,fitr(1),nel,1,info)
c
            do l=1,nel
              values(l)=values(l)+fitr(l)
            end do
          end do
        endif
c
      endif
c
      return
      end
c
c
c########################################################
      subroutine exch(fi)
c########################################################
c     This routine organizes local communication (exchanges 
c     values of variables) at subdomain boundaries. First
c     odd processors send to right and even receive from
c     left, then vice-versa; the same in both directions.
c********************************************************   
      parameter (nx=12,ny=12,nxy=nx*ny,nfi=4)
      common /index/ ni,nj,nim,njm,nij,nitp,ksw,ij,li(nx),
     *       imon,jmon,ijmon,maxit 
      common /mypvm/ me,nprx,npry,nproc,iproc,jproc
      common /com/ mytid,itids(100)
      dimension fi(nxy),fin(nx),fis(nx)
c
c==================================================
c     Communication in I-direction; odd IPROC sends
c     to right, receives from left, sends to left
c     and receives from its right neighbour
c==================================================
      if(mod(iproc,2).eq.1) then
c
        if(iproc.lt.nprx) then
          ij1=li(nim)+1
          call sendnb(fi,ij1,nj,itids(me+npry),2)
        endif
c
        if(iproc.gt.1) then
          ij1=li(1)+1
          call recvnb(fi,ij1,nj,itids(me-npry),2)
c
          ij1=li(2)+1
          call sendnb(fi,ij1,nj,itids(me-npry),1)
        endif
c
        if(iproc.lt.nprx) then
          ij1=li(ni)+1
          call recvnb(fi,ij1,nj,itids(me+npry),1)
        endif
c=================================================
c     Communication in I-direction; even IPROC
c     receives from left, sends to right, receives
c     from right and sends to its left neighbour
c=================================================
      else
c
        ij1=li(1)+1
        call recvnb(fi,ij1,nj,itids(me-npry),2)
c
        if(iproc.lt.nprx) then
          ij1=li(nim)+1
          call sendnb(fi,ij1,nj,itids(me+npry),2)
c
          ij1=li(ni)+1
          call recvnb(fi,ij1,nj,itids(me+npry),1)
        endif
c
        ij1=li(2)+1
        call sendnb(fi,ij1,nj,itids(me-npry),1)
      endif
c=================================================
c     Communication in J-direction (Data repacked 
c     to be contigous); odd JPROC sends to north,
c     receives from south, sends to south and
c     receives from its north neighbour
c=================================================
      if(mod(jproc,2).eq.1) then
c
        if(jproc.lt.npry) then
          do i=1,ni
            fin(i)=fi(li(i)+njm)
          end do
          call sendnb(fin,1,ni,itids(me+1),4)
        endif
c
        if(jproc.gt.1) then
          call recvnb(fis,1,ni,itids(me-1),4)
          do i=1,ni
            fi(li(i)+1)=fis(i)
          end do
c
          do i=1,ni
            fis(i)=fi(li(i)+2)
          end do
          call sendnb(fis,1,ni,itids(me-1),3)
        endif
c
        if(jproc.lt.npry) then
          call recvnb(fin,1,ni,itids(me+1),3)
          do i=1,ni
            fi(li(i)+nj)=fin(i)
          end do
        endif
c=================================================
c     Communication in J-direction (Data repacked 
c     to be contigous); even JPROC receives from 
c     south, sends to north, receives from north
c     and sends to its south neighbour
c=================================================
      else
c
          call recvnb(fis,1,ni,itids(me-1),4)
          do i=1,ni
            fi(li(i)+1)=fis(i)
          end do
c
        if(jproc.lt.npry) then
          do i=1,ni
            fin(i)=fi(li(i)+njm)
          end do
          call sendnb(fin,1,ni,itids(me+1),4)
c
          call recvnb(fin,1,ni,itids(me+1),3)
          do i=1,ni
            fi(li(i)+nj)=fin(i)
          end do
        endif
c
          do i=1,ni
            fis(i)=fi(li(i)+2)
          end do
          call sendnb(fis,1,ni,itids(me-1),3)
c
      endif
c
      return
      end
c
c
c#########################################
      subroutine exitnet
c#########################################
c     Exiting PVM environment
c*****************************************
c      include '/soft/include/fpvm3.h'
      include '/home/local/pvm/include/fpvm3.h'
c
      call pvmfexit(info)
      return
      end

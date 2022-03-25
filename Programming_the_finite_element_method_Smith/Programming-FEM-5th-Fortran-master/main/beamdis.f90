SUBROUTINE beamdis(loads,nf,ratmax,interp,nels,ell,argv,nlen,ips)
!
! This subroutine produces a PostScript output file "*.dis" displaying
! the deformed 1-D finite element mesh.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::loads(0:),ratmax,ell(:)
 INTEGER,INTENT(IN)::ips,nf(:,:),nels,interp,nlen
 REAL(iwp)::width,height,scale=72,sxy,xo,yo,x,y,dismag,vmax
 REAL(iwp)::xmin,xmax,ymin,ymax,xnow,dmax,zero=0.0_iwp,pt5=0.5_iwp,       &
   opt5=1.5_iwp,fpt5=5.5_iwp,d8=8.0_iwp,ept5=8.5_iwp,d11=11.0_iwp,        &
   one=1.0_iwp,two=2.0_iwp,thr=3.0_iwp,                                   &
   localx,globalx,na,nb,nc,nd,ll,wold,wnew
 INTEGER::i,j,nn
 CHARACTER(LEN=15)::argv
 REAL(iwp),ALLOCATABLE::xcoord(:)
 OPEN(ips,FILE=argv(1:nlen)//'.dis')
!
 nn=nels+1
 ALLOCATE(xcoord(nn))

 xmin=zero
 xmax=zero
 xnow=zero
 ymin=zero
 ymax=zero

 xcoord(1)=xnow

 DO i=2,nn
   xnow=xnow+ell(i-1)
   xcoord(i)=xnow
 END DO

 xmax=xcoord(nn)

 DO i=1,nn
    IF(loads(nf(1,i))<ymin)ymin=loads(nf(1,i))
    IF(loads(nf(1,i))>ymax)ymax=loads(nf(1,i))
 END DO

 width=xmax-xmin
 height=ymax-ymin
 dmax=ratmax*width
 IF(height>width)dmax=ratmax*height
!
 vmax=zero
 DO i=1,nn
     IF(ABS(loads(nf(1,i)))>vmax)vmax=ABS(loads(nf(1,i)))
 END DO
 dismag=dmax/vmax
!
 ymin=zero
 ymax=zero

 DO i=1,nn
   IF(dismag*loads(nf(1,i))<ymin)                            &
     ymin=dismag*loads(nf(1,i))
   IF(dismag*loads(nf(1,i))>ymax)                            &
     ymax=dismag*loads(nf(1,i))
!
   IF(loads(nf(1,i))<ymin)ymin=loads(nf(1,i))
   IF(loads(nf(1,i))>ymax)ymax=loads(nf(1,i))
 END DO
!
 width =xmax-xmin
 height=ymax-ymin
!
!                       allow 1.5" margin minimum on each side of figure
!
!                       portrait mode 
!
 IF(height.GE.d11/ept5*width)THEN
!
!                       height governs the scale
!
   sxy=scale*d8/height
   xo=scale*pt5*(ept5-d8*width/height)
   yo=scale*opt5
 ELSE
!
!                       width governs the scale
!
   sxy=scale*fpt5/width
   xo=scale*opt5
   yo=scale*pt5*(d11-fpt5*height/width)
 END IF
!
!
!                       start PostScript output
!
 WRITE(ips,'(a)')'%!PS-Adobe-1.0'
 WRITE(ips,'(a)')'%%DocumentFonts: none'
 WRITE(ips,'(a)')'%%Pages: 1'
 WRITE(ips,'(a)')'%%EndComments'
 WRITE(ips,'(a)')'/m {moveto} def'
 WRITE(ips,'(a)')'/l {lineto} def'
 WRITE(ips,'(a)')'/s {stroke} def'
 WRITE(ips,'(a)')'/c {closepath} def'
 WRITE(ips,'(a)')'%%EndProlog'
 WRITE(ips,'(a)')'%%Page: 0 1'
 WRITE(ips,'(a)')'gsave'
!
!                       draw the deformed mesh
!
 WRITE(ips,'(2f9.2,a)') xo, yo, ' translate'
 WRITE(ips,'(f9.2,a)') 0.5, ' setlinewidth'

 wnew=loads(nf(1,1))

 DO i=1,nels
    DO j=1,interp

    wold=wnew
    localx=j*ell(i)/interp
    globalx=localx+xcoord(i)
    ll=ell(i)
    na=(one/(ll**3))*(ll**3-thr*ll*localx**2+two*localx**3)
    nb=(one/(ll**2))*(localx*ll**2-two*ll*localx**2+localx**3)
    nc=(one/(ll**3))*(thr*ll*localx**2-two*localx**3)
    nd=(one/(ll**2))*(localx**3-ll*localx**2)
    wnew=na*loads(nf(1,i))+nb*loads(nf(2,i))+nc*loads(nf(1,1+i))+nd*loads(nf(2,i+1))

    x=sxy*((globalx-ell(i)/interp)-xmin)
    y=sxy*(dismag*wold-ymin)
    WRITE(ips,'(2f9.2,a)') x, y,' m'
    x=sxy*(globalx-xmin)
    y=sxy*(dismag*wnew-ymin)
    WRITE(ips,'(2f9.2,a)') x, y,' l'
    WRITE(ips,'(a)')'c s'

    END DO

 END DO
!
 WRITE(ips,'(a)')'grestore'
 WRITE(ips,'(a)')'showpage'
 CLOSE(ips)
!
RETURN
END SUBROUTINE beamdis


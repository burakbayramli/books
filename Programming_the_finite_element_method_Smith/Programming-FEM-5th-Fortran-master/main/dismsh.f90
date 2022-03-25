SUBROUTINE dismsh(loads,nf,ratmax,g_coord,g_num,argv,nlen,ips)
!
! This subroutine produces a PostScript output file "*.dis" displaying
! the deformed finite element mesh.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::g_coord(:,:),loads(0:),ratmax
 INTEGER,INTENT(IN)::g_num(:,:),ips,nf(:,:),nlen
 CHARACTER(*),INTENT(IN)::argv
 REAL(iwp)::width,height,scale=72,sxy,xo,yo,x,y,dismag,vmax
 REAL(iwp)::xmin,xmax,ymin,ymax,dmax,zero=0.0_iwp,pt5=0.5_iwp,            &
   opt5=1.5_iwp,fpt5=5.5_iwp,d8=8.0_iwp,ept5=8.5_iwp,d11=11.0_iwp
 INTEGER::i,ii,j,jj,nn,nel,nod
 OPEN(ips,FILE=argv(1:nlen)//'.dis')
!
 nn=UBOUND(nf,2)
 xmin=g_coord(1,1)
 xmax=g_coord(1,1)
 ymin=g_coord(2,1)
 ymax=g_coord(2,1)
 DO i=2,nn
   IF(g_coord(1,i)<xmin)xmin=g_coord(1,i)      
   IF(g_coord(1,i)>xmax)xmax=g_coord(1,i)      
   IF(g_coord(2,i)<ymin)ymin=g_coord(2,i)      
   IF(g_coord(2,i)>ymax)ymax=g_coord(2,i)      
 END DO
 width=xmax-xmin
 height=ymax-ymin
 dmax=ratmax*width
 IF(height>width)dmax=ratmax*height
!
 vmax=zero
 DO i=1,nn
   DO j=1,2
     IF(ABS(loads(nf(j,i)))>vmax)vmax=ABS(loads(nf(j,i)))
   END DO
 END DO
 dismag=dmax/vmax
!
 xmin=g_coord(1,1)
 xmax=g_coord(1,1)
 ymin=g_coord(2,1)
 ymax=g_coord(2,1)
 DO i=1,nn
   IF(g_coord(1,i)+dismag*loads(nf(1,i))<xmin)                            &
     xmin=g_coord(1,i)+dismag*loads(nf(1,i))
   IF(g_coord(1,i)+dismag*loads(nf(1,i))>xmax)                            &
     xmax=g_coord(1,i)+dismag*loads(nf(1,i))      
   IF(g_coord(2,i)+dismag*loads(nf(2,i))<ymin)                            &
     ymin=g_coord(2,i)+dismag*loads(nf(2,i))      
   IF(g_coord(2,i)+dismag*loads(nf(2,i))>ymax)                            &
     ymax=g_coord(2,i)+dismag*loads(nf(2,i))      
!
   IF(g_coord(1,i)<xmin)xmin=g_coord(1,i)
   IF(g_coord(1,i)>xmax)xmax=g_coord(1,i)
   IF(g_coord(2,i)<ymin)ymin=g_coord(2,i)
   IF(g_coord(2,i)>ymax)ymax=g_coord(2,i)
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
 nel=UBOUND(g_num,2)
 nod=UBOUND(g_num,1)
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
 IF(nod==5)nod=4
 IF(nod==9)nod=8
 IF(nod==10)nod=9
 IF(nod==15)nod=12
 DO i=1,nel
   ii=g_num(1,i)
   IF(ii==0)CYCLE
   x=sxy*(g_coord(1,ii)+dismag*loads(nf(1,ii))-xmin)
   y=sxy*(g_coord(2,ii)+dismag*loads(nf(2,ii))-ymin)
   WRITE(ips,'(2f9.2,a)') x, y,' m'
   DO j=2,nod
     jj=g_num(j,i)
     x=sxy*(g_coord(1,jj)+dismag*loads(nf(1,jj))-xmin)
     y=sxy*(g_coord(2,jj)+dismag*loads(nf(2,jj))-ymin)
     WRITE(ips,'(2f9.2,a)') x, y,' l'
   END DO
   WRITE(ips,'(a)')'c s'
 END DO
!
 WRITE(ips,'(a)')'grestore'
 WRITE(ips,'(a)')'showpage'
 CLOSE(ips)
!
RETURN
END SUBROUTINE dismsh



























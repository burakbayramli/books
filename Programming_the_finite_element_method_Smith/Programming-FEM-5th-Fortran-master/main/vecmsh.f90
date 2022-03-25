SUBROUTINE vecmsh(loads,nf,ratmax,cutoff,g_coord,g_num,argv,nlen,ips)
!
! This subroutine produces a PostScript output file "*.vec" displaying
! the nodal displacement vectors.
!
IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::g_coord(:,:),loads(0:),ratmax,cutoff
 INTEGER,INTENT(IN)::g_num(:,:),ips,nf(:,:),nlen
 REAL(iwp)::width,height,scale=72,sxy,xo,yo,x1,y1,x2,y2,dismag,           &
   zero=0.0_iwp,pt5=0.5_iwp,opt5=1.5_iwp,fpt5=5.5_iwp,d8=8.0_iwp,         &
   ept5=8.5_iwp,d11=11.0_iwp,xmin,xmax,ymin,ymax,dmax,vlen,vmax
 INTEGER::i,j,k,l,nn,nels,nod,ns,i1,i2,j1,j2
 INTEGER,ALLOCATABLE::corner(:,:)
 CHARACTER(*),INTENT(IN)::argv
 LOGICAL::draw
!                       formats
 OPEN(ips,FILE=argv(1:nlen)//'.vec')
!                       open output file and compute scale factors
 nn=UBOUND(nf,2)
!
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
 width =xmax-xmin
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
!
 DO i=1,nn
   IF(g_coord(1,i)+dismag*loads(nf(1,i)) < xmin)                          &
     xmin=g_coord(1,i)+dismag*loads(nf(1,i))      
   IF(g_coord(1,i)+dismag*loads(nf(1,i)) > xmax)                          &
     xmax=g_coord(1,i)+dismag*loads(nf(1,i))      
   IF(g_coord(2,i)+dismag*loads(nf(2,i)) < ymin)                          &
     ymin=g_coord(2,i)+dismag*loads(nf(2,i))      
   IF(g_coord(2,i)+dismag*loads(nf(2,i)) > ymax)                          &
     ymax=g_coord(2,i)+dismag*loads(nf(2,i))      
!
   IF(g_coord(1,i)<xmin)xmin=g_coord(1,i)
   IF(g_coord(1,i)>xmax)xmax=g_coord(1,i)
   IF(g_coord(2,i)<ymin)ymin=g_coord(2,i)
   IF(g_coord(2,i)>ymax)ymax=g_coord(2,i)
 END DO
!
 width=xmax-xmin
 height=ymax-ymin
!
!                       allow 1.5" margin minimum on each side of figure
!
!                       Portrait mode 
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
 WRITE(ips,'(a)')'%!PS-Adobe-1.0'
 WRITE(ips,'(a)')'%%DocumentFonts: none'
 WRITE(ips,'(a)')'%%Pages: 1'
 WRITE(ips,'(a)')'%%EndComments'
 WRITE(ips,'(a)')'/m {moveto} def'
 WRITE(ips,'(a)')'/l {lineto} def'
 WRITE(ips,'(a)')'/s {stroke} def'
 WRITE(ips,'(a)')'/c {closepath} def'
 WRITE(ips,'(a)')'/edef {exch def} bind def'
 WRITE(ips,'(a)')                                                         &
   '/arrow {/@to_y edef /@to_x edef /@from_y edef /@from_x edef'
 WRITE(ips,'(a)')'/@dx @to_x @from_x sub def /@dy @to_y @from_y sub def'
 WRITE(ips,'(a)')'/@length @dx @dx mul @dy @dy mul add sqrt def'
 WRITE(ips,'(a)')'/@angle @dy @dx atan def'
 WRITE(ips,'(a)')'gsave @from_x @from_y translate @angle rotate'
 WRITE(ips,'(a)')                                                         &
   '0 0 moveto @length 0 lineto currentpoint stroke newpath moveto'
 WRITE(ips,'(a)')'-4 -2 rlineto @length 0 moveto'
 WRITE(ips,'(a)')'-4  2 rlineto stroke grestore'
 WRITE(ips,'(a)')'} def'
 WRITE(ips,'(a)')'/*sf {'
 WRITE(ips,'(a)')'exch findfont exch'
 WRITE(ips,'(a)')                                                         &
   'dup type /arraytype eq {makefont}{scalefont} ifelse setfont'
 WRITE(ips,'(a)')'} bind def'
 WRITE(ips,'(a)')'/languagelevel where'
 WRITE(ips,'(a)')'{pop languagelevel} {1} ifelse'
 WRITE(ips,'(a)')'2 lt { % ifelse'
 WRITE(ips,'(a)')'/sf /*sf load def'
 WRITE(ips,'(a)')'} { % else'
 WRITE(ips,'(a)')'/sf /selectfont load def'
 WRITE(ips,'(a)')'} ifelse'
 WRITE(ips,'(a)')'%%EndProlog'
 WRITE(ips,'(a)')'%%Page: 0 1'
 WRITE(ips,'(a)')'gsave'
!
 WRITE(ips,'(2f9.2,a)')xo,yo, ' translate'
 WRITE(ips,'(f9.2,a)')0.5,' setlinewidth'
!
!                       draw the displacement vectors
!
 vmax=zero
 DO i=1,nn
   vlen=loads(nf(1,i))**2+loads(nf(2,i))**2
   IF(vlen>vmax)vmax=vlen
 END DO
 vmax=SQRT(vmax)*cutoff      
 DO i=1,nn
   vlen=SQRT(loads(nf(1,i))**2+loads(nf(2,i))**2)
   x1=sxy*(g_coord(1,i)-xmin)
   y1=sxy*(g_coord(2,i)-ymin)
   x2=sxy*(g_coord(1,i)+dismag*loads(nf(1,i))-xmin)
   y2=sxy*(g_coord(2,i)+dismag*loads(nf(2,i))-ymin)
   IF(vlen>vmax)THEN
     WRITE(ips,'(2f9.2,a,2f9.2,a)') x1, y1,' ', x2, y2, ' arrow'
     WRITE(ips,'(a)') 's'
   END IF
 END DO
!
!                       draw the mesh border                          
!
 nels=UBOUND(g_num,2)
 nod=UBOUND(g_num,1)
 IF(nod==3.OR.nod==6.OR.nod==10.OR.nod==15)ns=3
 IF(nod==4.OR.nod==5.OR.nod==8.OR.nod==9)ns=4
 ALLOCATE(corner(ns,2))
 IF(nod== 3)corner=RESHAPE((/1,2,3,2,3,1/),(/3,2/))
 IF(nod== 6)corner=RESHAPE((/1,3,5,3,5,1/),(/3,2/))
 IF(nod==10)corner=RESHAPE((/1,4,7,4,7,1/),(/3,2/))
 IF(nod==15)corner=RESHAPE((/1,5,9,5,9,1/),(/3,2/))
 IF(nod== 4)corner=RESHAPE((/1,2,3,4,2,3,4,1/),(/4,2/))
 IF(nod== 5)corner=RESHAPE((/1,2,3,4,2,3,4,1/),(/4,2/))
 IF(nod== 8)corner=RESHAPE((/1,3,5,7,3,5,7,1/),(/4,2/))
 IF(nod== 9)corner=RESHAPE((/1,3,5,7,3,5,7,1/),(/4,2/))
 DO i=1,nels
   DO j=1,ns
     draw=.TRUE.
     i1=g_num(corner(j,1),i)
     i2=g_num(corner(j,2),i)
     DO k=1,nels
       DO l=1,ns
         j1=g_num(corner(l,1),k)
         j2=g_num(corner(l,2),k)
         IF((i1==j2).AND.(i2==j1))THEN
           draw=.FALSE.
           EXIT
         END IF
       END DO
       IF(.NOT.draw)EXIT
     END DO
     IF(draw)THEN
       x1=sxy*(g_coord(1,i1)-xmin)
       y1=sxy*(g_coord(2,i1)-ymin)
       WRITE(ips,'(2f9.2,a)')x1, y1,' m'
       x1=sxy*(g_coord(1,i2)-xmin)
       y1=sxy*(g_coord(2,i2)-ymin)
       WRITE(ips,'(2f9.2,a)')x1, y1,' l'
       WRITE(ips,'(a)')' s'
     END IF
   END DO
 END DO
!                       close output file?
 WRITE(ips,'(a)')'grestore'
 WRITE(ips,'(a)')'showpage'
 CLOSE(ips)
RETURN
END SUBROUTINE vecmsh

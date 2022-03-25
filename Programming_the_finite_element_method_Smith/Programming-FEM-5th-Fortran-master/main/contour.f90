SUBROUTINE contour(loads,g_coord,g_num,ned,argv,nlen,ips)
!
! This subroutine produces a PostScript output file "*.con" displaying
! a contour map of loads over the finite element mesh.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN):: loads(0:),g_coord(:,:)
 INTEGER,INTENT(IN)::g_num(:,:),ned,ips,nlen
 CHARACTER(*),INTENT(IN)::argv
 REAL(iwp)::xmin,xmax,ymin,ymax,width,height,scale=72,sxy,xo,yo
 REAL(iwp)::pmin,pmax,ratio,x12,y12,x23,y23,x34,y34,x41,y41,x1,y1
 REAL(iwp)::pt5=0.5_iwp,elvn=11.0_iwp,ept5=8.5_iwp,eight=8.0_iwp,         &
   onept5=1.5_iwp,fpt5=5.5_iwp
 LOGICAL::s12,s23,s34,s41,draw
 INTEGER,ALLOCATABLE::corner(:,:)
 REAL(iwp),ALLOCATABLE::cont(:)
 INTEGER::i,j,k,l,nn,nels,nci,nod,ns,i1,i2,j1,j2
!
 OPEN(ips,FILE=argv(1:nlen)//'.con')
!
!                       compute size of mesh
!
 nn=UBOUND(g_coord,2)
 nod= UBOUND(g_num,1)
 nels=UBOUND(g_num,2)
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
!
!                       allow 1.5" margin minimum on each side of figure
!
!                       portrait mode 
!
 IF(height.GE.elvn/ept5*width)THEN 
!
!                       height governs the scale
!
   sxy=scale*eight/height
   xo=scale*pt5*(ept5-eight*width/height)
   yo=scale*onept5
 ELSE
!
!                       width governs the scale
!
   sxy=scale*fpt5/width
   xo=scale*onept5
   yo=scale*pt5*(elvn-fpt5*height/width)
 END IF
!
!                       find range of potentials and contour values
!
 nci=ned+1
 pmin=MINVAL(loads(1:))
 pmax=MAXVAL(loads(1:))
 ALLOCATE(cont(nci))
 cont(1)=pmin
 DO i=2,nci
   cont(i)=cont(i-1)+(pmax-pmin)/ned
 END DO
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
 WRITE(ips,'(a)')'%%EndProlog'
 WRITE(ips,'(a)')'%%Page: 0 1'
 WRITE(ips,'(a)')'gsave'
!
 WRITE(ips,'(2f9.2,a)') xo, yo, ' translate'
 WRITE(ips,'(f9.2,a)') 1.0, ' setlinewidth'
!
!                       draw the mesh outline
!
 IF(nod==3.OR.nod==6.OR.nod==10.OR.nod==15)ns=3
 IF(nod==4.OR.nod==8.OR.nod==9)ns=4
 ALLOCATE(corner(ns,2))
 IF(nod== 3)corner=RESHAPE((/1,2,3,2,3,1/),(/3,2/))
 IF(nod== 6)corner=RESHAPE((/1,3,5,3,5,1/),(/3,2/))
 IF(nod==10)corner=RESHAPE((/1,4,7,4,7,1/),(/3,2/))
 IF(nod==15)corner=RESHAPE((/1,5,9,5,9,1/),(/3,2/))
 IF(nod== 4)corner=RESHAPE((/1,2,3,4,2,3,4,1/),(/4,2/))
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
       WRITE(ips,'(2f9.2,a)') x1, y1,' m'
       x1=sxy*(g_coord(1,i2)-xmin)
       y1=sxy*(g_coord(2,i2)-ymin)
       WRITE(ips,'(2f9.2,a)') x1, y1,' l'
       WRITE(ips,'(a)')' s'
     END IF
   END DO
 END DO
!
!                       check intersection of contours with each element
!
 WRITE(ips,'(f9.2,a)') 0.5, ' setlinewidth'
 DO i=2,nci-1
   DO j=1,nels
     s12=.FALSE.
     s23=.FALSE.
     s34=.FALSE.
     s41=.FALSE.
     IF((loads(g_num(1,j))<=cont(i).AND.loads(g_num(2,j))>cont(i)).OR.    &
       (loads(g_num(2,j))<=cont(i).AND.loads(g_num(1,j))>cont(i)))        &
       s12=.TRUE.
     IF((loads(g_num(2,j))<=cont(i).AND.loads(g_num(3,j))>cont(i)).OR.    &
       (loads(g_num(3,j))<=cont(i).AND.loads(g_num(2,j))>cont(i)))        &
       s23=.TRUE.
     IF((loads(g_num(3,j))<=cont(i).AND.loads(g_num(4,j))>cont(i)).OR.    &
       (loads(g_num(4,j))<=cont(i).AND.loads(g_num(3,j))>cont(i)))        &
       s34=.TRUE.
     IF((loads(g_num(4,j))<=cont(i).AND.loads(g_num(1,j))>cont(i)).OR.    &
       (loads(g_num(1,j))<=cont(i).AND.loads(g_num(4,j))>cont(i)))        &
       s41=.TRUE.
     IF(s12)THEN
       ratio=(cont(i)-loads(g_num(1,j)))/                                 &
       (loads(g_num(2,j))-loads(g_num(1,j)))
       x12=sxy*(g_coord(1,g_num(1,j))+                                    &
         ratio*(g_coord(1,g_num(2,j))-g_coord(1,g_num(1,j)))-xmin)
       y12=sxy*(g_coord(2,g_num(1,j))+                                    &
         ratio*(g_coord(2,g_num(2,j))-g_coord(2,g_num(1,j)))-ymin)
     END IF
     IF(s23)THEN
       ratio=(cont(i)-loads(g_num(2,j)))/                                 &
         (loads(g_num(3,j))-loads(g_num(2,j)))
       x23=sxy*(g_coord(1,g_num(2,j))+                                    &
         ratio*(g_coord(1,g_num(3,j))-g_coord(1,g_num(2,j)))-xmin)
       y23=sxy*(g_coord(2,g_num(2,j))+                                    &
         ratio*(g_coord(2,g_num(3,j))-g_coord(2,g_num(2,j)))-ymin)
     END IF
     IF(s34)THEN
       ratio=(cont(i)-loads(g_num(3,j)))/                                 &
         (loads(g_num(4,j))-loads(g_num(3,j)))
       x34=sxy*(g_coord(1,g_num(3,j))+                                    &
         ratio*(g_coord(1,g_num(4,j))-g_coord(1,g_num(3,j)))-xmin)
       y34=sxy*(g_coord(2,g_num(3,j))+                                    &
         ratio*(g_coord(2,g_num(4,j))-g_coord(2,g_num(3,j)))-ymin)
     END IF
     IF(s41)THEN
       ratio=(cont(i)-loads(g_num(4,j)))/                                 &
         (loads(g_num(1,j))-loads(g_num(4,j)))
       x41=sxy*(g_coord(1,g_num(4,j))+                                    &
         ratio*(g_coord(1,g_num(1,j))-g_coord(1,g_num(4,j)))-xmin)
       y41=sxy*(g_coord(2,g_num(4,j))+                                    &
         ratio*(g_coord(2,g_num(1,j))-g_coord(2,g_num(4,j)))-ymin)
     END IF
!
!                       draw contours 
!
     IF(s12)THEN
       WRITE(ips,'(2f9.2,a)')x12,y12,' m'
       IF(s23)WRITE(ips,'(2f9.2,a)')x23,y23,' l s'
       IF(s34)WRITE(ips,'(2f9.2,a)')x34,y34,' l s'
       IF(s41)WRITE(ips,'(2f9.2,a)')x41,y41,' l s'
     END IF
     IF(s23)THEN
       WRITE(ips,'(2f9.2,a)')x23,y23,' m'
       IF(s34)WRITE(ips,'(2f9.2,a)')x34,y34,' l s'
       IF(s41)WRITE(ips,'(2f9.2,a)')x41,y41,' l s'
     END IF
     IF(s34.AND.s41)THEN
       WRITE(ips,'(2f9.2,a)')x34,y34,' m'
       WRITE(ips,'(2f9.2,a)')x41,y41,' l s'
     END IF
   END DO
 END DO
!                       close output file
 WRITE(ips,'(a)')'grestore'
 WRITE(ips,'(a)')'showpage'
 CLOSE(ips)
!
RETURN
END SUBROUTINE contour


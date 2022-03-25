SUBROUTINE geom_rect(element,iel,x_coords,y_coords,coord,num,dir)
! Used in p73
! This subroutine forms the coordinates and connectivity for a
! rectangular mesh of rt. angled triangular elements (3, 6, 10 or 15-node)
! or quadrilateral elements (4, 8 or 9-node) counting in the
! x- or y-dir. 
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x_coords(:),y_coords(:)
 REAL(iwp),INTENT(OUT)::coord(:,:)
 CHARACTER(LEN=15),INTENT(IN)::element
 CHARACTER(LEN=1),INTENT(IN)::dir
 INTEGER,INTENT(IN)::iel
 INTEGER,INTENT(OUT)::num(:)
 INTEGER::ip,iq,jel,fac1,nod,nxe,nye
 REAL(iwp)::pt5=0.5_iwp,two=2.0_iwp,d3=3.0_iwp 
 nxe=UBOUND(x_coords,1)-1
 nod=UBOUND(num,1)
 IF(element=='triangle')THEN
   nye=(UBOUND(y_coords,1)-1)*2
   IF(dir=='x'.OR.dir=='r')THEN
     jel=2*nxe*((iel-1)/(2*nxe))
     ip=(iel-jel+1)/2
     iq=2*((iel-1)/(2*nxe)+1)-1+((iel/2)*2)/iel
   ELSE  
     jel=(iel-1)/nye
     ip=jel+1
     iq=iel-nye*jel
   END IF
   SELECT CASE(nod)
   CASE(3)
     IF(MOD(iq,2)/=0)THEN
       IF(dir=='x'.OR.dir=='r')THEN
         num(1)=(nxe+1)*(iq-1)/2+ip
         num(2)=num(1)+1              
         num(3)=(nxe+1)*(iq+1)/2+ip
       ELSE
         num(1)=(ip-1)*(nye+2)/2+(iq+1)/2
         num(2)=num(1)+(nye+2)/2
         num(3)=num(1)+1
       END IF
!
       coord(1,1)=x_coords(ip)
       coord(1,2)=y_coords((iq+1)/2)
       coord(2,1)=x_coords(ip+1)   
       coord(2,2)=y_coords((iq+1)/2)
       coord(3,1)=x_coords(ip)   
       coord(3,2)=y_coords((iq+3)/2)
     ELSE
       IF(dir=='x'.OR.dir=='r')THEN
         num(1)=(nxe+1)*iq/2+ip+1     
         num(2)=num(1)-1               
         num(3)=(nxe+1)*(iq-2)/2+ip+1
       ELSE
         num(1)=ip*(nye+2)/2+(iq+2)/2
         num(2)=(ip-1)*(nye+2)/2+(iq+1)/2+1
         num(3)=num(1)-1
       END IF
!
       coord(1,1)=x_coords(ip+1)
       coord(1,2)=y_coords((iq+2)/2)
       coord(2,1)=x_coords(ip)   
       coord(2,2)=y_coords((iq+2)/2)
       coord(3,1)=x_coords(ip+1) 
       coord(3,2)=y_coords(iq/2)
     END IF
   CASE(6)
     IF(MOD(iq,2)/=0)THEN
       IF(dir=='x'.OR.dir=='r')THEN
         num(1)=(iq-1)*(2*nxe+1)+2*ip-1
         num(2)=num(1)+1 
         num(3)=num(1)+2 
         num(4)=(iq-1)*(2*nxe+1)+2*nxe+2*ip+1
         num(5)=(iq+1)*(2*nxe+1)+2*ip-1
         num(6)=num(4)-1 
       ELSE
         num(1)=2*(nye+1)*(ip-1)+iq
         num(2)=2*(nye+1)*(ip-1)+nye+1+iq
         num(3)=2*(nye+1)*ip+iq
         num(4)=num(2)+1
         num(5)=num(1)+2 
         num(6)=num(1)+1
       END IF
!
       coord(1,1)=x_coords(ip)
       coord(1,2)=y_coords((iq+1)/2)
       coord(3,1)=x_coords(ip+1)   
       coord(3,2)=y_coords((iq+1)/2)
       coord(5,1)=x_coords(ip)   
       coord(5,2)=y_coords((iq+3)/2)
     ELSE
       IF(dir=='x'.OR.dir=='r')THEN
         num(1)=iq*(2*nxe+1)+2*ip+1
         num(2)=num(1)-1 
         num(3)=num(1)-2 
         num(4)=(iq-2)*(2*nxe+1)+2*nxe+2*ip+1
         num(5)=(iq-2)*(2*nxe+1)+2*ip+1
         num(6)=num(4)+1 
       ELSE 
         num(1)=2*(nye+1)*ip+iq+1 
         num(2)=2*(nye+1)*(ip-1)+nye+iq+2
         num(3)=2*(nye+1)*(ip-1)+iq+1
         num(4)=num(2)-1 
         num(5)=num(1)-2
         num(6)=num(1)-1
       END IF
!
       coord(1,1)=x_coords(ip+1)
       coord(1,2)=y_coords((iq+2)/2)
       coord(3,1)=x_coords(ip)   
       coord(3,2)=y_coords((iq+2)/2)
       coord(5,1)=x_coords(ip+1) 
       coord(5,2)=y_coords(iq/2)
     END IF
     coord(2,:)=pt5*(coord(1,:)+coord(3,:))
     coord(4,:)=pt5*(coord(3,:)+coord(5,:))
     coord(6,:)=pt5*(coord(5,:)+coord(1,:))
   CASE(10)
     IF(MOD(iq,2)/=0)THEN
       IF(dir=='x'.OR.dir=='r')THEN
         num(1)=(iq-1)/2*(3*nxe+1)*3+3*ip-2
         num(2)=num(1)+1
         num(3)=num(1)+2
         num(4)=num(1)+3
         num(5)=(iq-1)/2*(3*nxe+1)*3+3*nxe+1+3*ip
         num(6)=(iq-1)/2*(3*nxe+1)*3+6*nxe+2+3*ip-1
         num(7)=(iq-1)/2*(3*nxe+1)*3+9*nxe+3+3*ip-2
         num(8)=num(6)-1
         num(9)=num(5)-2
         num(10)=num(9)+1
       ELSE
         num(1)=(9*(nye-2)/2+12)*(ip-1)+3*(iq-1)/2+1
         num(2)=(9*(nye-2)/2+12)*(ip-1)+3*(nye-2)/2+4+3*(iq-1)/2+1
         num(3)=(9*(nye-2)/2+12)*(ip-1)+3*(nye-2)+8+3*(iq-1)/2+1
         num(4)=(9*(nye-2)/2+12)*(ip-1)+9*(nye-2)/2+12+3*(iq-1)/2+1
         num(5)=num(3)+1 
         num(6)=num(2)+2
         num(7)=num(1)+3
         num(8)=num(1)+2
         num(9)=num(1)+1
         num(10)=num(2)+1
       END IF
!
       coord(1,1)=x_coords(ip)
       coord(2,1)=x_coords(ip)+(x_coords(ip+1)-x_coords(ip))/d3
       coord(3,1)=x_coords(ip)+two*(x_coords(ip+1)-x_coords(ip))/d3
       coord(4,1)=x_coords(ip+1)
       coord(4,2)=y_coords((iq+1)/2)
       coord(5,2)=y_coords((iq+1)/2)+                                     &
         (y_coords((iq+3)/2)-y_coords((iq+1)/2))/d3
       coord(6,2)=y_coords((iq+1)/2)+                                     &
         two*(y_coords((iq+3)/2)-y_coords((iq+1)/2))/d3
       coord(7,2)=y_coords((iq+3)/2)
     ELSE
       IF(dir=='x'.OR.dir=='r')THEN
         num(1)=(iq-2)/2*(3*nxe+1)*3+9*nxe+3+3*ip+1
         num(2)=num(1)-1
         num(3)=num(1)-2
         num(4)=num(1)-3
         num(5)=(iq-2)/2*(3*nxe+1)*3+6*nxe+2+3*ip-1
         num(6)=(iq-2)/2*(3*nxe+1)*3+3*nxe+1+3*ip
         num(7)=(iq-2)/2*(3*nxe+1)*3+3*ip+1
         num(8)=num(6)+1
         num(9)=num(5)+2
         num(10)=num(9)-1
       ELSE
         num(1)=(9*(nye-2)/2+12)*(ip-1)+9*(nye-2)/2+12+3*iq/2+1
         num(2)=(9*(nye-2)/2+12)*(ip-1)+3*(nye-2)+8+3*iq/2+1
         num(3)=(9*(nye-2)/2+12)*(ip-1)+3*(nye-2)/2+4+3*iq/2+1
         num(4)=(9*(nye-2)/2+12)*(ip-1)+3*iq/2+1
         num(5)=num(3)-1
         num(6)=num(2)-2
         num(7)=num(1)-3
         num(8)=num(1)-2
         num(9)=num(1)-1
         num(10)=num(2)-1
       END IF
!
       coord(1,1)=x_coords(ip+1)
       coord(2,1)=x_coords(ip+1)-(x_coords(ip+1)-x_coords(ip))/d3
       coord(3,1)=x_coords(ip+1)-two*(x_coords(ip+1)-x_coords(ip))/d3
       coord(4,1)=x_coords(ip)
       coord(4,2)=y_coords((iq+2)/2)
       coord(5,2)=y_coords((iq+2)/2)-(y_coords((iq+2)/2)-y_coords(iq/2))/d3
       coord(6,2)=y_coords((iq+2)/2)-                                     &
         two*(y_coords((iq+2)/2)-y_coords(iq/2))/d3
       coord(7,2) =y_coords(iq/2)
     END IF
     coord(5,1)=coord(3,1)
     coord(6,1)=coord(2,1)
     coord(7,1)=coord(1,1)
     coord(8,1)=coord(1,1)
     coord(9,1)=coord(1,1)
     coord(10,1)=coord(2,1)
     coord(1,2)=coord(4,2)
     coord(2,2)=coord(4,2)
     coord(3,2)=coord(4,2)
     coord(8,2)=coord(6,2)
     coord(9,2)=coord(5,2)
     coord(10,2)=coord(5,2)
   CASE(15)
     IF(MOD(iq,2)/=0)THEN
       IF(dir=='x'.OR.dir=='r')THEN
       fac1=4*(4*nxe+1)*(iq-1)/2
         num(1)=fac1+4*ip-3
         num(2)=num(1)+1
         num(3)=num(1)+2
         num(4)=num(1)+3
         num(5)=num(1)+4
         num(6)=fac1+ 4*nxe+1+4*ip
         num(7)=fac1+ 8*nxe+1+4*ip
         num(8)=fac1+12*nxe+1+4*ip
         num(9)=fac1+16*nxe+1+4*ip
         num(10)=num(8)-1
         num(11)=num(7)-2
         num(12)=num(6)-3
         num(13)=num(12)+1
         num(14)=num(12)+2
         num(15)=num(11)+1
       ELSE
         fac1=4*(2*nye+1)*(ip-1)+2*iq-1 
         num(1)=fac1
         num(2)=fac1+2*nye+1
         num(3)=fac1+4*nye+2 
         num(4)=fac1+6*nye+3 
         num(5)=fac1+8*nye+4
         num(6)=fac1+6*nye+4 
         num(7)=fac1+4*nye+4 
         num(8)=fac1+2*nye+4
         num(9)=fac1+4 
         num(10)=fac1+3 
         num(11)=fac1+2 
         num(12)=fac1+1
         num(13)=fac1+2*nye+2 
         num(14)=fac1+4*nye+3
         num(15)=fac1+2*nye+3  
       END IF
!
       coord(1,1)=x_coords(ip)
       coord(1,2)=y_coords((iq+1)/2)
       coord(5,1)=x_coords(ip+1)   
       coord(5,2)=y_coords((iq+1)/2)
       coord(9,1)=x_coords(ip)   
       coord(9,2)=y_coords((iq+3)/2)
     ELSE
       IF(dir=='x'.OR.dir=='r')THEN
         fac1=4*(4*nxe+1)*(iq-2)/2
         num(1)=fac1+16*nxe+5+4*ip
         num(2)=num(1)-1
         num(3)=num(1)-2
         num(4)=num(1)-3
         num(5)=num(1)-4
         num(6)=fac1+12*nxe+1+4*ip
         num(7)=fac1+8*nxe+1+4*ip
         num(8)=fac1+4*nxe+1+4*ip
         num(9)=fac1+4*ip+1
         num(10)=num(8)+1
         num(11)=num(7)+2
         num(12)=num(6)+3
         num(13)=num(12)-1
         num(14)=num(12)-2
         num(15)=num(11)-1
       ELSE
         fac1=4*(2*nye+1)*(ip-1)+2*iq+8*nye+5 
         num(1)=fac1 
         num(2)=fac1-2*nye-1
         num(3)=fac1-4*nye-2 
         num(4)=fac1-6*nye-3 
         num(5)=fac1-8*nye-4
         num(6)=fac1-6*nye-4  
         num(7)=fac1-4*nye-4 
         num(8)=fac1-2*nye-4
         num(9)=fac1-4
         num(10)=fac1-3 
         num(11)=fac1-2 
         num(12)=fac1-1
         num(13)=fac1-2*nye-2  
         num(14)=fac1-4*nye-3
         num(15)=fac1-2*nye-3 
       END IF
!
       coord(1,1)=x_coords(ip+1)
       coord(1,2)=y_coords((iq+2)/2)
       coord(5,1)=x_coords(ip)   
       coord(5,2)=y_coords((iq+2)/2)
       coord(9,1)=x_coords(ip+1) 
       coord(9,2)=y_coords(iq/2)
     END IF
     coord(3,:)=pt5*(coord(1,:)+coord(5,:))
     coord(7,:)=pt5*(coord(5,:)+coord(9,:))
     coord(11,:)=pt5*(coord(9,:)+coord(1,:))
     coord(2,:)=pt5*(coord(1,:)+coord(3,:))
     coord(4,:)=pt5*(coord(3,:)+coord(5,:))
     coord(6,:)=pt5*(coord(5,:)+coord(7,:))
     coord(8,:)=pt5*(coord(7,:)+coord(9,:))
     coord(10,:)=pt5*(coord(9,:)+coord(11,:))
     coord(12,:)=pt5*(coord(11,:)+coord(1,:))
     coord(15,:)=pt5*(coord(7,:)+coord(11,:))
     coord(14,:)=pt5*(coord(3,:)+coord(7,:))
     coord(13,:)=pt5*(coord(2,:)+coord(15,:))
   CASE DEFAULT
     WRITE(11,'(a)')"Wrong number of nodes for triangular element"
     STOP
   END SELECT
 ELSE
   nye=UBOUND(y_coords,1)-1
   IF(dir=='x'.OR.dir=='r')THEN
     iq=(iel-1)/nxe+1
     ip=iel-(iq-1)*nxe
   ELSE
     ip=(iel-1)/nye+1
     iq=iel-(ip-1)*nye
   END IF
   SELECT CASE(nod)
   CASE(4)
     IF(dir=='x'.OR.dir=='r')THEN
       num(1)=iq*(nxe+1)+ip		        		
       num(2)=(iq-1)*(nxe+1)+ip				
       num(3)=num(2)+1					
       num(4)=num(1)+1					
     ELSE
       num(1)=(ip-1)*(nye+1)+iq+1
       num(2)=num(1)-1
       num(3)=ip*(nye+1)+iq
       num(4)=num(3)+1
     END IF
!
     coord(1:2,1)=x_coords(ip)
     coord(3:4,1)=x_coords(ip+1)
     coord(1,2)=y_coords(iq+1)
     coord(2:3,2)=y_coords(iq)
     coord(4,2)=coord(1,2)
   CASE(5)
     IF(dir=='x'.OR.dir=='r')THEN
       num(1)=iq*(2*nxe+1)+ip
       num(2)=(iq-1)*(2*nxe+1)+ip
       num(3)=num(2)+1
       num(4)=num(1)+1
       num(5)=iq*(2*nxe+1)+ip-nxe
     ELSE
       num(1)=(ip-1)*(2*nye+1)+iq+1
       num(2)=num(1)-1
       num(3)=ip*(2*nye+1)+iq
       num(4)=num(3)+1
       num(5)=ip*(2*nye+1)+iq-nye
     END IF
!
     coord(1:2,1)=x_coords(ip)
     coord(3:4,1)=x_coords(ip+1)
     coord(1,2)=y_coords(iq+1)
     coord(2:3,2)=y_coords(iq)
     coord(4,2)=coord(1,2)
     coord(5,:)=0.25_iwp*(coord(1,:)+coord(2,:)+coord(3,:)+coord(4,:))
   CASE(8)
     IF(dir=='x'.OR.dir=='r')THEN
       num(1)=iq*(3*nxe+2)+2*ip-1                 
       num(2)=iq*(3*nxe+2)+ip-nxe-1		  
       num(3)=(iq-1)*(3*nxe+2)+2*ip-1		   
       num(4)=num(3)+1
       num(5)=num(4)+1
       num(6)=num(2)+1
       num(7)=num(1)+2
       num(8)=num(1)+1
     ELSE
       num(1)=(ip-1)*(3*nye+2)+2*iq+1
       num(2)=num(1)-1
       num(3)=num(1)-2
       num(4)=(ip-1)*(3*nye+2)+2*nye+iq+1
       num(5)=ip*(3*nye+2)+2*iq-1
       num(6)=num(5)+1
       num(7)=num(5)+2
       num(8)=num(4)+1
     END IF
!
     coord(1:3,1)=x_coords(ip)
     coord(5:7,1)=x_coords(ip+1)
     coord(4,1)=pt5*(coord(3,1)+coord(5,1))
     coord(8,1)=pt5*(coord(7,1)+coord(1,1))
     coord(1,2)=y_coords(iq+1)
     coord(7:8,2)=y_coords(iq+1)
     coord(3:5,2)=y_coords(iq)
     coord(2,2)=pt5*(coord(1,2)+coord(3,2))
     coord(6,2)=pt5*(coord(5,2)+coord(7,2))
   CASE(9)
     IF(dir=='x'.OR.dir=='r')THEN
       num(1)=iq*(4*nxe+2)+2*ip-1
       num(2)=iq*(4*nxe+2)+2*ip-nxe-4
       num(3)= (iq-1)*(4*nxe+2)+2*ip-1
       num(4)=num(3)+1
       num(5)=num(4)+1
       num(6)=num(2)+2
       num(7)=num(1)+2
       num(8)=num(1)+1
       num(9)=num(2)+1
     ELSE
       num(1)=(ip-1)*2*(2*nye+1)+2*iq+1
       num(2)=num(1)-1
       num(3)=num(1)-2
       num(4)=(ip-1)*2*(2*nye+1)+2*nye+2*iq
       num(5)=ip*2*(2*nye+1)+2*iq-1
       num(6)=num(5)+1
       num(7)=num(5)+2
       num(8)=num(4)+2
       num(9)=num(4)+1
     END IF
!
     coord(1:3,1)=x_coords(ip)
     coord(5:7,1)=x_coords(ip+1)
     coord(4,1)=pt5*(coord(3,1)+coord(5,1))
     coord(8,1)=pt5*(coord(7,1)+coord(1,1))
     coord(1,2)=y_coords(iq+1)
     coord(7:8,2)=y_coords(iq+1)
     coord(3:5,2)=y_coords(iq)
     coord(2,2)=pt5*(coord(1,2)+coord(3,2))
     coord(6,2)=pt5*(coord(5,2)+coord(7,2))
     coord(9,:)=pt5*(coord(4,:)+coord(8,:))
   CASE DEFAULT
     WRITE(11,'(a)')"Wrong number of nodes for quadrilateral element"
     STOP
   END SELECT
 END IF
RETURN
END SUBROUTINE geom_rect

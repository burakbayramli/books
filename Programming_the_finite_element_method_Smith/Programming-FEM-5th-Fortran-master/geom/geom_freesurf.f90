SUBROUTINE geom_freesurf(iel,nxe,fixed_seep,fixed_down,down,              &
  width,angs,surf,coord,num)
! Used in p73
! This subroutine forms the coordinates and steering vector
! for 4-node quads numbering in the x-direction
! (Laplace's equation, variable mesh, 1-freedom per node).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::width(:),angs(:),surf(:),down 
 !
 REAL(iwp),INTENT(OUT)::coord(:,:)
 INTEGER,INTENT(IN)::iel,nxe,fixed_seep,fixed_down
 !
 INTEGER,INTENT(OUT)::num(:)
 REAL(iwp)::angr(SIZE(angs)),pi,b1,b2,tan1,tan2,fac1,fac2,zero=0.0_iwp,   &
   pt5=0.5_iwp,one=1.0_iwp,small=0.0001_iwp,d180=180.0_iwp
 INTEGER::ip,iq
 pi=ACOS(-one)
 angr=angs*pi/d180   
 iq=(iel-1)/nxe+1
 ip=iel-(iq-1)*nxe
 num(1)=iq*(nxe+1)+ip
 num(2)=(iq-1)*(nxe+1)+ip
 num(3)=num(2)+1
 num(4)=num(1)+1
 IF(iq<=fixed_seep+1)THEN
   b1=(surf(ip)-down)/real(fixed_seep+1)
   b2=(surf(ip+1)-down)/real(fixed_seep+1)
   coord(1,2)=down+(fixed_seep+1-iq)*b1 
   coord(2,2)=down+(fixed_seep+2-iq)*b1
   coord(3,2)=down+(fixed_seep+2-iq)*b2 
   coord(4,2)=down+(fixed_seep+1-iq)*b2
 ELSE
   b1=real(fixed_down+fixed_seep-iq)/real(fixed_down-1) 
   b2=real(fixed_down+fixed_seep-iq+1)/real(fixed_down-1) 
   coord(1,2)=down*b1
   coord(2,2)=down*b2
   coord(3,2)=coord(2,2)
   coord(4,2)=coord(1,2)
 END IF
 IF(ABS(angr(ip)-pi*pt5)<small)THEN
   fac1=zero
 ELSE
   tan1=TAN(angr(ip))      
   fac1=one/tan1
 END IF
 IF(ABS(angr(ip+1)-pi*pt5)<small)THEN
   fac2=zero
 ELSE
   tan2=TAN(angr(ip+1))    
   fac2=one/tan2 
 END IF
 coord(1,1)=width(ip)+coord(1,2)*fac1 
 coord(2,1)=width(ip)+coord(2,2)*fac1
 coord(3,1)=width(ip+1)+coord(3,2)*fac2 
 coord(4,1)=width(ip+1)+coord(4,2)*fac2
RETURN
END SUBROUTINE geom_freesurf                                               

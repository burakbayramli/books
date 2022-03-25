SUBROUTINE bmat_nonaxi(bee,radius,coord,deriv,fun,iflag,lth)
!
! This subroutine forms the strain-displacement matrix for
! axisymmetric solids subjected to non-axisymmetric loading.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::deriv(:,:),fun(:),coord(:,:)
 REAL(iwp),INTENT(OUT)::bee(:,:),radius
 INTEGER,INTENT(IN)::iflag,lth
 INTEGER::nod,k,l,m,n
 nod=UBOUND(deriv,2) 
 bee=0.0_iwp
 radius=SUM(fun*coord(:,1))
 DO m=1,nod
   n=3*m      
   k=n-1      
   l=k-1
   bee(1,l)=deriv(1,m)
   bee(2,k)=deriv(2,m)
   bee(3,l)=fun(m)/radius
   bee(3,n)=iflag*lth*bee(3,l) 
   bee(4,l)=deriv(2,m)
   bee(4,k)=deriv(1,m)
   bee(5,k)=-iflag*lth*fun(m)/radius 
   bee(5,n)=deriv(2,m)
   bee(6,l)=bee(5,k) 
   bee(6,n)=deriv(1,m)-fun(m)/radius
 END DO
RETURN
END SUBROUTINE bmat_nonaxi



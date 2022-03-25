SUBROUTINE fmkdke(km,kp,c,ke,kd,theta)
!
! This subroutine builds up the 'coupled' stiffnesses ke and kd from 
! the 'elastic' stiffness km, fluid stiffness kp and coupling matrix c.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:),kp(:,:),c(:,:),theta
 REAL(iwp),INTENT(OUT)::ke(:,:),kd(:,:)
 INTEGER::ndof
 REAL::one=1.0_iwp
 ndof=SIZE(km,1)
 ke(:ndof,:ndof)=theta*km
 ke(:ndof,ndof+1:)=theta*c
 ke(ndof+1:,:ndof)=theta*TRANSPOSE(c)
 ke(ndof+1:,ndof+1:)=-theta**2*kp
 kd(:ndof,:ndof)=(theta-one)*km
 kd(:ndof,ndof+1:)=(theta-one)*c
 kd(ndof+1:,:ndof)=ke(ndof+1:,:ndof)
 kd(ndof+1:,ndof+1:)=theta*(one-theta)*kp
RETURN
END SUBROUTINE fmkdke
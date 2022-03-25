SUBROUTINE formke(km,kp,c,ke,theta)
!
! This subroutine creates the ke matrix for incremental Biot.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::km(:,:),kp(:,:),c(:,:),theta
 REAL(iwp),INTENT(OUT)::ke(:,:)
 INTEGER::ndof
 ndof=UBOUND(km,1)
 ke(:ndof,:ndof)=km 
 ke(:ndof,ndof+1:)=c
 ke(ndof+1:,:ndof)=TRANSPOSE(c) 
 ke(ndof+1:,ndof+1:)=-theta*kp
RETURN  
END SUBROUTINE formke

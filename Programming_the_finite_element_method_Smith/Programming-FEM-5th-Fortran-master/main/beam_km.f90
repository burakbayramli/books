SUBROUTINE beam_km(km,ei,ell)
! Used in p43, p46, p101, p111
! This subroutine forms the stiffness matrix of a
! beam element (bending only).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::ei,ell
 REAL(iwp),INTENT(OUT)::km(:,:)
 REAL(iwp)::two=2.0_iwp,d4=4.0_iwp,d6=6.0_iwp,d12=12.0_iwp
 km(1,1)=d12*ei/(ell*ell*ell) 
 km(3,3)=km(1,1)
 km(1,2)=d6*ei/(ell*ell) 
 km(2,1)=km(1,2) 
 km(1,4)=km(1,2)
 km(4,1)=km(1,4) 
 km(1,3)=-km(1,1) 
 km(3,1)=km(1,3) 
 km(3,4)=-km(1,2)
 km(4,3)=km(3,4) 
 km(2,3)=km(3,4) 
 km(3,2)=km(2,3)
 km(2,2)=d4*ei/ell
 km(4,4)=km(2,2) 
 km(2,4)=two*ei/ell 
 km(4,2)=km(2,4)
RETURN
END SUBROUTINE beam_km                                                      

SUBROUTINE rod_km(km,ea,length)
!
! This subroutine forms the stiffness matrix of a 1-d "rod" element.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::ea,length
 REAL(iwp),INTENT(OUT)::km(:,:)
 REAL(iwp)::one=1.0_iwp
 km(1,1)=one
 km(2,2)=one
 km(1,2)=-one
 km(2,1)=-one
 km=km*ea/length
RETURN
END SUBROUTINE rod_km

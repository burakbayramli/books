SUBROUTINE beam_mm(mm,fs,ell)
!
! This subroutine forms the consistent mass matrix of a beam element.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::fs,ell
 REAL(iwp),INTENT(OUT)::mm(:,:)
 REAL(iwp)::fac
 fac=(fs*ell)/420.0_iwp 
 mm(1,1)=156.0_iwp*fac
 mm(3,3)=mm(1,1)
 mm(1,2)=22.0_iwp*ell*fac
 mm(2,1)=mm(1,2)
 mm(3,4)=-mm(1,2)
 mm(4,3)=mm(3,4)
 mm(1,3)=54.0_iwp*fac
 mm(3,1)=mm(1,3)
 mm(1,4)=-13.0_iwp*ell*fac
 mm(4,1)=mm(1,4)
 mm(2,3)=-mm(1,4)
 mm(3,2)=mm(2,3)
 mm(2,2)=4.0_iwp*(ell**2)*fac
 mm(4,4)=mm(2,2)
 mm(2,4)=-3.0_iwp*(ell**2)*fac
 mm(4,2)=mm(2,4)
RETURN
END SUBROUTINE beam_mm  

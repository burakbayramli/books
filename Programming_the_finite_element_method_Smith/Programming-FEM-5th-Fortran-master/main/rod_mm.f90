SUBROUTINE rod_mm(mm,length)
!
! This subroutine forms the consistent mass matrix of a 1-d "rod" element.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),intent(in)::length
 REAL(iwp),intent(out)::mm(:,:)
 REAL(iwp)::one=1.0_iwp,d3=3.0_iwp,d6=6.0_iwp
 mm(1,1)=one/d3
 mm(1,2)=one/d6
 mm(2,1)=one/d6
 mm(2,2)=one/d3
 mm=mm*length
RETURN
END SUBROUTINE rod_mm

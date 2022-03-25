SUBROUTINE vmflow(stress,dsbar,vmfl)
!
! This subroutine forms the von-Mises flow vector.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::stress(:),dsbar
 REAL(iwp),INTENT(OUT)::vmfl(:)
 REAL(iwp)::sigm,onept5=1.5_iwp,two=2.0_iwp,d3=3.0_iwp
 sigm=(stress(1)+stress(2)+stress(4))/d3
 vmfl(1)=stress(1)-sigm
 vmfl(2)=stress(2)-sigm
 vmfl(3)=stress(3)*two 
 vmfl(4)=stress(4)-sigm
 vmfl=vmfl*onept5/dsbar
RETURN
END SUBROUTINE vmflow

SUBROUTINE fmdsig(dee,e,v)
!
! This subroutine returns the elastic dee matrix for ih=3 (plane stress),
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::e,v
 REAL(iwp),INTENT(OUT)::dee(:,:)
 REAL(iwp)::zero=0.0_iwp,pt5=0.5_iwp,one=1.0_iwp
 INTEGER::ih
 dee=zero  
 ih=UBOUND(dee,1)
 SELECT CASE(ih)
 CASE(3)
   dee=zero
   dee(1,1)=e/(one-v*v)
   dee(2,2)=dee(1,1)
   dee(3,3)=pt5*e/(one+v)
   dee(1,2)=v*dee(1,1)
   dee(2,1)=dee(1,2)
 CASE DEFAULT
   WRITE(*,*)'wrong size for dee matrix'
 END SELECT
RETURN
END SUBROUTINE fmdsig   

SUBROUTINE linmul_sky(kv,disps,loads,kdiag)
!
! This subroutine forms the product of symmetric matrix stored as
! a skyline and a vector.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kv(:),disps(0:)
 REAL(iwp),INTENT(OUT)::loads(0:)
 INTEGER,INTENT(IN)::kdiag(:)
 INTEGER::n,i,j,low,lup,k
 REAL(iwp)::x,zero=0.0_iwp
 n=UBOUND(disps,1)
 DO i=1,n
   x=zero 
   lup=kdiag(i)
   IF(i==1)low=lup
   IF(i/=1)low=kdiag(i-1)+1
   DO j=low,lup
     x=x+kv(j)*disps(i+j-lup) 
   END DO
   loads(i)=x
   IF(i==1)CYCLE   
   lup=lup-1
   DO j=low,lup
     k=i+j-lup-1
     loads(k)=loads(k)+kv(j)*disps(i)        
   END DO
 END DO
RETURN
END SUBROUTINE linmul_sky  

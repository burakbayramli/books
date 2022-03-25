SUBROUTINE banmul(kb,loads,ans)
! Used in p103
! This subroutine multiplies a symmetrical band kb by the vector loads.
! 
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kb(:,:),loads(0:)
 REAL(iwp),INTENT(OUT)::ans(0:)
 INTEGER::neq,nband,i,j 
 REAL(iwp)::x,zero=0.0_iwp
 neq=UBOUND(kb,1) 
 nband=UBOUND(kb,2)-1
 DO i=1,neq
   x=zero
   DO j=nband+1,1,-1
     IF(i+j>nband+1)x=x+kb(i,j)*loads(i+j-nband-1)
   END DO
   DO j=nband,1,-1
     IF(i-j<neq-nband)x=x+kb(i-j+nband+1,j)*loads(i-j+nband+1)
   END DO
   ans(i)=x
 END DO
RETURN
END SUBROUTINE banmul

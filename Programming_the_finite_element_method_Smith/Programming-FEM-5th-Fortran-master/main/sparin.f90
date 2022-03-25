SUBROUTINE sparin(kv,kdiag)
!
! This subroutine performs Cholesky factorisation on a symmetric
! skyline global matrix.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN OUT)::kv(:)
 INTEGER,INTENT(IN)::kdiag(:)
 INTEGER::n,i,ki,l,kj,j,ll,m,k
 REAL(iwp)::x
 n=UBOUND(kdiag,1)  
 kv(1)=SQRT(kv(1))
 DO i=2,n
   ki=kdiag(i)-i
   l=kdiag(i-1)-ki+1
   DO j=l,i
     x=kv(ki+j)
     kj=kdiag(j)-j
     IF(j/=1)THEN
       ll=kdiag(j-1)-kj+1
       ll=max(l,ll)
       IF(ll/=j)THEN
         m=j-1
         DO k=ll,m 
           x=x-kv(ki+k)*kv(kj+k) 
         END DO
       END IF
     END IF
     kv(ki+j)=x/kv(kj+j)
   END DO
   kv(ki+i)=SQRT(x)
 END DO
RETURN
END SUBROUTINE sparin

SUBROUTINE stability(kv,gv,kdiag,tol,limit,iters,evec,eval)
!
! This subroutine computes the smallest eigenvalue in a beam
! stability analysis.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(IN)::limit,kdiag(:)
 INTEGER,INTENT(OUT)::iters
 INTEGER::neq
 REAL(iwp),INTENT(IN OUT)::kv(:),gv(:),tol,eval
 REAL(iwp),INTENT(OUT)::evec(:)
 REAL(iwp)::big,zero=0.0_iwp,one=1.0_iwp
 LOGICAL::converged
 REAL(iwp),ALLOCATABLE::x0(:),x1(:)
 neq=UBOUND(kdiag,1)
 ALLOCATE(x0(0:neq),x1(0:neq))
 CALL sparin(kv,kdiag)
 iters=0
 x0=zero
 x0(1)=1.0_iwp
 DO
   iters=iters+1
   CALL linmul_sky(gv,x0,x1,kdiag)
   CALL spabac(kv,x1,kdiag)  
   big=MAXVAL(x1(1:))
   IF(ABS(MINVAL(x1(1:)))>big)big=MINVAL(x1(1:))
   x1=x1/big
   converged=(MAXVAL(ABS(x1(1:)-x0(1:)))/MAXVAL(ABS(x1(1:)))<tol)
   x0=x1
   IF(converged.OR.iters==limit)EXIT
 END DO
 x1(1:)=x1(1:)/SQRT(SUM(x1(1:)**2))
 evec=x1
 eval=one/big
RETURN
CONTAINS
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
SUBROUTINE spabac(kv,loads,kdiag)
!
! This subroutine performs Cholesky forward and back-substitution
! on a symmetric skyline global matrix.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::kv(:)
 REAL(iwp),INTENT(IN OUT)::loads(0:)
 INTEGER,INTENT(IN)::kdiag(:)
 INTEGER::n,i,ki,l,m,j,it,k
 REAL(iwp)::x
 n=UBOUND(kdiag,1)
 loads(1)=loads(1)/kv(1)
 DO i=2,n
   ki=kdiag(i)-i
   l=kdiag(i-1)-ki+1 
   x=loads(i)
   IF(l/=i)THEN
     m=i-1
     DO j=l,m 
       x=x-kv(ki+j)*loads(j)
     END DO
   END IF
   loads(i)=x/kv(ki+i)
 END DO
 DO it=2,n
   i=n+2-it
   ki=kdiag(i)-i
   x=loads(i)/kv(ki+i)
   loads(i)=x
   l=kdiag(i-1)-ki+1
   IF(l/=i)THEN
     m=i-1
     DO k=l,m
       loads(k)=loads(k)-x*kv(ki+k)
     END DO
   END IF
 END DO
 loads(1)=loads(1)/kv(1)
RETURN
END SUBROUTINE spabac               
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
END SUBROUTINE stability

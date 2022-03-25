SUBROUTINE gauss_band(pb,work)
!
! This subroutine performs gaussian reduction of an unsymmetric
! banded matrix pb. Array work used as working space.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN OUT)::pb(:,:),work(:,:)
 REAL(iwp)::s,zero=0.0_iwp,small=1.e-10_iwp
 INTEGER::n,iwp1,iq,iqp,iwp11,i,j,k,l,ip,k1
 n=UBOUND(pb,1)
 iwp1=(UBOUND(pb,2)-1)/2+1
 iq=2*iwp1-1
 iqp=iwp1
 iwp11=iwp1-1
 DO i=1,iwp11
   DO j=1,iq
     IF(j>=iwp1+i)THEN
       pb(i,j)=zero
       pb(n-i+1,j)=zero
     ELSE 
       pb(i,j)=pb(i,j+iwp1-i)
     END IF
   END DO
 END DO
 DO k=1,n
   l=k+iwp1-1
   IF(l>n)l=n
   ip=0
   s=small
   DO i=k,l
     IF(ABS(pb(i,1))<=s)CYCLE
     s=ABS(pb(i,1))
     ip=i
   END DO
   IF(ip==0)THEN
     WRITE(6,'("singular")')
     EXIT
   END IF
   IF(k==n)EXIT
   work(iwp1,k)=ip
   iqp=iqp-1
   j=iwp1+ip-k
   IF(iqp<j)iqp=j
   IF(j/=iwp1)THEN
     DO j=1,iqp
       s=pb(k,j)
       pb(k,j)=pb(ip,j)
       pb(ip,j)=s
     END DO
   END IF
   k1=k+1
   DO i=k1,l
     s=pb(i,1)/pb(k,1)
     DO j=2,iq
       IF(j>iqp)THEN
         pb(i,j-1)=pb(i,j)
       ELSE 
         pb(i,j-1)=pb(i,j)-s*pb(k,j)
       END IF
     END DO
     pb(i,iq)=zero
     work(i-k,k)=s
   END DO
 END DO
RETURN
END SUBROUTINE gauss_band

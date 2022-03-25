SUBROUTINE bisect(d,e,acheps,ifail)
!
! This subroutine finds the eigenvalues of a tridiagonal matrix,
! given with its diagonal elements in the array d(n) and
! its subdiagonal elements in the last n - 1 stores of the
! array e(n), using ql transformations. The eigenvalues are
! overwritten on the diagonal elements in the array d in
! ascending order. The subroutine will fail if any one
! eigenvalue takes more than 30 iterations.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::acheps
 REAL(iwp),INTENT(IN OUT)::d(0:),e(0:)
 INTEGER,INTENT(IN OUT)::ifail
 INTEGER::m,n,i,l,j,i1,m1,ii,aux
 REAL(iwp)::b,f,h,g,p,r,c,s,zero=0.0_iwp,pt5=0.5_iwp,one=1.0_iwp,         &
   two=2.0_iwp
 n=UBOUND(d,1)
 IF(n/=1)THEN
   DO i=2,n
     e(i-1)=e(i)
   END DO
 END IF
 e(n)=zero
 b=zero
 f=zero
 loop_1: DO l=1,n
   j=0
   h=acheps*(ABS(d(l))+ABS(e(l)))
   IF(b<h)b=h
   ! look for small sub diagonal element
   DO  m=l,n
     IF(ABS(e(m))<=b)EXIT
   END DO
   IF(m/=l)THEN
     loop_2: DO
       IF(j==30)THEN
         ifail=1
         EXIT loop_1
       END IF
       j = j + 1
       ! form shift
       g = d(l)
       h = d(l+1) - g
       IF(ABS(h)<ABS(e(l)))THEN
         p=h*pt5/e(l)
         r=SQRT(p*p+one)
         h=p+r
         IF(p<zero)h=p-r
         d(l)=e(l)/h
       ELSE
         p=two*e(l)/h
         r=SQRT(p*p+one)
         d(l)=e(l)*p/(one+r)
       END IF
       h=g-d(l)
       i1=l+1
       IF(i1<=n)THEN
         DO i=i1,n
           d(i)=d(i)-h
         END DO
       END IF
       f=f+h
       ! ql transformation
       p=d(m)
       c=one
       s=zero
       m1=m-1
       loop_4: DO ii=l,m1
         i=m1-ii+l
         g=c*e(i)
         h=c*p
         IF(ABS(p)>=ABS(e(i)))THEN
           c= e(i)/p
           r=SQRT(c*c+one)
           e(i+1)=s*p*r
           s=c/r
           c=one/r
         ELSE
           c=p/e(i)
           r=SQRT(c*c+one)
           e(i+1)=s*e(i)*r
           s=one/r
           c=c/r
         END IF
         p=c*d(i)-s*g
         d(i+1)=h+s*(c*g+s*d(i))
       END DO loop_4
       e(l)=s*p
       d(l)=c*p
       IF(ABS(e(l))<=b)EXIT loop_2
     END DO loop_2
   END IF
   p=d(l)+f
   ! order eigenvalue
   aux=0
   IF(l/=1)THEN
     loop_3: DO ii=2,l
       i=l-ii+2
       IF(p>=d(i-1))THEN
         aux=1
         EXIT loop_3
       END IF
       d(i)=d(i-1)
     END DO loop_3
   END IF
   IF(aux==0)THEN
     i=1
   END IF
   d(i) = p
   ifail=0
 END DO loop_1
RETURN
END SUBROUTINE bisect

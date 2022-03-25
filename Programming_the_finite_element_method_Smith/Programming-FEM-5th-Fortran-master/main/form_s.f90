SUBROUTINE form_s(gg,ell,kappa,omega,gamma,s)
!
! This subroutine forms the s vector in bicgstab(l)
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::gg(:,:),kappa
 INTEGER,INTENT(IN)::ell
 REAL(iwp),INTENT(OUT)::omega,gamma(:),s(:)
 REAL(iwp)::HH(ell-1,ell-1),gamma0(ell+1),p(ell-1),q(ell-1),gamma1(ell+1),&
   ngamma0,ngamma1,cosine,zero=0.0_iwp,one=1.0_iwp
 hh=-gg(2:ell,2:ell)
 CALL invert(hh)
 p=matmul(hh,gg(2:ell,1))
 q=matmul(hh,gg(2:ell,ell+1))
 gamma0(1)=one
 gamma0(ell+1)=zero
 gamma0(2:ell)=p
 gamma1(1)=zero
 gamma1(ell+1)=one
 gamma1(2:ell)=q
 ngamma0=DOT_PRODUCT(gamma0,MATMUL(gg,gamma0))
 ngamma1=DOT_PRODUCT(gamma1,MATMUL(gg,gamma1))
 omega=DOT_PRODUCT(gamma0,MATMUL(gg,gamma1))
 cosine=ABS(omega)/SQRT(ABS(ngamma0*ngamma1))
 omega=omega/ngamma1
 IF(cosine<kappa)omega=(kappa/cosine)*omega
 gamma=gamma0-omega*gamma1
 s(1:ell)=gamma(2:ell+1)
 s(ell+1)=zero
RETURN
CONTAINS
SUBROUTINE invert(matrix)
!
! This subroutine inverts a small square matrix onto itself.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN OUT)::matrix(:,:)
 REAL(iwp)::det,j11,j12,j13,j21,j22,j23,j31,j32,j33,con
 INTEGER::ndim,i,k
 ndim=UBOUND(matrix,1)
 IF(ndim==2)THEN
   det=matrix(1,1)*matrix(2,2)-matrix(1,2)*matrix(2,1)
   j11=matrix(1,1)
   matrix(1,1)=matrix(2,2)
   matrix(2,2)=j11
   matrix(1,2)=-matrix(1,2)
   matrix(2,1)=-matrix(2,1)
   matrix=matrix/det
 ELSE IF(ndim==3)THEN
   det=matrix(1,1)*(matrix(2,2)*matrix(3,3)-matrix(3,2)*matrix(2,3))
   det=det-matrix(1,2)*(matrix(2,1)*matrix(3,3)-matrix(3,1)*matrix(2,3))
   det=det+matrix(1,3)*(matrix(2,1)*matrix(3,2)-matrix(3,1)*matrix(2,2))
   j11=matrix(2,2)*matrix(3,3)-matrix(3,2)*matrix(2,3)
   j21=-matrix(2,1)*matrix(3,3)+matrix(3,1)*matrix(2,3)
   j31=matrix(2,1)*matrix(3,2)-matrix(3,1)*matrix(2,2)
   j12=-matrix(1,2)*matrix(3,3)+matrix(3,2)*matrix(1,3)
   j22=matrix(1,1)*matrix(3,3)-matrix(3,1)*matrix(1,3)
   j32=-matrix(1,1)*matrix(3,2)+matrix(3,1)*matrix(1,2)
   j13=matrix(1,2)*matrix(2,3)-matrix(2,2)*matrix(1,3)
   j23=-matrix(1,1)*matrix(2,3)+matrix(2,1)*matrix(1,3)
   j33=matrix(1,1)*matrix(2,2)-matrix(2,1)*matrix(1,2)
   matrix(1,1)=j11
   matrix(1,2)=j12
   matrix(1,3)=j13
   matrix(2,1)=j21
   matrix(2,2)=j22
   matrix(2,3)=j23
   matrix(3,1)=j31
   matrix(3,2)=j32
   matrix(3,3)=j33
   matrix=matrix/det
 ELSE
   DO k=1,ndim
     con=matrix(k,k)
     matrix(k,k)=1.0_iwp
     matrix(k,:)=matrix(k,:)/con
     DO i=1,ndim
       IF(i/=k)THEN
         con=matrix(i,k)
         matrix(i,k)=0.0_iwp
         matrix(i,:)=matrix(i,:)-matrix(k,:)*con
       END IF
     END DO
   END DO
 END IF
RETURN
END SUBROUTINE invert
END SUBROUTINE form_s











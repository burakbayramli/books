SUBROUTINE bandred(a,d,e)
! Used in p101-p103
! This subroutine transforms a real symmetric band matrix a,
! of order n and band width iw, to tridiagonal form by an appropriate
! sequence of Jacobi rotations. During the transformation the
! property of the band matrix is maintained. The method yields
! a tridiagonal matrix, the diagonal elements of which are in
! d(n) and off-diagonal elements in e(n).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN OUT)::a(:,:)
 REAL(iwp),INTENT(OUT)::d(0:),e(0:)
 INTEGER::iw,n2,n,k,maxr,irr,ir,kr,j,jm,iugl,j2,l,jl,maxl,i
 REAL(iwp)::g,b,s,c,c2,s2,cs,u,u1,zero=0.0_iwp,one=1.0_iwp,two=2.0_iwp
 n=UBOUND(a,1)
 iw=UBOUND(a,2)-1
 n2=n-2
 IF(n2>=1)THEN
   loop_1: DO k=1,n2
     maxr=iw
     IF(n-k<iw)maxr=n-k
     loop_2: DO irr=2,maxr
       ir=2+maxr-irr
       kr=k+ir
       loop_3: DO j=kr,n,iw
         IF(j/=kr)THEN
           IF(ABS(g)<TINY(g))EXIT loop_3
           jm=j-iw
           b=-a(jm-1,iw+1)/g
           iugl=j-iw
         ELSE
           IF(ABS(a(k,ir+1))<TINY(a(k,ir+1)))EXIT loop_3
           b=-a(k,ir)/a(k,ir+1)
           iugl=k
         END IF
         s=one/SQRT(one+b*b)
         c=b*s
         c2=c*c
         s2=s*s
         cs=c*s
         u=c2*a(j-1,1)-two*cs*a(j-1,2)+s2*a(j,1)
         u1=s2*a(j-1,1)+two*cs*a(j-1,2)+c2*a(j,1)
         a(j-1,2)=cs*(a(j-1,1)-a(j,1))+(c2-s2)*a(j-1,2)
         a(j-1,1)=u
         a(j,1)=u1
         j2=j-2
         DO l=iugl,j2
           jl=j-l
           u=c*a(l,jl)-s*a(l,jl+1)
           a(l,jl+1)=s*a(l,jl)+c*a(l,jl+1)
           a(l,jl)=u
         END DO
         jm=j-iw
         IF(j/=kr)a(jm-1,iw+1)=c*a(jm-1,iw+1)-s*g
         maxl=iw-1
         IF(n-j<iw-1)maxl=n-j
         IF(maxl>0)THEN
           DO l=1,maxl
             u=c*a(j-1,l+2)-s*a(j,l+1)
             a(j,l+1)=s*a(j-1,l+2)+c*a(j,l+1)
             a(j-1,l+2)=u
           END DO
         END IF
         IF(j+iw<=n)THEN
           g=-s*a(j,iw+1)
           a(j,iw+1)=c*a(j,iw+1)
         END IF
       END DO loop_3
     END DO loop_2
   END DO loop_1
 END IF
 e(1)=zero
 d(1:n)=a(1:n,1)
 IF(2<=n)THEN
   DO i=2,n
     e(i)=a(i-1,2)
   END DO
 END IF
RETURN
END SUBROUTINE bandred

SUBROUTINE loc_to_glob(local,global,gamma,coord)
!
! This subroutine transforms the local end reactions and
! moments into the global system (3-d).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::local(:),gamma,coord(:,:)
 REAL(iwp),INTENT(OUT)::global(:)
 REAL(iwp)::t(12,12),r0(3,3),x1,x2,y1,y2,z1,z2,xl,yl,zl,pi,gamrad,cg,sg,  &
   den,ell,x,sum,zero=0.0_iwp,one=1.0_iwp,d180=180.0_iwp
 INTEGER::i,j,k
 x1=coord(1,1)
 y1=coord(1,2)
 z1=coord(1,3)
 x2=coord(2,1)
 y2=coord(2,2)
 z2=coord(2,3)
 xl=x2-x1
 yl=y2-y1
 zl=z2-z1
 ell=SQRT(xl*xl+yl*yl+zl*zl)
 t=zero
 pi=ACOS(-one)
 gamrad=gamma*pi/d180
 cg=COS(gamrad)
 sg=SIN(gamrad)
 den=ell*SQRT(xl*xl+zl*zl)
 IF(den/=zero)THEN
   r0(1,1)=xl/ell
   r0(2,1)=yl/ell
   r0(3,1)=zl/ell
   r0(1,2)=(-xl*yl*cg-ell*zl*sg)/den
   r0(2,2)=den*cg/(ell*ell)
   r0(3,2)=(-yl*zl*cg+ell*xl*sg)/den
   r0(1,3)=(xl*yl*sg-ell*zl*cg)/den
   r0(2,3)=-den*sg/(ell*ell)
   r0(3,3)=(yl*zl*sg+ell*xl*cg)/den
 ELSE
   r0(1,1)=zero
   r0(3,1)=zero
   r0(2,2)=zero
   r0(2,3)=zero
   r0(2,1)=one
   r0(1,2)=-cg
   r0(3,3)=cg
   r0(3,2)=sg
   r0(1,3)=sg
 END IF
 DO i=1,3
   DO j=1,3 
     x=r0(i,j)
     DO k=0,9,3
       t(i+k,j+k)=x
     END DO
   END DO
 END DO
 DO i=1,12
   sum=zero
   DO j=1,12
     sum=sum+t(i,j)*local(j)
   END DO
   global(i)=sum
 END DO
RETURN
END SUBROUTINE loc_to_glob    

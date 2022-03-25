SUBROUTINE hinge(coord,holdr,action,react,prop,iel,etype,gamma)
!
! This subroutine forms the end forces and moments to be
! applied to a member if a joint has gone plastic.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::holdr(:,:),coord(:,:),action(:),prop(:,:),gamma(:)
 REAL(iwp),INTENT(OUT)::react(:)
 INTEGER,INTENT(IN)::etype(:),iel
 REAL(iwp)::ell,x1,x2,y1,y2,z1,z2,csch,snch,bm1,bm2,bm3,bm4,s1,s2,s3,s4,  &
   s5,mpy,mpz,mpx,gam,zero=0.0_iwp
 REAL(iwp),ALLOCATABLE::global(:),local(:),total(:)
 INTEGER::ndim,ndof
 ndim=UBOUND(coord,2)
 ndof=UBOUND(action,1)
 ALLOCATE(global(ndof),local(ndof),total(ndof))
 bm1=zero
 bm2=zero
 bm3=zero
 bm4=zero
 total(:)=holdr(:,iel)
 SELECT CASE(ndim)
 CASE(1)
   mpy=prop(2,etype(iel))
   ell=coord(2,1)-coord(1,1)
   s1=total(2)+action(2)
   s2=total(4)+action(4)
   IF(ABS(s1)>mpy)THEN
     if(s1> zero)bm1= mpy-s1
     if(s1<=zero)bm1=-mpy-s1
   END IF
   IF(ABS(s2)>mpy)THEN
     IF(s2> zero)bm2= mpy-s2
     IF(s2<=zero)bm2=-mpy-s2
   END IF
   react(1)= (bm1+bm2)/ell
   react(2)=bm1
   react(3)=-react(1)
   react(4)=bm2
 CASE(2)
   mpy=prop(3,etype(iel))  
   x1=coord(1,1)
   y1=coord(1,2)
   x2=coord(2,1)
   y2=coord(2,2)
   ell=SQRT((y2-y1)**2+(x2-x1)**2)
   csch=(x2-x1)/ell
   snch=(y2-y1)/ell
   s1=total(3)+action(3)
   s2=total(6)+action(6)
   IF(ABS(s1)>mpy)THEN 
     IF(s1> zero)bm1= mpy-s1
     IF(s1<=zero)bm1=-mpy-s1
   END IF
   IF(ABS(s2)>mpy)THEN
     IF(s2> zero)bm2= mpy-s2
     IF(s2<=zero)bm2=-mpy-s2
   END IF
   react(1)=-(bm1+bm2)*snch/ell
   react(2)= (bm1+bm2)*csch/ell
   react(3)=bm1
   react(4)=-react(1)
   react(5)=-react(2)
   react(6)=bm2
 CASE(3)
   gam=gamma(iel)
   mpy=prop(5,etype(iel))
   mpz=prop(6,etype(iel))
   mpx=prop(7,etype(iel))
   x1=coord(1,1)
   y1=coord(1,2)
   z1=coord(1,3)
   x2=coord(2,1)
   y2=coord(2,2)
   z2=coord(2,3)
   ell=SQRT((z2-z1)**2+(y2-y1)**2+(x2-x1)**2)
   global=total+action
   call glob_to_loc(local,global,gam,coord)
   global=zero
   s1=local(5)
   s2=local(11)
   IF(ABS(s1)>mpy)THEN 
     IF(s1> zero)bm1= mpy-s1
     IF(s1<=zero)bm1=-mpy-s1
   END IF
   IF(ABS(s2)>mpy)THEN
     IF(s2> zero)bm2= mpy-s2
     IF(s2<=zero)bm2=-mpy-s2
   END IF
   local( 3)=-(bm1+bm2)/ell
   local( 9)=-local(3)
   local( 5)= bm1
   local(11)= bm2
   s3=local(6)
   s4=local(12)
   IF(ABS(s3)>mpz)THEN
     IF(s3> zero)bm1= mpz-s3
     IF(s3<=zero)bm1=-mpz-s3
   END IF
   IF(ABS(s4)>mpy)THEN
     IF(s4> zero)bm2= mpz-s4
     IF(s4<=zero)bm2=-mpz-s4
   END IF
   local( 2)=(bm3+bm4)/ell
   local( 8)=-local(2)
   local( 6)= bm3
   local(12)= bm4
   s5=local(4)
   IF(ABS(s5)>mpx)THEN
     IF(s5> zero)global(4)= mpx-s5
     IF(s5<=zero)global(4)=-mpx-s5
   END IF
   local(10)=-local(4)
   CALL loc_to_glob(local,react,gam,coord)
 END SELECT
RETURN
CONTAINS
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
SUBROUTINE glob_to_loc(local,global,gamma,coord)
!
! This subroutine transforms the global end reactions and
! moments into the local system (2- or 3-d).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::global(:),gamma,coord(:,:)
 REAL(iwp),INTENT(OUT)::local(:)
 REAL(iwp)::t(12,12),r0(3,3),x1,x2,y1,y2,z1,z2,xl,yl,zl,pi,gamrad,cg,sg,  &
   den,ell,x,sum,zero=0.0_iwp,one=1.0_iwp,d180=180.0_iwp
 INTEGER::i,j,k,ndim
 ndim=UBOUND(coord,2)
 SELECT CASE(ndim)
 CASE(2)
   x1=coord(1,1)
   y1=coord(1,2)
   x2=coord(2,1)
   y2=coord(2,2)
   ell=SQRT((x2-x1)**2+(y2-y1)**2)
   cg=(x2-x1)/ell
   sg=(y2-y1)/ell
   local(1)=cg*global(1)+sg*global(2)
   local(2)=cg*global(2)-sg*global(1)
   local(3)=global(3)
   local(4)=cg*global(4)+sg*global(5)
   local(5)=cg*global(5)-sg*global(4)
   local(6)=global(6)
 CASE(3)
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
     r0(1,2)=yl/ell
     r0(1,3)=zl/ell
     r0(2,1)=(-xl*yl*cg-ell*zl*sg)/den
     r0(2,2)=den*cg/(ell*ell)
     r0(2,3)=(-yl*zl*cg+ell*xl*sg)/den
     r0(3,1)=(xl*yl*sg-ell*zl*cg)/den
     r0(3,2)=-den*sg/(ell*ell)
     r0(3,3)=(yl*zl*sg+ell*xl*cg)/den
   ELSE
     r0(1,1)=zero
     r0(1,3)=zero
     r0(2,2)=zero
     r0(3,2)=zero
     r0(1,2)=one
     r0(2,1)=-cg
     r0(3,3)=cg
     r0(2,3)=sg
     r0(3,1)=sg
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
       sum=sum+t(i,j)*global(j)
     END DO
     local(i)=sum
   END DO
 END SELECT
RETURN
END SUBROUTINE glob_to_loc    
END SUBROUTINE hinge

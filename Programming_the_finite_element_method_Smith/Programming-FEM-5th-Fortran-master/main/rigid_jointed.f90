SUBROUTINE rigid_jointed(km,prop,gamma,etype,iel,coord) 
!
! This subroutine forms the stiffness matrix of a
! general beam/column element (1-, 2- or 3-d).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::gamma(:),coord(:,:),prop(:,:)
 INTEGER,INTENT(IN)::etype(:),iel
 REAL(iwp),INTENT(OUT)::km(:,:)
 INTEGER::ndim,i,j,k
 REAL(iwp)::ell,x1,x2,y1,y2,z1,z2,c,s,e1,e2,e3,e4,pi,xl,yl,zl,cg,sg,den,  &
   ea,ei,eiy,eiz,gj,a1,a2,a3,a4,a5,a6,a7,a8,sum,gamrad,x,t(12,12),        &
   tt(12,12),cc(12,12),r0(3,3),zero=0.0_iwp,pt5=0.5_iwp,one=1.0_iwp,      &
   two=2.0_iwp,d4=4.0_iwp,d6=6.0_iwp,d12=12.0_iwp,d180=180.0_iwp
 ndim=UBOUND(coord,2)
 SELECT CASE(ndim)
 CASE(1)
   ei=prop(1,etype(iel))
   ell=coord(2,1)-coord(1,1)
   km(1,1)=d12*ei/(ell*ell*ell) 
   km(3,3)=km(1,1)
   km(1,2)=d6*ei/(ell*ell) 
   km(2,1)=km(1,2) 
   km(1,4)=km(1,2)
   km(4,1)=km(1,4) 
   km(1,3)=-km(1,1) 
   km(3,1)=km(1,3) 
   km(3,4)=-km(1,2)
   km(4,3)=km(3,4) 
   km(2,3)=km(3,4) 
   km(3,2)=km(2,3)
   km(2,2)=d4*ei/ell
   km(4,4)=km(2,2) 
   km(2,4)=two*ei/ell 
   km(4,2)=km(2,4)
 CASE(2)
   ea=prop(1,etype(iel))
   ei=prop(2,etype(iel))
   x1=coord(1,1)
   y1=coord(1,2)
   x2=coord(2,1)
   y2=coord(2,2)
   ell=SQRT((y2-y1)**2+(x2-x1)**2)
   c=(x2-x1)/ell
   s=(y2-y1)/ell
   e1=ea/ell
   e2=d12*ei/(ell*ell*ell)
   e3=ei/ell
   e4=d6*ei/(ell*ell)
   km(1,1)=c*c*e1+s*s*e2
   km(4,4)=km(1,1)
   km(1,2)=s*c*(e1-e2)
   km(2,1)=km(1,2)
   km(4,5)=km(1,2)
   km(5,4)=km(4,5)
   km(1,3)=-s*e4
   km(3,1)=km(1,3)
   km(1,6)=km(1,3)
   km(6,1)=km(1,6)
   km(3,4)=s*e4 
   km(4,3)=km(3,4)
   km(4,6)=km(3,4)
   km(6,4)=km(4,6)
   km(1,4)=-km(1,1) 
   km(4,1)=km(1,4)
   km(1,5)=s*c*(-e1+e2)
   km(5,1)=km(1,5)
   km(2,4)=km(1,5)
   km(4,2)=km(2,4)
   km(2,2)=s*s*e1+c*c*e2
   km(5,5)=km(2,2)
   km(2,5)=-km(2,2)
   km(5,2)=km(2,5)
   km(2,3)=c*e4
   km(3,2)=km(2,3)
   km(2,6)=km(2,3)
   km(6,2)=km(2,6)
   km(3,3)=d4*e3
   km(6,6)=km(3,3)
   km(3,5)=-c*e4
   km(5,3)=km(3,5)
   km(5,6)=km(3,5)
   km(6,5)=km(5,6)
   km(3,6)=two*e3
   km(6,3)=km(3,6)
 CASE(3)
   ea=prop(1,etype(iel))
   eiy=prop(2,etype(iel))
   eiz=prop(3,etype(iel))
   gj=prop(4,etype(iel))
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
   km=zero
   t=zero
   tt=zero
   a1=ea/ell
   a2=d12*eiz/(ell*ell*ell)
   a3=d12*eiy/(ell*ell*ell)
   a4=d6*eiz/(ell*ell)
   a5=d6*eiy/(ell*ell)
   a6=d4*eiz/ell
   a7=d4*eiy/ell
   a8=gj/ell
   km(1,1)=a1
   km(7,7)=a1
   km(1,7)=-a1
   km(7,1)=-a1
   km(2,2)=a2
   km(8,8)=a2
   km(2,8)=-a2
   km(8,2)=-a2
   km(3,3)=a3
   km(9,9)=a3
   km(3,9)=-a3
   km(9,3)=-a3
   km(4,4)=a8
   km(10,10)=a8
   km(4,10)=-a8
   km(10,4)=-a8
   km(5,5)=a7
   km(11,11)=a7
   km(5,11)=pt5*a7
   km(11,5)=pt5*a7
   km(6,6)=a6
   km(12,12)=a6
   km(6,12)=pt5*a6
   km(12,6)=pt5*a6
   km(2,6)=a4
   km(6,2)=a4
   km(2,12)=a4
   km(12,2)=a4
   km(6,8)=-a4
   km(8,6)=-a4
   km(8,12)=-a4
   km(12,8)=-a4
   km(5,9)=a5
   km(9,5)=a5
   km(9,11)=a5
   km(11,9)=a5
   km(3,5)=-a5
   km(5,3)=-a5
   km(3,11)=-a5
   km(11,3)=-a5
   pi=ACOS(-one)
   gamrad=gamma(iel)*pi/d180
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
         tt(j+k,i+k)=x
       END DO
     END DO
   END DO
   DO i=1,12
     DO j=1,12
       sum=zero
       DO k=1,12
         sum=sum+km(i,k)*t(k,j)
       END DO
       cc(i,j)=sum
     END DO
   END DO
   DO i=1,12
     DO j=1,12
       sum=zero
       DO k=1,12
         sum=sum+tt(i,k)*cc(k,j)
       END DO
       km(i,j)=sum
     END DO
   END DO
 END SELECT
RETURN
END SUBROUTINE rigid_jointed                

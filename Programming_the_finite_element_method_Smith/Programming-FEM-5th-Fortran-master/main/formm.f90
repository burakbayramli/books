SUBROUTINE formm(stress,m1,m2,m3)
!
! This subroutine forms the derivatives of the invariants with respect to
! stress in 2- or 3-d.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::stress(:)
 REAL(iwp),INTENT(OUT)::m1(:,:),m2(:,:),m3(:,:)
 REAL(iwp)::sx,sy,txy,tyz,tzx,sz,dx,dy,dz,sigm,zero=0.0_iwp,one=1.0_iwp,  &
   two=2.0_iwp,three=3.0_iwp,six=6.0_iwp,nine=9.0_iwp
 INTEGER::nst,i,j
 nst=UBOUND(stress,1)
 SELECT CASE(nst)
 CASE(4)
   sx=stress(1)
   sy=stress(2)
   txy=stress(3)
   sz=stress(4)
   dx=(two*sx-sy-sz)/three
   dy=(two*sy-sz-sx)/three
   dz=(two*sz-sx-sy)/three
   sigm=(sx+sy+sz)/three
   m1=zero
   m2=zero
   m3=zero
   m1(1,1:2)=one
   m1(2,1:2)=one
   m1(4,1:2)=one
   m1(1,4)=one
   m1(4,4)=one
   m1(2,4)=one
   m1=m1/nine/sigm
   m2(1,1)=two/three
   m2(2,2)=two/three
   m2(4,4)= two/three
   m2(2,4)=-one/three
   m2(4,2)=-one/three
   m2(1,2)=-one/three
   m2(2,1)=-one/three
   m2(1,4)=-one/three
   m2(4,1)=-one/three
   m2(3,3)=two
   m3(3,3)=-dz
   m3(1:2,3)=txy/three
   m3(3,1:2)=txy/three
   m3(3,4)=-two*txy/three
   m3(4,3)=-two*txy/three
   m3(1,1)=dx/three
   m3(2,4)=dx/three
   m3(4,2)=dx/three
   m3(2,2)=dy/three
   m3(1,4)=dy/three
   m3(4,1)=dy/three
   m3(4,4)=dz/three
   m3(1,2)=dz/three
   m3(2,1)=dz/three
 CASE(6)
   sx=stress(1)
   sy=stress(2)    
   sz=stress(3)
   txy=stress(4)  
   tyz=stress(5) 
   tzx=stress(6)
   sigm=(sx+sy+sz)/three
   dx=sx-sigm  
   dy=sy-sigm 
   dz=sz-sigm
   m1=zero
   m2=zero
   m1(1:3,1:3)=one/(three*sigm)
   DO i=1,3 
     m2(i,i)=two 
     m2(i+3,i+3)=six 
   END DO
   m2(1,2)=-one
   m2(1,3)=-one 
   m2(2,3)=-one
   m3(1,1)=dx
   m3(1,2)=dz 
   m3(1,3)=dy 
   m3(1,4)=txy  
   m3(1,5)=-two*tyz
   m3(1,6)=tzx 
   m3(2,2)=dy 
   m3(2,3)=dx 
   m3(2,4)=txy
   m3(2,5)=tyz 
   m3(2,6)=-two*tzx 
   m3(3,3)=dz
   m3(3,4)=-two*txy
   m3(3,5)=tyz 
   m3(3,6)=tzx
   m3(4,4)=-three*dz 
   m3(4,5)=three*tzx
   m3(4,6)=three*tyz
   m3(5,5)=-three*dx
   m3(5,6)=three*txy 
   m3(6,6)=-three*dy
   DO i=1,6 
     DO j=i+1,6
       m1(j,i)=m1(i,j) 
       m2(j,i)=m2(i,j)   
       m3(j,i)=m3(i,j)
     END DO
   END DO
   m1=m1/three
   m2=m2/three
   m3=m3/three
 CASE DEFAULT
   WRITE(*,*)"nst size not recognised in formm"
 END SELECT
RETURN   
END SUBROUTINE formm


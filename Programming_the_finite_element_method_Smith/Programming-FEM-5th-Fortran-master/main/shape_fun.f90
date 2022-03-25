SUBROUTINE shape_fun(fun,points,i)
!
!   This subroutine computes the values of the shape functions.
!   to local coordinates
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER,INTENT(in)::i
 REAL(iwp),INTENT(IN)::points(:,:)
 REAL(iwp),INTENT(OUT)::fun(:)
 REAL(iwp)::eta,xi,etam,etap,xim,xip,zetam,zetap,c1,c2,c3     
 REAL(iwp)::t1,t2,t3,t4,t5,t6,t7,t8,t9
 REAL(iwp)::zeta,xi0,eta0,zeta0
 INTEGER::xii(20),etai(20),zetai(20),l,ndim,nod
 REAL,PARAMETER::pt125=0.125_iwp,pt25=0.25_iwp,pt5=0.5_iwp,pt75=0.75_iwp, &
   one=1.0_iwp,two=2.0_iwp,d3=3.0_iwp,d4=4.0_iwp,d8=8.0_iwp,d9=9.0_iwp,   &
   d16=16.0_iwp,d27=27.0_iwp,d32=32.0_iwp,d64=64.0_iwp,d128=128.0_iwp
 ndim=UBOUND(points,2)
 nod=UBOUND(fun,1)  
 SELECT CASE(ndim)
 CASE(1) ! one dimensional case
   xi=points(i,1)
   SELECT CASE(nod)
   CASE(2)
     t1=-one-xi 
     t2= one-xi
     fun(1)=t2/two 
     fun(2)=-t1/two
   CASE(3)
     t1=-one-xi 
     t2=-xi 
     t3=one-xi
     fun(1)=t2*t3/two 
     fun(2)=-t1*t3 
     fun(3)=t1*t2/two
   CASE(4)
     t1=-one-xi 
     t2=-one/d3-xi 
     t3=one/d3-xi 
     t4=one-xi
     fun(1)=t2*t3*t4*d9/d16  
     fun(2)=-t1*t3*t4*d27/d16
     fun(3)=t1*t2*t4*d27/d16 
     fun(4)=-t1*t2*t3*d9/d16
   CASE(5)
     t1=-one -xi 
     t2=-pt5-xi 
     t3=-xi 
     t4=pt5-xi 
     t5=one-xi
     fun(1)=t2*t3*t4*t5*two/d3 
     fun(2)=-t1*t3*t4*t5*d8/d3
     fun(3)=t1*t2*t4*t5*d4 
     fun(4)=-t1*t2*t3*t5*d8/d3
     fun(5)=t1*t2*t3*t4*two/d3
   CASE DEFAULT
     WRITE(*,*)"wrong number of nodes in shape_fun"
   END SELECT
 CASE(2) ! two dimensional case
   c1=points(i,1)
   c2=points(i,2)
   c3=one-c1-c2 
   xi=points(i,1)
   eta=points(i,2)
   etam=pt25*(one-eta)
   etap=pt25*(one+eta)
   xim=pt25*(one-xi)
   xip=pt25*(one+xi)
   SELECT CASE(nod)
   CASE(3)
     fun = (/c1,c3,c2/)  
   CASE(6)
     fun(1)=(two*c1-one)*c1 
     fun(2)=d4*c3*c1
     fun(3)=(two*c3-one)*c3 
     fun(4)=d4*c2*c3      
     fun(5)=(two*c2-one)*c2
     fun(6)=d4*c1*c2 
   CASE(10)
     fun(1)= ((d3*c1-one)*(d3*c1-two)*c1)/two
     fun(2)= -(d9*(d3*c1-one)*(c1+c2-one)*c1)/two
     fun(3)=  (d9*(d3*c1+d3*c2-two)*(c1+c2-one)*c1)/two
     fun(4)=-((d3*c1+d3*c2-one)*(d3*c1+d3*c2-two)*(c1+c2-one))/two    
     fun(5)=  (d9*(d3*c1+d3*c2-two)*(c1+c2-one)*c2)/two
     fun(6)= -(d9*(c1+c2-one)*(d3*c2-one)*c2)/two
     fun(7)= ((d3*c2-one)*(d3*c2-two)*c2)/two
     fun(8)=  (d9*(d3*c2-one)*c1*c2)/two
     fun(9)=  (d9*(d3*c1-one)*c1*c2)/two
     fun(10)=-d27*((c2-one)+c1)*c1*c2
   CASE(15)
     t1=c1-pt25  
     t2=c1-pt5 
     t3=c1-pt75   
     t4=c2-pt25
     t5=c2-pt5   
     t6=c2-pt75 
     t7=c3-pt25  
     t8=c3-pt5 
     t9=c3-pt75
     fun(1)=d32/d3*c1*t1*t2*t3   
     fun(2)=d128/d3*c3*c1*t1*t2
     fun(3)=d64*c3*c1*t1*t7      
     fun(4)=d128/d3*c3*c1*t7*t8
     fun(5)=d32/d3*c3*t7*t8*t9   
     fun(6)=d128/d3*c2*c3*t7*t8
     fun(7)=d64*c2*c3*t4*t7      
     fun(8)=d128/d3*c2*c3*t4*t5
     fun(9)=d32/d3*c2*t4*t5*t6   
     fun(10)=d128/d3*c1*c2*t4*t5
     fun(11)=d64*c1*c2*t1*t4     
     fun(12)=d128/d3*c1*c2*t1*t2
     fun(13)=d128*c1*c2*t1*c3    
     fun(15)=d128*c1*c2*c3*t4
     fun(14)=d128*c1*c2*c3*t7      
   CASE(4)
     fun=(/d4*xim*etam,d4*xim*etap,d4*xip*etap,d4*xip*etam/)
   CASE(5)
     fun=(/d4*xim*etam-pt25*(one-xi**2)*(one-eta**2),			  &
           d4*xim*etap-pt25*(one-xi**2)*(one-eta**2),			  &
           d4*xip*etap-pt25*(one-xi**2)*(one-eta**2),			  &
           d4*xip*etam-pt25*(one-xi**2)*(one-eta**2),			  &
           (one-xi**2)*(one-eta**2)/)
   CASE(8)
     fun=(/d4*etam*xim*(-xi-eta-one),d32*etam*xim*etap,                   &
           d4*etap*xim*(-xi+eta-one),d32*xim*xip*etap,                    &
           d4*etap*xip*(xi+eta-one), d32*etap*xip*etam,                   &
           d4*xip*etam*(xi-eta-one), d32*xim*xip*etam/)
   CASE(9)
     etam=eta-one
     etap=eta+one
     xim=xi-one
     xip=xi+one
     fun=(/pt25*xi*xim*eta*etam,-pt5*xi*xim*etap*etam,                    &
           pt25*xi*xim*eta*etap,-pt5*xip*xim*eta*etap,                    &
           pt25*xi*xip*eta*etap,-pt5*xi*xip*etap*etam,                    &
           pt25*xi*xip*eta*etam,-pt5*xip*xim*eta*etam,                    &
           xip*xim*etap*etam/)
   CASE DEFAULT
     WRITE(*,*)"wrong number of nodes in shape_fun"
   END SELECT
 CASE(3) ! d3 dimensional case
   xi=points(i,1)
   eta=points(i,2)
   zeta=points(i,3)
   etam=one-eta 
   xim=one-xi  
   zetam=one-zeta
   etap=eta+one 
   xip=xi+one   
   zetap=zeta+one
   SELECT CASE(nod)
   CASE(4)
     fun(1)=xi   
     fun(2)=eta 
     fun(3)=zeta 
     fun(4)=one-fun(1)-fun(2)-fun(3)
   CASE(8)
     fun=(/pt125*xim*etam*zetam,pt125*xim*etam*zetap,                     &
           pt125*xip*etam*zetap,pt125*xip*etam*zetam,                     &
           pt125*xim*etap*zetam,pt125*xim*etap*zetap,                     &
           pt125*xip*etap*zetap,pt125*xip*etap*zetam/)
   CASE(14) ! type 6 element
     fun(1) = (xi*eta+xi*zeta+two*xi+eta*zeta+two*eta+two*zeta+two)*      &
       (xi-one)*(eta-one)*(zeta-one)/d8
     fun(2) =-(xi*eta-xi*zeta+two*xi-eta*zeta+two*eta-two*zeta+two)*      &
       (xi-one)*(eta-one)*(zeta+one)/d8
     fun(3) =-(xi*eta-xi*zeta+two*xi+eta*zeta-two*eta+two*zeta-two)*      &
       (xi+one)*(eta-one)*(zeta+one)/d8
     fun(4) = (xi*eta+xi*zeta+two*xi-eta*zeta-two*eta-two*zeta-two)*      &
       (xi+one)*(eta-one)*(zeta-one)/d8
     fun(5) =-(xi+one)*(xi-one)*(eta-one)*(zeta+one)*(zeta-one)/two
     fun(6) =-(xi-one)*(eta+one)*(eta-one)*(zeta+one)*(zeta-one)/two
     fun(7) = (xi+one)*(xi-one)*(eta+one)*(eta-one)*(zeta+one)/two
     fun(8) = (xi+one)*(eta+one)*(eta-one)*(zeta+one)*(zeta-one)/two
     fun(9) =-(xi+one)*(xi-one)*(eta+one)*(eta-one)*(zeta-one)/two
     fun(10)= (xi*eta-xi*zeta-two*xi+eta*zeta+two*eta-two*zeta-two)*      &
       (xi-one)*(eta+one)*(zeta-one)/d8
     fun(11)=-(xi*eta+xi*zeta-two*xi-eta*zeta+two*eta+two*zeta-two)*      &
       (xi-one)*(eta+one)*(zeta+one)/d8
     fun(12)=-(xi*eta+xi*zeta-two*xi+eta*zeta-two*eta-two*zeta+two)*      &
       (xi+one)*(eta+one)*(zeta+one)/d8
     fun(13)= (xi*eta-xi*zeta-two*xi-eta*zeta-two*eta+two*zeta+two)*      &
       (xi+one)*(eta+one)*(zeta-one)/d8
     fun(14)= (xi+one)*(xi-one)*(eta+one)*(zeta+one)*(zeta-one)/two
   CASE(20)
     xii=(/-1,-1,-1,0,1,1,1,0,-1,-1,1,1,-1,-1,-1,0,1,1,1,0/)
     etai=(/-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,1,1,1,1,1,1,1,1/)
     zetai=(/-1,0,1,1,1,0,-1,-1,-1,1,1,-1,-1,0,1,1,1,0,-1,-1/)
     DO l=1,20
       xi0=xi*xii(l)
       eta0=eta*etai(l)
       zeta0=zeta*zetai(l)
       IF(l==4.OR.l==8.OR.l==16.OR.l==20)THEN
         fun(l)=pt25*(one-xi*xi)*(one+eta0)*(one+zeta0)
       ELSE IF(l>=9.AND.l<=12)THEN
         fun(l)=pt25*(one+xi0)*(one-eta*eta)*(one+zeta0)
       ELSE IF(l==2.OR.l==6.OR.l==14.OR.l==18)THEN
         fun(l)=pt25*(one+xi0)*(one+eta0)*(one-zeta*zeta)
       ELSE
         fun(l)=pt125*(one+xi0)*(one+eta0)*(one+zeta0)*(xi0+eta0+zeta0-2)
       END IF
     END DO
   CASE DEFAULT
     WRITE(*,*)"wrong number of nodes in shape_fun"
   END SELECT
 CASE DEFAULT
   WRITE(*,*)"wrong number of dimensions in shape_fun"
 END SELECT
RETURN
END SUBROUTINE shape_fun 

SUBROUTINE stiff4(km,coord,ym,pr)
!
! This subroutine generates the "analytical" stiffness matrix
! for a four node quadrilateral in plane strain based on nip=4.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),ym,pr
 REAL(iwp),INTENT(OUT)::km(:,:)
 REAL(iwp)::e1,e2,g,x1,x2,x3,x4,y1,y2,y3,y4,a2,a2st3,alph,beta,c11,c21,   &
   c31,c41,c51,c61,c71,c81,c22,c32,c42,c52,c62,c72,c82,c33,c43,c53,c63,   &
   c73,c83,c44,c54,c64,c74,c84,c55,c65,c75,c85,c66,c76,c86,c77,c87,c88,   &
   f1,f2,s1,s2,s3,s4,t1,t2,t3,t4,pt5=0.5_iwp,one=1.0_iwp,two=2.0_iwp,     &
   d3=3.0_iwp,zero=0.0_iwp
 INTEGER::i,j
 e1=ym*(one-pr)/(one+pr)/(1-two*pr)
 e2=pr*e1/(one-pr)
 g=ym/two/(one+pr)      
!
 x1=coord(1,1)
 x2=coord(2,1)
 x3=coord(3,1)
 x4=coord(4,1)
 y1=coord(1,2)
 y2=coord(2,2)
 y3=coord(3,2)
 y4=coord(4,2)
!
 a2=(x4-x2)*(y3-y1)-(x3-x1)*(y4-y2)
 a2st3=d3*a2*a2
!
 CALL groupa(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c11=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(1,1)=c11
!
 CALL groupa(x2,x3,x4,x1,y2,y3,y4,y1,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c33=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(3,3)=c33
!
 CALL groupa(x3,x4,x1,x2,y3,y4,y1,y2,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c55=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(5,5)=c55
!
 CALL groupa(x4,x1,x2,x3,y4,y1,y2,y3,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c77=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(7,7)=c77
!
 CALL groupa(y4,y3,y2,y1,x4,x3,x2,x1,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c88=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(8,8)=c88
!
 CALL groupa(y1,y4,y3,y2,x1,x4,x3,x2,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c22=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(2,2)=c22
!
 CALL groupa(y2,y1,y4,y3,x2,x1,x4,x3,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c44=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(4,4)=c44
!
 CALL groupa(y3,y2,y1,y4,x3,x2,x1,x4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c66=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(6,6)=c66
! 
 CALL groupb(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c21=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(2,1)=c21
!
 CALL groupb(x2,x3,x4,x1,y2,y3,y4,y1,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c43=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(4,3)=c43
!
 CALL groupb(x3,x4,x1,x2,y3,y4,y1,y2,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c65=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(6,5)=c65
!
 CALL groupb(x4,x1,x2,x3,y4,y1,y2,y3,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c87=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(8,7)=c87
!
 CALL groupc(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c31=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(3,1)=c31
!
 CALL groupc(x2,x3,x4,x1,y2,y3,y4,y1,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c53=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(5,3)=c53
!
 CALL groupc(x3,x4,x1,x2,y3,y4,y1,y2,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c75=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(7,5)=c75
!
 CALL groupc(x4,x1,x2,x3,y4,y1,y2,y3,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c71=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(7,1)=c71
! 
 CALL groupc(y4,y3,y2,y1,x4,x3,x2,x1,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c86=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(8,6)=c86
!
 CALL groupc(y1,y4,y3,y2,x1,x4,x3,x2,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c82=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(8,2)=c82
!
 CALL groupc(y2,y1,y4,y3,x2,x1,x4,x3,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c42=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(4,2)=c42
!
 CALL groupc(y3,y2,y1,y4,x3,x2,x1,x4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c64=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(6,4)=c64
! 
 CALL groupd(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x1,x2,x3,x4,y1,y2,y3,y4,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c41=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(4,1)=c41
!
 CALL groupd(x2,x3,x4,x1,y2,y3,y4,y1,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x2,x3,x4,x1,y2,y3,y4,y1,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c63=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(6,3)=c63
!
 CALL groupd(x3,x4,x1,x2,y3,y4,y1,y2,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x3,x4,x1,x2,y3,y4,y1,y2,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c85=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(8,5)=c85
!
 CALL groupd(x4,x1,x2,x3,y4,y1,y2,y3,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x4,x1,x2,x3,y4,y1,y2,y3,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c72=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(7,2)=c72
!
 CALL groupd(y1,y2,y3,y4,x1,x2,x3,x4,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x1,x2,x3,x4,y1,y2,y3,y4,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c32=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(3,2)=c32
!
 CALL groupd(y2,y3,y4,y1,x2,x3,x4,x1,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x2,x3,x4,x1,y2,y3,y4,y1,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c54=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(5,4)=c54
!
 CALL groupd(y3,y4,y1,y2,x3,x4,x1,x2,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x3,x4,x1,x2,y3,y4,y1,y2,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c76=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(7,6)=c76
!
 CALL groupd(y4,y1,y2,y3,x4,x1,x2,x3,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x4,x1,x2,x3,y4,y1,y2,y3,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c81=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(8,1)=c81
!
 CALL groupe(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c51=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(5,1)=c51
!
 CALL groupe(x2,x3,x4,x1,y2,y3,y4,y1,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c73=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(7,3)=c73
!
 CALL groupe(y2,y1,y4,y3,x2,x1,x4,x3,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c84=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(8,4)=c84
!
 CALL groupe(y3,y2,y1,y4,x3,x2,x1,x4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 alph=a2*(s1*e1+s2*g)+f1*(s3*e1+s4*g)
 beta=a2*(t1*e1+t2*g)+f2*(t3*e1+t4*g)
 c62=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(6,2)=c62
!
 CALL groupf(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x1,x2,x3,x4,y1,y2,y3,y4,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c61=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(6,1)=c61
!
 CALL groupf(x2,x3,x4,x1,y2,y3,y4,y1,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x2,x3,x4,x1,y2,y3,y4,y1,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c83=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(8,3)=c83
!
 CALL groupf(y1,y2,y3,y4,x1,x2,x3,x4,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x1,x2,x3,x4,y1,y2,y3,y4,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c52=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(5,2)=c52
!
 CALL groupf(y2,y3,y4,y1,x2,x3,x4,x1,s1,s2,s3,s4,t1,t2,t3,t4)
 CALL f1f2(x2,x3,x4,x1,y2,y3,y4,y1,f1,f2)
 alph=a2*(s1*e2+s2*g)+f1*(s3*e2+s4*g)
 beta=a2*(t1*e2+t2*g)+f2*(t3*e2+t4*g)
 c74=(alph/(a2st3-f1**2)+beta/(a2st3-f2**2))*pt5
 km(7,4)=c74
!
 DO i=1,8
   DO j=i+1,8 
     km(i,j)=km(j,i) 
   END DO 
 END DO
!
RETURN 
CONTAINS 
SUBROUTINE groupa(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x1,x2,x3,x4,y1,y2,y3,y4
 REAL(iwp),INTENT(OUT)::s1,s2,s3,s4,t1,t2,t3,t4,f1,f2
 s1=two*(y4-y2)**2
 s2=two*(x4-x2)**2
 s3=-s1/two
 s4=-s2/two
 t1=(y2-y3)**2+(y3-y4)**2+(y4-y2)**2
 t2=(x2-x3)**2+(x3-x4)**2+(x4-x2)**2
 t3=(y4-y3)**2-(y3-y2)**2
 t4=(x4-x3)**2-(x3-x2)**2
 f1=(x1+x3)*(y4-y2)-(y1+y3)*(x4-x2)-two*(x2*y4-x4*y2)
 f2=(y2+y4)*(x3-x1)-(x2+x4)*(y3-y1)-two*(x3*y1-x1*y3)
RETURN
END SUBROUTINE groupa
SUBROUTINE groupb(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x1,x2,x3,x4,y1,y2,y3,y4
 REAL(iwp),INTENT(OUT)::s1,s2,s3,s4,t1,t2,t3,t4,f1,f2
 s1=two*(x2-x4)*(y4-y2)
 s2=s1
 s3=-s1/2
 s4=s3
 t1=x2*(y4-two*y2+y3)+x3*(y2-two*y3+y4)+x4*(y2-two*y4+y3)
 t2=t1
 t3=x2*(y2-y3)+x3*(y4-y2)+x4*(y3-y4)
 t4=t3
 f1=(x1+x3)*(y4-y2)-(y1+y3)*(x4-x2)-two*(x2*y4-x4*y2)
 f2=(y2+y4)*(x3-x1)-(x2+x4)*(y3-y1)-two*(x3*y1-x1*y3)
RETURN
END SUBROUTINE groupb
SUBROUTINE groupc(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x1,x2,x3,x4,y1,y2,y3,y4
 REAL(iwp),INTENT(OUT)::s1,s2,s3,s4,t1,t2,t3,t4,f1,f2
 s1=(y4-y2)*(two*y1-y3-y4)
 s2=(x4-x2)*(two*x1-x3-x4)
 s3=(y4-y2)*(y4-y1)
 s4=(x4-x2)*(x4-x1)
 t1=(y3-y1)*(two*y2-y3-y4)
 t2=(x3-x1)*(two*x2-x3-x4)
 t3=(y3-y1)*(y3-y2)
 t4=(x3-x1)*(x3-x2)
 f1=(x1+x3)*(y4-y2)-(y1+y3)*(x4-x2)-two*(x2*y4-x4*y2)
 f2=(y2+y4)*(x3-x1)-(x2+x4)*(y3-y1)-two*(x3*y1-x1*y3)
RETURN
END SUBROUTINE groupc
SUBROUTINE groupd(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x1,x2,x3,x4,y1,y2,y3,y4
 REAL(iwp),INTENT(OUT)::s1,s2,s3,s4,t1,t2,t3,t4
 s1=(x3-x1)*(y4-y2)+(x4-x1)*(y4-y2)
 s2=(y3-y1)*(x4-x2)+(y4-y1)*(x4-x2)
 s3=(x4-x1)*(y2-y4)
 s4=(y4-y1)*(x2-x4)
 t1=(x3-x1)*(y4-y2)+(x3-x1)*(y3-y2)
 t2=(y3-y1)*(x4-x2)+(y3-y1)*(x3-x2)
 t3=(x3-x1)*(y2-y3)
 t4=(y3-y1)*(x2-x3)
RETURN
END SUBROUTINE groupd
SUBROUTINE groupe(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4,f1,f2)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x1,x2,x3,x4,y1,y2,y3,y4
 REAL(iwp),INTENT(OUT)::s1,s2,s3,s4,t1,t2,t3,t4,f1,f2
 s1=-(y4-y2)**2
 s2=-(x4-x2)**2
 s3=zero
 s4=zero
 t1=(y3+y1)*(y4+y2)-two*(y4-y2)**2-two*(y1*y3+y2*y4)
 t2=(x3+x1)*(x4+x2)-two*(x4-x2)**2-two*(x1*x3+x2*x4)
 t3=(y4-y2)*(y1-y2+y3-y4)
 t4=(x4-x2)*(x1-x2+x3-x4)
 f1=(x1+x3)*(y4-y2)-(y1+y3)*(x4-x2)-two*(x2*y4-x4*y2)
 f2=(y2+y4)*(x3-x1)-(x2+x4)*(y3-y1)-two*(x3*y1-x1*y3)
RETURN
END SUBROUTINE groupe
SUBROUTINE groupf(x1,x2,x3,x4,y1,y2,y3,y4,s1,s2,s3,s4,t1,t2,t3,t4)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x1,x2,x3,x4,y1,y2,y3,y4
 REAL(iwp),INTENT(OUT)::s1,s2,s3,s4,t1,t2,t3,t4
 s1=(x4-x2)*(y4-y2)
 s2=s1
 s3=zero
 s4=zero
 t1=(x4-x2)*(y4-y2)+(x2-x1)*(y2-y3)+(x4-x1)*(y4-y3)
 t2=(y4-y2)*(x4-x2)+(y2-y1)*(x2-x3)+(y4-y1)*(x4-x3)
 t3=(x2-x1)*(y3-y2)+(x4-x1)*(y4-y3)
 t4=(y2-y1)*(x3-x2)+(y4-y1)*(x4-x3)
RETURN
END SUBROUTINE groupf
SUBROUTINE f1f2(x1,x2,x3,x4,y1,y2,y3,y4,f1,f2)
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::x1,x2,x3,x4,y1,y2,y3,y4
 REAL(iwp),INTENT(OUT)::f1,f2
 f1=(x1+x3)*(y4-y2)-(y1+y3)*(x4-x2)-two*(x2*y4-x4*y2)
 f2=(y2+y4)*(x3-x1)-(x2+x4)*(y3-y1)-two*(x3*y1-x1*y3)
RETURN
END SUBROUTINE f1f2
END SUBROUTINE stiff4


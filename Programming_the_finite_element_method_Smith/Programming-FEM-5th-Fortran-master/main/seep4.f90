SUBROUTINE seep4(kp,coord,perm)
!
! This contains the "analytical" conductivity matrix
! for a four node quadrilateral based on nip=4.
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(:,:),perm(:,:)
 REAL(iwp),INTENT(OUT)::kp(:,:)
 INTEGER::i,j
 REAL(iwp)::x1,x2,x3,x4,y1,y2,y3,y4,y31,y42,x31,x42,x34,y34,a2,f1,f2,kx,  &
   ky,ht(4),it(4),temp,s1,s2,t1,t2,t3,gdiag,goppo,two=2.0_iwp,d3=3.0_iwp
! 
!                        nodal coordinates
! 
 x1=coord(1,1)
 x2=coord(2,1)
 x3=coord(3,1)
 x4=coord(4,1)
 y1=coord(1,2)
 y2=coord(2,2)
 y3=coord(3,2)
 y4=coord(4,2)
 kx=perm(1,1)
 ky=perm(2,2)
! 
!                       procedure definitions
! 
 y31=(y3-y1)
 y42=(y4-y2)
 x31=(x3-x1)
 x42=(x4-x2)
 x34=x3-x4
 y34=y3-y4
 ht(1)=x4-x1
 ht(2)=x1-x2
 ht(3)=x2-x3
 it(1)=y4-y1
 it(2)=y1-y2
 it(3)=y2-y3
 a2=y42*x31-y31*x42
 f1=2*y42*x34-2*x42*y34-a2
 f2=2*y31*x34-2*x31*y34-a2
 s1=kx*y42**2+ky*x42**2
 s2=kx*y34**2+ky*x34**2
 t1=kx*y42*y34+ky*x42*x34
 t2=kx*y31*y34+ky*x31*x34
 t3=kx*y42*y31+x31*x42*ky
! 
!                       calculate parent terms
! 
 gdiag=(-(two*a2+f1)*s1)/(two*(d3*a2**2-f1**2))
 goppo=-(s1*(two*a2+f2)+two*t1*(a2+f2)+two*a2*s2)/(two*(d3*a2**2-f2**2))
 kp(1,1)=gdiag+goppo
!
 gdiag=((two*a2+f1)*t3-(a2+f1)*t1)/(two*(d3*a2**2-f1**2))
 goppo=((two*a2+f2)*t3+(a2+f2)*t2)/(two*(d3*a2**2-f2**2))
 kp(2,1)=gdiag+goppo
!
 gdiag=(s1*a2)/(two*(d3*a2**2-f1**2))
 goppo=((two*a2+f2)*s1+(a2+f2)*(two*t1-t3)+two*a2*(s2-t2))/               &
   (two*(d3*a2**2-f2**2))
 kp(3,1)=gdiag+goppo
!
!                       use parent terms and transform
! 
 temp=y31
 y31=y42
 y42=-temp
 temp=x31 
 x31=x42
 x42=-temp
 x34=ht(1)
 y34=it(1)
!
 f1=2*y42*x34-2*x42*y34-a2
 f2=2*y31*x34-2*x31*y34-a2
 s1=kx*y42**2+ky*x42**2
 s2=kx*y34**2+ky*x34**2
 t1=kx*y42*y34+ky*x42*x34
 t2=kx*y31*y34+ky*x31*x34
 t3=kx*y42*y31+x31*x42*ky
!
 gdiag=(-(two*a2+f1)*s1)/(two*(d3*a2**2-f1**2))
 goppo=-(s1*(two*a2+f2)+two*t1*(a2+f2)+two*a2*s2)/(two*(d3*a2**2-f2**2))
 kp(2,2)=gdiag+goppo
!
 gdiag=((two*a2+f1)*t3-(a2+f1)*t1)/(two*(d3*a2**2-f1**2))
 goppo=((two*a2+f2)*t3+(a2+f2)*t2)/(two*(d3*a2**2-f2**2))
 kp(3,2)=gdiag+goppo
!
 gdiag=(s1*a2)/(two*(d3*a2**2-f1**2))
 goppo=((two*a2+f2)*s1+(a2+f2)*(two*t1-t3)+two*a2*(s2-t2))/               &
   (two*(d3*a2**2-f2**2))
 kp(4,2)=gdiag+goppo
!
 temp=y31
 y31=y42
 y42=-temp
 temp=x31
 x31=x42
 x42=-temp
 x34=ht(2)
 y34=it(2)
!
 f1=2*y42*x34-2*x42*y34-a2
 f2=2*y31*x34-2*x31*y34-a2
 s1=kx*y42**2+ky*x42**2
 s2=kx*y34**2+ky*x34**2
 t1=kx*y42*y34+ky*x42*x34
 t2=kx*y31*y34+ky*x31*x34
 t3=kx*y42*y31+x31*x42*ky
!
 gdiag=(-(two*a2+f1)*s1)/(two*(d3*a2**2-f1**2))
 goppo=-(s1*(two*a2+f2)+two*t1*(a2+f2)+two*a2*s2)/(two*(d3*a2**2-f2**2))
 kp(3,3)=gdiag+goppo
!
 gdiag=((two*a2+f1)*t3-(a2+f1)*t1)/(two*(d3*a2**2-f1**2))
 goppo=((two*a2+f2)*t3+(a2+f2)*t2)/(two*(d3*a2**2-f2**2))
 kp(4,3)=gdiag+goppo
!
 temp=y31
 y31=y42
 y42=-temp
 temp=x31
 x31=x42
 x42=-temp
 x34=ht(3)
 y34=it(3)
!
 f1=2*y42*x34-2*x42*y34-a2
 f2=2*y31*x34-2*x31*y34-a2
 s1=kx*y42**2+ky*x42**2
 s2=kx*y34**2+ky*x34**2
 t1=kx*y42*y34+ky*x42*x34
 t2=kx*y31*y34+ky*x31*x34
 t3=kx*y42*y31+x31*x42*ky
!
 gdiag=(-(two*a2+f1)*s1)/(two*(d3*a2**2-f1**2))
 goppo=-(s1*(two*a2+f2)+two*t1*(a2+f2)+two*a2*s2)/(two*(d3*a2**2-f2**2))
 kp(4,4)=gdiag+goppo
!
 gdiag=((two*a2+f1)*t3-(a2+f1)*t1)/(two*(d3*a2**2-f1**2))
 goppo=((two*a2+f2)*t3+(a2+f2)*t2)/(two*(d3*a2**2-f2**2))
 kp(4,1)=gdiag+goppo
! 
!                       mirror matrix about main diagonal
! 
 DO i=2,4
   DO j=1,i-1
     kp(j,i)=kp(i,j)
   END DO
 END DO
RETURN
END SUBROUTINE seep4  

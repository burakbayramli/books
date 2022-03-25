SUBROUTINE deemat(dee,e,v)
!
! This subroutine returns the elastic dee matrix for ih=3 (plane strain),
! ih=4 (axisymmetry or plane strain elastoplasticity) or ih=6
! (three dimensions).
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::e,v
 REAL(iwp),INTENT(OUT)::dee(:,:)
 REAL(iwp)::v1,v2,c,vv,zero=0.0_iwp,pt5=0.5_iwp,one=1.0_iwp,two=2.0_iwp
 INTEGER::i,ih
 dee=zero  
 ih=UBOUND(dee,1)
 v1=one-v
 c=e/((one+v)*(one-two*v))
 SELECT CASE(ih)
 CASE(3)
   dee(1,1)=v1*c
   dee(2,2)=v1*c
   dee(1,2)=v*c
   dee(2,1)=v*c
   dee(3,3)=pt5*c*(one-two*v)
 CASE(4)
   dee(1,1)=v1*c
   dee(2,2)=v1*c
   dee(4,4)=v1*c
   dee(3,3)=pt5*c*(one-two*v) 
   dee(1,2)=v*c
   dee(2,1)=v*c
   dee(1,4)=v*c
   dee(4,1)=v*c
   dee(2,4)=v*c
   dee(4,2)=v*c
 CASE(6)
   v2=v/(one-v)
   vv=(one-two*v)/(one-v)*pt5
   DO i=1,3
     dee(i,i)=one
   END DO
   DO i=4,6
     dee(i,i)=vv
   END DO
   dee(1,2)=v2
   dee(2,1)=v2
   dee(1,3)=v2
   dee(3,1)=v2
   dee(2,3)=v2
   dee(3,2)=v2
   dee=dee*e/(two*(one+v)*vv)
 CASE DEFAULT
   WRITE(*,*)'wrong size for dee matrix'
 END SELECT
RETURN
END SUBROUTINE deemat    

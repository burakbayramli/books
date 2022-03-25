SUBROUTINE stiff3(km,coord,ym,pr)
 IMPLICIT NONE
!
!      analytical stiffness matrix of a 3-node triangle in plane strain
!
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,j
 REAL(iwp),INTENT(IN)::coord(3,2),ym,pr
 REAL(iwp),INTENT(OUT)::km(6,6)
 REAL(iwp)::x1,y1,x2,y2,x3,y3,cf
 REAL(iwp)::d1=1._iwp,d2=2._iwp,d3=3._iwp,d4=4._iwp,d5=5._iwp,d6=6._iwp
 x1=coord(1,1)
 y1=coord(1,2)
 x2=coord(2,1)
 y2=coord(2,2)
 x3=coord(3,1)
 y3=coord(3,2)
cf=ym/(d1+pr)/(-d1+d2*pr)/(-x1*y2+x1*y3-x2*y3+y1*x2-y1*x3+y2*x3)/d4
!-----------------------------------------------------------------------
 km(1,1)=(-d2*y2**2+d4*y2*y3-d2*y3**2+d2*pr*y2**2-d4*pr*y2*y3+            &
   d2*pr*y3**2-x2**2+d2*x2**2*pr+d2*x2*x3-d4*x2*x3*pr-x3**2+d2*x3**2*pr)*cf
 km(1,2)=(x2-x3)*(y2-y3)*cf
 km(1,3)=-(d2*y2*y3-d2*y2*y1-d2*y3**2+d2*y3*y1-d2*pr*y2*y3+d2*pr*y2*y1+   &
   d2*pr*y3**2-d2*pr*y3*y1-x2*x1+d2*x2*x1*pr+x2*x3-d2*x2*x3*pr+x3*x1-     &
   d2*x3*x1*pr-x3**2+d2*x3**2*pr)*cf
 km(1,4)=-(d2*pr*x1*y2-d2*pr*x1*y3-d2*pr*y2*x3-x2*y3+d2*x2*y3*pr+y1*x2-   &
   d2*y1*x2*pr+y3*x3-y1*x3+d2*y1*x3*pr)*cf
 km(1,5)=-(d2*y2*y1-d2*y2**2-d2*y3*y1+d2*y2*y3-d2*pr*y2*y1+d2*pr*y2**2+   &
   d2*pr*y3*y1-d2*pr*y2*y3+x2*x1-d2*x2*x1*pr-x2**2+d2*x2**2*pr-x3*x1+     &
   d2*x3*x1*pr+x2*x3-d2*x2*x3*pr)*cf
 km(1,6)=(d2*pr*x1*y2-d2*pr*x1*y3+d2*x2*y3*pr+y1*x2-d2*y1*x2*pr-y2*x2-    &
   y1*x3+d2*y1*x3*pr+y2*x3-d2*pr*y2*x3)*cf
!-----------------------------------------------------------------------
 km(2,2)=(-d2*x2**2+d4*x2*x3-d2*x3**2+d2*x2**2*pr-d4*x2*x3*pr+d2*x3**2*pr-&
   y2**2+d2*pr*y2**2+d2*y2*y3-d4*pr*y2*y3-y3**2+d2*pr*y3**2)*cf
 km(2,3)=(d2*x2*y3*pr-d2*y1*x2*pr+d2*y1*x3*pr-x1*y2+d2*pr*x1*y2+y2*x3-    &
   d2*pr*y2*x3+x1*y3-d2*pr*x1*y3-y3*x3)*cf
 km(2,4)=-(-d2*x2*x1+d2*x2*x3+d2*x3*x1-d2*x3**2+d2*x2*x1*pr-d2*x2*x3*pr-  &
   d2*x3*x1*pr+d2*x3**2*pr+y2*y3-d2*pr*y2*y3-y2*y1+d2*pr*y2*y1-y3**2+     &
   d2*pr*y3**2+y3*y1-d2*pr*y3*y1)*cf
 km(2,5)=(d2*y1*x2*pr-d2*y1*x3*pr+d2*pr*y2*x3+x1*y2-d2*pr*x1*y2-y2*x2-    &
   x1*y3+d2*pr*x1*y3+x2*y3-d2*x2*y3*pr)*cf
 km(2,6)=(-d2*x2*x1+d2*x2**2+d2*x3*x1-d2*x2*x3+d2*x2*x1*pr-d2*x2**2*pr-   &
   d2*x3*x1*pr+d2*x2*x3*pr-y2*y1+d2*pr*y2*y1+y2**2-d2*pr*y2**2+y3*y1-     &
   d2*pr*y3*y1-y2*y3+d2*pr*y2*y3)*cf
!-----------------------------------------------------------------------
 km(3,3)=(-d2*y3**2+d4*y3*y1-d2*y1**2+d2*pr*y3**2-d4*pr*y3*y1+d2*pr*y1**2-&
   x1**2+d2*x1**2*pr+d2*x3*x1-d4*x3*x1*pr-x3**2+d2*x3**2*pr)*cf
 km(3,4)=(x1-x3)*(-y3+y1)*cf
 km(3,5)=-(d2*y3*y1-d2*y2*y3-d2*y1**2+d2*y2*y1-d2*pr*y3*y1+d2*pr*y2*y3+   &
   d2*pr*y1**2-d2*pr*y2*y1-x1**2+d2*x1**2*pr+x2*x1-d2*x2*x1*pr+x3*x1-     &
   d2*x3*x1*pr-x2*x3+d2*x2*x3*pr)*cf
 km(3,6)=-(-d2*pr*x1*y3+d2*x2*y3*pr-d2*y1*x2*pr+y1*x1-x1*y2+d2*pr*x1*y2-  &
   y1*x3+d2*y1*x3*pr+y2*x3-d2*pr*y2*x3)*cf
!-----------------------------------------------------------------------
 km(4,4)=(-d2*x1**2+d4*x3*x1-d2*x3**2+d2*x1**2*pr-d4*x3*x1*pr+d2*x3**2*pr-&
   y3**2+d2*pr*y3**2+d2*y3*y1-d4*pr*y3*y1-y1**2+d2*pr*y1**2)*cf
 km(4,5)=(d2*pr*x1*y2+d2*y1*x3*pr-d2*pr*y2*x3+x1*y3-d2*pr*x1*y3-x2*y3+    &
   d2*x2*y3*pr-y1*x1+y1*x2-d2*y1*x2*pr)*cf
 km(4,6)=-(-d2*x1**2+d2*x2*x1+d2*x3*x1-d2*x2*x3+d2*x1**2*pr-d2*x2*x1*pr-  &
   d2*x3*x1*pr+d2*x2*x3*pr+y3*y1-d2*pr*y3*y1-y2*y3+d2*pr*y2*y3-y1**2+     &
   d2*pr*y1**2+y2*y1-d2*pr*y2*y1)*cf
!-----------------------------------------------------------------------
 km(5,5)=(-d2*y1**2+d4*y2*y1-d2*y2**2+d2*pr*y1**2-d4*pr*y2*y1+d2*pr*y2**2-&
   x1**2+d2*x1**2*pr+d2*x2*x1-d4*x2*x1*pr-x2**2+d2*x2**2*pr)*cf
 km(5,6)=(x1-x2)*(y1-y2)*cf
!-----------------------------------------------------------------------
 km(6,6)=(-d2*x1**2+d4*x2*x1-d2*x2**2+d2*x1**2*pr-d4*x2*x1*pr+d2*x2**2*pr-&
   y1**2+d2*pr*y1**2+d2*y2*y1-d4*pr*y2*y1-y2**2+d2*pr*y2**2)*cf
!-----------------------------LOWER TRIANGLE----------------------------
 km(2,1)=km(1,2)
 km(3,1)=km(1,3)
 km(3,2)=km(2,3)
 km(4,1)=km(1,4)
 km(4,2)=km(2,4)
 km(4,3)=km(3,4)
 km(5,1)=km(1,5)
 km(5,2)=km(2,5)
 km(5,3)=km(3,5)
 km(5,4)=km(4,5)
 km(6,1)=km(1,6)
 km(6,2)=km(2,6)
 km(6,3)=km(3,6)
 km(6,4)=km(4,6)
 km(6,5)=km(5,6)
RETURN
END SUBROUTINE stiff3


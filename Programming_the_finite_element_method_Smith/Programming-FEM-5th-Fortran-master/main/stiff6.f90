SUBROUTINE stiff6(km,coord,ym,pr)
!
!    exact stiffness matrix of a 6-node triangle 
!    with straight sides in plane strain
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 REAL(iwp),INTENT(IN)::coord(6,2),ym,pr
 REAL(iwp),INTENT(OUT)::km(12,12)
 REAL(iwp)::x1,x3,x5,y1,y3,y5,cf
 REAL(iwp)::d0=0._iwp,d1=1._iwp,d2=2._iwp,d3=3._iwp,d4=4._iwp,d12=12._iwp
 x1=coord(1,1)
 y1=coord(1,2)
 x3=coord(3,1)
 y3=coord(3,2)
 x5=coord(5,1)
 y5=coord(5,2) 
 cf=ym/(-d1+d2*pr)/(d1+pr)/(x1*y3-x1*y5+x3*y5-y1*x3+y1*x5-y3*x5)
!----------------------------------------------------------------------------
 km(1,1)=-(-d2*y3**2+d2*y3**2*pr+d4*y3*y5-d4*y3*y5*pr-d2*y5**2+           &
   d2*y5**2*pr-x3**2+d2*x3**2*pr+d2*x3*x5-d4*x3*x5*pr-x5**2+              &
   d2*x5**2*pr)/d4*cf
 km(1,2)=-(x3-x5)*(y3-y5)/d4*cf
 km(1,3)=(d2*y3*y5-d2*y5**2+d2*y5**2*pr+x3*x5-x5**2+d2*x5**2*pr+d2*y5*y1+ &
   x5*x1-x3*x1-d2*y3*y5*pr-d2*x5*x1*pr+d2*pr*y3*y1-d2*y5*pr*y1+           &
   d2*x3*x1*pr-d2*x3*x5*pr-d2*y3*y1)/d3*cf
 km(1,4)=(d2*pr*y3*x1-d2*pr*y5*x1+d2*x5*y1*pr-d2*x3*y1*pr+x5*y5-y1*x5+    &
   y1*x3-x3*y5+d2*pr*x3*y5-d2*pr*y3*x5)/d3*cf
 km(1,5)=-(d2*y3*y5-d2*y5**2+d2*y5**2*pr+x3*x5-x5**2+d2*x5**2*pr+         &
   d2*y5*y1+x5*x1-x3*x1-d2*y3*y5*pr-d2*x5*x1*pr+d2*pr*y3*y1-d2*y5*pr*y1+  &
   d2*x3*x1*pr-d2*x3*x5*pr-d2*y3*y1)/d12*cf
 km(1,6)=-(d2*pr*y3*x1-d2*pr*y5*x1+d2*x5*y1*pr-d2*x3*y1*pr+x5*y5-y1*x5+   &
   y1*x3-x3*y5+d2*pr*x3*y5-d2*pr*y3*x5)/d12*cf
 km(1,7)=d0
 km(1,8)=d0
 km(1,9)=(-x3*x5-d2*y3*y1+x5*x1-x3*x1+d2*y5*y1+d2*pr*y3*y1-d2*y3*y5+      &
   d2*x3*x1*pr-d2*y3**2*pr-d2*x3**2*pr-d2*y5*pr*y1+d2*x3*x5*pr+d2*y3**2-  &
   d2*x5*x1*pr+x3**2+d2*y3*y5*pr)/d12*cf
 km(1,10)=(-d2*pr*y5*x1+d2*pr*x3*y5-d2*x3*y1*pr+d2*x5*y1*pr-d2*pr*y3*x5+  &
   y3*x5-y1*x5+y1*x3-y3*x3+d2*pr*y3*x1)/d12*cf
 km(1,11)=-(-x3*x5-d2*y3*y1+x5*x1-x3*x1+d2*y5*y1+d2*pr*y3*y1-d2*y3*y5+    &
   d2*x3*x1*pr-d2*y3**2*pr-d2*x3**2*pr-d2*y5*pr*y1+d2*x3*x5*pr+d2*y3**2-  &
   d2*x5*x1*pr+x3**2+d2*y3*y5*pr)/d3*cf
 km(1,12)=-(-d2*pr*y5*x1+d2*pr*x3*y5-d2*x3*y1*pr+d2*x5*y1*pr-d2*pr*y3*x5+ &
   y3*x5-y1*x5+y1*x3-y3*x3+d2*pr*y3*x1)/d3*cf
!--------------------------------------------------------------------------
 km(2,2)=-(-d2*x3**2+d4*x3*x5-d2*x5**2+d2*x3**2*pr-d4*x3*x5*pr+           &
   d2*x5**2*pr-y3**2+d2*y3**2*pr+d2*y3*y5-d4*y3*y5*pr-y5**2+              &
   d2*y5**2*pr)/d4*cf
 km(2,3)=-(d2*pr*y3*x1-d2*pr*y5*x1+d2*x5*y1*pr-d2*x3*y1*pr-x5*y5+y3*x5-   &
   x1*y3+x1*y5+d2*pr*x3*y5-d2*pr*y3*x5)/d3*cf
 km(2,4)=(y3*y5-y5**2+d2*y5**2*pr+d2*x3*x5-d2*x5**2+d2*x5**2*pr+y5*y1+    &
   d2*x5*x1-d2*x3*x1-d2*y3*y5*pr-d2*x5*x1*pr+d2*pr*y3*y1-d2*y5*pr*y1+     &
   d2*x3*x1*pr-d2*x3*x5*pr-y3*y1)/d3*cf
 km(2,5)=(d2*pr*y3*x1-d2*pr*y5*x1+d2*x5*y1*pr-d2*x3*y1*pr-x5*y5+y3*x5-    &
   x1*y3+x1*y5+d2*pr*x3*y5-d2*pr*y3*x5)/d12*cf
 km(2,6)=-(y3*y5-y5**2+d2*y5**2*pr+d2*x3*x5-d2*x5**2+d2*x5**2*pr+y5*y1+   &
   d2*x5*x1-d2*x3*x1-d2*y3*y5*pr-d2*x5*x1*pr+d2*pr*y3*y1-d2*y5*pr*y1+     &
   d2*x3*x1*pr-d2*x3*x5*pr-y3*y1)/d12*cf
 km(2,7)=d0
 km(2,8)=d0
 km(2,9)=-(-d2*pr*y5*x1+d2*pr*x3*y5-d2*x3*y1*pr+d2*x5*y1*pr-d2*pr*y3*x5+  &
   x1*y5-x1*y3-x3*y5+y3*x3+d2*pr*y3*x1)/d12*cf
 km(2,10)=(d2*x3*x1*pr-d2*x5*x1*pr-d2*y5*pr*y1+d2*pr*y3*y1+d2*x3*x5*pr+   &
   y3**2+d2*y3*y5*pr+d2*x3**2-d2*y3**2*pr-y3*y5+y5*y1-y3*y1-d2*x3*x5+     &
   d2*x5*x1-d2*x3*x1-d2*x3**2*pr)/d12*cf
 km(2,11)=(-d2*pr*y5*x1+d2*pr*x3*y5-d2*x3*y1*pr+d2*x5*y1*pr-d2*pr*y3*x5+  &
   x1*y5-x1*y3-x3*y5+y3*x3+d2*pr*y3*x1)/d3*cf
 km(2,12)=-(d2*x3*x1*pr-d2*x5*x1*pr-d2*y5*pr*y1+d2*pr*y3*y1+d2*x3*x5*pr+  &
   y3**2+d2*y3*y5*pr+d2*x3**2-d2*y3**2*pr-y3*y5+y5*y1-y3*y1-d2*x3*x5+     &
   d2*x5*x1-d2*x3*x1-d2*x3**2*pr)/d3*cf
!--------------------------------------------------------------------------
 km(3,3)=-d2/d3*(-d2*y3**2+d2*y3*y5-d2*y5**2+d2*y3**2*pr+d2*y5**2*pr-     &
   x3**2+x3*x5-x5**2+d2*x3**2*pr+d2*x5**2*pr+d2*pr*y1**2-d2*y1**2+        &
   d2*x1**2*pr-x1**2+d2*y5*y1+x5*x1+x3*x1-d2*y3*y5*pr-d2*x5*x1*pr-        &
   d2*pr*y3*y1-d2*y5*pr*y1-d2*x3*x1*pr-d2*x3*x5*pr+d2*y3*y1)*cf
 km(3,4)=-(d2*x5*y5-y3*x5-y1*x5-x3*y5+d2*y3*x3-y1*x3-x1*y5-x1*y3+         &
   d2*x1*y1)/d3*cf
 km(3,5)=km(1,3)
 km(3,6)=km(1,4)
 km(3,7)=-d2/d3*(-x3*x5-d2*y3*y1+x5*x1-x3*x1+d2*y5*y1+d2*pr*y3*y1-        &
   d2*y3*y5+d2*x3*x1*pr-d2*y3**2*pr-d2*x3**2*pr-d2*y5*pr*y1+d2*x3*x5*pr+  &
   d2*y3**2-d2*x5*x1*pr+x3**2+d2*y3*y5*pr)*cf
 km(3,8)=-(-y1*x5+y3*x5+y1*x3+x1*y3-x1*y5+x3*y5-d2*y3*x3)/d3*cf
 km(3,9)=d0
 km(3,10)=d0
 km(3,11)=d2/d3*(-d2*y3*y5-x3*x5+d2*pr*y1**2-d2*y1**2+d2*x1**2*pr-x1**2+  &
   d2*y5*y1+x5*x1+x3*x1+d2*y3*y5*pr-d2*x5*x1*pr-d2*pr*y3*y1-d2*y5*pr*y1-  &
   d2*x3*x1*pr+d2*x3*x5*pr+d2*y3*y1)*cf
 km(3,12)=(d2*x1*y1-y1*x5+y3*x5-y1*x3-x1*y3-x1*y5+x3*y5)/d3*cf
!--------------------------------------------------------------------------
 km(4,4)=-d2/d3*(-y3**2+y3*y5-y5**2+d2*y3**2*pr+d2*y5**2*pr-d2*x3**2+     &
   d2*x3*x5-d2*x5**2+d2*x3**2*pr+d2*x5**2*pr+d2*pr*y1**2-y1**2+           &
   d2*x1**2*pr-d2*x1**2+y5*y1+d2*x5*x1+d2*x3*x1-d2*y3*y5*pr-d2*x5*x1*pr-  &
   d2*pr*y3*y1-d2*y5*pr*y1-d2*x3*x1*pr-d2*x3*x5*pr+y3*y1)*cf
 km(4,5)=km(2,3)
 km(4,6)=km(2,4)
 km(4,7)=km(3,8)
 km(4,8)=-d2/d3*(d2*x3*x1*pr-d2*x5*x1*pr-d2*y5*pr*y1+d2*pr*y3*y1+         &
   d2*x3*x5*pr+y3**2+d2*y3*y5*pr+d2*x3**2-d2*y3**2*pr-y3*y5+y5*y1-y3*y1-  &
   d2*x3*x5+d2*x5*x1-d2*x3*x1-d2*x3**2*pr)*cf
 km(4,9)=d0
 km(4,10)=d0
 km(4,11)=km(3,12)
 km(4,12)=d2/d3*(-y3*y5-d2*x3*x5+d2*pr*y1**2-y1**2+d2*x1**2*pr-d2*x1**2+  &
   y5*y1+d2*x5*x1+d2*x3*x1+d2*y3*y5*pr-d2*x5*x1*pr-d2*pr*y3*y1-           &
   d2*y5*pr*y1-d2*x3*x1*pr+d2*x3*x5*pr+y3*y1)*cf
!--------------------------------------------------------------------------
 km(5,5)=-(-d2*y5**2+d4*y5*y1-d2*y1**2+d2*y5**2*pr-d4*y5*pr*y1+           &
   d2*pr*y1**2-x1**2+d2*x1**2*pr+d2*x5*x1-d4*x5*x1*pr-x5**2+              &
   d2*x5**2*pr)/d4*cf
 km(5,6)=-(x1-x5)*(-y5+y1)/d4*cf
 km(5,7)=(-d2*y3*y5-x3*x5+d2*pr*y1**2-d2*y1**2+d2*x1**2*pr-x1**2+         &
   d2*y5*y1+x5*x1+x3*x1+d2*y3*y5*pr-d2*x5*x1*pr-d2*pr*y3*y1-d2*y5*pr*y1-  &
   d2*x3*x1*pr+d2*x3*x5*pr+d2*y3*y1)/d3*cf
 km(5,8)=(-d2*pr*y3*x5-d2*pr*y5*x1+d2*x5*y1*pr-y1*x5+y3*x5+x1*y1+         &
   d2*pr*y3*x1-x1*y3-d2*x3*y1*pr+d2*pr*x3*y5)/d3*cf
 km(5,9)=-(-d2*y3*y5-x3*x5+d2*pr*y1**2-d2*y1**2+d2*x1**2*pr-x1**2+        &
   d2*y5*y1+x5*x1+x3*x1+d2*y3*y5*pr-d2*x5*x1*pr-d2*pr*y3*y1-d2*y5*pr*y1-  &
   d2*x3*x1*pr+d2*x3*x5*pr+d2*y3*y1)/d12*cf
 km(5,10)=-(-d2*pr*y3*x5-d2*pr*y5*x1+d2*x5*y1*pr-y1*x5+y3*x5+x1*y1+       &
   d2*pr*y3*x1-x1*y3-d2*x3*y1*pr+d2*pr*x3*y5)/d12*cf
 km(5,11)=d0
 km(5,12)=d0
!--------------------------------------------------------------------------
 km(6,6)=-(-d2*x1**2+d4*x5*x1-d2*x5**2+d2*x1**2*pr-d4*x5*x1*pr+           &
   d2*x5**2*pr-y5**2+d2*y5**2*pr+d2*y5*y1-d4*y5*pr*y1-y1**2+              &
   d2*pr*y1**2)/d4*cf
 km(6,7)=-(-d2*x3*y1*pr+d2*pr*x3*y5-d2*pr*y5*x1+d2*x5*y1*pr-d2*pr*y3*x5+  &
   d2*pr*y3*x1+x1*y5-x3*y5-x1*y1+y1*x3)/d3*cf
 km(6,8)=(-y3*y5-d2*x3*x5+d2*pr*y1**2-y1**2+d2*x1**2*pr-d2*x1**2+y5*y1+   &
   d2*x5*x1+d2*x3*x1+d2*y3*y5*pr-d2*x5*x1*pr-d2*pr*y3*y1-d2*y5*pr*y1-     &
   d2*x3*x1*pr+d2*x3*x5*pr+y3*y1)/d3*cf
 km(6,9)=(-d2*x3*y1*pr+d2*pr*x3*y5-d2*pr*y5*x1+d2*x5*y1*pr-d2*pr*y3*x5+   &
   d2*pr*y3*x1+x1*y5-x3*y5-x1*y1+y1*x3)/d12*cf
 km(6,10)=-(-y3*y5-d2*x3*x5+d2*pr*y1**2-y1**2+d2*x1**2*pr-d2*x1**2+y5*y1+ &
   d2*x5*x1+d2*x3*x1+d2*y3*y5*pr-d2*x5*x1*pr-d2*pr*y3*y1-d2*y5*pr*y1-     &
   d2*x3*x1*pr+d2*x3*x5*pr+y3*y1)/d12*cf
 km(6,11)=d0
 km(6,12)=d0
!--------------------------------------------------------------------------
 km(7,7)=km(3,3)
 km(7,8)=km(3,4)
 km(7,9)=km(5,7)
 km(7,10)=km(5,8)
 km(7,11)=d2/d3*(d2*y3*y5-d2*y5**2+d2*y5**2*pr+x3*x5-x5**2+d2*x5**2*pr+   &
   d2*y5*y1+x5*x1-x3*x1-d2*y3*y5*pr-d2*x5*x1*pr+d2*pr*y3*y1-d2*y5*pr*y1+  &
   d2*x3*x1*pr-d2*x3*x5*pr-d2*y3*y1)*cf
!--------------------------------------------------------------------------
 km(7,12)=(d2*x5*y5-y1*x5-y3*x5+y1*x3+x1*y3-x1*y5-x3*y5)/d3*cf
 km(8,8)=km(4,4)
 km(8,9)=km(6,7)
 km(8,10)=km(6,8)
 km(8,11)=km(7,12)
 km(8,12)=d2/d3*(y3*y5-y5**2+d2*y5**2*pr+d2*x3*x5-d2*x5**2+d2*x5**2*pr+   &
   y5*y1+d2*x5*x1-d2*x3*x1-d2*y3*y5*pr-d2*x5*x1*pr+d2*pr*y3*y1-           &
   d2*y5*pr*y1+d2*x3*x1*pr-d2*x3*x5*pr-y3*y1)*cf
!--------------------------------------------------------------------------
 km(9,9)=-(-d2*y1**2+d4*y3*y1-d2*y3**2+d2*pr*y1**2-d4*pr*y3*y1+           &
   d2*y3**2*pr-x1**2+d2*x1**2*pr+d2*x3*x1-d4*x3*x1*pr-x3**2+              &
   d2*x3**2*pr)/d4*cf
 km(9,10)=-(x1-x3)*(y1-y3)/d4*cf
 km(9,11)=km(1,11)
 km(9,12)=km(2,11)
!--------------------------------------------------------------------------
 km(10,10)=-(d2*x3**2*pr-y1**2-d2*x3**2+d2*y3*y1+d2*x1**2*pr+d2*y3**2*pr- &
   y3**2-d2*x1**2+d2*pr*y1**2+d4*x3*x1-d4*pr*y3*y1-d4*x3*x1*pr)/d4*cf
 km(10,11)=km(1,12)
 km(10,12)=km(2,12)
!--------------------------------------------------------------------------
 km(11,11)=km(3,3)
 km(11,12)=km(3,4)
!--------------------------------------------------------------------------
 km(12,12)=km(4,4)
!---------------------------------LOWER TRIANGLE---------------------------
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
 km(7,1)=km(1,7)
 km(7,2)=km(2,7)
 km(7,3)=km(3,7)
 km(7,4)=km(4,7)
 km(7,5)=km(5,7)
 km(7,6)=km(6,7)
 km(8,1)=km(1,8)
 km(8,2)=km(2,8)
 km(8,3)=km(3,8)
 km(8,4)=km(4,8)
 km(8,5)=km(5,8)
 km(8,6)=km(6,8)
 km(8,7)=km(7,8)
 km(9,1)=km(1,9)
 km(9,2)=km(2,9)
 km(9,3)=km(3,9)
 km(9,4)=km(4,9)
 km(9,5)=km(5,9)
 km(9,6)=km(6,9)
 km(9,7)=km(7,9)
 km(9,8)=km(8,9)
 km(10,1)=km(1,10)
 km(10,2)=km(2,10)
 km(10,3)=km(3,10)
 km(10,4)=km(4,10)
 km(10,5)=km(5,10)
 km(10,6)=km(6,10)
 km(10,7)=km(7,10)
 km(10,8)=km(8,10)
 km(10,9)=km(9,10)
 km(11,1)=km(1,11)
 km(11,2)=km(2,11)
 km(11,3)=km(3,11)
 km(11,4)=km(4,11)
 km(11,5)=km(5,11)
 km(11,6)=km(6,11)
 km(11,7)=km(7,11)
 km(11,8)=km(8,11)
 km(11,9)=km(9,11)
 km(11,10)=km(10,11)
 km(12,1)=km(1,12)
 km(12,2)=km(2,12)
 km(12,3)=km(3,12)
 km(12,4)=km(4,12)
 km(12,5)=km(5,12)
 km(12,6)=km(6,12)
 km(12,7)=km(7,12)
 km(12,8)=km(8,12)
 km(12,9)=km(9,12)
 km(12,10)=km(10,12)
 km(12,11)=km(11,12)
RETURN
END SUBROUTINE stiff6


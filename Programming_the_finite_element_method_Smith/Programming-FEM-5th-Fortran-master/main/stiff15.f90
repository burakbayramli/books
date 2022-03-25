SUBROUTINE stiff15(km,coord,ym,pr)
!
!    exact stiffness matrix of a 15-node triangle
!    with straight sides in plane strain
!
 IMPLICIT NONE
 INTEGER,PARAMETER::iwp=SELECTED_REAL_KIND(15)
 INTEGER::i,j
 REAL(iwp),INTENT(IN)::coord(15,2),ym,pr
 REAL(iwp),INTENT(OUT)::km(30,30)
 REAL(iwp)::x1,x5,x9,y1,y5,y9,cf
 REAL(iwp)::d1=1._iwp,d2=2._iwp,d3=3._iwp,d4=4._iwp,d5=5._iwp,            &
   d6=6._iwp,d7=7._iwp,d8=8._iwp,d9=9._iwp,d10=10._iwp,d12=12._iwp,       &
   d13=13._iwp,d14=14._iwp,d15=15._iwp,d16=16._iwp,d17=17._iwp,           &
   d18=18._iwp,d19=19._iwp,d20=20._iwp,d23=23._iwp,d24=24._iwp,           &
   d26=26._iwp,d27=27._iwp,d28=28._iwp,d29=29._iwp,d30=30._iwp,           &
   d31=31._iwp,d32=32._iwp,d33=33._iwp,d34=34._iwp,d35=35._iwp,           &
   d36=36._iwp,d38=38._iwp,d39=39._iwp,d40=40._iwp,d43=43._iwp,           &
   d45=45._iwp,d46=46._iwp,d47=47._iwp,d48=48._iwp,d54=54._iwp,           &
   d55=55._iwp,d56=56._iwp,d58=58._iwp,d62=62._iwp,d64=64._iwp,           &
   d66=66._iwp,d68=68._iwp,d70=70._iwp,d77=77._iwp,d78=78._iwp,           &
   d86=86._iwp,d90=90._iwp,d92=92._iwp,d97=97._iwp,d107=107._iwp,         &
   d109=109._iwp,d110=110._iwp,d115=115._iwp,d121=121._iwp,d124=124._iwp, &
   d128=128._iwp,d153=153._iwp,d154=154._iwp,d175=175._iwp,d184=184._iwp, &
   d187=187._iwp,d189=189._iwp,d193=193._iwp,d201=201._iwp,d218=218._iwp, &
   d221=221._iwp,d230=230._iwp,d233=233._iwp,d242=242._iwp,d241=241._iwp, &
   d252=252._iwp,d264=264._iwp,d306=306._iwp,d315=315._iwp,d386=386._iwp, &
   d402=402._iwp,d442=442._iwp,d466=466._iwp,d482=482._iwp,d945=945._iwp, &
   d3780=3780._iwp
 x1=coord(1,1)
 y1=coord(1,2)
 x5=coord(5,1)
 y5=coord(5,2)
 x9=coord(9,1)
 y9=coord(9,2)
 cf=ym/(x1*y5-x1*y9+x5*y9-y1*x5+y1*x9-y5*x9)/(-d1+d2*pr)/(d1+pr)
!----------------------------------------------------------------------------
 km(1,1)=-d47/d252*(-d2*y5**2+d4*y5*y9-d2*y9**2+d2*pr*y5**2-              &
   d4*pr*y5*y9+d2*pr*y9**2-x5**2+d2*x5**2*pr+d2*x5*x9-d4*x5*x9*pr-x9**2+  &
   d2*x9**2*pr)*cf
 km(1,2)=-d47/d252*(x5-x9)*(y5-y9)*cf
 km(1,3)=d4/d945*(-d92*x5*x1+d184*y9*y1+d92*x9*x1-d184*y5*y1+d30*y5**2+   &
   d124*y5*y9-d154*y9**2-d30*pr*y5**2+d154*pr*y9**2+d15*x5**2+d62*x5*x9-  &
   d77*x9**2-d30*x5**2*pr+d154*x9**2*pr-d124*pr*y5*y9+d184*pr*y5*y1-      &
   d184*pr*y9*y1-d124*x5*x9*pr-d184*x9*x1*pr+d184*x5*x1*pr)*cf
 km(1,4)=d4/d945*(d184*pr*x5*y9-d184*pr*y5*x9+d92*y1*x5-d92*y1*x9+        &
   d15*y5*x9-d77*x5*y9-d15*y5*x5+d184*x9*y1*pr-d184*x5*y1*pr+             &
   d184*pr*y5*x1-d184*pr*y9*x1+d77*y9*x9)*cf
 km(1,5)=-(-d201*x5*x1+d402*y9*y1+d201*x9*x1-d402*y5*y1-d40*y5**2+        &
   d482*y5*y9-d442*y9**2+d40*pr*y5**2+d442*pr*y9**2-d20*x5**2+d241*x5*x9- &
   d221*x9**2+d40*x5**2*pr+d442*x9**2*pr-d482*pr*y5*y9+d402*pr*y5*y1-     &
   d402*pr*y9*y1-d482*x5*x9*pr-d402*x9*x1*pr+d402*x5*x1*pr)/d945*cf
 km(1,6)=-(d402*pr*x5*y9-d402*pr*y5*x9+d201*y1*x5-d201*y1*x9-d20*y5*x9-   &
   d221*x5*y9+d20*y5*x5+d402*x9*y1*pr-d402*x5*y1*pr+d402*pr*y5*x1-        &
   d402*pr*y9*x1+d221*y9*x9)/d945*cf
 km(1,7)=d4/d945*(-d24*x5*x1+d48*y9*y1+d24*x9*x1-d48*y5*y1-d10*y5**2+     &
   d68*y5*y9-d58*y9**2+d10*pr*y5**2+d58*pr*y9**2-d5*x5**2+d34*x5*x9-      &
   d29*x9**2+d10*x5**2*pr+d58*x9**2*pr-d68*pr*y5*y9+d48*pr*y5*y1-         &
   d48*pr*y9*y1-d68*x5*x9*pr-d48*x9*x1*pr+d48*x5*x1*pr)*cf
 km(1,8)=d4/d945*(d48*pr*x5*y9-d48*pr*y5*x9+d24*y1*x5-d24*y1*x9-          &
   d5*y5*x9-d29*x5*y9+d5*y5*x5+d48*x9*y1*pr-d48*x5*y1*pr+d48*pr*y5*x1-    &
   d48*pr*y9*x1+d29*y9*x9)*cf
 km(1,9)=-d107/d3780*(d2*y5*y9-d2*y5*y1-d2*y9**2+d2*y9*y1-d2*pr*y5*y9+    &
   d2*pr*y5*y1+d2*pr*y9**2-d2*pr*y9*y1+x5*x9-d2*x5*x9*pr-x5*x1+           &
   d2*x5*x1*pr-x9**2+d2*x9**2*pr+x9*x1-d2*x9*x1*pr)*cf
 km(1,10)=-d107/d3780*(-d2*pr*y5*x9+d2*pr*y5*x1-d2*pr*y9*x1-x5*y9+        &
   d2*pr*x5*y9+y1*x5-d2*x5*y1*pr+y9*x9-y1*x9+d2*x9*y1*pr)*cf
 km(1,11)=-d4/d189*(-d2*y5**2+d4*y5*y9-d2*y9**2+d2*pr*y5**2-d4*pr*y5*y9+  &
   d2*pr*y9**2-x5**2+d2*x5**2*pr+d2*x5*x9-d4*x5*x9*pr-x9**2+d2*x9**2*pr)*cf
 km(1,12)=-d4/d189*(-y5*x9-x5*y9+y5*x5+y9*x9)*cf
 km(1,13)=km(1,11)
 km(1,14)=km(1,12)
 km(1,15)=km(1,11)
 km(1,16)=km(1,12)
 km(1,17)=d107/d3780*(-d2*y5*y1+d2*y5**2+d2*y9*y1-d2*y5*y9+d2*pr*y5*y1-   &
   d2*pr*y5**2-d2*pr*y9*y1+d2*pr*y5*y9-x5*x1+d2*x5*x1*pr+x5**2-           &
   d2*x5**2*pr+x9*x1-d2*x9*x1*pr-x5*x9+d2*x5*x9*pr)*cf
 km(1,18)=d107/d3780*(d2*pr*x5*y9-d2*pr*y5*x9+y1*x5-y1*x9+y5*x9-y5*x5+    &
   d2*x9*y1*pr-d2*x5*y1*pr+d2*pr*y5*x1-d2*pr*y9*x1)*cf
 km(1,19)=-d4/d945*(-d24*x5*x1+d48*y9*y1+d24*x9*x1-d48*y5*y1+d58*y5**2-   &
   d68*y5*y9+d10*y9**2-d58*pr*y5**2-d10*pr*y9**2+d29*x5**2-d34*x5*x9+     &
   d5*x9**2-d58*x5**2*pr-d10*x9**2*pr+d68*pr*y5*y9+d48*pr*y5*y1-          &
   d48*pr*y9*y1+d68*x5*x9*pr-d48*x9*x1*pr+d48*x5*x1*pr)*cf
 km(1,20)=-d4/d945*(d48*pr*x5*y9-d48*pr*y5*x9+d24*y1*x5-d24*y1*x9+        &
   d29*y5*x9+d5*x5*y9-d29*y5*x5+d48*x9*y1*pr-d48*x5*y1*pr+d48*pr*y5*x1-   &
   d48*pr*y9*x1-d5*y9*x9)*cf
 km(1,21)=(-d201*x5*x1+d402*y9*y1+d201*x9*x1-d402*y5*y1+d442*y5**2-       &
   d482*y5*y9+d40*y9**2-d442*pr*y5**2-d40*pr*y9**2+d221*x5**2-d241*x5*x9+ &
   d20*x9**2-d442*x5**2*pr-d40*x9**2*pr+d482*pr*y5*y9+d402*pr*y5*y1-      &
   d402*pr*y9*y1+d482*x5*x9*pr-d402*x9*x1*pr+d402*x5*x1*pr)/d945*cf
 km(1,22)=(d402*pr*x5*y9-d402*pr*y5*x9+d201*y1*x5-d201*y1*x9+d221*y5*x9   &
   +d20*x5*y9-d221*y5*x5+d402*x9*y1*pr-d402*x5*y1*pr+d402*pr*y5*x1-       &
   d402*pr*y9*x1-d20*y9*x9)/d945*cf
 km(1,23)=-d4/d945*(-d92*x5*x1+d184*y9*y1+d92*x9*x1-d184*y5*y1+           &
   d154*y5**2-d124*y5*y9-d30*y9**2-d154*pr*y5**2+d30*pr*y9**2+d77*x5**2-  &
   d62*x5*x9-d15*x9**2-d154*x5**2*pr+d30*x9**2*pr+d124*pr*y5*y9+          &
   d184*pr*y5*y1-d184*pr*y9*y1+d124*x5*x9*pr-d184*x9*x1*pr+               &
   d184*x5*x1*pr)*cf
 km(1,24)=-d4/d945*(d184*pr*x5*y9-d184*pr*y5*x9+d92*y1*x5-d92*y1*x9+      &
   d77*y5*x9-d15*x5*y9-d77*y5*x5+d184*x9*y1*pr-d184*x5*y1*pr+             &
   d184*pr*y5*x1-d184*pr*y9*x1+d15*y9*x9)*cf
 km(1,25)=d8/d189*(-d2*y5**2+d4*y5*y9-d2*y9**2+d2*pr*y5**2-d4*pr*y5*y9+   &
   d2*pr*y9**2-x5**2+d2*x5**2*pr+d2*x5*x9-d4*x5*x9*pr-x9**2+d2*x9**2*pr)*cf
 km(1,26)=d8/d189*(-y5*x9-x5*y9+y5*x5+y9*x9)*cf
 km(1,27)=km(1,25)
 km(1,28)=km(1,26)
 km(1,29)=km(1,25)
 km(1,30)=km(1,26)
!----------------------------------------------------------------------------
 km(2,2)=-d47/d252*(-d2*x5**2+d4*x5*x9-d2*x9**2+d2*x5**2*pr-d4*x5*x9*     &
   pr+d2*x9**2*pr-y5**2+d2*pr*y5**2+d2*y5*y9-d4*pr*y5*y9-y9**2+           &
   d2*pr*y9**2)*cf
 km(2,3)=-d4/d945*(184*pr*x5*y9-d184*pr*y5*x9+d77*y5*x9-d92*x1*y5+        &
   d92*x1*y9-d15*x5*y9+d15*y5*x5+d184*x9*y1*pr-d184*x5*y1*pr+             &
   d184*pr*y5*x1-d184*pr*y9*x1-d77*y9*x9)*cf
 km(2,4)=d4/d945*(-d184*x5*x1+d92*y9*y1+d184*x9*x1-d92*y5*y1+d15*y5**2+   &
   d62*y5*y9-d77*y9**2-d30*pr*y5**2+d154*pr*y9**2+d30*x5**2+d124*x5*x9-   &
   d154*x9**2-d30*x5**2*pr+d154*x9**2*pr-d124*pr*y5*y9+d184*pr*y5*y1-     &
   d184*pr*y9*y1-d124*x5*x9*pr-d184*x9*x1*pr+d184*x5*x1*pr)*cf
 km(2,5)=(402*pr*x5*y9-d402*pr*y5*x9+d221*y5*x9-d201*x1*y5+d201*x1*y9+    &
   d20*x5*y9-d20*y5*x5+d402*x9*y1*pr-d402*x5*y1*pr+d402*pr*y5*x1-         &
   d402*pr*y9*x1-d221*y9*x9)/d945*cf
 km(2,6)=-(-d402*x5*x1+d201*y9*y1+d402*x9*x1-d201*y5*y1-d20*y5**2+        &
   d241*y5*y9-d221*y9**2+d40*pr*y5**2+d442*pr*y9**2-d40*x5**2+            &
   d482*x5*x9-d442*x9**2+d40*x5**2*pr+d442*x9**2*pr-d482*pr*y5*y9+        &
   d402*pr*y5*y1-d402*pr*y9*y1-d482*x5*x9*pr-d402*x9*x1*pr+               &
   d402*x5*x1*pr)/d945*cf
 km(2,7)=-d4/d945*(48*pr*x5*y9-d48*pr*y5*x9+d29*y5*x9-d24*x1*y5+          &
   d24*x1*y9+d5*x5*y9-d5*y5*x5+d48*x9*y1*pr-d48*x5*y1*pr+d48*pr*y5*x1-    &
   d48*pr*y9*x1-d29*y9*x9)*cf
 km(2,8)=d4/d945*(-d48*x5*x1+d24*y9*y1+d48*x9*x1-d24*y5*y1-d5*y5**2+      &
   d34*y5*y9-d29*y9**2+d10*pr*y5**2+d58*pr*y9**2-d10*x5**2+d68*x5*x9-     &
   d58*x9**2+d10*x5**2*pr+d58*x9**2*pr-d68*pr*y5*y9+d48*pr*y5*y1-         &
   d48*pr*y9*y1-d68*x5*x9*pr-d48*x9*x1*pr+d48*x5*x1*pr)*cf
 km(2,9)=d107/d3780*(2*pr*x5*y9-d2*x5*y1*pr+d2*x9*y1*pr+y5*x9-            &
   d2*pr*y5*x9-x1*y5+d2*pr*y5*x1-y9*x9+x1*y9-d2*pr*y9*x1)*cf
 km(2,10)=-d107/d3780*(-d2*x5*x1+y9*y1+d2*x9*x1-y5*y1+y5*y9-y9**2+        &
   d2*pr*y9**2+d2*x5*x9-d2*x9**2+d2*x9**2*pr-d2*pr*y5*y9+d2*pr*y5*y1-     &
   d2*pr*y9*y1-d2*x5*x9*pr-d2*x9*x1*pr+d2*x5*x1*pr)*cf
 km(2,11)=km(1,12)
 km(2,12)=-d4/d189*(-d2*x5**2+d4*x5*x9-d2*x9**2+d2*x5**2*pr-              &
   d4*x5*x9*pr+d2*x9**2*pr-y5**2+d2*pr*y5**2+d2*y5*y9-d4*pr*y5*y9-y9**2+  &
   d2*pr*y9**2)*cf
 km(2,13)=km(1,12)
 km(2,14)=km(2,12)
 km(2,15)=km(1,12)
 km(2,16)=km(2,12)
 km(2,17)=-d107/d3780*(-d2*x5*y1*pr+d2*x9*y1*pr-d2*pr*y5*x9-x1*y5+        &
   d2*pr*y5*x1+y5*x5+x1*y9-d2*pr*y9*x1-x5*y9+d2*pr*x5*y9)*cf
 km(2,18)=d107/d3780*(-d2*x5*x1+y9*y1+d2*x9*x1-y5*y1+y5**2-y5*y9-         &
   d2*pr*y5**2+d2*x5**2-d2*x5*x9-d2*x5**2*pr+d2*pr*y5*y9+d2*pr*y5*y1-     &
   d2*pr*y9*y1+d2*x5*x9*pr-d2*x9*x1*pr+d2*x5*x1*pr)*cf
 km(2,19)=d4/d945*(48*pr*x5*y9-d48*pr*y5*x9-d5*y5*x9-d24*x1*y5+           &
   d24*x1*y9-d29*x5*y9+d29*y5*x5+d48*x9*y1*pr-d48*x5*y1*pr+d48*pr*y5*x1-  &
   d48*pr*y9*x1+d5*y9*x9)*cf
 km(2,20)=-d4/d945*(-d48*x5*x1+d24*y9*y1+d48*x9*x1-d24*y5*y1+d29*y5**2-   &
   d34*y5*y9+d5*y9**2-d58*pr*y5**2-d10*pr*y9**2+d58*x5**2-d68*x5*x9+      &
   d10*x9**2-d58*x5**2*pr-d10*x9**2*pr+d68*pr*y5*y9+d48*pr*y5*y1-         &
   d48*pr*y9*y1+d68*x5*x9*pr-d48*x9*x1*pr+d48*x5*x1*pr)*cf
 km(2,21)=-(402*pr*x5*y9-d402*pr*y5*x9-d20*y5*x9-d201*x1*y5+d201*x1*y9-   &
   d221*x5*y9+d221*y5*x5+d402*x9*y1*pr-d402*x5*y1*pr+d402*pr*y5*x1-       &
   d402*pr*y9*x1+d20*y9*x9)/d945*cf
 km(2,22)=(-d402*x5*x1+d201*y9*y1+d402*x9*x1-d201*y5*y1+d221*y5**2-       &
   d241*y5*y9+d20*y9**2-d442*pr*y5**2-d40*pr*y9**2+d442*x5**2-d482*x5*x9+ &
   d40*x9**2-d442*x5**2*pr-d40*x9**2*pr+d482*pr*y5*y9+d402*pr*y5*y1-      &
   d402*pr*y9*y1+d482*x5*x9*pr-d402*x9*x1*pr+d402*x5*x1*pr)/d945*cf
 km(2,23)=d4/d945*(184*pr*x5*y9-d184*pr*y5*x9+d15*y5*x9-d92*x1*y5+        &
   d92*x1*y9-d77*x5*y9+d77*y5*x5+d184*x9*y1*pr-d184*x5*y1*pr+             &
   d184*pr*y5*x1-d184*pr*y9*x1-d15*y9*x9)*cf
 km(2,24)=-d4/d945*(-d184*x5*x1+d92*y9*y1+d184*x9*x1-d92*y5*y1+           &
   d77*y5**2-d62*y5*y9-d15*y9**2-d154*pr*y5**2+d30*pr*y9**2+d154*x5**2-   &
   d124*x5*x9-d30*x9**2-d154*x5**2*pr+d30*x9**2*pr+d124*pr*y5*y9+         &
   d184*pr*y5*y1-d184*pr*y9*y1+d124*x5*x9*pr-d184*x9*x1*pr+               &
   d184*x5*x1*pr)*cf
 km(2,25)=km(1,26)
 km(2,26)=d8/d189*(-d2*x5**2+d4*x5*x9-d2*x9**2+d2*x5**2*pr-               &
   d4*x5*x9*pr+d2*x9**2*pr-y5**2+d2*pr*y5**2+d2*y5*y9-d4*pr*y5*y9-y9**2+  &
   d2*pr*y9**2)*cf
 km(2,27)=km(1,26)
 km(2,28)=km(2,26)
 km(2,29)=km(1,26)
 km(2,30)=km(2,26)
!----------------------------------------------------------------------------
 km(3,3)=-d32/d945*(62*pr*y1**2+d62*x1**2*pr+d31*x5*x1+d62*y9*y1+         &
   d31*x9*x1+d62*y5*y1-d62*y1**2-d31*x1**2-d54*y5**2+d46*y5*y9-           &
   d54*y9**2+d54*pr*y5**2+d54*pr*y9**2-d27*x5**2+d23*x5*x9-d27*x9**2+     &
   d54*x5**2*pr+d54*x9**2*pr-d46*pr*y5*y9-d62*pr*y5*y1-d62*pr*y9*y1-      &
   d46*x5*x9*pr-d62*x9*x1*pr-d62*x5*x1*pr)*cf
 km(3,4)=-d16/d945*(62*x1*y1-d31*y1*x5-d31*y1*x9-d23*y5*x9-d31*x1*y5-     &
   d31*x1*y9-d23*x5*y9+d54*y5*x5+d54*y9*x9)*cf
 km(3,5)=d8/d945*(90*pr*y1**2+d90*x1**2*pr-d31*x5*x1+d242*y9*y1+          &
   d121*x9*x1-d62*y5*y1-d90*y1**2-d45*x1**2-d78*y5**2+d218*y5*y9-         &
   d230*y9**2+d78*pr*y5**2+d230*pr*y9**2-d39*x5**2+d109*x5*x9-d115*x9**2+ &
   d78*x5**2*pr+d230*x9**2*pr-d218*pr*y5*y9+d62*pr*y5*y1-d242*pr*y9*y1-   &
   d218*x5*x9*pr-d242*x9*x1*pr+d62*x5*x1*pr)*cf
 km(3,6)=d4/d945*(264*pr*x5*y9-d264*pr*y5*x9+d90*x1*y1+d97*y1*x5-         &
   d187*y1*x9-d43*y5*x9-d35*x1*y5-d55*x1*y9-d175*x5*y9+d78*y5*x5+         &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1+d230*y9*x9)*cf
 km(3,7)=-d32/d189*(2*pr*y1**2+d2*x1**2*pr-x5*x1+d6*y9*y1+d3*x9*x1-       &
   d2*y5*y1-d2*y1**2-x1**2-d2*y5**2+d6*y5*y9-d6*y9**2+d2*pr*y5**2+        &
   d6*pr*y9**2-x5**2+d3*x5*x9-d3*x9**2+d2*x5**2*pr+d6*x9**2*pr-           &
   d6*pr*y5*y9+d2*pr*y5*y1-d6*pr*y9*y1-d6*x5*x9*pr-d6*x9*x1*pr+           &
   d2*x5*x1*pr)*cf
 km(3,8)=-d16/d945*(32*pr*x5*y9-d32*pr*y5*x9+d10*x1*y1+d13*y1*x5-         &
   d23*y1*x9-d7*y5*x9-d3*x1*y5-d7*x1*y9-d23*x5*y9+d10*y5*x5+d32*x9*y1*pr- &
   d32*x5*y1*pr+d32*pr*y5*x1-d32*pr*y9*x1+d30*y9*x9)*cf
 km(3,9)=d4/d945*(10*pr*y1**2+d10*x1**2*pr-d24*x5*x1+d68*y9*y1+           &
   d34*x9*x1-d48*y5*y1-d10*y1**2-d5*x1**2+d48*y5*y9-d58*y9**2+            &
   d58*pr*y9**2+d24*x5*x9-d29*x9**2+d58*x9**2*pr-d48*pr*y5*y9+            &
   d48*pr*y5*y1-d68*pr*y9*y1-d48*x5*x9*pr-d68*x9*x1*pr+d48*x5*x1*pr)*cf
 km(3,10)=d4/d945*(48*pr*x5*y9-d48*pr*y5*x9+d5*x1*y1+d24*y1*x5-           &
   d29*y1*x9-d5*x1*y9-d24*x5*y9+d48*x9*y1*pr-d48*x5*y1*pr+d48*pr*y5*x1-   &
   d48*pr*y9*x1+d29*y9*x9)*cf
 km(3,11)=-d32/d945*(-d3*x5*x1+d6*y9*y1+d3*x9*x1-d6*y5*y1+d10*y5**2-      &
   d14*y5*y9+d4*y9**2-d10*pr*y5**2-d4*pr*y9**2+d5*x5**2-d7*x5*x9+         &
   d2*x9**2-d10*x5**2*pr-d4*x9**2*pr+d14*pr*y5*y9+d6*pr*y5*y1-            &
   d6*pr*y9*y1+d14*x5*x9*pr-d6*x9*x1*pr+d6*x5*x1*pr)*cf
 km(3,12)=-d16/d945*(3*y1*x5-d3*y1*x9+d7*y5*x9+d3*x1*y5-d3*x1*y9+         &
   d7*x5*y9-d10*y5*x5-d4*y9*x9)*cf
 km(3,13)=-d8/d945*(-d5*x5*x1+d10*y9*y1+d5*x9*x1-d10*y5*y1+d18*y5**2-     &
   d26*y5*y9+d8*y9**2-d18*pr*y5**2-d8*pr*y9**2+d9*x5**2-d13*x5*x9+        &
   d4*x9**2-d18*x5**2*pr-d8*x9**2*pr+d26*pr*y5*y9+d10*pr*y5*y1-           &
   d10*pr*y9*y1+d26*x5*x9*pr-d10*x9*x1*pr+d10*x5*x1*pr)*cf
 km(3,14)=-d4/d945*(5*y1*x5-d5*y1*x9+d13*y5*x9+d5*x1*y5-d5*x1*y9+         &
   d13*x5*y9-d18*y5*x5-d8*y9*x9)*cf
 km(3,15)=-d32/d315*(-d2*y5*y1+d2*y5**2+d2*y9*y1-d2*y5*y9+d2*pr*y5*y1-    &
   d2*pr*y5**2-d2*pr*y9*y1+d2*pr*y5*y9-x5*x1+d2*x5*x1*pr+x5**2-           &
   d2*x5**2*pr+x9*x1-d2*x9*x1*pr-x5*x9+d2*x5*x9*pr)*cf
 km(3,16)=-d16/d315*(y1*x5-y1*x9+y5*x9+x1*y5-x1*y9+x5*y9-d2*y5*x5)*cf
 km(3,17)=-d4/d189*(2*pr*y1**2+d2*x1**2*pr+d2*x5*x1+d4*y5*y1-             &
   d2*y1**2-x1**2-d2*y5**2+d2*pr*y5**2-x5**2+d2*x5**2*pr-d4*pr*y5*y1-     &
   d4*x5*x1*pr)*cf
 km(3,18)=-d4/d189*(x1*y1-y1*x5-x1*y5+y5*x5)*cf
 km(3,19)=d32/d945*(10*pr*y1**2+d10*x1**2*pr+d7*x5*x1+d6*y9*y1+           &
   d3*x9*x1+d14*y5*y1-d10*y1**2-d5*x1**2-d4*y5**2-d6*y5*y9+d4*pr*y5**2-   &
   d2*x5**2-d3*x5*x9+d4*x5**2*pr+d6*pr*y5*y9-d14*pr*y5*y1-d6*pr*y9*y1+    &
   d6*x5*x9*pr-d6*x9*x1*pr-d14*x5*x1*pr)*cf
 km(3,20)=d16/d945*(10*x1*y1-d7*y1*x5-d3*y1*x9+d3*y5*x9-d7*x1*y5-         &
   d3*x1*y9+d3*x5*y9+d4*y5*x5)*cf
 km(3,21)=-d8/d945*(90*pr*y1**2+d90*x1**2*pr+d55*x5*x1+d70*y9*y1+         &
   d35*x9*x1+d110*y5*y1-d90*y1**2-d45*x1**2-d12*y5**2-d86*y5*y9+d8*y9**2+ &
   d12*pr*y5**2-d8*pr*y9**2-d6*x5**2-d43*x5*x9+d4*x9**2+d12*x5**2*pr-     &
   d8*x9**2*pr+d86*pr*y5*y9-d110*pr*y5*y1-d70*pr*y9*y1+d86*x5*x9*pr-      &
   d70*x9*x1*pr-d110*x5*x1*pr)*cf
 km(3,22)=-d4/d945*(90*x1*y1-d55*y1*x5-d35*y1*x9+d43*y5*x9-d55*x1*y5-     &
   d35*x1*y9+d43*x5*y9+d12*y5*x5-d8*y9*x9)*cf
 km(3,23)=d32/d945*(62*pr*y1**2+d62*x1**2*pr+d31*x5*x1+d62*y9*y1+         &
   d31*x9*x1+d62*y5*y1-d62*y1**2-d31*x1**2-d4*y5**2-d54*y5*y9-d4*y9**2+   &
   d4*pr*y5**2+d4*pr*y9**2-d2*x5**2-d27*x5*x9-d2*x9**2+d4*x5**2*pr+       &
   d4*x9**2*pr+d54*pr*y5*y9-d62*pr*y5*y1-d62*pr*y9*y1+d54*x5*x9*pr-       &
   d62*x9*x1*pr-d62*x5*x1*pr)*cf
 km(3,24)=d16/d945*(62*x1*y1-d31*y1*x5-d31*y1*x9+d27*y5*x9-d31*x1*y5-     &
   d31*x1*y9+d27*x5*y9+d4*y5*x5+d4*y9*x9)*cf
 km(3,25)=-d64/d945*(-d19*x5*x1+d38*y9*y1+d19*x9*x1-d38*y5*y1+            &
   d36*y5**2-d34*y5*y9-d2*y9**2-d36*pr*y5**2+d2*pr*y9**2+d18*x5**2-       &
   d17*x5*x9-x9**2-d36*x5**2*pr+d2*x9**2*pr+d34*pr*y5*y9+d38*pr*y5*y1-    &
   d38*pr*y9*y1+d34*x5*x9*pr-d38*x9*x1*pr+d38*x5*x1*pr)*cf
 km(3,26)=-d32/d945*(19*y1*x5-d19*y1*x9+d17*y5*x9+d19*x1*y5-d19*x1*y9+    &
   d17*x5*y9-d36*y5*x5+d2*y9*x9)*cf
 km(3,27)=d64/d945*(-d5*x5*x1+d10*y9*y1+d5*x9*x1-d10*y5*y1+d12*y5**2-     &
   d14*y5*y9+d2*y9**2-d12*pr*y5**2-d2*pr*y9**2+d6*x5**2-d7*x5*x9+x9**2-   &
   d12*x5**2*pr-d2*x9**2*pr+d14*pr*y5*y9+d10*pr*y5*y1-d10*pr*y9*y1+       &
   d14*x5*x9*pr-d10*x9*x1*pr+d10*x5*x1*pr)*cf
 km(3,28)=d32/d945*(5*y1*x5-d5*y1*x9+d7*y5*x9+d5*x1*y5-d5*x1*y9+          &
   d7*x5*y9-d12*y5*x5-d2*y9*x9)*cf
 km(3,29)=km(3,27)
 km(3,30)=km(3,28)
!--------------------------------------------------------------------------
 km(4,4)=-d32/d945*(62*pr*y1**2+d62*x1**2*pr+d62*x5*x1+d31*y9*y1+         &
   d62*x9*x1+d31*y5*y1-d31*y1**2-d62*x1**2-d27*y5**2+d23*y5*y9-d27*y9**2+ &
   d54*pr*y5**2+d54*pr*y9**2-d54*x5**2+d46*x5*x9-d54*x9**2+d54*x5**2*pr+  &
   d54*x9**2*pr-d46*pr*y5*y9-d62*pr*y5*y1-d62*pr*y9*y1-d46*x5*x9*pr-      &
   d62*x9*x1*pr-d62*x5*x1*pr)*cf
 km(4,5)=-d4/d945*(264*pr*x5*y9-d264*pr*y5*x9-d90*x1*y1+d35*y1*x5+        &
   d55*y1*x9+d175*y5*x9-d97*x1*y5+d187*x1*y9+d43*x5*y9-d78*y5*x5+         &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1-d230*y9*x9)*cf
 km(4,6)=d8/d945*(90*pr*y1**2+d90*x1**2*pr-d62*x5*x1+d121*y9*y1+          &
   d242*x9*x1-d31*y5*y1-d45*y1**2-d90*x1**2-d39*y5**2+d109*y5*y9-         &
   d115*y9**2+d78*pr*y5**2+d230*pr*y9**2-d78*x5**2+d218*x5*x9-d230*x9**2+ &
   d78*x5**2*pr+d230*x9**2*pr-d218*pr*y5*y9+d62*pr*y5*y1-d242*pr*y9*y1-   &
   d218*x5*x9*pr-d242*x9*x1*pr+d62*x5*x1*pr)*cf
 km(4,7)=d16/d945*(32*pr*x5*y9-d32*pr*y5*x9-d10*x1*y1+d3*y1*x5+           &
   d7*y1*x9+d23*y5*x9-d13*x1*y5+d23*x1*y9+d7*x5*y9-d10*y5*x5+             &
   d32*x9*y1*pr-d32*x5*y1*pr+d32*pr*y5*x1-d32*pr*y9*x1-d30*y9*x9)*cf
 km(4,8)=-d32/d189*(2*pr*y1**2+d2*x1**2*pr-d2*x5*x1+d3*y9*y1+             &
   d6*x9*x1-y5*y1-y1**2-d2*x1**2-y5**2+d3*y5*y9-d3*y9**2+d2*pr*y5**2+     &
   d6*pr*y9**2-d2*x5**2+d6*x5*x9-d6*x9**2+d2*x5**2*pr+d6*x9**2*pr-        &
   d6*pr*y5*y9+d2*pr*y5*y1-d6*pr*y9*y1-d6*x5*x9*pr-d6*x9*x1*pr+           &
   d2*x5*x1*pr)*cf
 km(4,9)=-d4/d945*(48*pr*x5*y9-d48*pr*y5*x9-d5*x1*y1+d5*y1*x9+            &
   d24*y5*x9-d24*x1*y5+d29*x1*y9+d48*x9*y1*pr-d48*x5*y1*pr+d48*pr*y5*x1-  &
   d48*pr*y9*x1-d29*y9*x9)*cf
 km(4,10)=d4/d945*(10*pr*y1**2+d10*x1**2*pr-d48*x5*x1+d34*y9*y1+          &
   d68*x9*x1-d24*y5*y1-d5*y1**2-d10*x1**2+d24*y5*y9-d29*y9**2+            &
   d58*pr*y9**2+d48*x5*x9-d58*x9**2+d58*x9**2*pr-d48*pr*y5*y9+            &
   d48*pr*y5*y1-d68*pr*y9*y1-d48*x5*x9*pr-d68*x9*x1*pr+d48*x5*x1*pr)*cf
 km(4,11)=km(3,12)
 km(4,12)=-d32/d945*(-d6*x5*x1+d3*y9*y1+d6*x9*x1-d3*y5*y1+d5*y5**2-       &
   d7*y5*y9+d2*y9**2-d10*pr*y5**2-d4*pr*y9**2+d10*x5**2-d14*x5*x9+        &
   d4*x9**2-d10*x5**2*pr-d4*x9**2*pr+d14*pr*y5*y9+d6*pr*y5*y1-            &
   d6*pr*y9*y1+d14*x5*x9*pr-d6*x9*x1*pr+d6*x5*x1*pr)*cf
 km(4,13)=km(3,14)
 km(4,14)=-d8/d945*(-d10*x5*x1+d5*y9*y1+d10*x9*x1-d5*y5*y1+d9*y5**2-      &
   d13*y5*y9+d4*y9**2-d18*pr*y5**2-d8*pr*y9**2+d18*x5**2-d26*x5*x9+       &
   d8*x9**2-d18*x5**2*pr-d8*x9**2*pr+d26*pr*y5*y9+d10*pr*y5*y1-           &
   d10*pr*y9*y1+d26*x5*x9*pr-d10*x9*x1*pr+d10*x5*x1*pr)*cf
 km(4,15)=km(3,16)
 km(4,16)=-d32/d315*(-d2*x5*x1+y9*y1+d2*x9*x1-y5*y1+y5**2-y5*y9-          &
   d2*pr*y5**2+d2*x5**2-d2*x5*x9-d2*x5**2*pr+d2*pr*y5*y9+d2*pr*y5*y1-     &
   d2*pr*y9*y1+d2*x5*x9*pr-d2*x9*x1*pr+d2*x5*x1*pr)*cf
 km(4,17)=km(3,18)
 km(4,18)=-d4/d189*(2*pr*y1**2+d2*x1**2*pr+d4*x5*x1+d2*y5*y1-y1**2-       &
   d2*x1**2-y5**2+d2*pr*y5**2-d2*x5**2+d2*x5**2*pr-d4*pr*y5*y1-           &
   d4*x5*x1*pr)*cf
 km(4,19)=km(3,20)
 km(4,20)=d32/d945*(10*pr*y1**2+d10*x1**2*pr+d14*x5*x1+d3*y9*y1+          &
   d6*x9*x1+d7*y5*y1-d5*y1**2-d10*x1**2-d2*y5**2-d3*y5*y9+d4*pr*y5**2-    &
   d4*x5**2-d6*x5*x9+d4*x5**2*pr+d6*pr*y5*y9-d14*pr*y5*y1-d6*pr*y9*y1+    &
   d6*x5*x9*pr-d6*x9*x1*pr-d14*x5*x1*pr)*cf
 km(4,21)=km(3,22)
 km(4,22)=-d8/d945*(90*pr*y1**2+d90*x1**2*pr+d110*x5*x1+d35*y9*y1+        &
   d70*x9*x1+d55*y5*y1-d45*y1**2-d90*x1**2-d6*y5**2-d43*y5*y9+d4*y9**2+   &
   d12*pr*y5**2-d8*pr*y9**2-d12*x5**2-d86*x5*x9+d8*x9**2+d12*x5**2*pr-    &
   d8*x9**2*pr+d86*pr*y5*y9-d110*pr*y5*y1-d70*pr*y9*y1+d86*x5*x9*pr-      &
   d70*x9*x1*pr-d110*x5*x1*pr)*cf
 km(4,23)=km(3,24)
 km(4,24)=d32/d945*(62*pr*y1**2+d62*x1**2*pr+d62*x5*x1+d31*y9*y1+         &
   d62*x9*x1+d31*y5*y1-d31*y1**2-d62*x1**2-d2*y5**2-d27*y5*y9-d2*y9**2+   &
   d4*pr*y5**2+d4*pr*y9**2-d4*x5**2-d54*x5*x9-d4*x9**2+d4*x5**2*pr+       &
   d4*x9**2*pr+d54*pr*y5*y9-d62*pr*y5*y1-d62*pr*y9*y1+d54*x5*x9*pr-       &
   d62*x9*x1*pr-d62*x5*x1*pr)*cf
 km(4,25)=km(3,26)
 km(4,26)=-d64/d945*(-d38*x5*x1+d19*y9*y1+d38*x9*x1-d19*y5*y1+            &
   d18*y5**2-d17*y5*y9-y9**2-d36*pr*y5**2+d2*pr*y9**2+d36*x5**2-          &
   d34*x5*x9-d2*x9**2-d36*x5**2*pr+d2*x9**2*pr+d34*pr*y5*y9+d38*pr*y5*y1- &
   d38*pr*y9*y1+d34*x5*x9*pr-d38*x9*x1*pr+d38*x5*x1*pr)*cf
 km(4,27)=km(3,28)
 km(4,28)=d64/d945*(-d10*x5*x1+d5*y9*y1+d10*x9*x1-d5*y5*y1+d6*y5**2-      &
   d7*y5*y9+y9**2-d12*pr*y5**2-d2*pr*y9**2+d12*x5**2-d14*x5*x9+d2*x9**2-  &
   d12*x5**2*pr-d2*x9**2*pr+d14*pr*y5*y9+d10*pr*y5*y1-d10*pr*y9*y1+       &
   d14*x5*x9*pr-d10*x9*x1*pr+d10*x5*x1*pr)*cf
 km(4,29)=km(3,28)
 km(4,30)=km(4,28)
!--------------------------------------------------------------------------
 km(5,5)=-d2/d315*(386*pr*y1**2+d386*x1**2*pr+d153*x5*x1+d466*y9*y1+      &
   d233*x9*x1+d306*y5*y1-d386*y1**2-d193*x1**2-d386*y5**2+d466*y5*y9-     &
   d466*y9**2+d386*pr*y5**2+d466*pr*y9**2-d193*x5**2+d233*x5*x9-          &
   d233*x9**2+d386*x5**2*pr+d466*x9**2*pr-d466*pr*y5*y9-d306*pr*y5*y1-    &
   d466*pr*y9*y1-d466*x5*x9*pr-d466*x9*x1*pr-d306*x5*x1*pr)*cf
 km(5,6)=-(386*x1*y1-d153*y1*x5-d233*y1*x9-d233*y5*x9-d153*x1*y5-         &
   d233*x1*y9-d233*x5*y9+d386*y5*x5+d466*y9*x9)/d315*cf
 km(5,7)=d8/d945*(78*pr*y1**2+d78*x1**2*pr-d31*x5*x1+d218*y9*y1+          &
   d109*x9*x1-d62*y5*y1-d78*y1**2-d39*x1**2-d90*y5**2+d242*y5*y9-         &
   d230*y9**2+d90*pr*y5**2+d230*pr*y9**2-d45*x5**2+d121*x5*x9-d115*x9**2+ &
   d90*x5**2*pr+d230*x9**2*pr-d242*pr*y5*y9+d62*pr*y5*y1-d218*pr*y9*y1-   &
   d242*x5*x9*pr-d218*x9*x1*pr+d62*x5*x1*pr)*cf
 km(5,8)=d4/d945*(264*pr*x5*y9-d264*pr*y5*x9+d78*x1*y1+d97*y1*x5-         &
   d175*y1*x9-d55*y5*x9-d35*x1*y5-d43*x1*y9-d187*x5*y9+d90*y5*x5+         &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1+d230*y9*x9)*cf
 km(5,9)=-(40*pr*y1**2+d40*x1**2*pr-d201*x5*x1+d482*y9*y1+d241*x9*x1-     &
   d402*y5*y1-d40*y1**2-d20*x1**2+d402*y5*y9-d442*y9**2+d442*pr*y9**2+    &
   d201*x5*x9-d221*x9**2+d442*x9**2*pr-d402*pr*y5*y9+d402*pr*y5*y1-       &
   d482*pr*y9*y1-d402*x5*x9*pr-d482*x9*x1*pr+d402*x5*x1*pr)/d945*cf
 km(5,10)=-(402*pr*x5*y9-d402*pr*y5*x9+d20*x1*y1+d201*y1*x5-              &
   d221*y1*x9-d20*x1*y9-d201*x5*y9+d402*x9*y1*pr-d402*x5*y1*pr+           &
   d402*pr*y5*x1-d402*pr*y9*x1+d221*y9*x9)/d945*cf
 km(5,11)=d8/d945*(8*pr*y1**2+d8*x1**2*pr-d35*x5*x1+d86*y9*y1+            &
   d43*x9*x1-d70*y5*y1-d8*y1**2-d4*x1**2+d90*y5**2-d110*y5*y9+d12*y9**2-  &
   d90*pr*y5**2-d12*pr*y9**2+d45*x5**2-d55*x5*x9+d6*x9**2-d90*x5**2*pr-   &
   d12*x9**2*pr+d110*pr*y5*y9+d70*pr*y5*y1-d86*pr*y9*y1+d110*x5*x9*pr-    &
   d86*x9*x1*pr+d70*x5*x1*pr)*cf
 km(5,12)=d4/d945*(8*x1*y1+d35*y1*x5-d43*y1*x9+d55*y5*x9+d35*x1*y5-       &
   d43*x1*y9+d55*x5*y9-d90*y5*x5-d12*y9*x9)*cf
 km(5,13)=-d2/d315*(8*pr*y1**2+d8*x1**2*pr+d31*x5*x1-d46*y9*y1-           &
 d23*x9*x1+d62*y5*y1-d8*y1**2-d4*x1**2-d62*y5**2+d62*y5*y9-d8*y9**2+      &
 d62*pr*y5**2+d8*pr*y9**2-d31*x5**2+d31*x5*x9-d4*x9**2+d62*x5**2*pr+      &
 d8*x9**2*pr-d62*pr*y5*y9-d62*pr*y5*y1+d46*pr*y9*y1-d62*x5*x9*pr+         &
 d46*x9*x1*pr-d62*x5*x1*pr)*cf
 km(5,14)=-(8*x1*y1-d31*y1*x5+d23*y1*x9-d31*y5*x9-d31*x1*y5+d23*x1*y9-    &
   d31*x5*y9+d62*y5*x5+d8*y9*x9)/d315*cf
 km(5,15)=d8/d945*(8*pr*y1**2+d8*x1**2*pr+d13*x5*x1-d10*y9*y1-d5*x9*x1+   &
   d26*y5*y1-d8*y1**2-d4*x1**2-d18*y5**2+d10*y5*y9+d18*pr*y5**2-d9*x5**2+ &
   d5*x5*x9+d18*x5**2*pr-d10*pr*y5*y9-d26*pr*y5*y1+d10*pr*y9*y1-          &
   d10*x5*x9*pr+d10*x9*x1*pr-d26*x5*x1*pr)*cf
 km(5,16)=d4/d945*(8*x1*y1-d13*y1*x5+d5*y1*x9-d5*y5*x9-d13*x1*y5+         &
   d5*x1*y9-d5*x5*y9+d18*y5*x5)*cf
 km(5,17)=km(3,17)
 km(5,18)=km(3,18)
 km(5,19)=d8/d945*(18*pr*y1**2+d18*x1**2*pr+d13*x5*x1+d10*y9*y1+          &
   d5*x9*x1+d26*y5*y1-d18*y1**2-d9*x1**2-d8*y5**2-d10*y5*y9+d8*pr*y5**2-  &
   d4*x5**2-d5*x5*x9+d8*x5**2*pr+d10*pr*y5*y9-d26*pr*y5*y1-d10*pr*y9*y1+  &
   d10*x5*x9*pr-d10*x9*x1*pr-d26*x5*x1*pr)*cf
 km(5,20)=d4/d945*(18*x1*y1-d13*y1*x5-d5*y1*x9+d5*y5*x9-d13*x1*y5-        &
   d5*x1*y9+d5*x5*y9+d8*y5*x5)*cf
 km(5,21)=-d2/d315*(62*pr*y1**2+d62*x1**2*pr+d31*x5*x1+d62*y9*y1+         &
   d31*x9*x1+d62*y5*y1-d62*y1**2-d31*x1**2-d8*y5**2-d46*y5*y9-d8*y9**2+   &
   d8*pr*y5**2+d8*pr*y9**2-d4*x5**2-d23*x5*x9-d4*x9**2+d8*x5**2*pr+       &
   d8*x9**2*pr+d46*pr*y5*y9-d62*pr*y5*y1-d62*pr*y9*y1+d46*x5*x9*pr-       &
   d62*x9*x1*pr-d62*x5*x1*pr)*cf
 km(5,22)=-(62*x1*y1-d31*y1*x5-d31*y1*x9+d23*y5*x9-d31*x1*y5-d31*x1*y9+   &
   d23*x5*y9+d8*y5*x5+d8*y9*x9)/d315*cf
 km(5,23)=-d8/d945*(90*pr*y1**2+d90*x1**2*pr+d35*x5*x1+d110*y9*y1+        &
   d55*x9*x1+d70*y5*y1-d90*y1**2-d45*x1**2+d8*y5**2-d86*y5*y9-d12*y9**2-  &
   d8*pr*y5**2+d12*pr*y9**2+d4*x5**2-d43*x5*x9-d6*x9**2-d8*x5**2*pr+      &
   d12*x9**2*pr+d86*pr*y5*y9-d70*pr*y5*y1-d110*pr*y9*y1+d86*x5*x9*pr-     &
   d110*x9*x1*pr-d70*x5*x1*pr)*cf
 km(5,24)=-d4/d945*(90*x1*y1-d35*y1*x5-d55*y1*x9+d43*y5*x9-d35*x1*y5-     &
   d55*x1*y9+d43*x5*y9-d8*y5*x5+d12*y9*x9)*cf
 km(5,25)=d16/d315*(56*pr*y1**2+d56*x1**2*pr+d23*x5*x1+d66*y9*y1+         &
   d33*x9*x1+d46*y5*y1-d56*y1**2-d28*x1**2+d8*y5**2-d62*y5*y9-d2*y9**2-   &
   d8*pr*y5**2+d2*pr*y9**2+d4*x5**2-d31*x5*x9-x9**2-d8*x5**2*pr+          &
   d2*x9**2*pr+d62*pr*y5*y9-d46*pr*y5*y1-d66*pr*y9*y1+d62*x5*x9*pr-       &
   d66*x9*x1*pr-d46*x5*x1*pr)*cf
 km(5,26)=d8/d315*(56*x1*y1-d23*y1*x5-d33*y1*x9+d31*y5*x9-d23*x1*y5-      &
   d33*x1*y9+d31*x5*y9-d8*y5*x5+d2*y9*x9)*cf
 km(5,27)=-d16/d315*(8*pr*y1**2+d8*x1**2*pr-d23*x5*x1+d62*y9*y1+          &
   d31*x9*x1-d46*y5*y1-d8*y1**2-d4*x1**2+d56*y5**2-d66*y5*y9+d2*y9**2-    &
   d56*pr*y5**2-d2*pr*y9**2+d28*x5**2-d33*x5*x9+x9**2-d56*x5**2*pr-       &
   d2*x9**2*pr+d66*pr*y5*y9+d46*pr*y5*y1-d62*pr*y9*y1+d66*x5*x9*pr-       &
   d62*x9*x1*pr+d46*x5*x1*pr)*cf
 km(5,28)=-d8/d315*(8*x1*y1+d23*y1*x5-d31*y1*x9+d33*y5*x9+d23*x1*y5-      &
   d31*x1*y9+d33*x5*y9-d56*y5*x5-d2*y9*x9)*cf
 km(5,29)=-d16/d315*(8*pr*y1**2+d8*x1**2*pr+d9*x5*x1-d2*y9*y1-x9*x1+      &
   d18*y5*y1-d8*y1**2-d4*x1**2-d8*y5**2-d2*y5*y9+d2*y9**2+d8*pr*y5**2-    &
   d2*pr*y9**2-d4*x5**2-x5*x9+x9**2+d8*x5**2*pr-d2*x9**2*pr+d2*pr*y5*y9-  &
   d18*pr*y5*y1+d2*pr*y9*y1+d2*x5*x9*pr+d2*x9*x1*pr-d18*x5*x1*pr)*cf
 km(5,30)=-d8/d315*(8*x1*y1-d9*y1*x5+y1*x9+y5*x9-d9*x1*y5+x1*y9+x5*y9+    &
   d8*y5*x5-d2*y9*x9)*cf
!--------------------------------------------------------------------------
 km(6,6)=-d2/d315*(386*pr*y1**2+d386*x1**2*pr+d306*x5*x1+d233*y9*y1+      &
   d466*x9*x1+d153*y5*y1-d193*y1**2-d386*x1**2-d193*y5**2+d233*y5*y9-     &
   d233*y9**2+d386*pr*y5**2+d466*pr*y9**2-d386*x5**2+d466*x5*x9-          &
   d466*x9**2+d386*x5**2*pr+d466*x9**2*pr-d466*pr*y5*y9-d306*pr*y5*y1-    &
   d466*pr*y9*y1-d466*x5*x9*pr-d466*x9*x1*pr-d306*x5*x1*pr)*cf
 km(6,7)=-d4/d945*(264*pr*x5*y9-d264*pr*y5*x9-d78*x1*y1+d35*y1*x5+        &
   d43*y1*x9+d187*y5*x9-d97*x1*y5+d175*x1*y9+d55*x5*y9-d90*y5*x5+         &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1-d230*y9*x9)*cf
 km(6,8)=d8/d945*(78*pr*y1**2+d78*x1**2*pr-d62*x5*x1+d109*y9*y1+          &
   d218*x9*x1-d31*y5*y1-d39*y1**2-d78*x1**2-d45*y5**2+d121*y5*y9-         &
   d115*y9**2+d90*pr*y5**2+d230*pr*y9**2-d90*x5**2+d242*x5*x9-d230*x9**2+ &
   d90*x5**2*pr+d230*x9**2*pr-d242*pr*y5*y9+d62*pr*y5*y1-d218*pr*y9*y1-   &
   d242*x5*x9*pr-d218*x9*x1*pr+d62*x5*x1*pr)*cf
 km(6,9)=(402*pr*x5*y9-d402*pr*y5*x9-d20*x1*y1+d20*y1*x9+d201*y5*x9-      &
   d201*x1*y5+d221*x1*y9+d402*x9*y1*pr-d402*x5*y1*pr+d402*pr*y5*x1-       &
   d402*pr*y9*x1-d221*y9*x9)/d945*cf
 km(6,10)=-(40*pr*y1**2+d40*x1**2*pr-d402*x5*x1+d241*y9*y1+d482*x9*x1-    &
   d201*y5*y1-d20*y1**2-d40*x1**2+d201*y5*y9-d221*y9**2+d442*pr*y9**2+    &
   d402*x5*x9-d442*x9**2+d442*x9**2*pr-d402*pr*y5*y9+d402*pr*y5*y1-       &
   d482*pr*y9*y1-d402*x5*x9*pr-d482*x9*x1*pr+d402*x5*x1*pr)/d945*cf
 km(6,11)=km(5,12)
 km(6,12)=d8/d945*(8*pr*y1**2+d8*x1**2*pr-d70*x5*x1+d43*y9*y1+            &
   d86*x9*x1-d35*y5*y1-d4*y1**2-d8*x1**2+d45*y5**2-d55*y5*y9+d6*y9**2-    &
   d90*pr*y5**2-d12*pr*y9**2+d90*x5**2-d110*x5*x9+d12*x9**2-d90*x5**2*pr- &
   d12*x9**2*pr+d110*pr*y5*y9+d70*pr*y5*y1-d86*pr*y9*y1+d110*x5*x9*pr-    &
   d86*x9*x1*pr+d70*x5*x1*pr)*cf
 km(6,13)=km(5,14)
 km(6,14)=-d2/d315*(8*pr*y1**2+d8*x1**2*pr+d62*x5*x1-d23*y9*y1-           &
   d46*x9*x1+d31*y5*y1-d4*y1**2-d8*x1**2-d31*y5**2+d31*y5*y9-d4*y9**2+    &
   d62*pr*y5**2+d8*pr*y9**2-d62*x5**2+d62*x5*x9-d8*x9**2+d62*x5**2*pr+    &
   d8*x9**2*pr-d62*pr*y5*y9-d62*pr*y5*y1+d46*pr*y9*y1-d62*x5*x9*pr+       &
   d46*x9*x1*pr-d62*x5*x1*pr)*cf
 km(6,15)=km(5,16)
 km(6,16)=d8/d945*(8*pr*y1**2+d8*x1**2*pr+d26*x5*x1-d5*y9*y1-d10*x9*x1+   &
   d13*y5*y1-d4*y1**2-d8*x1**2-d9*y5**2+d5*y5*y9+d18*pr*y5**2-d18*x5**2+  &
   d10*x5*x9+d18*x5**2*pr-d10*pr*y5*y9-d26*pr*y5*y1+d10*pr*y9*y1-         &
   d10*x5*x9*pr+d10*x9*x1*pr-d26*x5*x1*pr)*cf
 km(6,17)=km(3,18)
 km(6,18)=km(4,18)
 km(6,19)=km(5,20)
 km(6,20)=d8/d945*(18*pr*y1**2+d18*x1**2*pr+d26*x5*x1+d5*y9*y1+           &
   d10*x9*x1+d13*y5*y1-d9*y1**2-d18*x1**2-d4*y5**2-d5*y5*y9+d8*pr*y5**2-  &
   d8*x5**2-d10*x5*x9+d8*x5**2*pr+d10*pr*y5*y9-d26*pr*y5*y1-d10*pr*y9*y1+ &
   d10*x5*x9*pr-d10*x9*x1*pr-d26*x5*x1*pr)*cf
 km(6,21)=km(5,22)
 km(6,22)=-d2/d315*(62*pr*y1**2+d62*x1**2*pr+d62*x5*x1+d31*y9*y1+         &
   d62*x9*x1+d31*y5*y1-d31*y1**2-d62*x1**2-d4*y5**2-d23*y5*y9-d4*y9**2+   &
   d8*pr*y5**2+d8*pr*y9**2-d8*x5**2-d46*x5*x9-d8*x9**2+d8*x5**2*pr+       &
   d8*x9**2*pr+d46*pr*y5*y9-d62*pr*y5*y1-d62*pr*y9*y1+d46*x5*x9*pr-       &
   d62*x9*x1*pr-d62*x5*x1*pr)*cf
 km(6,23)=km(5,24)
 km(6,24)=-d8/d945*(90*pr*y1**2+d90*x1**2*pr+d70*x5*x1+d55*y9*y1+         &
   d110*x9*x1+d35*y5*y1-d45*y1**2-d90*x1**2+d4*y5**2-d43*y5*y9-d6*y9**2-  &
   d8*pr*y5**2+d12*pr*y9**2+d8*x5**2-d86*x5*x9-d12*x9**2-d8*x5**2*pr+     &
   d12*x9**2*pr+d86*pr*y5*y9-d70*pr*y5*y1-d110*pr*y9*y1+d86*x5*x9*pr-     &
   d110*x9*x1*pr-d70*x5*x1*pr)*cf
 km(6,25)=km(5,26)
 km(6,26)=d16/d315*(56*pr*y1**2+d56*x1**2*pr+d46*x5*x1+d33*y9*y1+         &
   d66*x9*x1+d23*y5*y1-d28*y1**2-d56*x1**2+d4*y5**2-d31*y5*y9-y9**2-      &
   d8*pr*y5**2+d2*pr*y9**2+d8*x5**2-d62*x5*x9-d2*x9**2-d8*x5**2*pr+       &
   d2*x9**2*pr+d62*pr*y5*y9-d46*pr*y5*y1-d66*pr*y9*y1+d62*x5*x9*pr-       &
   d66*x9*x1*pr-d46*x5*x1*pr)*cf
 km(6,27)=km(5,28)
 km(6,28)=-d16/d315*(8*pr*y1**2+d8*x1**2*pr-d46*x5*x1+d31*y9*y1+          &
   d62*x9*x1-d23*y5*y1-d4*y1**2-d8*x1**2+d28*y5**2-d33*y5*y9+y9**2-       &
   d56*pr*y5**2-d2*pr*y9**2+d56*x5**2-d66*x5*x9+d2*x9**2-d56*x5**2*pr-    &
   d2*x9**2*pr+d66*pr*y5*y9+d46*pr*y5*y1-d62*pr*y9*y1+d66*x5*x9*pr-       &
   d62*x9*x1*pr+d46*x5*x1*pr)*cf
 km(6,29)=km(5,30)
 km(6,30)=-d16/d315*(8*pr*y1**2+d8*x1**2*pr+d18*x5*x1-y9*y1-d2*x9*x1+     &
   d9*y5*y1-d4*y1**2-d8*x1**2-d4*y5**2-y5*y9+y9**2+d8*pr*y5**2-           &
   d2*pr*y9**2-d8*x5**2-d2*x5*x9+d2*x9**2+d8*x5**2*pr-d2*x9**2*pr+        &
   d2*pr*y5*y9-d18*pr*y5*y1+d2*pr*y9*y1+d2*x5*x9*pr+d2*x9*x1*pr-          &
   d18*x5*x1*pr)*cf
!--------------------------------------------------------------------------
 km(7,7)=-d32/d945*(54*pr*y1**2+d54*x1**2*pr+d31*x5*x1+d46*y9*y1+         &
   d23*x9*x1+d62*y5*y1-d54*y1**2-d27*x1**2-d62*y5**2+d62*y5*y9-d54*y9**2+ &
   d62*pr*y5**2+d54*pr*y9**2-d31*x5**2+d31*x5*x9-d27*x9**2+d62*x5**2*pr+  &
   d54*x9**2*pr-d62*pr*y5*y9-d62*pr*y5*y1-d46*pr*y9*y1-d62*x5*x9*pr-      &
   d46*x9*x1*pr-d62*x5*x1*pr)*cf
 km(7,8)=-d16/d945*(54*x1*y1-d31*y1*x5-d23*y1*x9-d31*y5*x9-d31*x1*y5-     &
   d23*x1*y9-d31*x5*y9+d62*y5*x5+d54*y9*x9)*cf
 km(7,9)=-d4/d945*(30*pr*y1**2+d30*x1**2*pr+d92*x5*x1-d124*y9*y1-         &
   d62*x9*x1+d184*y5*y1-d30*y1**2-d15*x1**2-d184*y5*y9+d154*y9**2-        &
   d154*pr*y9**2-d92*x5*x9+d77*x9**2-d154*x9**2*pr+d184*pr*y5*y9-         &
   d184*pr*y5*y1+d124*pr*y9*y1+d184*x5*x9*pr+d124*x9*x1*pr-               &
   d184*x5*x1*pr)*cf
 km(7,10)=d4/d945*(184*pr*x5*y9-d184*pr*y5*x9-d15*x1*y1+d92*y1*x5-        &
   d77*y1*x9+d15*x1*y9-d92*x5*y9+d184*x9*y1*pr-d184*x5*y1*pr+             &
   d184*pr*y5*x1-d184*pr*y9*x1+d77*y9*x9)*cf
 km(7,11)=d32/d945*(4*pr*y1**2+d4*x1**2*pr+d31*x5*x1-d54*y9*y1-           &
   d27*x9*x1+d62*y5*y1-d4*y1**2-d2*x1**2-d62*y5**2+d62*y5*y9-d4*y9**2+    &
   d62*pr*y5**2+d4*pr*y9**2-d31*x5**2+d31*x5*x9-d2*x9**2+d62*x5**2*pr+    &
   d4*x9**2*pr-d62*pr*y5*y9-d62*pr*y5*y1+d54*pr*y9*y1-d62*x5*x9*pr+       &
   d54*x9*x1*pr-d62*x5*x1*pr)*cf
 km(7,12)=d16/d945*(4*x1*y1-d31*y1*x5+d27*y1*x9-d31*y5*x9-d31*x1*y5+      &
   d27*x1*y9-d31*x5*y9+d62*y5*x5+d4*y9*x9)*cf
 km(7,13)=-d8/d945*(12*pr*y1**2+d12*x1**2*pr+d55*x5*x1-d86*y9*y1-         &
   d43*x9*x1+d110*y5*y1-d12*y1**2-d6*x1**2-d90*y5**2+d70*y5*y9+d8*y9**2+  &
   d90*pr*y5**2-d8*pr*y9**2-d45*x5**2+d35*x5*x9+d4*x9**2+d90*x5**2*pr-    &
   d8*x9**2*pr-d70*pr*y5*y9-d110*pr*y5*y1+d86*pr*y9*y1-d70*x5*x9*pr+      &
   d86*x9*x1*pr-d110*x5*x1*pr)*cf
 km(7,14)=-d4/d945*(12*x1*y1-d55*y1*x5+d43*y1*x9-d35*y5*x9-d55*x1*y5+     &
   d43*x1*y9-d35*x5*y9+d90*y5*x5-d8*y9*x9)*cf
 km(7,15)=d32/d945*(4*pr*y1**2+d4*x1**2*pr+d7*x5*x1-d6*y9*y1-d3*x9*x1+    &
   d14*y5*y1-d4*y1**2-d2*x1**2-d10*y5**2+d6*y5*y9+d10*pr*y5**2-d5*x5**2+  &
   d3*x5*x9+d10*x5**2*pr-d6*pr*y5*y9-d14*pr*y5*y1+d6*pr*y9*y1-            &
   d6*x5*x9*pr+d6*x9*x1*pr-d14*x5*x1*pr)*cf
 km(7,16)=d16/d945*(4*x1*y1-d7*y1*x5+d3*y1*x9-d3*y5*x9-d7*x1*y5+          &
   d3*x1*y9-d3*x5*y9+d10*y5*x5)*cf
 km(7,17)=km(3,17)
 km(7,18)=km(3,18)
 km(7,19)=d32/d315*(-d2*pr*y5*y1+x5*x1+d2*y9*y1+x9*x1-d2*x9*x1*pr+        &
   d2*pr*y1**2+d2*x1**2*pr+d2*x5*x9*pr-d2*y1**2+d2*y5*y1-d2*y5*y9-        &
   d2*x5*x1*pr-x5*x9+d2*pr*y5*y9-x1**2-d2*pr*y9*y1)*cf
 km(7,20)=d16/d315*(2*x1*y1-y1*x5-y1*x9+y5*x9-x1*y5-x1*y9+x5*y9)*cf
 km(7,21)=d8/d945*(18*pr*y1**2+d18*x1**2*pr+d5*x5*x1+d26*y9*y1+           &
   d13*x9*x1+d10*y5*y1-d18*y1**2-d9*x1**2-d10*y5*y9-d8*y9**2+d8*pr*y9**2- &
   d5*x5*x9-d4*x9**2+d8*x9**2*pr+d10*pr*y5*y9-d10*pr*y5*y1-d26*pr*y9*y1+  &
   d10*x5*x9*pr-d26*x9*x1*pr-d10*x5*x1*pr)*cf
 km(7,22)=d4/d945*(18*x1*y1-d5*y1*x5-d13*y1*x9+d5*y5*x9-d5*x1*y5-         &
   d13*x1*y9+d5*x5*y9+d8*y9*x9)*cf
 km(7,23)=d32/d945*(10*pr*y1**2+d10*x1**2*pr+d3*x5*x1+d14*y9*y1+          &
   d7*x9*x1+d6*y5*y1-d10*y1**2-d5*x1**2-d6*y5*y9-d4*y9**2+d4*pr*y9**2-    &
   d3*x5*x9-d2*x9**2+d4*x9**2*pr+d6*pr*y5*y9-d6*pr*y5*y1-d14*pr*y9*y1+    &
   d6*x5*x9*pr-d14*x9*x1*pr-d6*x5*x1*pr)*cf
 km(7,24)=d16/d945*(10*x1*y1-d3*y1*x5-d7*y1*x9+d3*y5*x9-d3*x1*y5-         &
   d7*x1*y9+d3*x5*y9+d4*y9*x9)*cf
 km(7,25)=-d64/d945*(12*pr*y1**2+d12*x1**2*pr+d5*x5*x1+d14*y9*y1+         &
   d7*x9*x1+d10*y5*y1-d12*y1**2-d6*x1**2-d10*y5*y9-d2*y9**2+d2*pr*y9**2-  &
   d5*x5*x9-x9**2+d2*x9**2*pr+d10*pr*y5*y9-d10*pr*y5*y1-d14*pr*y9*y1+     &
   d10*x5*x9*pr-d14*x9*x1*pr-d10*x5*x1*pr)*cf
 km(7,26)=-d32/d945*(12*x1*y1-d5*y1*x5-d7*y1*x9+d5*y5*x9-d5*x1*y5-        &
   d7*x1*y9+d5*x5*y9+d2*y9*x9)*cf
 km(7,27)=d64/d945*(36*pr*y1**2+d36*x1**2*pr+d19*x5*x1+d34*y9*y1+         &
   d17*x9*x1+d38*y5*y1-d36*y1**2-d18*x1**2-d38*y5*y9+d2*y9**2-            &
   d2*pr*y9**2-d19*x5*x9+x9**2-d2*x9**2*pr+d38*pr*y5*y9-d38*pr*y5*y1-     &
   d34*pr*y9*y1+d38*x5*x9*pr-d34*x9*x1*pr-d38*x5*x1*pr)*cf
 km(7,28)=d32/d945*(36*x1*y1-d19*y1*x5-d17*y1*x9+d19*y5*x9-d19*x1*y5-     &
   d17*x1*y9+d19*x5*y9-d2*y9*x9)*cf
 km(7,29)=km(7,25)
 km(7,30)=km(7,26)
!--------------------------------------------------------------------------
 km(8,8)=-d32/d945*(54*pr*y1**2+d54*x1**2*pr+d62*x5*x1+d23*y9*y1+         &
   d46*x9*x1+d31*y5*y1-d27*y1**2-d54*x1**2-d31*y5**2+d31*y5*y9-d27*y9**2+ &   
   d62*pr*y5**2+d54*pr*y9**2-d62*x5**2+d62*x5*x9-d54*x9**2+d62*x5**2*pr+  &
   d54*x9**2*pr-d62*pr*y5*y9-d62*pr*y5*y1-d46*pr*y9*y1-d62*x5*x9*pr-      &
   d46*x9*x1*pr-d62*x5*x1*pr)*cf
 km(8,9)=-d4/d945*(184*pr*x5*y9-d184*pr*y5*x9+d15*x1*y1-d15*y1*x9+        &
   d92*y5*x9-d92*x1*y5+d77*x1*y9+d184*x9*y1*pr-d184*x5*y1*pr+             &
   d184*pr*y5*x1-d184*pr*y9*x1-d77*y9*x9)*cf
 km(8,10)=-d4/d945*(30*pr*y1**2+d30*x1**2*pr+d184*x5*x1-d62*y9*y1-        &
   d124*x9*x1+d92*y5*y1-d15*y1**2-d30*x1**2-d92*y5*y9+d77*y9**2-          &
   d154*pr*y9**2-d184*x5*x9+d154*x9**2-d154*x9**2*pr+d184*pr*y5*y9-       &
   d184*pr*y5*y1+d124*pr*y9*y1+d184*x5*x9*pr+d124*x9*x1*pr-               &
   d184*x5*x1*pr)*cf
 km(8,11)=km(7,12)
 km(8,12)=d32/d945*(4*pr*y1**2+d4*x1**2*pr+d62*x5*x1-d27*y9*y1-           &
   d54*x9*x1+d31*y5*y1-d2*y1**2-d4*x1**2-d31*y5**2+d31*y5*y9-d2*y9**2+    &
   d62*pr*y5**2+d4*pr*y9**2-d62*x5**2+d62*x5*x9-d4*x9**2+d62*x5**2*pr+    &
   d4*x9**2*pr-d62*pr*y5*y9-d62*pr*y5*y1+d54*pr*y9*y1-d62*x5*x9*pr+       &
   d54*x9*x1*pr-d62*x5*x1*pr)*cf
 km(8,13)=km(7,14)
 km(8,14)=-d8/d945*(12*pr*y1**2+d12*x1**2*pr+d110*x5*x1-d43*y9*y1-        &
   d86*x9*x1+d55*y5*y1-d6*y1**2-d12*x1**2-d45*y5**2+d35*y5*y9+d4*y9**2+   &
   d90*pr*y5**2-d8*pr*y9**2-d90*x5**2+d70*x5*x9+d8*x9**2+d90*x5**2*pr-    &
   d8*x9**2*pr-d70*pr*y5*y9-d110*pr*y5*y1+d86*pr*y9*y1-d70*x5*x9*pr+      &
   d86*x9*x1*pr-d110*x5*x1*pr)*cf
 km(8,15)=km(7,16)
 km(8,16)=d32/d945*(4*pr*y1**2+d4*x1**2*pr+d14*x5*x1-d3*y9*y1-            &
   d6*x9*x1+d7*y5*y1-d2*y1**2-d4*x1**2-d5*y5**2+d3*y5*y9+d10*pr*y5**2-    &
   d10*x5**2+d6*x5*x9+d10*x5**2*pr-d6*pr*y5*y9-d14*pr*y5*y1+d6*pr*y9*y1-  &
   d6*x5*x9*pr+d6*x9*x1*pr-d14*x5*x1*pr)*cf
 km(8,17)=km(3,18)
 km(8,18)=km(4,18)
 km(8,19)=km(7,20)
 km(8,20)=d32/d315*(-d2*x5*x9+d2*pr*y5*y9-d2*x5*x1*pr+d2*pr*y1**2+        &
   d2*x1**2*pr-d2*pr*y9*y1+d2*x5*x1+y9*y1-d2*pr*y5*y1-d2*x9*x1*pr+y5*y1+  &
   d2*x5*x9*pr-d2*x1**2+d2*x9*x1-y5*y9-y1**2)*cf
 km(8,21)=km(7,22)
 km(8,22)=d8/d945*(18*pr*y1**2+d18*x1**2*pr+d10*x5*x1+d13*y9*y1+          &
   d26*x9*x1+d5*y5*y1-d9*y1**2-d18*x1**2-d5*y5*y9-d4*y9**2+d8*pr*y9**2-   &
   d10*x5*x9-d8*x9**2+d8*x9**2*pr+d10*pr*y5*y9-d10*pr*y5*y1-d26*pr*y9*y1+ &
   d10*x5*x9*pr-d26*x9*x1*pr-d10*x5*x1*pr)*cf
 km(8,23)=km(7,24)
 km(8,24)=d32/d945*(10*pr*y1**2+d10*x1**2*pr+d6*x5*x1+d7*y9*y1+           &
   d14*x9*x1+d3*y5*y1-d5*y1**2-d10*x1**2-d3*y5*y9-d2*y9**2+d4*pr*y9**2-   &
   d6*x5*x9-d4*x9**2+d4*x9**2*pr+d6*pr*y5*y9-d6*pr*y5*y1-d14*pr*y9*y1+    &
   d6*x5*x9*pr-d14*x9*x1*pr-d6*x5*x1*pr)*cf
 km(8,25)=km(7,26)
 km(8,26)=-d64/d945*(12*pr*y1**2+d12*x1**2*pr+d10*x5*x1+d7*y9*y1+         &
   d14*x9*x1+d5*y5*y1-d6*y1**2-d12*x1**2-d5*y5*y9-y9**2+d2*pr*y9**2-      &
   d10*x5*x9-d2*x9**2+d2*x9**2*pr+d10*pr*y5*y9-d10*pr*y5*y1-d14*pr*y9*y1+ &
   d10*x5*x9*pr-d14*x9*x1*pr-d10*x5*x1*pr)*cf
 km(8,27)=km(7,28)
 km(8,28)=d64/d945*(36*pr*y1**2+d36*x1**2*pr+d38*x5*x1+d17*y9*y1+         &
   d34*x9*x1+d19*y5*y1-d18*y1**2-d36*x1**2-d19*y5*y9+y9**2-d2*pr*y9**2-   &
   d38*x5*x9+d2*x9**2-d2*x9**2*pr+d38*pr*y5*y9-d38*pr*y5*y1-d34*pr*y9*y1+ &
   d38*x5*x9*pr-d34*x9*x1*pr-d38*x5*x1*pr)*cf
 km(8,29)=km(7,26)
 km(8,30)=km(8,26)
!--------------------------------------------------------------------------
 km(9,9)=-d47/d252*(-d2*y9**2+d4*y9*y1-d2*y1**2+d2*pr*y9**2-              &
   d4*pr*y9*y1+d2*pr*y1**2-x9**2+d2*x9**2*pr+d2*x9*x1-d4*x9*x1*pr-x1**2+  &
   d2*x1**2*pr)*cf
 km(9,10)=-d47/d252*(-x9+x1)*(-y9+y1)*cf
 km(9,11)=d4/d945*(154*pr*y1**2+d154*x1**2*pr+d92*x5*x1+d124*y9*y1+       &
   d62*x9*x1+d184*y5*y1-d154*y1**2-d77*x1**2-d184*y5*y9+d30*y9**2-        &
   d30*pr*y9**2-d92*x5*x9+d15*x9**2-d30*x9**2*pr+d184*pr*y5*y9-           &
   d184*pr*y5*y1-d124*pr*y9*y1+d184*x5*x9*pr-d124*x9*x1*pr-               &
   d184*x5*x1*pr)*cf
 km(9,12)=d4/d945*(184*pr*x5*y9-d184*pr*y5*x9+d77*x1*y1-d77*y1*x9+        &
   d92*y5*x9-d92*x1*y5+d15*x1*y9+d184*x9*y1*pr-d184*x5*y1*pr+             &
   d184*pr*y5*x1-d184*pr*y9*x1-d15*y9*x9)*cf
 km(9,13)=-(442*pr*y1**2+d442*x1**2*pr+d201*x5*x1+d482*y9*y1+             &
   d241*x9*x1+d402*y5*y1-d442*y1**2-d221*x1**2-d402*y5*y9-d40*y9**2+      &
   d40*pr*y9**2-d201*x5*x9-d20*x9**2+d40*x9**2*pr+d402*pr*y5*y9-          &
   d402*pr*y5*y1-d482*pr*y9*y1+d402*x5*x9*pr-d482*x9*x1*pr-               &
   d402*x5*x1*pr)/d945*cf
 km(9,14)=-(402*pr*x5*y9-d402*pr*y5*x9+d221*x1*y1-d221*y1*x9+             &
   d201*y5*x9-d201*x1*y5-d20*x1*y9+d402*x9*y1*pr-d402*x5*y1*pr+           &
   d402*pr*y5*x1-d402*pr*y9*x1+d20*y9*x9)/d945*cf
 km(9,15)=d4/d945*(58*pr*y1**2+d58*x1**2*pr+d24*x5*x1+d68*y9*y1+          &
   d34*x9*x1+d48*y5*y1-d58*y1**2-d29*x1**2-d48*y5*y9-d10*y9**2+           &
   d10*pr*y9**2-d24*x5*x9-d5*x9**2+d10*x9**2*pr+d48*pr*y5*y9-             &
   d48*pr*y5*y1-d68*pr*y9*y1+d48*x5*x9*pr-d68*x9*x1*pr-d48*x5*x1*pr)*cf
 km(9,16)=d4/d945*(48*pr*x5*y9-d48*pr*y5*x9+d29*x1*y1-d29*y1*x9+          &
   d24*y5*x9-d24*x1*y5-d5*x1*y9+d48*x9*y1*pr-d48*x5*y1*pr+d48*pr*y5*x1-   &
   d48*pr*y9*x1+d5*y9*x9)*cf
 km(9,17)=-d107/d3780*(-d2*pr*y5*y1+x5*x1+d2*y9*y1+x9*x1-                 &
   d2*x9*x1*pr+d2*pr*y1**2+d2*x1**2*pr+d2*x5*x9*pr-d2*y1**2+d2*y5*y1-     &
   d2*y5*y9-d2*x5*x1*pr-x5*x9+d2*pr*y5*y9-x1**2-d2*pr*y9*y1)*cf
 km(9,18)=-d107/d3780*(2*pr*x5*y9-d2*pr*y5*x9+x1*y1+y5*x9-x1*y5+          &
   d2*x9*y1*pr-y1*x9+d2*pr*y5*x1-d2*pr*y9*x1-d2*x5*y1*pr)*cf
 km(9,19)=-d4/d189*(-d2*y9**2+d4*y9*y1-d2*y1**2+d2*pr*y9**2-              &
   d4*pr*y9*y1+d2*pr*y1**2-x9**2+d2*x9**2*pr+d2*x9*x1-d4*x9*x1*pr-x1**2+  &
   d2*x1**2*pr)*cf
 km(9,20)=-d4/d189*(x1*y1-y1*x9-x1*y9+y9*x9)*cf
 km(9,21)=km(9,19)
 km(9,22)=km(9,20)
 km(9,23)=km(9,19)
 km(9,24)=km(9,20)
 km(9,25)=d8/d189*(-d2*y9**2+d4*y9*y1-d2*y1**2+d2*pr*y9**2-d4*pr*y9*y1+   &
   d2*pr*y1**2-x9**2+d2*x9**2*pr+d2*x9*x1-d4*x9*x1*pr-x1**2+d2*x1**2*pr)*cf
 km(9,26)=d8/d189*(x1*y1-y1*x9-x1*y9+y9*x9)*cf
 km(9,27)=km(9,25)
 km(9,28)=km(9,26)
 km(9,29)=km(9,25)
 km(9,30)=km(9,26)
!--------------------------------------------------------------------------
 km(10,10)=-d47/d252*(-d2*x9**2+d4*x9*x1-d2*x1**2+d2*x9**2*pr-            &
   d4*x9*x1*pr+d2*x1**2*pr-y9**2+d2*pr*y9**2+d2*y9*y1-d4*pr*y9*y1-y1**2+  &
   d2*pr*y1**2)*cf
 km(10,11)=-d4/d945*(184*pr*x5*y9-d184*pr*y5*x9-d77*x1*y1+d92*y1*x5-      &
   d15*y1*x9+d77*x1*y9-d92*x5*y9+d184*x9*y1*pr-d184*x5*y1*pr+             &
   d184*pr*y5*x1-d184*pr*y9*x1+d15*y9*x9)*cf
 km(10,12)=d4/d945*(154*pr*y1**2+d154*x1**2*pr+d184*x5*x1+d62*y9*y1+      &
   d124*x9*x1+d92*y5*y1-d77*y1**2-d154*x1**2-d92*y5*y9+d15*y9**2-         &
   d30*pr*y9**2-d184*x5*x9+d30*x9**2-d30*x9**2*pr+d184*pr*y5*y9-          &
   d184*pr*y5*y1-d124*pr*y9*y1+d184*x5*x9*pr-d124*x9*x1*pr-               &
   d184*x5*x1*pr)*cf
 km(10,13)=(402*pr*x5*y9-d402*pr*y5*x9-d221*x1*y1+d201*y1*x5+d20*y1*x9+   &
   d221*x1*y9-d201*x5*y9+d402*x9*y1*pr-d402*x5*y1*pr+d402*pr*y5*x1-       &
   d402*pr*y9*x1-d20*y9*x9)/d945*cf
 km(10,14)=-(442*pr*y1**2+d442*x1**2*pr+d402*x5*x1+d241*y9*y1+            &
   d482*x9*x1+d201*y5*y1-d221*y1**2-d442*x1**2-d201*y5*y9-d20*y9**2+      &
   d40*pr*y9**2-d402*x5*x9-d40*x9**2+d40*x9**2*pr+d402*pr*y5*y9-          &
   d402*pr*y5*y1-d482*pr*y9*y1+d402*x5*x9*pr-d482*x9*x1*pr-               &
   d402*x5*x1*pr)/d945*cf
 km(10,15)=-d4/d945*(48*pr*x5*y9-d48*pr*y5*x9-d29*x1*y1+d24*y1*x5+        &
   d5*y1*x9+d29*x1*y9-d24*x5*y9+d48*x9*y1*pr-d48*x5*y1*pr+d48*pr*y5*x1-   &
   d48*pr*y9*x1-d5*y9*x9)*cf
 km(10,16)=d4/d945*(58*pr*y1**2+d58*x1**2*pr+d48*x5*x1+d34*y9*y1+         &
   d68*x9*x1+d24*y5*y1-d29*y1**2-d58*x1**2-d24*y5*y9-d5*y9**2+            &
   d10*pr*y9**2-d48*x5*x9-d10*x9**2+d10*x9**2*pr+d48*pr*y5*y9-            &
   d48*pr*y5*y1-d68*pr*y9*y1+d48*x5*x9*pr-d68*x9*x1*pr-d48*x5*x1*pr)*cf
 km(10,17)=d107/d3780*(2*x9*y1*pr-d2*pr*y5*x9+d2*pr*y5*x1+x1*y9-          &
   d2*pr*y9*x1-x5*y9+d2*pr*x5*y9-x1*y1+y1*x5-d2*x5*y1*pr)*cf
 km(10,18)=-d107/d3780*(-d2*x5*x9+d2*pr*y5*y9-d2*x5*x1*pr+d2*pr*y1**2+    &
   d2*x1**2*pr-d2*pr*y9*y1+d2*x5*x1+y9*y1-d2*pr*y5*y1-d2*x9*x1*pr+y5*y1+  &
   d2*x5*x9*pr-d2*x1**2+d2*x9*x1-y5*y9-y1**2)*cf
 km(10,19)=km(9,20)
 km(10,20)=-d4/d189*(-d2*x9**2+d4*x9*x1-d2*x1**2+d2*x9**2*pr-             &
   d4*x9*x1*pr+d2*x1**2*pr-y9**2+d2*pr*y9**2+d2*y9*y1-d4*pr*y9*y1-y1**2+  &
   d2*pr*y1**2)*cf
 km(10,21)=km(9,20)
 km(10,22)=km(10,20)
 km(10,23)=km(9,20)
 km(10,24)=km(10,20)
 km(10,25)=km(9,26)
 km(10,26)=d8/d189*(-d2*x9**2+d4*x9*x1-d2*x1**2+d2*x9**2*pr-d4*x9*x1*pr+  &
   d2*x1**2*pr-y9**2+d2*pr*y9**2+d2*y9*y1-d4*pr*y9*y1-y1**2+d2*pr*y1**2)*cf
 km(10,27)=km(9,26)
 km(10,28)=km(10,26)
 km(10,29)=km(9,26)
 km(10,30)=km(10,26)
!--------------------------------------------------------------------------
 km(11,11)=km(7,7)
 km(11,12)=km(7,8)
 km(11,13)=d8/d945*(230*pr*y1**2+d230*x1**2*pr+d121*x5*x1+d218*y9*y1+     &
   d109*x9*x1+d242*y5*y1-d230*y1**2-d115*x1**2-d90*y5**2-d62*y5*y9-       &
   d78*y9**2+d90*pr*y5**2+d78*pr*y9**2-d45*x5**2-d31*x5*x9-d39*x9**2+     &
   d90*x5**2*pr+d78*x9**2*pr+d62*pr*y5*y9-d242*pr*y5*y1-d218*pr*y9*y1+    &
   d62*x5*x9*pr-d218*x9*x1*pr-d242*x5*x1*pr)*cf
 km(11,14)=d4/d945*(264*pr*x5*y9-d264*pr*y5*x9+d230*x1*y1-d55*y1*x5-      &
   d175*y1*x9+d97*y5*x9-d187*x1*y5-d43*x1*y9-d35*x5*y9+d90*y5*x5+         &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1+d78*y9*x9)*cf
 km(11,15)=-d32/d189*(6*pr*y1**2+d6*x1**2*pr+d3*x5*x1+d6*y9*y1+           &
   d3*x9*x1+d6*y5*y1-d6*y1**2-d3*x1**2-d2*y5**2-d2*y5*y9-d2*y9**2+        &
   d2*pr*y5**2+d2*pr*y9**2-x5**2-x5*x9-x9**2+d2*x5**2*pr+d2*x9**2*pr+     &
   d2*pr*y5*y9-d6*pr*y5*y1-d6*pr*y9*y1+d2*x5*x9*pr-d6*x9*x1*pr-           &
   d6*x5*x1*pr)*cf
 km(11,16)=-d16/d945*(32*pr*x5*y9-d32*pr*y5*x9+d30*x1*y1-d7*y1*x5-        &
   d23*y1*x9+d13*y5*x9-d23*x1*y5-d7*x1*y9-d3*x5*y9+d10*y5*x5+             &
   d32*x9*y1*pr-d32*x5*y1*pr+d32*pr*y5*x1-d32*pr*y9*x1+d10*y9*x9)*cf
 km(11,17)=d4/d945*(58*pr*y1**2+d58*x1**2*pr+d34*x5*x1+d48*y9*y1+         &
   d24*x9*x1+d68*y5*y1-d58*y1**2-d29*x1**2-d10*y5**2-d48*y5*y9+           &
   d10*pr*y5**2-d5*x5**2-d24*x5*x9+d10*x5**2*pr+d48*pr*y5*y9-             &
   d68*pr*y5*y1-d48*pr*y9*y1+d48*x5*x9*pr-d48*x9*x1*pr-d68*x5*x1*pr)*cf
 km(11,18)=d4/d945*(48*pr*x5*y9-d48*pr*y5*x9+d29*x1*y1-d5*y1*x5-          &
   d24*y1*x9+d24*y5*x9-d29*x1*y5+d5*y5*x5+d48*x9*y1*pr-d48*x5*y1*pr+      &
   d48*pr*y5*x1-d48*pr*y9*x1)*cf
 km(11,19)=d32/d945*(4*pr*y1**2+d4*x1**2*pr-d3*x5*x1+d14*y9*y1+           &
   d7*x9*x1-d6*y5*y1-d4*y1**2-d2*x1**2+d6*y5*y9-d10*y9**2+d10*pr*y9**2+   &
   d3*x5*x9-d5*x9**2+d10*x9**2*pr-d6*pr*y5*y9+d6*pr*y5*y1-d14*pr*y9*y1-   &
   d6*x5*x9*pr-d14*x9*x1*pr+d6*x5*x1*pr)*cf
 km(11,20)=d16/d945*(4*x1*y1+d3*y1*x5-d7*y1*x9-d3*y5*x9+d3*x1*y5-         &
   d7*x1*y9-d3*x5*y9+d10*y9*x9)*cf
 km(11,21)=d8/d945*(8*pr*y1**2+d8*x1**2*pr-d5*x5*x1+d26*y9*y1+            &
   d13*x9*x1-d10*y5*y1-d8*y1**2-d4*x1**2+d10*y5*y9-d18*y9**2+             &
   d18*pr*y9**2+d5*x5*x9-d9*x9**2+d18*x9**2*pr-d10*pr*y5*y9+              &
   d10*pr*y5*y1-d26*pr*y9*y1-d10*x5*x9*pr-d26*x9*x1*pr+d10*x5*x1*pr)*cf
 km(11,22)=d4/d945*(8*x1*y1+d5*y1*x5-d13*y1*x9-d5*y5*x9+d5*x1*y5-         &
   d13*x1*y9-d5*x5*y9+d18*y9*x9)*cf
 km(11,23)=d32/d315*(2*y5*y9-d2*y5*y1-d2*y9**2+d2*y9*y1-d2*pr*y5*y9+      &
   d2*pr*y5*y1+d2*pr*y9**2-d2*pr*y9*y1+x5*x9-d2*x5*x9*pr-x5*x1+           &
   d2*x5*x1*pr-x9**2+d2*x9**2*pr+x9*x1-d2*x9*x1*pr)*cf
 km(11,24)=d16/d315*(y1*x5-y1*x9-y5*x9+x1*y5-x1*y9-x5*y9+d2*y9*x9)*cf
 km(11,25)=-d64/d945*(2*pr*y1**2+d2*x1**2*pr-d5*x5*x1+d14*y9*y1+          &
   d7*x9*x1-d10*y5*y1-d2*y1**2-x1**2+d10*y5*y9-d12*y9**2+d12*pr*y9**2+    &
   d5*x5*x9-d6*x9**2+d12*x9**2*pr-d10*pr*y5*y9+d10*pr*y5*y1-d14*pr*y9*y1- &
   d10*x5*x9*pr-d14*x9*x1*pr+d10*x5*x1*pr)*cf
 km(11,26)=-d32/d945*(2*x1*y1+d5*y1*x5-d7*y1*x9-d5*y5*x9+d5*x1*y5-        &
   d7*x1*y9-d5*x5*y9+d12*y9*x9)*cf
 km(11,27)=-d64/d945*(2*pr*y1**2+d2*x1**2*pr+d19*x5*x1-d34*y9*y1-         &
   d17*x9*x1+d38*y5*y1-d2*y1**2-x1**2-d38*y5*y9+d36*y9**2-d36*pr*y9**2-   &
   d19*x5*x9+d18*x9**2-d36*x9**2*pr+d38*pr*y5*y9-d38*pr*y5*y1+            &
   d34*pr*y9*y1+d38*x5*x9*pr+d34*x9*x1*pr-d38*x5*x1*pr)*cf
 km(11,28)=-d32/d945*(2*x1*y1-d19*y1*x5+d17*y1*x9+d19*y5*x9-d19*x1*y5+    &
   d17*x1*y9+d19*x5*y9-d36*y9*x9)*cf
 km(11,29)=km(11,25)
 km(11,30)=km(11,26)
!--------------------------------------------------------------------------
 km(12,12)=km(8,8)
 km(12,13)=-d4/d945*(264*pr*x5*y9-d264*pr*y5*x9-d230*x1*y1+d187*y1*x5+    &
   d43*y1*x9+d35*y5*x9+d55*x1*y5+d175*x1*y9-d97*x5*y9-d90*y5*x5+          &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1-d78*y9*x9)*cf
 km(12,14)=d8/d945*(230*pr*y1**2+d230*x1**2*pr+d242*x5*x1+d109*y9*y1+     &
   d218*x9*x1+d121*y5*y1-d115*y1**2-d230*x1**2-d45*y5**2-d31*y5*y9-       &
   d39*y9**2+d90*pr*y5**2+d78*pr*y9**2-d90*x5**2-d62*x5*x9-d78*x9**2+     &
   d90*x5**2*pr+d78*x9**2*pr+d62*pr*y5*y9-d242*pr*y5*y1-d218*pr*y9*y1+    &
   d62*x5*x9*pr-d218*x9*x1*pr-d242*x5*x1*pr)*cf
 km(12,15)=d16/d945*(32*pr*x5*y9-d32*pr*y5*x9-d30*x1*y1+d23*y1*x5+        &
   d7*y1*x9+d3*y5*x9+d7*x1*y5+d23*x1*y9-d13*x5*y9-d10*y5*x5+d32*x9*y1*pr- &
   d32*x5*y1*pr+d32*pr*y5*x1-d32*pr*y9*x1-d10*y9*x9)*cf
 km(12,16)=-d32/d189*(6*pr*y1**2+d6*x1**2*pr+d6*x5*x1+d3*y9*y1+           &
   d6*x9*x1+d3*y5*y1-d3*y1**2-d6*x1**2-y5**2-y5*y9-y9**2+d2*pr*y5**2+     &
   d2*pr*y9**2-d2*x5**2-d2*x5*x9-d2*x9**2+d2*x5**2*pr+d2*x9**2*pr+        &
   d2*pr*y5*y9-d6*pr*y5*y1-d6*pr*y9*y1+d2*x5*x9*pr-d6*x9*x1*pr-           &
   d6*x5*x1*pr)*cf
 km(12,17)=-d4/d945*(48*pr*x5*y9-d48*pr*y5*x9-d29*x1*y1+d29*y1*x5+        &
   d5*x1*y5+d24*x1*y9-d24*x5*y9-d5*y5*x5+d48*x9*y1*pr-d48*x5*y1*pr+       &
   d48*pr*y5*x1-d48*pr*y9*x1)*cf
 km(12,18)=d4/d945*(58*pr*y1**2+d58*x1**2*pr+d68*x5*x1+d24*y9*y1+         &
   d48*x9*x1+d34*y5*y1-d29*y1**2-d58*x1**2-d5*y5**2-d24*y5*y9+            &
   d10*pr*y5**2-d10*x5**2-d48*x5*x9+d10*x5**2*pr+d48*pr*y5*y9-            &
   d68*pr*y5*y1-d48*pr*y9*y1+d48*x5*x9*pr-d48*x9*x1*pr-d68*x5*x1*pr)*cf
 km(12,19)=km(11,20)
 km(12,20)=d32/d945*(4*pr*y1**2+d4*x1**2*pr-d6*x5*x1+d7*y9*y1+            &
   d14*x9*x1-d3*y5*y1-d2*y1**2-d4*x1**2+d3*y5*y9-d5*y9**2+d10*pr*y9**2+   &
   d6*x5*x9-d10*x9**2+d10*x9**2*pr-d6*pr*y5*y9+d6*pr*y5*y1-d14*pr*y9*y1-  &
   d6*x5*x9*pr-d14*x9*x1*pr+d6*x5*x1*pr)*cf
 km(12,21)=km(11,22)
 km(12,22)=d8/d945*(8*pr*y1**2+d8*x1**2*pr-d10*x5*x1+d13*y9*y1+           &
   d26*x9*x1-d5*y5*y1-d4*y1**2-d8*x1**2+d5*y5*y9-d9*y9**2+d18*pr*y9**2+   &
   d10*x5*x9-d18*x9**2+d18*x9**2*pr-d10*pr*y5*y9+d10*pr*y5*y1-            &
   d26*pr*y9*y1-d10*x5*x9*pr-d26*x9*x1*pr+d10*x5*x1*pr)*cf
 km(12,23)=km(11,24)
 km(12,24)=d32/d315*(-d2*x5*x1+y9*y1+d2*x9*x1-y5*y1+y5*y9-y9**2+          &
   d2*pr*y9**2+d2*x5*x9-d2*x9**2+d2*x9**2*pr-d2*pr*y5*y9+d2*pr*y5*y1-     &
   d2*pr*y9*y1-d2*x5*x9*pr-d2*x9*x1*pr+d2*x5*x1*pr)*cf
 km(12,25)=km(11,26)
 km(12,26)=-d64/d945*(2*pr*y1**2+d2*x1**2*pr-d10*x5*x1+d7*y9*y1+          &
   d14*x9*x1-d5*y5*y1-y1**2-d2*x1**2+d5*y5*y9-d6*y9**2+d12*pr*y9**2+      &
   d10*x5*x9-d12*x9**2+d12*x9**2*pr-d10*pr*y5*y9+d10*pr*y5*y1-            &
   d14*pr*y9*y1-d10*x5*x9*pr-d14*x9*x1*pr+d10*x5*x1*pr)*cf
 km(12,27)=km(11,28)
 km(12,28)=-d64/d945*(2*pr*y1**2+d2*x1**2*pr+d38*x5*x1-d17*y9*y1-         &
   d34*x9*x1+d19*y5*y1-y1**2-d2*x1**2-d19*y5*y9+d18*y9**2-d36*pr*y9**2-   &
   d38*x5*x9+d36*x9**2-d36*x9**2*pr+d38*pr*y5*y9-d38*pr*y5*y1+            &
   d34*pr*y9*y1+d38*x5*x9*pr+d34*x9*x1*pr-d38*x5*x1*pr)*cf
 km(12,29)=km(11,26)
 km(12,30)=km(12,26)
!--------------------------------------------------------------------------
 km(13,13)=-d2/d315*(466*pr*y1**2+d466*x1**2*pr+d233*x5*x1+d466*y9*y1+    &
   d233*x9*x1+d466*y5*y1-d466*y1**2-d233*x1**2-d386*y5**2+d306*y5*y9-     &
   d386*y9**2+d386*pr*y5**2+d386*pr*y9**2-d193*x5**2+d153*x5*x9-          &
   d193*x9**2+d386*x5**2*pr+d386*x9**2*pr-d306*pr*y5*y9-d466*pr*y5*y1-    &
   d466*pr*y9*y1-d306*x5*x9*pr-d466*x9*x1*pr-d466*x5*x1*pr)*cf
 km(13,14)=-(466*x1*y1-d233*y1*x5-d233*y1*x9-d153*y5*x9-d233*x1*y5-       &
   d233*x1*y9-d153*x5*y9+d386*y5*x5+d386*y9*x9)/d315*cf
 km(13,15)=d8/d945*(230*pr*y1**2+d230*x1**2*pr+d109*x5*x1+d242*y9*y1+     &
   d121*x9*x1+d218*y5*y1-d230*y1**2-d115*x1**2-d78*y5**2-d62*y5*y9-       &
   d90*y9**2+d78*pr*y5**2+d90*pr*y9**2-d39*x5**2-d31*x5*x9-d45*x9**2+     &
   d78*x5**2*pr+d90*x9**2*pr+d62*pr*y5*y9-d218*pr*y5*y1-d242*pr*y9*y1+    &
   d62*x5*x9*pr-d242*x9*x1*pr-d218*x5*x1*pr)*cf
 km(13,16)=d4/d945*(264*pr*x5*y9-d264*pr*y5*x9+d230*x1*y1-d43*y1*x5-      &
   d187*y1*x9+d97*y5*x9-d175*x1*y5-d55*x1*y9-d35*x5*y9+d78*y5*x5+         &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1+d90*y9*x9)*cf
 km(13,17)=-(442*pr*y1**2+d442*x1**2*pr+d241*x5*x1+d402*y9*y1+            &
   d201*x9*x1+d482*y5*y1-d442*y1**2-d221*x1**2-d40*y5**2-d402*y5*y9+      &
   d40*pr*y5**2-d20*x5**2-d201*x5*x9+d40*x5**2*pr+d402*pr*y5*y9-          &
   d482*pr*y5*y1-d402*pr*y9*y1+d402*x5*x9*pr-d402*x9*x1*pr-               &
   d482*x5*x1*pr)/d945*cf
 km(13,18)=-(402*pr*x5*y9-d402*pr*y5*x9+d221*x1*y1-d20*y1*x5-             &
   d201*y1*x9+d201*y5*x9-d221*x1*y5+d20*y5*x5+d402*x9*y1*pr-              &
   d402*x5*y1*pr+d402*pr*y5*x1-d402*pr*y9*x1)/d945*cf
 km(13,19)=-d8/d945*(12*pr*y1**2+d12*x1**2*pr-d43*x5*x1+d110*y9*y1+       &
   d55*x9*x1-d86*y5*y1-d12*y1**2-d6*x1**2+d8*y5**2+d70*y5*y9-d90*y9**2-   &
   d8*pr*y5**2+d90*pr*y9**2+d4*x5**2+d35*x5*x9-d45*x9**2-d8*x5**2*pr+     &
   d90*x9**2*pr-d70*pr*y5*y9+d86*pr*y5*y1-d110*pr*y9*y1-d70*x5*x9*pr-     &
   d110*x9*x1*pr+d86*x5*x1*pr)*cf
 km(13,20)=-d4/d945*(12*x1*y1+d43*y1*x5-d55*y1*x9-d35*y5*x9+d43*x1*y5-    &
   d55*x1*y9-d35*x5*y9-d8*y5*x5+d90*y9*x9)*cf
 km(13,21)=-d2/d315*(8*pr*y1**2+d8*x1**2*pr-d23*x5*x1+d62*y9*y1+          &
   d31*x9*x1-d46*y5*y1-d8*y1**2-d4*x1**2-d8*y5**2+d62*y5*y9-d62*y9**2+    &
   d8*pr*y5**2+d62*pr*y9**2-d4*x5**2+d31*x5*x9-d31*x9**2+d8*x5**2*pr+     &
   d62*x9**2*pr-d62*pr*y5*y9+d46*pr*y5*y1-d62*pr*y9*y1-d62*x5*x9*pr-      &
   d62*x9*x1*pr+d46*x5*x1*pr)*cf
 km(13,22)=-(8*x1*y1+d23*y1*x5-d31*y1*x9-d31*y5*x9+d23*x1*y5-d31*x1*y9-   &
   d31*x5*y9+d8*y5*x5+d62*y9*x9)/d315*cf
 km(13,23)=d8/d945*(-d5*x5*x1+d10*y9*y1+d5*x9*x1-d10*y5*y1-d8*y5**2+      &
   d26*y5*y9-d18*y9**2+d8*pr*y5**2+d18*pr*y9**2-d4*x5**2+d13*x5*x9-       &
   d9*x9**2+d8*x5**2*pr+d18*x9**2*pr-d26*pr*y5*y9+d10*pr*y5*y1-           &
   d10*pr*y9*y1-d26*x5*x9*pr-d10*x9*x1*pr+d10*x5*x1*pr)*cf
 km(13,24)=d4/d945*(5*y1*x5-d5*y1*x9-d13*y5*x9+d5*x1*y5-d5*x1*y9-         &
 d13*x5*y9+d8*y5*x5+d18*y9*x9)*cf
 km(13,25)=d16/d315*(2*pr*y1**2+d2*x1**2*pr+x5*x1+d2*y9*y1+x9*x1+         &
   d2*y5*y1-d2*y1**2-x1**2+d8*y5**2-d18*y5*y9+d8*y9**2-d8*pr*y5**2-       &
   d8*pr*y9**2+d4*x5**2-d9*x5*x9+d4*x9**2-d8*x5**2*pr-d8*x9**2*pr+        &
   d18*pr*y5*y9-d2*pr*y5*y1-d2*pr*y9*y1+d18*x5*x9*pr-d2*x9*x1*pr-         &
   d2*x5*x1*pr)*cf
 km(13,26)=d8/d315*(2*x1*y1-y1*x5-y1*x9+d9*y5*x9-x1*y5-x1*y9+d9*x5*y9-    &
   d8*y5*x5-d8*y9*x9)*cf
 km(13,27)=d16/d315*(2*pr*y1**2+d2*x1**2*pr+d33*x5*x1-d62*y9*y1-          &
   d31*x9*x1+d66*y5*y1-d2*y1**2-x1**2-d56*y5**2+d46*y5*y9+d8*y9**2+       &
   d56*pr*y5**2-d8*pr*y9**2-d28*x5**2+d23*x5*x9+d4*x9**2+d56*x5**2*pr-    &
   d8*x9**2*pr-d46*pr*y5*y9-d66*pr*y5*y1+d62*pr*y9*y1-d46*x5*x9*pr+       &
   d62*x9*x1*pr-d66*x5*x1*pr)*cf
 km(13,28)=d8/d315*(2*x1*y1-d33*y1*x5+d31*y1*x9-d23*y5*x9-d33*x1*y5+      &
   d31*x1*y9-d23*x5*y9+d56*y5*x5-d8*y9*x9)*cf
 km(13,29)=d16/d315*(2*pr*y1**2+d2*x1**2*pr-d31*x5*x1+d66*y9*y1+          &
   d33*x9*x1-d62*y5*y1-d2*y1**2-x1**2+d8*y5**2+d46*y5*y9-d56*y9**2-       &
   d8*pr*y5**2+d56*pr*y9**2+d4*x5**2+d23*x5*x9-d28*x9**2-d8*x5**2*pr+     &
   d56*x9**2*pr-d46*pr*y5*y9+d62*pr*y5*y1-d66*pr*y9*y1-d46*x5*x9*pr-      &
   d66*x9*x1*pr+d62*x5*x1*pr)*cf
 km(13,30)=d8/d315*(2*x1*y1+d31*y1*x5-d33*y1*x9-d23*y5*x9+d31*x1*y5-      &
   d33*x1*y9-d23*x5*y9-d8*y5*x5+d56*y9*x9)*cf
!--------------------------------------------------------------------------
 km(14,14)=-d2/d315*(466*pr*y1**2+d466*x1**2*pr+d466*x5*x1+d233*y9*y1+    &
   d466*x9*x1+d233*y5*y1-d233*y1**2-d466*x1**2-d193*y5**2+d153*y5*y9-     &
   d193*y9**2+d386*pr*y5**2+d386*pr*y9**2-d386*x5**2+d306*x5*x9-          &
   d386*x9**2+d386*x5**2*pr+d386*x9**2*pr-d306*pr*y5*y9-d466*pr*y5*y1-    &
   d466*pr*y9*y1-d306*x5*x9*pr-d466*x9*x1*pr-d466*x5*x1*pr)*cf
 km(14,15)=-d4/d945*(264*pr*x5*y9-d264*pr*y5*x9-d230*x1*y1+d175*y1*x5+    &
   d55*y1*x9+d35*y5*x9+d43*x1*y5+d187*x1*y9-d97*x5*y9-d78*y5*x5+          &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1-d90*y9*x9)*cf
 km(14,16)=d8/d945*(230*pr*y1**2+d230*x1**2*pr+d218*x5*x1+d121*y9*y1+     &
   d242*x9*x1+d109*y5*y1-d115*y1**2-d230*x1**2-d39*y5**2-d31*y5*y9-       &
   d45*y9**2+d78*pr*y5**2+d90*pr*y9**2-d78*x5**2-d62*x5*x9-d90*x9**2+     &
   d78*x5**2*pr+d90*x9**2*pr+d62*pr*y5*y9-d218*pr*y5*y1-d242*pr*y9*y1+    &
   d62*x5*x9*pr-d242*x9*x1*pr-d218*x5*x1*pr)*cf
 km(14,17)=(402*pr*x5*y9-d402*pr*y5*x9-d221*x1*y1+d221*y1*x5+             &
   d20*x1*y5+d201*x1*y9-d201*x5*y9-d20*y5*x5+d402*x9*y1*pr-d402*x5*y1*pr+ &
   d402*pr*y5*x1-d402*pr*y9*x1)/d945*cf
 km(14,18)=-(442*pr*y1**2+d442*x1**2*pr+d482*x5*x1+d201*y9*y1+            &
   d402*x9*x1+d241*y5*y1-d221*y1**2-d442*x1**2-d20*y5**2-d201*y5*y9+      &
   d40*pr*y5**2-d40*x5**2-d402*x5*x9+d40*x5**2*pr+d402*pr*y5*y9-          &
   d482*pr*y5*y1-d402*pr*y9*y1+d402*x5*x9*pr-d402*x9*x1*pr-               &
   d482*x5*x1*pr)/d945*cf
 km(14,19)=km(13,20)
 km(14,20)=-d8/d945*(12*pr*y1**2+d12*x1**2*pr-d86*x5*x1+d55*y9*y1+        &
   d110*x9*x1-d43*y5*y1-d6*y1**2-d12*x1**2+d4*y5**2+d35*y5*y9-d45*y9**2-  &
   d8*pr*y5**2+d90*pr*y9**2+d8*x5**2+d70*x5*x9-d90*x9**2-d8*x5**2*pr+     &
   d90*x9**2*pr-d70*pr*y5*y9+d86*pr*y5*y1-d110*pr*y9*y1-d70*x5*x9*pr-     &
   d110*x9*x1*pr+d86*x5*x1*pr)*cf
 km(14,21)=km(13,22)
 km(14,22)=-d2/d315*(8*pr*y1**2+d8*x1**2*pr-d46*x5*x1+d31*y9*y1+          &
   d62*x9*x1-d23*y5*y1-d4*y1**2-d8*x1**2-d4*y5**2+d31*y5*y9-d31*y9**2+    &
   d8*pr*y5**2+d62*pr*y9**2-d8*x5**2+d62*x5*x9-d62*x9**2+d8*x5**2*pr+     &
   d62*x9**2*pr-d62*pr*y5*y9+d46*pr*y5*y1-d62*pr*y9*y1-d62*x5*x9*pr-      &
   d62*x9*x1*pr+d46*x5*x1*pr)*cf
 km(14,23)=km(13,24)
 km(14,24)=d8/d945*(-d10*x5*x1+d5*y9*y1+d10*x9*x1-d5*y5*y1-d4*y5**2+      &
   d13*y5*y9-d9*y9**2+d8*pr*y5**2+d18*pr*y9**2-d8*x5**2+d26*x5*x9-        &
   d18*x9**2+d8*x5**2*pr+d18*x9**2*pr-d26*pr*y5*y9+d10*pr*y5*y1-          &
   d10*pr*y9*y1-d26*x5*x9*pr-d10*x9*x1*pr+d10*x5*x1*pr)*cf
 km(14,25)=km(13,26)
 km(14,26)=d16/d315*(2*pr*y1**2+d2*x1**2*pr+d2*x5*x1+y9*y1+               &
   d2*x9*x1+y5*y1-y1**2-d2*x1**2+d4*y5**2-d9*y5*y9+d4*y9**2-d8*pr*y5**2-  &
   d8*pr*y9**2+d8*x5**2-d18*x5*x9+d8*x9**2-d8*x5**2*pr-d8*x9**2*pr+       &
   d18*pr*y5*y9-d2*pr*y5*y1-d2*pr*y9*y1+d18*x5*x9*pr-d2*x9*x1*pr-         &
   d2*x5*x1*pr)*cf
 km(14,27)=km(13,28)
 km(14,28)=d16/d315*(2*pr*y1**2+d2*x1**2*pr+d66*x5*x1-d31*y9*y1-          &
   d62*x9*x1+d33*y5*y1-y1**2-d2*x1**2-d28*y5**2+d23*y5*y9+d4*y9**2+       &
   d56*pr*y5**2-d8*pr*y9**2-d56*x5**2+d46*x5*x9+d8*x9**2+d56*x5**2*pr-    &
   d8*x9**2*pr-d46*pr*y5*y9-d66*pr*y5*y1+d62*pr*y9*y1-d46*x5*x9*pr+       &
   d62*x9*x1*pr-d66*x5*x1*pr)*cf
 km(14,29)=km(13,30)
 km(14,30)=d16/d315*(2*pr*y1**2+d2*x1**2*pr-d62*x5*x1+d33*y9*y1+          &
   d66*x9*x1-d31*y5*y1-y1**2-d2*x1**2+d4*y5**2+d23*y5*y9-d28*y9**2-       &
   d8*pr*y5**2+d56*pr*y9**2+d8*x5**2+d46*x5*x9-d56*x9**2-d8*x5**2*pr+     &
   d56*x9**2*pr-d46*pr*y5*y9+d62*pr*y5*y1-d66*pr*y9*y1-d46*x5*x9*pr-      &
   d66*x9*x1*pr+d62*x5*x1*pr)*cf
!--------------------------------------------------------------------------
 km(15,15)=-d32/d945*(54*pr*y1**2+d54*x1**2*pr+d23*x5*x1+d62*y9*y1+       &
   d31*x9*x1+d46*y5*y1-d54*y1**2-d27*x1**2-d54*y5**2+d62*y5*y9-d62*y9**2+ &
   d54*pr*y5**2+d62*pr*y9**2-d27*x5**2+d31*x5*x9-d31*x9**2+d54*x5**2*pr+  &
   d62*x9**2*pr-d62*pr*y5*y9-d46*pr*y5*y1-d62*pr*y9*y1-d62*x5*x9*pr-      &
   d62*x9*x1*pr-d46*x5*x1*pr)*cf
 km(15,16)=-d16/d945*(54*x1*y1-d23*y1*x5-d31*y1*x9-d31*y5*x9-d23*x1*y5-   &
   d31*x1*y9-d31*x5*y9+d54*y5*x5+d62*y9*x9)*cf
 km(15,17)=d4/d945*(154*pr*y1**2+d154*x1**2*pr+d62*x5*x1+d184*y9*y1+      &
   d92*x9*x1+d124*y5*y1-d154*y1**2-d77*x1**2+d30*y5**2-d184*y5*y9-        &
   d30*pr*y5**2+d15*x5**2-d92*x5*x9-d30*x5**2*pr+d184*pr*y5*y9-           &
   d124*pr*y5*y1-d184*pr*y9*y1+d184*x5*x9*pr-d184*x9*x1*pr-               &
   d124*x5*x1*pr)*cf
 km(15,18)=d4/d945*(184*pr*x5*y9-d184*pr*y5*x9+d77*x1*y1+d15*y1*x5-       &
   d92*y1*x9+d92*y5*x9-d77*x1*y5-d15*y5*x5+d184*x9*y1*pr-d184*x5*y1*pr+   &
   d184*pr*y5*x1-d184*pr*y9*x1)*cf
 km(15,19)=d32/d945*(4*pr*y1**2+d4*x1**2*pr-d27*x5*x1+d62*y9*y1+          &
   d31*x9*x1-d54*y5*y1-d4*y1**2-d2*x1**2-d4*y5**2+d62*y5*y9-d62*y9**2+    &
   d4*pr*y5**2+d62*pr*y9**2-d2*x5**2+d31*x5*x9-d31*x9**2+d4*x5**2*pr+     &
   d62*x9**2*pr-d62*pr*y5*y9+d54*pr*y5*y1-d62*pr*y9*y1-d62*x5*x9*pr-      &
   d62*x9*x1*pr+d54*x5*x1*pr)*cf
 km(15,20)=d16/d945*(4*x1*y1+d27*y1*x5-d31*y1*x9-d31*y5*x9+d27*x1*y5-     &
   d31*x1*y9-d31*x5*y9+d4*y5*x5+d62*y9*x9)*cf
 km(15,21)=d8/d945*(8*pr*y1**2+d8*x1**2*pr+d43*x5*x1-d70*y9*y1-           &
   d35*x9*x1+d86*y5*y1-d8*y1**2-d4*x1**2+d12*y5**2-d110*y5*y9+d90*y9**2-  &
   d12*pr*y5**2-d90*pr*y9**2+d6*x5**2-d55*x5*x9+d45*x9**2-d12*x5**2*pr-   &
   d90*x9**2*pr+d110*pr*y5*y9-d86*pr*y5*y1+d70*pr*y9*y1+d110*x5*x9*pr+    &
   d70*x9*x1*pr-d86*x5*x1*pr)*cf
 km(15,22)=d4/d945*(8*x1*y1-d43*y1*x5+d35*y1*x9+d55*y5*x9-d43*x1*y5+      &
   d35*x1*y9+d55*x5*y9-d12*y5*x5-d90*y9*x9)*cf
 km(15,23)=d32/d945*(-d3*x5*x1+d6*y9*y1+d3*x9*x1-d6*y5*y1-d4*y5**2+       &
   d14*y5*y9-d10*y9**2+d4*pr*y5**2+d10*pr*y9**2-d2*x5**2+d7*x5*x9-        &
   d5*x9**2+d4*x5**2*pr+d10*x9**2*pr-d14*pr*y5*y9+d6*pr*y5*y1-            &
   d6*pr*y9*y1-d14*x5*x9*pr-d6*x9*x1*pr+d6*x5*x1*pr)*cf
 km(15,24)=d16/d945*(3*y1*x5-d3*y1*x9-d7*y5*x9+d3*x1*y5-d3*x1*y9-         &
   d7*x5*y9+d4*y5*x5+d10*y9*x9)*cf
 km(15,25)=-d64/d945*(2*pr*y1**2+d2*x1**2*pr+d7*x5*x1-d10*y9*y1-          &
   d5*x9*x1+d14*y5*y1-d2*y1**2-x1**2-d12*y5**2+d10*y5*y9+d12*pr*y5**2-    &
   d6*x5**2+d5*x5*x9+d12*x5**2*pr-d10*pr*y5*y9-d14*pr*y5*y1+d10*pr*y9*y1- &
   d10*x5*x9*pr+d10*x9*x1*pr-d14*x5*x1*pr)*cf
 km(15,26)=-d32/d945*(2*x1*y1-d7*y1*x5+d5*y1*x9-d5*y5*x9-d7*x1*y5+        &
   d5*x1*y9-d5*x5*y9+d12*y5*x5)*cf
 km(15,27)=km(15,25)
 km(15,28)=km(15,26)
 km(15,29)=-d64/d945*(2*pr*y1**2+d2*x1**2*pr-d17*x5*x1+d38*y9*y1+         &
   d19*x9*x1-d34*y5*y1-d2*y1**2-x1**2+d36*y5**2-d38*y5*y9-d36*pr*y5**2+   &
   d18*x5**2-d19*x5*x9-d36*x5**2*pr+d38*pr*y5*y9+d34*pr*y5*y1-            &
   d38*pr*y9*y1+d38*x5*x9*pr-d38*x9*x1*pr+d34*x5*x1*pr)*cf
 km(15,30)=-d32/d945*(2*x1*y1+d17*y1*x5-d19*y1*x9+d19*y5*x9+d17*x1*y5-    &
   d19*x1*y9+d19*x5*y9-d36*y5*x5)*cf
!--------------------------------------------------------------------------
 km(16,16)=-d32/d945*(54*pr*y1**2+d54*x1**2*pr+d46*x5*x1+d31*y9*y1+       &
   d62*x9*x1+d23*y5*y1-d27*y1**2-d54*x1**2-d27*y5**2+d31*y5*y9-d31*y9**2+ &
   d54*pr*y5**2+d62*pr*y9**2-d54*x5**2+d62*x5*x9-d62*x9**2+d54*x5**2*pr+  &
   d62*x9**2*pr-d62*pr*y5*y9-d46*pr*y5*y1-d62*pr*y9*y1-d62*x5*x9*pr-      &
   d62*x9*x1*pr-d46*x5*x1*pr)*cf
 km(16,17)=-d4/d945*(184*pr*x5*y9-d184*pr*y5*x9-d77*x1*y1+d77*y1*x5-      &
   d15*x1*y5+d92*x1*y9-d92*x5*y9+d15*y5*x5+d184*x9*y1*pr-d184*x5*y1*pr+   &
   d184*pr*y5*x1-d184*pr*y9*x1)*cf
 km(16,18)=d4/d945*(154*pr*y1**2+d154*x1**2*pr+d124*x5*x1+d92*y9*y1+      &
   d184*x9*x1+d62*y5*y1-d77*y1**2-d154*x1**2+d15*y5**2-d92*y5*y9-         &
   d30*pr*y5**2+d30*x5**2-d184*x5*x9-d30*x5**2*pr+d184*pr*y5*y9-          &
   d124*pr*y5*y1-d184*pr*y9*y1+d184*x5*x9*pr-d184*x9*x1*pr-               &
   d124*x5*x1*pr)*cf
 km(16,19)=km(15,20)
 km(16,20)=d32/d945*(4*pr*y1**2+d4*x1**2*pr-d54*x5*x1+d31*y9*y1+          &
   d62*x9*x1-d27*y5*y1-d2*y1**2-d4*x1**2-d2*y5**2+d31*y5*y9-d31*y9**2+    &
   d4*pr*y5**2+d62*pr*y9**2-d4*x5**2+d62*x5*x9-d62*x9**2+d4*x5**2*pr+     &
   d62*x9**2*pr-d62*pr*y5*y9+d54*pr*y5*y1-d62*pr*y9*y1-d62*x5*x9*pr-      &
   d62*x9*x1*pr+d54*x5*x1*pr)*cf
 km(16,21)=km(15,22)
 km(16,22)=d8/d945*(8*pr*y1**2+d8*x1**2*pr+d86*x5*x1-d35*y9*y1-           &
   d70*x9*x1+d43*y5*y1-d4*y1**2-d8*x1**2+d6*y5**2-d55*y5*y9+d45*y9**2-    &
   d12*pr*y5**2-d90*pr*y9**2+d12*x5**2-d110*x5*x9+d90*x9**2-d12*x5**2*pr- &
   d90*x9**2*pr+d110*pr*y5*y9-d86*pr*y5*y1+d70*pr*y9*y1+d110*x5*x9*pr+    &
   d70*x9*x1*pr-d86*x5*x1*pr)*cf
 km(16,23)=km(15,24)
 km(16,24)=d32/d945*(-d6*x5*x1+d3*y9*y1+d6*x9*x1-d3*y5*y1-d2*y5**2+       &
   d7*y5*y9-d5*y9**2+d4*pr*y5**2+d10*pr*y9**2-d4*x5**2+d14*x5*x9-         &
   d10*x9**2+d4*x5**2*pr+d10*x9**2*pr-d14*pr*y5*y9+d6*pr*y5*y1-           &
   d6*pr*y9*y1-d14*x5*x9*pr-d6*x9*x1*pr+d6*x5*x1*pr)*cf
 km(16,25)=km(15,26)
 km(16,26)=-d64/d945*(2*pr*y1**2+d2*x1**2*pr+d14*x5*x1-d5*y9*y1-          &
   d10*x9*x1+d7*y5*y1-y1**2-d2*x1**2-d6*y5**2+d5*y5*y9+d12*pr*y5**2-      &
   d12*x5**2+d10*x5*x9+d12*x5**2*pr-d10*pr*y5*y9-d14*pr*y5*y1+            &
   d10*pr*y9*y1-d10*x5*x9*pr+d10*x9*x1*pr-d14*x5*x1*pr)*cf
 km(16,27)=km(15,26)
 km(16,28)=km(16,26)
 km(16,29)=km(15,30)
 km(16,30)=-d64/d945*(2*pr*y1**2+d2*x1**2*pr-d34*x5*x1+d19*y9*y1+         &
   d38*x9*x1-d17*y5*y1-y1**2-d2*x1**2+d18*y5**2-d19*y5*y9-d36*pr*y5**2+   &
   d36*x5**2-d38*x5*x9-d36*x5**2*pr+d38*pr*y5*y9+d34*pr*y5*y1-            &
   d38*pr*y9*y1+d38*x5*x9*pr-d38*x9*x1*pr+d34*x5*x1*pr)*cf
!--------------------------------------------------------------------------
 km(17,17)=-d47/d252*(2*pr*y1**2+d2*x1**2*pr+d2*x5*x1+d4*y5*y1-           &
   d2*y1**2-x1**2-d2*y5**2+d2*pr*y5**2-x5**2+d2*x5**2*pr-d4*pr*y5*y1-     &
   d4*x5*x1*pr)*cf
 km(17,18)=-d47/d252*(x1-x5)*(y1-y5)*cf
 km(17,19)=-d4/d945*(30*pr*y1**2+d30*x1**2*pr-d62*x5*x1+d184*y9*y1+       &
   d92*x9*x1-d124*y5*y1-d30*y1**2-d15*x1**2+d154*y5**2-d184*y5*y9-        &
   d154*pr*y5**2+d77*x5**2-d92*x5*x9-d154*x5**2*pr+d184*pr*y5*y9+         &
   d124*pr*y5*y1-d184*pr*y9*y1+d184*x5*x9*pr-d184*x9*x1*pr+               &
   d124*x5*x1*pr)*cf
 km(17,20)=d4/d945*(184*pr*x5*y9-d184*pr*y5*x9-d15*x1*y1+d15*y1*x5-       &
   d77*x1*y5+d92*x1*y9-d92*x5*y9+d77*y5*x5+d184*x9*y1*pr-d184*x5*y1*pr+   &
   d184*pr*y5*x1-d184*pr*y9*x1)*cf
 km(17,21)=-(40*pr*y1**2+d40*x1**2*pr+d241*x5*x1-d402*y9*y1-d201*x9*x1+   &
   d482*y5*y1-d40*y1**2-d20*x1**2-d442*y5**2+d402*y5*y9+d442*pr*y5**2-    &
   d221*x5**2+d201*x5*x9+d442*x5**2*pr-d402*pr*y5*y9-d482*pr*y5*y1+       &
   d402*pr*y9*y1-d402*x5*x9*pr+d402*x9*x1*pr-d482*x5*x1*pr)/d945*cf
 km(17,22)=-(402*pr*x5*y9-d402*pr*y5*x9+d20*x1*y1-d20*y1*x5-d221*x1*y5+   &
   d201*x1*y9-d201*x5*y9+d221*y5*x5+d402*x9*y1*pr-d402*x5*y1*pr+          &
   d402*pr*y5*x1-d402*pr*y9*x1)/d945*cf
 km(17,23)=d4/d945*(10*pr*y1**2+d10*x1**2*pr+d34*x5*x1-d48*y9*y1-         &
   d24*x9*x1+d68*y5*y1-d10*y1**2-d5*x1**2-d58*y5**2+d48*y5*y9+            &
   d58*pr*y5**2-d29*x5**2+d24*x5*x9+d58*x5**2*pr-d48*pr*y5*y9-            &
   d68*pr*y5*y1+d48*pr*y9*y1-d48*x5*x9*pr+d48*x9*x1*pr-d68*x5*x1*pr)*cf
 km(17,24)=d4/d945*(48*pr*x5*y9-d48*pr*y5*x9+d5*x1*y1-d5*y1*x5-           &
   d29*x1*y5+d24*x1*y9-d24*x5*y9+d29*y5*x5+d48*x9*y1*pr-d48*x5*y1*pr+     &
   d48*pr*y5*x1-d48*pr*y9*x1)*cf
 km(17,25)=d8/d189*(2*pr*y1**2+d2*x1**2*pr+d2*x5*x1+d4*y5*y1-             &
   d2*y1**2-x1**2-d2*y5**2+d2*pr*y5**2-x5**2+d2*x5**2*pr-d4*pr*y5*y1-     &
   d4*x5*x1*pr)*cf
 km(17,26)=d8/d189*(x1*y1-y1*x5-x1*y5+y5*x5)*cf
 km(17,27)=km(17,25)
 km(17,28)=km(17,26)
 km(17,29)=km(17,25)
 km(17,30)=km(17,26)
!--------------------------------------------------------------------------
 km(18,18)=-d47/d252*(2*pr*y1**2+d2*x1**2*pr+d4*x5*x1+d2*y5*y1-y1**2-     &
   d2*x1**2-y5**2+d2*pr*y5**2-d2*x5**2+d2*x5**2*pr-d4*pr*y5*y1-           &
   d4*x5*x1*pr)*cf
 km(18,19)=-d4/d945*(184*pr*x5*y9-d184*pr*y5*x9+d15*x1*y1+d77*y1*x5-      &
   d92*y1*x9+d92*y5*x9-d15*x1*y5-d77*y5*x5+d184*x9*y1*pr-d184*x5*y1*pr+   &
   d184*pr*y5*x1-d184*pr*y9*x1)*cf
 km(18,20)=-d4/d945*(30*pr*y1**2+d30*x1**2*pr-d124*x5*x1+d92*y9*y1+       &
   d184*x9*x1-d62*y5*y1-d15*y1**2-d30*x1**2+d77*y5**2-d92*y5*y9-          &
   d154*pr*y5**2+d154*x5**2-d184*x5*x9-d154*x5**2*pr+d184*pr*y5*y9+       &
   d124*pr*y5*y1-d184*pr*y9*y1+d184*x5*x9*pr-d184*x9*x1*pr+               &
   d124*x5*x1*pr)*cf
 km(18,21)=(402*pr*x5*y9-d402*pr*y5*x9-d20*x1*y1+d221*y1*x5-d201*y1*x9+   &
   d201*y5*x9+d20*x1*y5-d221*y5*x5+d402*x9*y1*pr-d402*x5*y1*pr+           &
   d402*pr*y5*x1-d402*pr*y9*x1)/d945*cf
 km(18,22)=-(40*pr*y1**2+d40*x1**2*pr+d482*x5*x1-d201*y9*y1-d402*x9*x1+   &
   d241*y5*y1-d20*y1**2-d40*x1**2-d221*y5**2+d201*y5*y9+d442*pr*y5**2-    &
   d442*x5**2+d402*x5*x9+d442*x5**2*pr-d402*pr*y5*y9-d482*pr*y5*y1+       &
   d402*pr*y9*y1-d402*x5*x9*pr+d402*x9*x1*pr-d482*x5*x1*pr)/d945*cf
 km(18,23)=-d4/d945*(48*pr*x5*y9-d48*pr*y5*x9-d5*x1*y1+d29*y1*x5-         &
   d24*y1*x9+d24*y5*x9+d5*x1*y5-d29*y5*x5+d48*x9*y1*pr-d48*x5*y1*pr+      &
   d48*pr*y5*x1-d48*pr*y9*x1)*cf
 km(18,24)=d4/d945*(10*pr*y1**2+d10*x1**2*pr+d68*x5*x1-d24*y9*y1-         &
   d48*x9*x1+d34*y5*y1-d5*y1**2-d10*x1**2-d29*y5**2+d24*y5*y9+            &
   d58*pr*y5**2-d58*x5**2+d48*x5*x9+d58*x5**2*pr-d48*pr*y5*y9-            &
   d68*pr*y5*y1+d48*pr*y9*y1-d48*x5*x9*pr+d48*x9*x1*pr-d68*x5*x1*pr)*cf
 km(18,25)=km(17,26)
 km(18,26)=d8/d189*(2*pr*y1**2+d2*x1**2*pr+d4*x5*x1+d2*y5*y1-y1**2-       &
   d2*x1**2-y5**2+d2*pr*y5**2-d2*x5**2+d2*x5**2*pr-d4*pr*y5*y1-           &
   d4*x5*x1*pr)*cf
 km(18,27)=km(17,26)
 km(18,28)=km(18,26)
 km(18,29)=km(17,26)
 km(18,30)=km(18,26)
!--------------------------------------------------------------------------
 km(19,19)=km(15,15)
 km(19,20)=km(15,16)
 km(19,21)=d8/d945*(78*pr*y1**2+d78*x1**2*pr+d109*x5*x1-d62*y9*y1-        &
   d31*x9*x1+d218*y5*y1-d78*y1**2-d39*x1**2-d230*y5**2+d242*y5*y9-        &
   d90*y9**2+d230*pr*y5**2+d90*pr*y9**2-d115*x5**2+d121*x5*x9-d45*x9**2+  &
   d230*x5**2*pr+d90*x9**2*pr-d242*pr*y5*y9-d218*pr*y5*y1+d62*pr*y9*y1-   &
   d242*x5*x9*pr+d62*x9*x1*pr-d218*x5*x1*pr)*cf
 km(19,22)=d4/d945*(264*pr*x5*y9-d264*pr*y5*x9+d78*x1*y1-d43*y1*x5-       &
   d35*y1*x9-d55*y5*x9-d175*x1*y5+d97*x1*y9-d187*x5*y9+d230*y5*x5+        &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1+d90*y9*x9)*cf
 km(19,23)=-d32/d189*(2*pr*y1**2+d2*x1**2*pr+d3*x5*x1-d2*y9*y1-x9*x1+     &
   d6*y5*y1-d2*y1**2-x1**2-d6*y5**2+d6*y5*y9-d2*y9**2+d6*pr*y5**2+        &
   d2*pr*y9**2-d3*x5**2+d3*x5*x9-x9**2+d6*x5**2*pr+d2*x9**2*pr-           &
   d6*pr*y5*y9-d6*pr*y5*y1+d2*pr*y9*y1-d6*x5*x9*pr+d2*x9*x1*pr-           &
   d6*x5*x1*pr)*cf
 km(19,24)=-d16/d945*(32*pr*x5*y9-d32*pr*y5*x9+d10*x1*y1-d7*y1*x5-        &
   d3*y1*x9-d7*y5*x9-d23*x1*y5+d13*x1*y9-d23*x5*y9+d30*y5*x5+             &
   d32*x9*y1*pr-d32*x5*y1*pr+d32*pr*y5*x1-d32*pr*y9*x1+d10*y9*x9)*cf
 km(19,25)=-d64/d945*(12*pr*y1**2+d12*x1**2*pr+d7*x5*x1+d10*y9*y1+        &
   d5*x9*x1+d14*y5*y1-d12*y1**2-d6*x1**2-d2*y5**2-d10*y5*y9+              &
   d2*pr*y5**2-x5**2-d5*x5*x9+d2*x5**2*pr+d10*pr*y5*y9-d14*pr*y5*y1-      &
   d10*pr*y9*y1+d10*x5*x9*pr-d10*x9*x1*pr-d14*x5*x1*pr)*cf
 km(19,26)=-d32/d945*(12*x1*y1-d7*y1*x5-d5*y1*x9+d5*y5*x9-d7*x1*y5-       &
   d5*x1*y9+d5*x5*y9+d2*y5*x5)*cf
 km(19,27)=km(19,25)
 km(19,28)=km(19,26)
 km(19,29)=d64/d945*(36*pr*y1**2+d36*x1**2*pr+d17*x5*x1+d38*y9*y1+        &
   d19*x9*x1+d34*y5*y1-d36*y1**2-d18*x1**2+d2*y5**2-d38*y5*y9-            &
   d2*pr*y5**2+x5**2-d19*x5*x9-d2*x5**2*pr+d38*pr*y5*y9-d34*pr*y5*y1-     &
   d38*pr*y9*y1+d38*x5*x9*pr-d38*x9*x1*pr-d34*x5*x1*pr)*cf
 km(19,30)=d32/d945*(36*x1*y1-d17*y1*x5-d19*y1*x9+d19*y5*x9-d17*x1*y5-    &
   d19*x1*y9+d19*x5*y9-d2*y5*x5)*cf
!--------------------------------------------------------------------------
 km(20,20)=km(16,16)
 km(20,21)=-d4/d945*(264*pr*x5*y9-d264*pr*y5*x9-d78*x1*y1+d175*y1*x5-     &
   d97*y1*x9+d187*y5*x9+d43*x1*y5+d35*x1*y9+d55*x5*y9-d230*y5*x5+         &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1-d90*y9*x9)*cf
 km(20,22)=d8/d945*(78*pr*y1**2+d78*x1**2*pr+d218*x5*x1-d31*y9*y1-        &
   d62*x9*x1+d109*y5*y1-d39*y1**2-d78*x1**2-d115*y5**2+d121*y5*y9-        &
   d45*y9**2+d230*pr*y5**2+d90*pr*y9**2-d230*x5**2+d242*x5*x9-d90*x9**2+  &
   d230*x5**2*pr+d90*x9**2*pr-d242*pr*y5*y9-d218*pr*y5*y1+d62*pr*y9*y1-   &
   d242*x5*x9*pr+d62*x9*x1*pr-d218*x5*x1*pr)*cf
 km(20,23)=d16/d945*(32*pr*x5*y9-d32*pr*y5*x9-d10*x1*y1+d23*y1*x5-        &
   d13*y1*x9+d23*y5*x9+d7*x1*y5+d3*x1*y9+d7*x5*y9-d30*y5*x5+d32*x9*y1*pr- &
   d32*x5*y1*pr+d32*pr*y5*x1-d32*pr*y9*x1-d10*y9*x9)*cf
 km(20,24)=-d32/d189*(2*pr*y1**2+d2*x1**2*pr+d6*x5*x1-y9*y1-d2*x9*x1+     &
   d3*y5*y1-y1**2-d2*x1**2-d3*y5**2+d3*y5*y9-y9**2+d6*pr*y5**2+           &
   d2*pr*y9**2-d6*x5**2+d6*x5*x9-d2*x9**2+d6*x5**2*pr+d2*x9**2*pr-        &
   d6*pr*y5*y9-d6*pr*y5*y1+d2*pr*y9*y1-d6*x5*x9*pr+d2*x9*x1*pr-           &
   d6*x5*x1*pr)*cf
 km(20,25)=km(19,26)
 km(20,26)=-d64/d945*(12*pr*y1**2+d12*x1**2*pr+d14*x5*x1+d5*y9*y1+        &
   d10*x9*x1+d7*y5*y1-d6*y1**2-d12*x1**2-y5**2-d5*y5*y9+d2*pr*y5**2-      &
   d2*x5**2-d10*x5*x9+d2*x5**2*pr+d10*pr*y5*y9-d14*pr*y5*y1-d10*pr*y9*y1+ &
   d10*x5*x9*pr-d10*x9*x1*pr-d14*x5*x1*pr)*cf
 km(20,27)=km(19,26)
 km(20,28)=km(20,26)
 km(20,29)=km(19,30)
 km(20,30)=d64/d945*(36*pr*y1**2+d36*x1**2*pr+d34*x5*x1+d19*y9*y1+        &
   d38*x9*x1+d17*y5*y1-d18*y1**2-d36*x1**2+y5**2-d19*y5*y9-d2*pr*y5**2+   &
   d2*x5**2-d38*x5*x9-d2*x5**2*pr+d38*pr*y5*y9-d34*pr*y5*y1-d38*pr*y9*y1+ &
   d38*x5*x9*pr-d38*x9*x1*pr-d34*x5*x1*pr)*cf
!--------------------------------------------------------------------------
 km(21,21)=-d2/d315*(386*pr*y1**2+d386*x1**2*pr+d233*x5*x1+d306*y9*y1+    &
   d153*x9*x1+d466*y5*y1-d386*y1**2-d193*x1**2-d466*y5**2+d466*y5*y9-     &
   d386*y9**2+d466*pr*y5**2+d386*pr*y9**2-d233*x5**2+d233*x5*x9-          &
   d193*x9**2+d466*x5**2*pr+d386*x9**2*pr-d466*pr*y5*y9-d466*pr*y5*y1-    &
   d306*pr*y9*y1-d466*x5*x9*pr-d306*x9*x1*pr-d466*x5*x1*pr)*cf
 km(21,22)=-(386*x1*y1-d233*y1*x5-d153*y1*x9-d233*y5*x9-d233*x1*y5-       &
   d153*x1*y9-d233*x5*y9+d466*y5*x5+d386*y9*x9)/d315*cf
 km(21,23)=d8/d945*(90*pr*y1**2+d90*x1**2*pr+d121*x5*x1-d62*y9*y1-        &
   d31*x9*x1+d242*y5*y1-d90*y1**2-d45*x1**2-d230*y5**2+d218*y5*y9-        &
   d78*y9**2+d230*pr*y5**2+d78*pr*y9**2-d115*x5**2+d109*x5*x9-d39*x9**2+  &
   d230*x5**2*pr+d78*x9**2*pr-d218*pr*y5*y9-d242*pr*y5*y1+d62*pr*y9*y1-   &
   d218*x5*x9*pr+d62*x9*x1*pr-d242*x5*x1*pr)*cf
 km(21,24)=d4/d945*(264*pr*x5*y9-d264*pr*y5*x9+d90*x1*y1-d55*y1*x5-       &
   d35*y1*x9-d43*y5*x9-d187*x1*y5+d97*x1*y9-d175*x5*y9+d230*y5*x5+        &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1+d78*y9*x9)*cf
 km(21,25)=d16/d315*(56*pr*y1**2+d56*x1**2*pr+d33*x5*x1+d46*y9*y1+        &
   d23*x9*x1+d66*y5*y1-d56*y1**2-d28*x1**2-d2*y5**2-d62*y5*y9+d8*y9**2+   &
   d2*pr*y5**2-d8*pr*y9**2-x5**2-d31*x5*x9+d4*x9**2+d2*x5**2*pr-          &
   d8*x9**2*pr+d62*pr*y5*y9-d66*pr*y5*y1-d46*pr*y9*y1+d62*x5*x9*pr-       &
   d46*x9*x1*pr-d66*x5*x1*pr)*cf
 km(21,26)=d8/d315*(56*x1*y1-d33*y1*x5-d23*y1*x9+d31*y5*x9-d33*x1*y5-     &
   d23*x1*y9+d31*x5*y9+d2*y5*x5-d8*y9*x9)*cf
 km(21,27)=-d16/d315*(8*pr*y1**2+d8*x1**2*pr-x5*x1+d18*y9*y1+d9*x9*x1-    &
   d2*y5*y1-d8*y1**2-d4*x1**2+d2*y5**2-d2*y5*y9-d8*y9**2-d2*pr*y5**2+     &
   d8*pr*y9**2+x5**2-x5*x9-d4*x9**2-d2*x5**2*pr+d8*x9**2*pr+d2*pr*y5*y9+  &
   d2*pr*y5*y1-d18*pr*y9*y1+d2*x5*x9*pr-d18*x9*x1*pr+d2*x5*x1*pr)*cf
 km(21,28)=-d8/d315*(8*x1*y1+y1*x5-d9*y1*x9+y5*x9+x1*y5-d9*x1*y9+x5*y9-   &
    d2*y5*x5+d8*y9*x9)*cf
 km(21,29)=-d16/d315*(8*pr*y1**2+d8*x1**2*pr+d31*x5*x1-d46*y9*y1-         &
   d23*x9*x1+d62*y5*y1-d8*y1**2-d4*x1**2+d2*y5**2-d66*y5*y9+d56*y9**2-    &
   d2*pr*y5**2-d56*pr*y9**2+x5**2-d33*x5*x9+d28*x9**2-d2*x5**2*pr-        &
   d56*x9**2*pr+d66*pr*y5*y9-d62*pr*y5*y1+d46*pr*y9*y1+d66*x5*x9*pr+      &
   d46*x9*x1*pr-d62*x5*x1*pr)*cf
 km(21,30)=-d8/d315*(8*x1*y1-d31*y1*x5+d23*y1*x9+d33*y5*x9-d31*x1*y5+     &
   d23*x1*y9+d33*x5*y9-d2*y5*x5-d56*y9*x9)*cf
!--------------------------------------------------------------------------
 km(22,22)=-d2/d315*(386*pr*y1**2+d386*x1**2*pr+d466*x5*x1+d153*y9*y1+    &
   d306*x9*x1+d233*y5*y1-d193*y1**2-d386*x1**2-d233*y5**2+d233*y5*y9-     &
   d193*y9**2+d466*pr*y5**2+d386*pr*y9**2-d466*x5**2+d466*x5*x9-          &
   d386*x9**2+d466*x5**2*pr+d386*x9**2*pr-d466*pr*y5*y9-d466*pr*y5*y1-    &
   d306*pr*y9*y1-d466*x5*x9*pr-d306*x9*x1*pr-d466*x5*x1*pr)*cf
 km(22,23)=-d4/d945*(264*pr*x5*y9-d264*pr*y5*x9-d90*x1*y1+d187*y1*x5-     &
   d97*y1*x9+d175*y5*x9+d55*x1*y5+d35*x1*y9+d43*x5*y9-d230*y5*x5+         &
   d264*x9*y1*pr-d264*x5*y1*pr+d264*pr*y5*x1-d264*pr*y9*x1-d78*y9*x9)*cf
 km(22,24)=d8/d945*(90*pr*y1**2+d90*x1**2*pr+d242*x5*x1-d31*y9*y1-        &
   d62*x9*x1+d121*y5*y1-d45*y1**2-d90*x1**2-d115*y5**2+d109*y5*y9-        &
   d39*y9**2+d230*pr*y5**2+d78*pr*y9**2-d230*x5**2+d218*x5*x9-d78*x9**2+  &
   d230*x5**2*pr+d78*x9**2*pr-d218*pr*y5*y9-d242*pr*y5*y1+d62*pr*y9*y1-   &
   d218*x5*x9*pr+d62*x9*x1*pr-d242*x5*x1*pr)*cf
 km(22,25)=km(21,26)
 km(22,26)=d16/d315*(56*pr*y1**2+d56*x1**2*pr+d66*x5*x1+d23*y9*y1+        &
   d46*x9*x1+d33*y5*y1-d28*y1**2-d56*x1**2-y5**2-d31*y5*y9+d4*y9**2+      &
   d2*pr*y5**2-d8*pr*y9**2-d2*x5**2-d62*x5*x9+d8*x9**2+d2*x5**2*pr-       &
   d8*x9**2*pr+d62*pr*y5*y9-d66*pr*y5*y1-d46*pr*y9*y1+d62*x5*x9*pr-       &
   d46*x9*x1*pr-d66*x5*x1*pr)*cf
 km(22,27)=km(21,28)
 km(22,28)=-d16/d315*(8*pr*y1**2+d8*x1**2*pr-d2*x5*x1+d9*y9*y1+           &
   d18*x9*x1-y5*y1-d4*y1**2-d8*x1**2+y5**2-y5*y9-d4*y9**2-d2*pr*y5**2+    &
   d8*pr*y9**2+d2*x5**2-d2*x5*x9-d8*x9**2-d2*x5**2*pr+d8*x9**2*pr+        &
   d2*pr*y5*y9+d2*pr*y5*y1-d18*pr*y9*y1+d2*x5*x9*pr-d18*x9*x1*pr+         &
   d2*x5*x1*pr)*cf
 km(22,29)=km(21,30)
 km(22,30)=-d16/d315*(8*pr*y1**2+d8*x1**2*pr+d62*x5*x1-d23*y9*y1-         &
   d46*x9*x1+d31*y5*y1-d4*y1**2-d8*x1**2+y5**2-d33*y5*y9+d28*y9**2-       &
   d2*pr*y5**2-d56*pr*y9**2+d2*x5**2-d66*x5*x9+d56*x9**2-d2*x5**2*pr-     &
   d56*x9**2*pr+d66*pr*y5*y9-d62*pr*y5*y1+d46*pr*y9*y1+d66*x5*x9*pr+      &
   d46*x9*x1*pr-d62*x5*x1*pr)*cf
!--------------------------------------------------------------------------
 km(23,23)=km(3,3)
 km(23,24)=km(3,4)
 km(23,25)=d64/d945*(-d19*x5*x1+d38*y9*y1+d19*x9*x1-d38*y5*y1+d2*y5**2+   &
   d34*y5*y9-d36*y9**2-d2*pr*y5**2+d36*pr*y9**2+x5**2+d17*x5*x9-          &
   d18*x9**2-d2*x5**2*pr+d36*x9**2*pr-d34*pr*y5*y9+d38*pr*y5*y1-          &
   d38*pr*y9*y1-d34*x5*x9*pr-d38*x9*x1*pr+d38*x5*x1*pr)*cf
 km(23,26)=d32/d945*(19*y1*x5-d19*y1*x9-d17*y5*x9+d19*x1*y5-d19*x1*y9-    &
   d17*x5*y9-d2*y5*x5+d36*y9*x9)*cf
 km(23,27)=-d64/d945*(-d5*x5*x1+d10*y9*y1+d5*x9*x1-d10*y5*y1-d2*y5**2+    &
   d14*y5*y9-d12*y9**2+d2*pr*y5**2+d12*pr*y9**2-x5**2+d7*x5*x9-d6*x9**2+  &
   d2*x5**2*pr+d12*x9**2*pr-d14*pr*y5*y9+d10*pr*y5*y1-d10*pr*y9*y1-       &
   d14*x5*x9*pr-d10*x9*x1*pr+d10*x5*x1*pr)*cf
 km(23,28)=-d32/d945*(5*y1*x5-d5*y1*x9-d7*y5*x9+d5*x1*y5-d5*x1*y9-        &
   d7*x5*y9+d2*y5*x5+d12*y9*x9)*cf
 km(23,29)=km(23,27)
 km(23,30)=km(23,28)
!--------------------------------------------------------------------------
 km(24,24)=km(4,4)
 km(24,25)=km(23,26)
 km(24,26)=d64/d945*(-d38*x5*x1+d19*y9*y1+d38*x9*x1-d19*y5*y1+y5**2+      &
   d17*y5*y9-d18*y9**2-d2*pr*y5**2+d36*pr*y9**2+d2*x5**2+d34*x5*x9-       &
   d36*x9**2-d2*x5**2*pr+d36*x9**2*pr-d34*pr*y5*y9+d38*pr*y5*y1-          &
   d38*pr*y9*y1-d34*x5*x9*pr-d38*x9*x1*pr+d38*x5*x1*pr)*cf
 km(24,27)=km(23,28)
 km(24,28)=-d64/d945*(-d10*x5*x1+d5*y9*y1+d10*x9*x1-d5*y5*y1-y5**2+       &
   d7*y5*y9-d6*y9**2+d2*pr*y5**2+d12*pr*y9**2-d2*x5**2+d14*x5*x9-         &
   d12*x9**2+d2*x5**2*pr+d12*x9**2*pr-d14*pr*y5*y9+d10*pr*y5*y1-          &
   d10*pr*y9*y1-d14*x5*x9*pr-d10*x9*x1*pr+d10*x5*x1*pr)*cf
 km(24,29)=km(23,28)
 km(24,30)=km(24,28)
!--------------------------------------------------------------------------
 km(25,25)=-d128/d45*(2*pr*y1**2+d2*x1**2*pr+x5*x1+d2*y9*y1+x9*x1+        &
   d2*y5*y1-d2*y1**2-x1**2-d2*y5**2+d2*y5*y9-d2*y9**2+d2*pr*y5**2+        &
   d2*pr*y9**2-x5**2+x5*x9-x9**2+d2*x5**2*pr+d2*x9**2*pr-d2*pr*y5*y9-     &
   d2*pr*y5*y1-d2*pr*y9*y1-d2*x5*x9*pr-d2*x9*x1*pr-d2*x5*x1*pr)*cf
 km(25,26)=-d64/d45*(2*x1*y1-y1*x5-y1*x9-y5*x9-x1*y5-x1*y9-x5*y9+         &
   d2*y5*x5+d2*y9*x9)*cf
 km(25,27)=d128/d315*(2*pr*y1**2+d2*x1**2*pr-d3*x5*x1+d10*y9*y1+          &
   d5*x9*x1-d6*y5*y1-d2*y1**2-x1**2-d2*y5**2+d10*y5*y9-d10*y9**2+         &
   d2*pr*y5**2+d10*pr*y9**2-x5**2+d5*x5*x9-d5*x9**2+d2*x5**2*pr+          &
   d10*x9**2*pr-d10*pr*y5*y9+d6*pr*y5*y1-d10*pr*y9*y1-d10*x5*x9*pr-       &
   d10*x9*x1*pr+d6*x5*x1*pr)*cf
 km(25,28)=d64/d315*(2*x1*y1+d3*y1*x5-d5*y1*x9-d5*y5*x9+d3*x1*y5-         &
   d5*x1*y9-d5*x5*y9+d2*y5*x5+d10*y9*x9)*cf
 km(25,29)=d128/d315*(2*pr*y1**2+d2*x1**2*pr+d5*x5*x1-d6*y9*y1-           &
   d3*x9*x1+d10*y5*y1-d2*y1**2-x1**2-d10*y5**2+d10*y5*y9-d2*y9**2+        &
   d10*pr*y5**2+d2*pr*y9**2-d5*x5**2+d5*x5*x9-x9**2+d10*x5**2*pr+         &
   d2*x9**2*pr-d10*pr*y5*y9-d10*pr*y5*y1+d6*pr*y9*y1-d10*x5*x9*pr+        &
   d6*x9*x1*pr-d10*x5*x1*pr)*cf
 km(25,30)=d64/d315*(2*x1*y1-d5*y1*x5+d3*y1*x9-d5*y5*x9-d5*x1*y5+         &
   d3*x1*y9-d5*x5*y9+d10*y5*x5+d2*y9*x9)*cf
!--------------------------------------------------------------------------
 km(26,26)=-d128/d45*(2*pr*y1**2+d2*x1**2*pr+d2*x5*x1+y9*y1+              &
   d2*x9*x1+y5*y1-y1**2-d2*x1**2-y5**2+y5*y9-y9**2+d2*pr*y5**2+           &
   d2*pr*y9**2-d2*x5**2+d2*x5*x9-d2*x9**2+d2*x5**2*pr+d2*x9**2*pr-        &
   d2*pr*y5*y9-d2*pr*y5*y1-d2*pr*y9*y1-d2*x5*x9*pr-d2*x9*x1*pr-           &
   d2*x5*x1*pr)*cf
 km(26,27)=km(25,28)
 km(26,28)=d128/d315*(2*pr*y1**2+d2*x1**2*pr-d6*x5*x1+d5*y9*y1+           &
   d10*x9*x1-d3*y5*y1-y1**2-d2*x1**2-y5**2+d5*y5*y9-d5*y9**2+d2*pr*y5**2+ &
   d10*pr*y9**2-d2*x5**2+d10*x5*x9-d10*x9**2+d2*x5**2*pr+d10*x9**2*pr-    &
   d10*pr*y5*y9+d6*pr*y5*y1-d10*pr*y9*y1-d10*x5*x9*pr-d10*x9*x1*pr+       &
   d6*x5*x1*pr)*cf
 km(26,29)=km(25,30)
 km(26,30)=d128/d315*(2*pr*y1**2+d2*x1**2*pr+d10*x5*x1-d3*y9*y1-          &
   d6*x9*x1+d5*y5*y1-y1**2-d2*x1**2-d5*y5**2+d5*y5*y9-y9**2+d10*pr*y5**2+ &
   d2*pr*y9**2-d10*x5**2+d10*x5*x9-d2*x9**2+d10*x5**2*pr+d2*x9**2*pr-     &
   d10*pr*y5*y9-d10*pr*y5*y1+d6*pr*y9*y1-d10*x5*x9*pr+d6*x9*x1*pr-        &
   d10*x5*x1*pr)*cf
!--------------------------------------------------------------------------
 km(27,27)=km(25,25)
 km(27,28)=km(25,26)
 km(27,29)=d128/d315*(10*pr*y1**2+d10*x1**2*pr+d5*x5*x1+d10*y9*y1+        &
   d5*x9*x1+d10*y5*y1-d10*y1**2-d5*x1**2-d2*y5**2-d6*y5*y9-d2*y9**2+      &
   d2*pr*y5**2+d2*pr*y9**2-x5**2-d3*x5*x9-x9**2+d2*x5**2*pr+d2*x9**2*pr+  &
   d6*pr*y5*y9-d10*pr*y5*y1-d10*pr*y9*y1+d6*x5*x9*pr-d10*x9*x1*pr-        &
   d10*x5*x1*pr)*cf
 km(27,30)=d64/d315*(10*x1*y1-d5*y1*x5-d5*y1*x9+d3*y5*x9-d5*x1*y5-        &
   d5*x1*y9+d3*x5*y9+d2*y5*x5+d2*y9*x9)*cf
!--------------------------------------------------------------------------
 km(28,28)=km(26,26)
 km(28,29)=km(27,30)
 km(28,30)=d128/d315*(10*pr*y1**2+d10*x1**2*pr+d10*x5*x1+d5*y9*y1+        &
   d10*x9*x1+d5*y5*y1-d5*y1**2-d10*x1**2-y5**2-d3*y5*y9-y9**2+            &
   d2*pr*y5**2+d2*pr*y9**2-d2*x5**2-d6*x5*x9-d2*x9**2+d2*x5**2*pr+        &
   d2*x9**2*pr+d6*pr*y5*y9-d10*pr*y5*y1-d10*pr*y9*y1+d6*x5*x9*pr-         &
   d10*x9*x1*pr-d10*x5*x1*pr)*cf
!--------------------------------------------------------------------------
 km(29,29)=km(25,25)
 km(29,30)=km(25,26)
!--------------------------------------------------------------------------
 km(30,30)=km(26,26)
!----------------------------LOWER TRIANGLE--------------------------------
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
 km(13,1)=km(1,13)
 km(13,2)=km(2,13)
 km(13,3)=km(3,13)
 km(13,4)=km(4,13)
 km(13,5)=km(5,13)
 km(13,6)=km(6,13)
 km(13,7)=km(7,13)
 km(13,8)=km(8,13)
 km(13,9)=km(9,13)
 km(13,10)=km(10,13)
 km(13,11)=km(11,13)
 km(13,12)=km(12,13)
 km(14,1)=km(1,14)
 km(14,2)=km(2,14)
 km(14,3)=km(3,14)
 km(14,4)=km(4,14)
 km(14,5)=km(5,14)
 km(14,6)=km(6,14)
 km(14,7)=km(7,14)
 km(14,8)=km(8,14)
 km(14,9)=km(9,14)
 km(14,10)=km(10,14)
 km(14,11)=km(11,14)
 km(14,12)=km(12,14)
 km(14,13)=km(13,14)
 km(15,1)=km(1,15)
 km(15,2)=km(2,15)
 km(15,3)=km(3,15)
 km(15,4)=km(4,15)
 km(15,5)=km(5,15)
 km(15,6)=km(6,15)
 km(15,7)=km(7,15)
 km(15,8)=km(8,15)
 km(15,9)=km(9,15)
 km(15,10)=km(10,15)
 km(15,11)=km(11,15)
 km(15,12)=km(12,15)
 km(15,13)=km(13,15)
 km(15,14)=km(14,15)
 km(16,1)=km(1,16)
 km(16,2)=km(2,16)
 km(16,3)=km(3,16)
 km(16,4)=km(4,16)
 km(16,5)=km(5,16)
 km(16,6)=km(6,16)
 km(16,7)=km(7,16)
 km(16,8)=km(8,16)
 km(16,9)=km(9,16)
 km(16,10)=km(10,16)
 km(16,11)=km(11,16)
 km(16,12)=km(12,16)
 km(16,13)=km(13,16)
 km(16,14)=km(14,16)
 km(16,15)=km(15,16)
 km(17,1)=km(1,17)
 km(17,2)=km(2,17)
 km(17,3)=km(3,17)
 km(17,4)=km(4,17)
 km(17,5)=km(5,17)
 km(17,6)=km(6,17)
 km(17,7)=km(7,17)
 km(17,8)=km(8,17)
 km(17,9)=km(9,17)
 km(17,10)=km(10,17)
 km(17,11)=km(11,17)
 km(17,12)=km(12,17)
 km(17,13)=km(13,17)
 km(17,14)=km(14,17)
 km(17,15)=km(15,17)
 km(17,16)=km(16,17)
 km(18,1)=km(1,18)
 km(18,2)=km(2,18)
 km(18,3)=km(3,18)
 km(18,4)=km(4,18)
 km(18,5)=km(5,18)
 km(18,6)=km(6,18)
 km(18,7)=km(7,18)
 km(18,8)=km(8,18)
 km(18,9)=km(9,18)
 km(18,10)=km(10,18)
 km(18,11)=km(11,18)
 km(18,12)=km(12,18)
 km(18,13)=km(13,18)
 km(18,14)=km(14,18)
 km(18,15)=km(15,18)
 km(18,16)=km(16,18)
 km(18,17)=km(17,18)
 km(19,1)=km(1,19)
 km(19,2)=km(2,19)
 km(19,3)=km(3,19)
 km(19,4)=km(4,19)
 km(19,5)=km(5,19)
 km(19,6)=km(6,19)
 km(19,7)=km(7,19)
 km(19,8)=km(8,19)
 km(19,9)=km(9,19)
 km(19,10)=km(10,19)
 km(19,11)=km(11,19)
 km(19,12)=km(12,19)
 km(19,13)=km(13,19)
 km(19,14)=km(14,19)
 km(19,15)=km(15,19)
 km(19,16)=km(16,19)
 km(19,17)=km(17,19)
 km(19,18)=km(18,19)
 km(20,1)=km(1,20)
 km(20,2)=km(2,20)
 km(20,3)=km(3,20)
 km(20,4)=km(4,20)
 km(20,5)=km(5,20)
 km(20,6)=km(6,20)
 km(20,7)=km(7,20)
 km(20,8)=km(8,20)
 km(20,9)=km(9,20)
 km(20,10)=km(10,20)
 km(20,11)=km(11,20)
 km(20,12)=km(12,20)
 km(20,13)=km(13,20)
 km(20,14)=km(14,20)
 km(20,15)=km(15,20)
 km(20,16)=km(16,20)
 km(20,17)=km(17,20)
 km(20,18)=km(18,20)
 km(20,19)=km(19,20)
 km(21,1)=km(1,21)
 km(21,2)=km(2,21)
 km(21,3)=km(3,21)
 km(21,4)=km(4,21)
 km(21,5)=km(5,21)
 km(21,6)=km(6,21)
 km(21,7)=km(7,21)
 km(21,8)=km(8,21)
 km(21,9)=km(9,21)
 km(21,10)=km(10,21)
 km(21,11)=km(11,21)
 km(21,12)=km(12,21)
 km(21,13)=km(13,21)
 km(21,14)=km(14,21)
 km(21,15)=km(15,21)
 km(21,16)=km(16,21)
 km(21,17)=km(17,21)
 km(21,18)=km(18,21)
 km(21,19)=km(19,21)
 km(21,20)=km(20,21)
 km(22,1)=km(1,22)
 km(22,2)=km(2,22)
 km(22,3)=km(3,22)
 km(22,4)=km(4,22)
 km(22,5)=km(5,22)
 km(22,6)=km(6,22)
 km(22,7)=km(7,22)
 km(22,8)=km(8,22)
 km(22,9)=km(9,22)
 km(22,10)=km(10,22)
 km(22,11)=km(11,22)
 km(22,12)=km(12,22)
 km(22,13)=km(13,22)
 km(22,14)=km(14,22)
 km(22,15)=km(15,22)
 km(22,16)=km(16,22)
 km(22,17)=km(17,22)
 km(22,18)=km(18,22)
 km(22,19)=km(19,22)
 km(22,20)=km(20,22)
 km(22,21)=km(21,22)
 km(23,1)=km(1,23)
 km(23,2)=km(2,23)
 km(23,3)=km(3,23)
 km(23,4)=km(4,23)
 km(23,5)=km(5,23)
 km(23,6)=km(6,23)
 km(23,7)=km(7,23)
 km(23,8)=km(8,23)
 km(23,9)=km(9,23)
 km(23,10)=km(10,23)
 km(23,11)=km(11,23)
 km(23,12)=km(12,23)
 km(23,13)=km(13,23)
 km(23,14)=km(14,23)
 km(23,15)=km(15,23)
 km(23,16)=km(16,23)
 km(23,17)=km(17,23)
 km(23,18)=km(18,23)
 km(23,19)=km(19,23)
 km(23,20)=km(20,23)
 km(23,21)=km(21,23)
 km(23,22)=km(22,23)
 km(24,1)=km(1,24)
 km(24,2)=km(2,24)
 km(24,3)=km(3,24)
 km(24,4)=km(4,24)
 km(24,5)=km(5,24)
 km(24,6)=km(6,24)
 km(24,7)=km(7,24)
 km(24,8)=km(8,24)
 km(24,9)=km(9,24)
 km(24,10)=km(10,24)
 km(24,11)=km(11,24)
 km(24,12)=km(12,24)
 km(24,13)=km(13,24)
 km(24,14)=km(14,24)
 km(24,15)=km(15,24)
 km(24,16)=km(16,24)
 km(24,17)=km(17,24)
 km(24,18)=km(18,24)
 km(24,19)=km(19,24)
 km(24,20)=km(20,24)
 km(24,21)=km(21,24)
 km(24,22)=km(22,24)
 km(24,23)=km(23,24)
 km(25,1)=km(1,25)
 km(25,2)=km(2,25)
 km(25,3)=km(3,25)
 km(25,4)=km(4,25)
 km(25,5)=km(5,25)
 km(25,6)=km(6,25)
 km(25,7)=km(7,25)
 km(25,8)=km(8,25)
 km(25,9)=km(9,25)
 km(25,10)=km(10,25)
 km(25,11)=km(11,25)
 km(25,12)=km(12,25)
 km(25,13)=km(13,25)
 km(25,14)=km(14,25)
 km(25,15)=km(15,25)
 km(25,16)=km(16,25)
 km(25,17)=km(17,25)
 km(25,18)=km(18,25)
 km(25,19)=km(19,25)
 km(25,20)=km(20,25)
 km(25,21)=km(21,25)
 km(25,22)=km(22,25)
 km(25,23)=km(23,25)
 km(25,24)=km(24,25)
 km(26,1)=km(1,26)
 km(26,2)=km(2,26)
 km(26,3)=km(3,26)
 km(26,4)=km(4,26)
 km(26,5)=km(5,26)
 km(26,6)=km(6,26)
 km(26,7)=km(7,26)
 km(26,8)=km(8,26)
 km(26,9)=km(9,26)
 km(26,10)=km(10,26)
 km(26,11)=km(11,26)
 km(26,12)=km(12,26)
 km(26,13)=km(13,26)
 km(26,14)=km(14,26)
 km(26,15)=km(15,26)
 km(26,16)=km(16,26)
 km(26,17)=km(17,26)
 km(26,18)=km(18,26)
 km(26,19)=km(19,26)
 km(26,20)=km(20,26)
 km(26,21)=km(21,26)
 km(26,22)=km(22,26)
 km(26,23)=km(23,26)
 km(26,24)=km(24,26)
 km(26,25)=km(25,26)
 km(27,1)=km(1,27)
 km(27,2)=km(2,27)
 km(27,3)=km(3,27)
 km(27,4)=km(4,27)
 km(27,5)=km(5,27)
 km(27,6)=km(6,27)
 km(27,7)=km(7,27)
 km(27,8)=km(8,27)
 km(27,9)=km(9,27)
 km(27,10)=km(10,27)
 km(27,11)=km(11,27)
 km(27,12)=km(12,27)
 km(27,13)=km(13,27)
 km(27,14)=km(14,27)
 km(27,15)=km(15,27)
 km(27,16)=km(16,27)
 km(27,17)=km(17,27)
 km(27,18)=km(18,27)
 km(27,19)=km(19,27)
 km(27,20)=km(20,27)
 km(27,21)=km(21,27)
 km(27,22)=km(22,27)
 km(27,23)=km(23,27)
 km(27,24)=km(24,27)
 km(27,25)=km(25,27)
 km(27,26)=km(26,27)
 km(28,1)=km(1,28)
 km(28,2)=km(2,28)
 km(28,3)=km(3,28)
 km(28,4)=km(4,28)
 km(28,5)=km(5,28)
 km(28,6)=km(6,28)
 km(28,7)=km(7,28)
 km(28,8)=km(8,28)
 km(28,9)=km(9,28)
 km(28,10)=km(10,28)
 km(28,11)=km(11,28)
 km(28,12)=km(12,28)
 km(28,13)=km(13,28)
 km(28,14)=km(14,28)
 km(28,15)=km(15,28)
 km(28,16)=km(16,28)
 km(28,17)=km(17,28)
 km(28,18)=km(18,28)
 km(28,19)=km(19,28)
 km(28,20)=km(20,28)
 km(28,21)=km(21,28)
 km(28,22)=km(22,28)
 km(28,23)=km(23,28)
 km(28,24)=km(24,28)
 km(28,25)=km(25,28)
 km(28,26)=km(26,28)
 km(28,27)=km(27,28)
 km(29,1)=km(1,29)
 km(29,2)=km(2,29)
 km(29,3)=km(3,29)
 km(29,4)=km(4,29)
 km(29,5)=km(5,29)
 km(29,6)=km(6,29)
 km(29,7)=km(7,29)
 km(29,8)=km(8,29)
 km(29,9)=km(9,29)
 km(29,10)=km(10,29)
 km(29,11)=km(11,29)
 km(29,12)=km(12,29)
 km(29,13)=km(13,29)
 km(29,14)=km(14,29)
 km(29,15)=km(15,29)
 km(29,16)=km(16,29)
 km(29,17)=km(17,29)
 km(29,18)=km(18,29)
 km(29,19)=km(19,29)
 km(29,20)=km(20,29)
 km(29,21)=km(21,29)
 km(29,22)=km(22,29)
 km(29,23)=km(23,29)
 km(29,24)=km(24,29)
 km(29,25)=km(25,29)
 km(29,26)=km(26,29)
 km(29,27)=km(27,29)
 km(29,28)=km(28,29)
 km(30,1)=km(1,30)
 km(30,2)=km(2,30)
 km(30,3)=km(3,30)
 km(30,4)=km(4,30)
 km(30,5)=km(5,30)
 km(30,6)=km(6,30)
 km(30,7)=km(7,30)
 km(30,8)=km(8,30)
 km(30,9)=km(9,30)
 km(30,10)=km(10,30)
 km(30,11)=km(11,30)
 km(30,12)=km(12,30)
 km(30,13)=km(13,30)
 km(30,14)=km(14,30)
 km(30,15)=km(15,30)
 km(30,16)=km(16,30)
 km(30,17)=km(17,30)
 km(30,18)=km(18,30)
 km(30,19)=km(19,30)
 km(30,20)=km(20,30)
 km(30,21)=km(21,30)
 km(30,22)=km(22,30)
 km(30,23)=km(23,30)
 km(30,24)=km(24,30)
 km(30,25)=km(25,30)
 km(30,26)=km(26,30)
 km(30,27)=km(27,30)
 km(30,28)=km(28,30)
 km(30,29)=km(29,30)
RETURN
END SUBROUTINE stiff15
 

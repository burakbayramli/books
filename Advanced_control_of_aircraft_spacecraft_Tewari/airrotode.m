% Program for specifying governing ODEs expressed as
% state equations for the 2PBVP (to be called by 'bvp4c.m')
% (c) 2009 Ashish Tewari
function dydx=airrotode(x,y)
global tf;
global J;
global Rbar;
P=y(1);Q=y(2);R=y(3);
q1=y(4);q2=y(5);q3=y(6);q4=y(7);
jx=J(1,1);jy=J(2,2);jz=J(3,3);jxz=J(1,3);
Jxz=J(1,3);
D=jx*jz-jxz^2;
j1=jxz*(jy-jx-jz)/D;
j2=(jxz^2+jz*(jy-jz))/D;
j3=jxz/jy; j4=(jz-jx)/jy;
j5=(jxz^2+jx*(jx-jy))/D;
j6=jxz*(jz-jy-jx)/D;
Fw=[j1*Q j1*P+j2*R j2*Q; 2*j3*P+j4*R 0 j4*P-2*j3*R
j5*Q j5*P+j6*R j6*Q];
Gw=0.5*[q4 -q3 q2; q3 q4 -q1;
-q2 q1 q4; -q1 -q2 -q3];
Gq=0.5*[0 R -Q P; -R 0 P Q;
Q -P 0 R; -P -Q -R 0];
Fu=[jz/D 0 -jxz/D; 0 1/jy 0; -jxz/D 0 jx/D];
if x<tf
  u=-0.5*inv(Rbar)*(Fu')*y(8:10,1);
else
  u=zeros(3,1);
end
dydx(1:3,1)=[j1*P*Q+j2*Q*R+(u(1,1)*jz-u(3,1)*jxz)/D;
j3*(P^2-R^2)+j4*P*R+u(2,1)/jy;
j5*P*Q+j6*Q*R+(u(3,1)*jx/D-u(1,1)*jxz)/D];
dydx(4:7,1)=0.5*Gq*y(4:7,1);
dydx(8:14,1)=-[Fw' Gw'; zeros(4,3) Gq']*y(8:14,1);
% Program for specifying boundary conditions for the 2PBVP.
% (To be called by 'bvp4c.m')
function res=airrotbc(ya,yb)
global wi;
global Qi;
res=[ya(1:3,1)-wi
ya(4:7,1)-Qi
yb(1:3,1)
yb(11:14,1)];
  

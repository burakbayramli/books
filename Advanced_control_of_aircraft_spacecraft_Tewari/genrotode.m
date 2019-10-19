% Program for specifying governing ODEs expressed as
% state equations for the 2PBVP (to be called by 'bvp4c.m')
function dydx=genrotode(x,y)
global tf;
global J;
global R;
wx=y(1);wy=y(2);wz=y(3);
q1=y(4);q2=y(5);q3=y(6);q4=y(7);
jx=(J(2,2)-J(3,3))/J(1,1);
jy=(J(3,3)-J(1,1))/J(2,2);
jz=(J(1,1)-J(2,2))/J(3,3);
Fw=[0 jx*wz jx*wy; jy*wz 0 jy*wx;
jz*wy jz*wx 0];
Gw=0.5*[q4 -q3 q2; q3 q4 -q1;
-q2 q1 q4; -q1 -q2 -q3];
Gq=0.5*[0 wz -wy wx; -wz 0 wx wy;
wy -wx 0 wz; -wx -wy -wz 0];
if x<tf
u=-0.5*inv(R)*inv(J)*y(8:10,1);
else
u=zeros(3,1);
end
dydx(1:3,1)=[jx*y(2)*y(3)+u(1,1)/J(1,1);
jy*y(1)*y(3)+u(2,1)/J(2,2);
jz*y(1)*y(2)+u(3,1)/J(3,3)];
dydx(4:7,1)=0.5*Gq*y(4:7,1);
dydx(8:14,1)=-[Fw' Gw'; zeros(4,3) Gq']*y(8:14,1);
% Program for specifying boundary conditions for the 2PBVP.
% (To be called by 'bvp4c.m')
function res=genrotbc(ya,yb)
global wi;
global wf;
global Qi;
global Qf;
res=[ya(1:3,1)-wi
ya(4:7,1)-Qi
yb(1:3,1)-wf
yb(4:7,1)-Qf];

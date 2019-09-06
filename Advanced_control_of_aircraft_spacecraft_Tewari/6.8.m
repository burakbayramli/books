global R1; R1=1;
global R2; R2=10;
global tf; tf=650;
dtr=pi/180;
solinit = bvpinit(linspace(0,tf,5),[30*dtr 6478 8 -0.1*dtr 0 0 0 0]);
sol = bvp4c(@normballisticode,@manentrybc,solinit);
x = linspace(0,tf);
y = deval(sol,x);
u1=-0.5*y(7,:)/R1;
u2=-0.5*y(8,:)./(R2*y(3,:));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
60 200
50 150
40 R 2 =2
30 R 2 =5
function res=manentrybc(ya,yb)
  mu=398600.4;
  r0=6378.14;
  dtr=pi/180;
  res=[ya(1)-30*dtr
       ya(2)-r0-100
       ya(3)-8
       ya(4)+0.1*dtr
       yb(1)-60*dtr
       yb(2)-r0-5
       yb(3)-2
       yb(8)];

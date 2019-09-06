function xdot=missile_state(t,x);
global T; % (nx1) vector of time points
global aT; % (nx3) matrix of target acceleration data
global kr;
global kv;
global M;
global CD;
global S;
global tb;
global m0;
global mf;
g=9.81;
rT=x(1:3,1);
vT=x(4:6,1);
rm=x(7:9,1);
vm=x(10:12,1);
ad=interp1(T,aT,t);
xdot(1:3,1)=vT;
xdot(4:6,1)=ad';
r=rm-rT;
v=vm-vT;
u=-kr*r+ad'-kv*v;
xdot(7:9,1)=vm;
m=m0-(m0-mf)*t/tb;
h=rm(3,1);
if h<0
h=0;
xdot(10:12,1)=zeros(3,1);
else
vel=norm(vm);
atmosp = atmosphere(h,vel,1.0);
rho = atmosp(2);
mach = atmosp(3);
Cd=interp1(M,CD,mach);
xdot(10:12,1)=u-[0 0 g]'-0.5*rho*norm(vm)*vm*S*Cd/m;
end

function [u,D,fT]=missile_state_u(t,x);
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
n=size(t,1);
u=[];D=[];fT=[];
for i=1:n-1
time=t(i,1);
timep1=t(i+1,1);
rT=x(i,1:3);
vT=x(i,4:6);
rm=x(i,7:9);
vm=x(i,10:12);
r=rm-rT;
v=vm-vT;
u(:,i)=-kr*r'-kv*v'+(x(i+1,10:12)-x(i,10:12))'/(timep1-time);
m=m0-(m0-mf)*time/tb;
atmosp = atmosphere(rm(1,3),norm(vm),1.0);
rho = atmosp(2);
mach = atmosp(3);
Cd=interp1(M,CD,mach);
drag=0.5*rho*norm(vm)*vm*S*Cd;
D(i,:)=drag;
fT(:,i)=-u(:,i)*m+[0 0 g]'+drag';
end

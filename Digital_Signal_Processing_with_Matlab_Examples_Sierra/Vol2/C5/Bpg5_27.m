%observer example
%state space system model (2 tank system):
A1=1; A2=1; R1=0.5; R2=0.4;
A=[-1/(R1*A1) 1/(R1*A1); 1/(R1*A2) -(1/A2)*((1/R1)+(1/R2))];
B=[1/A1; 0]; C=[0 1]; D=0;
Ts=0.1; %sampling period
csys=ss(A,B,C,D); %setting the continuous time model
dsys=c2d(csys,Ts,'zoh'); %getting the discrete-time model
[a,b,c,d]=ssdata(dsys); %retrieves discrete-time model matrices

% system simulation preparation
Nf=40; %simulation horizon
x1=zeros(1,Nf); % for x1(n) record
x2=zeros(1,Nf); % for x2(n) record
y=zeros(1,Nf); % for y(n) record
x=[1;0]; % state vector with initial tank levels
u=0.1; %constant input

% observer simulation preparation
K=[0.3; 0.3]; %observer constants
er=zeros(2,Nf); % for error record
xe1=zeros(1,Nf); % for xe1(n) record
xe2=zeros(1,Nf); % for xe2(n) record
xe=[0.5; 0.1]; % observer state vector with initial values
   
%behaviour of the system and the observer after initial state
% with constant input u
for nn=1:Nf,
   x1(nn)=x(1); x2(nn)=x(2); %recording the system state
   y(nn)=c*x; %recording the system output
   xe1(nn)=xe(1); xe2(nn)=xe(2); %recording the observer state
   er(:,nn)=x-xe; %recording the error
   xn=(a*x)+(b*u); %next system state
   x=xn; %system state actualization
   xen=(a*xe)+(b*u)+(K*(y(nn)-(c*xe))); %next observer state
   xe=xen; %observer state actualization   
end;  

% display of states evolution
figure(1)
plot([0 Nf],[0 0],'g'); hold on; %horizontal axis
plot([0 0],[-0.2 1.2],'k'); %vertical axis
plot(x1,'r'); %plots x1
plot(x2,'b'); %plots x2
plot(xe1,'mx'); %plots xe1
plot(xe2,'kx'); %plots xe2
xlabel('sampling periods');
title('system and observer states');

% display of error evolution
figure(2)
plot([0 Nf],[0 0],'g'); hold on; %horizontal axis
plot([0 0],[-0.2 0.6],'k'); %vertical axis
plot(er(1,:),'r'); %plots x1 error
plot(er(2,:),'b'); %plots x2 error
xlabel('sampling periods');
title('observation error');

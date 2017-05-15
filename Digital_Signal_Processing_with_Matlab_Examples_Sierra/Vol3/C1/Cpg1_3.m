%Kalman filter example, in noisy conditions
%state space system model (2 tank system):
A1=1; A2=1; R1=0.5; R2=0.4;
cA=[-1/(R1*A1) 1/(R1*A1); 1/(R1*A2) -(1/A2)*((1/R1)+(1/R2))];
cB=[1/A1; 0]; cC=[1 0; 0 1]; cD=0;
Ts=0.1; %sampling period
csys=ss(cA,cB,cC,cD); %setting the continuous time model
dsys=c2d(csys,Ts,'zoh'); %getting the discrete-time model
[A,B,C,D]=ssdata(dsys); %retrieves discrete-time model matrices

%simulation horizon
Nf=40;

%process noise
Sw=[12e-4 0; 0 6e-4]; %cov
sn=zeros(2,Nf); 
sn(1,:)=sqrt(Sw(1,1))*randn(1,Nf);
sn(2,:)=sqrt(Sw(2,2))*randn(1,Nf);
%observation noise
Sv=[6e-4 0; 0 15e-4]; %cov.
on=zeros(2,Nf);
on(1,:)=sqrt(Sv(1,1))*randn(1,Nf);
on(2,:)=sqrt(Sv(2,2))*randn(1,Nf); 

% system simulation preparation
%space for recording x1(n), x2(n)
x1=zeros(1,Nf-1); x2=zeros(1,Nf-1);  
x=[1;0]; % state vector with initial tank levels
u=0.4; %constant input

% Kalman filter simulation preparation
%space for matrices
K=zeros(2,2,Nf); M=zeros(2,2,Nf); P=zeros(2,2,Nf);
%space for recording er(n), xe1(n), xe2(n),ym(n)
er=zeros(2,Nf-1); xe1=zeros(1,Nf-1); xe2=zeros(1,Nf-1);rym=zeros(2,Nf);
xe=[0.5; 0.2]; % filter state vector with initial values
   
%behaviour of the system and the Kalman filter after initial state
% with constant input u
for nn=1:Nf-1,
   x1(nn)=x(1); x2(nn)=x(2); %recording the system state
   xe1(nn)=xe(1); xe2(nn)=xe(2); %recording the observer state
   er(:,nn)=x-xe; %recording the error 
   %
   %system simulation
   xn=(A*x)+(B*u)+sn(nn); %next system state
   x=xn; %system state actualization
   ym=(C*x)+on(:,nn); %output measurement
   rym(:,nn)=ym;
   %
   %Prediction
    xa=(A*xe)+(B*u); %a priori state
    M(:,:,nn+1)=(A*P(:,:,nn)*A')+ Sw;    
   %Update 
    K(:,:,nn+1)=(M(:,:,nn+1)*C')*inv((C*M(:,:,nn+1)*C')+Sv);
    P(:,:,nn+1)=M(:,:,nn+1)-(K(:,:,nn+1)*C*M(:,:,nn+1));
    xe=xa+(K(:,:,nn+1)*(ym-(C*xa))); %estimated (a posteriori) state   
end;  

%----------------------------------------------------------------
% display of system outputs
figure(1)
plot([0 Nf],[0 0],'g'); hold on; %horizontal axis
plot([0 0],[-0.2 1.2],'k'); %vertical axis
plot(rym(1,:),'r'); %plots y1
plot(rym(2,:),'b'); %plots y2
xlabel('sampling periods');
title('system outputs');

% display of state evolution
figure(2)
plot([0 Nf],[0 0],'g'); hold on; %horizontal axis
plot([0 0],[-0.2 1.2],'k'); %vertical axis
plot(x1,'r'); %plots x1
plot(x2,'b'); %plots x2
plot(xe1,'mx'); %plots xe1
plot(xe2,'kx'); %plots xe2
xlabel('sampling periods');
title('system and Kalman filter states');


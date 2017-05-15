%Fixed-point smoothing example
L=5; %lag
%state space system model (2 tank system):
A1=1; A2=1; R1=0.5; R2=0.4;
cA=[-1/(R1*A1) 1/(R1*A1); 1/(R1*A2) -(1/A2)*((1/R1)+(1/R2))];
cB=[1/A1; 0]; cC=[1 0; 0 1]; cD=0;
Ts=0.1; %sampling period
csys=ss(cA,cB,cC,cD); dsys=c2d(csys,Ts,'zoh'); %discrete-time model
[A,B,C,D]=ssdata(dsys); %retrieves discrete-time model matrices

%simulation horizon
Nf=40;

%process noise
Sw=[12e-4 0; 0 6e-4]; %cov
sn=zeros(2,Nf); 
sn(1,:)=sqrt(Sw(1,1))*randn(1,Nf); sn(2,:)=sqrt(Sw(2,2))*randn(1,Nf);
%observation noise
Sv=[6e-4 0; 0 15e-4]; %cov.
on=zeros(2,Nf);
on(1,:)=sqrt(Sv(1,1))*randn(1,Nf); on(2,:)=sqrt(Sv(2,2))*randn(1,Nf); 

% system simulation preparation 
x=[1;0]; % state vector with initial tank levels
u=0.4; %constant input

% Kalman filter simulation preparation
%space for matrices
K=zeros(2,2); M=zeros(2,2); P=zeros(2,2);
xe=[0.5; 0.2]; % filter state vector with initial values
%space for recording xe(n),ym(n),P
rxe=zeros(2,Nf-1);
rym=zeros(2,Nf);
rym(:,1)=C*x; %initial value
rP=zeros(2,2,Nf);

%behaviour of the system and the Kalman filter after initial state
% with constant input u
for nn=1:Nf-1,
  %system simulation
  xn=(A*x)+(B*u)+sn(nn); %next system state
  x=xn; %system state actualization
  ym=(C*x)+on(:,nn); %output measurement
    
   %Prediction
    xa=(A*xe)+(B*u); %a priori state
    M=(A*P*A')+ Sw;    
   %Update 
    K=(M*C')*inv((C*M*C')+Sv);
    P=M-(K*C*M);
    xe=xa+(K*(ym-(C*xa))); %estimated (a posteriori) state   
    
   %recording xe(n),ym(n),P(n)
    rxe(:,nn)=xe;   
    rym(:,nn+1)=ym;
    rP(:,:,nn+1)=P;
 end;  

 %Smoothing-----------------------------
% Smoothing preparation
Nfix=10; %the fixed point
%space for matrices
N=zeros(2,2); P11=zeros(2,2); P21=zeros(2,2);
% augmented state vectors
axa=zeros(4,1);
axp=zeros(4,1);
% augmented input
bu=zeros(4,1); bu(1:2,1)=B*u;
% augmented A matrix
aA=diag(ones(4,1)); aA(1:2,1:2)=A; 
% augmented K
aK=zeros(4,2);
%space for recording xs(Nfix), P11(n)
rxs=zeros(2,Nf);
rP11=zeros(2,2,Nf);

%action:
P11=rP(:,:,Nfix); P21=P11; %initial values
axa(1:2,1)=rxe(:,Nfix); %initial values
axa(3:4,1)=rxe(:,Nfix); %initial values

for nn=Nfix:Nf,
   M=(A*P11*A')+Sw;
   N=(C*P11*C')+Sv;
   ivN=inv(N);
   
   K=(A*P11*C')*ivN; 
   Ka=(P21*C')*ivN;
   aK(1:2,:)=K; aK(3:4,:)=Ka;

   axp=(aA*axa)+bu+aK*(rym(:,nn)-C*axa(1:2,1));
   
   axa=axp; %actualization
      
   rP21(:,:,nn)=P21; %recording
   rxs(:,nn)=axp(3:4,1);
   
   P11=M-(K*N*K');
   P21=P21*(A-(K*C))';
      
end;   

   
%----------------------------------------------------------------

% display of smoothed state at Nfix

figure(3)
plot(rxs(1,Nfix:end),'r'); %plots xs1
hold on;
plot(rxs(2,Nfix:end),'b'); %plots xs2
axis([0 Nf 0 0.6]);
xlabel('sampling periods');
title('State smoothing at Nfix');


% display of Covariance evolution

figure(4)
subplot(2,2,1)
plot(squeeze(rP21(1,1,Nfix:end)),'k');
title('Evolution of covariance');
subplot(2,2,2)
plot(squeeze(rP21(1,2,Nfix:end)),'k');
subplot(2,2,3)
plot(squeeze(rP21(2,1,Nfix:end)),'k');
subplot(2,2,4)
plot(squeeze(rP21(2,2,Nfix:end)),'k');



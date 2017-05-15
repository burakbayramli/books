%Fixed-interval smoothing example
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
%space for recording xa(n), xe(n)
rxa=zeros(2,Nf-1);rxe=zeros(2,Nf-1);
   
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
    
   %recording xa(n), xe(n)
   rxa(:,nn)=xa;
   rxe(:,nn)=xe;
 end;  

%Smoothing-----------------------------
xs=zeros(2,Nf);

xs(:,Nf)=xe; %final estimated state
for nn=(Nf-1):-1:1,
   G=(P*A')*inv(M);
   xs(:,nn)=rxe(:,nn)+(G*(xs(:,nn+1)-rxa(:,nn)));
end;   

%----------------------------------------------------------------

% display of state evolution

figure(3)
plot(xs(1,:),'r'); %plots xs1
hold on;
plot(xs(2,:),'b'); %plots xs2
plot(rxe(1,:),'mx'); %plots xe1
plot(rxe(2,:),'kx'); %plots xe2
axis([0 Nf 0 1]);
xlabel('sampling periods');
title('Kalman filter states(x) and Smoothed states(-)');


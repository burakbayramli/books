%Extended Kalman filter example
%Radar monitoring of falling body

%----------------------------------------------------
%Prepare for the simulation of the falling body

T=0.4; %sampling period
g=-9.81;
rho0=1.225; %air density, sea level
k=6705.6; %density vs. altitude constant
L=100; %horizontal distance radar<->object
L2=L^2;

Nf=100; %maximum number of samples

rx=zeros(3,Nf); %space for state record
tim=0:T:(Nf-1)*T; %time

%process noise
Sw=[10^5 0 0; 0 10^3 0; 0 0 10^2]; %cov
bn=randn(3,Nf); sn=zeros(3,Nf); 
sn(1,:)=sqrt(Sw(1,1))*bn(1,:); %state noise along simulation
sn(2,:)=sqrt(Sw(2,2))*bn(2,:);  %"   "   "
sn(3,:)=sqrt(Sw(3,3))*bn(3,:);  %"   "   "
%observation noise
Sv=10^6; %cov.
on=sqrt(Sv)*randn(1,Nf); %observation noise along simulation

%----------------------------------------------------
%Prepare for filtering

%space for matrices
K=zeros(3,Nf); M=zeros(3,3,Nf); P=zeros(3,3,Nf);
%space for recording er(n), xe(n)
rer=zeros(3,Nf); rxe=zeros(3,Nf);

W=eye(3,3); %process noise jacobian
V=1; %observation noise jacobian

%----------------------------------------------------
%Behaviour of the system and the filter after initial state

x=[10^5; -5000; 400]; %initial state
xe=x; % initial values of filter state
xa=xe; %initial intermediate state

nn=1;
while nn<Nf+1,
   
   %estimation recording
   rxe(:,nn)=xe; %state
   rer(:,nn)=x-xe; %error
   
   %system
   rx(:,nn)=x; %state recording
   rho=rho0*exp(-x(1)/k); %air density   
   d=(rho*(x(2)^2))/(2*x(3)); %drag
        
   %next system state
   x(1)=x(1)+(x(2)*T)+sn(1,nn);
   x(2)=x(2)+((g+d)*T)+sn(2,nn);
   x(3)=x(3)+sn(3,nn);   
   %system output
   y=on(nn)+sqrt(L2+(x(1)^2));
   ym=y; %measurement
      
 %Prediction
   %a priori state
    rho=rho0*exp(-xe(1)/k); %air density
    d=(rho*(xe(2)^2))/(2*xe(3)); %drag
    xa(1)=xe(1)+(xe(2)*T);
    xa(2)=xe(2)+((g+d)*T);
    xa(3)=xe(3); 
   %a priori cov.
    f21=-d/k; f22=(rho*xe(2)/xe(3)); f23=-(d/xe(3));
    F=[0 1 0; f21 f22 f23; 0 0 0]; %state jacobian   
    M(:,:,nn+1)=(F*P(:,:,nn)*F')+ (W*Sw*W');  
    %
  %Update 
    ya=sqrt(L2+xa(1)^2);
    h1=xa(1)/ya;
    H=[h1 0 0]; %measurement jacobian
    K(:,nn+1)=(M(:,:,nn+1)*H')*inv((H*M(:,:,nn+1)*H')+(V*Sv*V'));
    P(:,:,nn+1)=M(:,:,nn+1)-(K(:,nn+1)*H*M(:,:,nn+1));
    xe=xa+(K(:,nn+1)*(ym-ya)); %estimated (a posteriori) state 
            
    nn=nn+1;       
end; 

%------------------------------------------------------
%display

figure(1)
subplot(1,2,1)
plot(tim,rx(1,1:Nf),'kx'); hold on;
plot(tim,rxe(1,1:Nf),'r');
title('altitude'); xlabel('seconds')
axis([0 Nf*T 0 12*10^4]);

subplot(1,2,2)
plot(tim,rx(2,1:Nf),'kx'); hold on;
plot(tim,rxe(2,1:Nf),'r');
title('velocity'); xlabel('seconds');
axis([0 Nf*T -6000 1000]);


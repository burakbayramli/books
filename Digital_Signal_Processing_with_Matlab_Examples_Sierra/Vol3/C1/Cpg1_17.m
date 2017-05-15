%Unscented Kalman filter example
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
rd=zeros(1,Nf); %space for drag record
ry=zeros(1,Nf); %space for measurement record
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

%UKF parameters (to be edited here)
N=3; %space dimension
alpha=0.7; kappa=0; beta=2;
%pre-computation of constants
lambda= ((alpha^2)*(N+kappa))-N;
aab=(1-(alpha^2)+beta); 
lN=lambda+N; LaN=lambda/lN; aaN=aab+LaN;

%----------------------------------------------------
%Behaviour of the system and the filter after initial state

x=[10^5; -5000; 400]; %initial state
xe=x; % initial value of filter state
xa=xe; %initial intermediate state
xs=zeros(3,7); %space for sigma points
xas=zeros(3,7); %space for propagated sigma points
yas=zeros(1,7); %"   "  "

P(:,:,1)=0.001*eye(3,3); %cov. non-zero init.

nn=1;

while nn<Nf+1,
   
   %estimation recording
   rxe(:,nn)=xe; %state
   rer(:,nn)=x-xe; %error
   
   %system
   rx(:,nn)=x; %state recording
   rho=rho0*exp(-x(1)/k); %air density
   d=(rho*(x(2)^2))/(2*x(3)); %drag
   rd(nn)=d; %drag recording
     
   %next system state
   x(1)=x(1)+(x(2)*T)+sn(1,nn);
   x(2)=x(2)+((g+d)*T)+sn(2,nn);
   x(3)=x(3)+sn(3,nn);   
   %system output
   y=on(nn)+sqrt(L2+(x(1)^2));
   ym=y; %measurement
   ry(nn)=ym; %measurement recording 
   
 %Prediction
   %sigma points
   sqP=chol(lN*P(:,:,nn)); %matrix square root
   xs(:,7)=xe;
   xs(:,1)=xe+sqP(1,:)'; xs(:,2)=xe+sqP(2,:)'; xs(:,3)=xe+sqP(3,:)';
   xs(:,4)=xe-sqP(1,:)'; xs(:,5)=xe-sqP(2,:)'; xs(:,6)=xe-sqP(3,:)';       
       
   %a priori state
   %propagation of sigma points (state transition)
   for m=1:7,
    rho=rho0*exp(-xs(1,m)/k); %air density
    d=(rho*(xs(2,m)^2))/(2*xs(3,m)); %drag
    xas(1,m)=xs(1,m)+(xs(2,m)*T);
    xas(2,m)=xs(2,m)+((g+d)*T);
    xas(3,m)=xs(3,m);
   end;
   
   %a priori state mean (a weighted sum)
 	xa=0;
   for m=1:6,
     xa=xa+(xas(:,m));
   end;
   xa=xa/(2*lN);
   xa=xa+(LaN*xas(:,7));
            
   %a priori cov.
   aux=zeros(3,3); aux1=zeros(3,3); 
   for m=1:6,
     aux=aux+((xas(:,m)-xa(:))*(xas(:,m)-xa(:))');
   end;
   aux=aux/(2*lN);
   aux1=((xas(:,7)-xa(:))*(xas(:,7)-xa(:))');
   aux=aux+(aaN*aux1);
   M(:,:,nn+1)=aux+Sw;
   
   
 %Update 
      
   %propagation of sigma points (measurement)
   for m=1:7,
      yas(m)=sqrt(L2+(xas(1,m)^2));
   end;
    
   %measurement mean
   ya=0;
   for m=1:6,
      ya=ya+yas(m);
   end;   
   ya=ya/(2*lN);
   ya=ya+(LaN*yas(7));
    
   %measurement cov.
   aux2=0;
   for m=1:6,
     aux2=aux2+((yas(m)-ya)^2);
   end;
   aux2=aux2/(2*lN);
   aux2=aux2+(aaN*((yas(7)-ya)^2));
   Syy=aux2+Sv;
    
   %cross cov
   aux2=0; 
   for m=1:6,
      aux2=aux2+((xas(:,m)-xa(:))*(yas(m)-ya));
   end;
   aux2=aux2/(2*lN);
   aux2=aux2+(aaN*((xas(:,7)-xa(:))*(yas(7)-ya)));
   Sxy=aux2;
   
   %Kalman gain, etc.
   K(:,nn+1)=Sxy*inv(Syy);
   P(:,:,nn+1)=M(:,:,nn+1)-(K(:,nn+1)*Syy*K(:,nn+1)');
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


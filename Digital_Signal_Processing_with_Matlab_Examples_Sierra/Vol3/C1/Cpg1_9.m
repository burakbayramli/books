%Jacobians
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

tim=0:T:(Nf-1)*T; %time

%space for recording jacobians
rf2=zeros(3,Nf); rh1=zeros(1,Nf);

W=eye(3,3); %process noise jacobian
V=1; %observation noise jacobian

x=[10^5; -5000; 400]; %initial state

%----------------------------------------------------
%Simulation

nn=1;
while nn<Nf+1,
   
   %system
   rx(:,nn)=x; %state recording
   rho=rho0*exp(-x(1)/k); %air density   
   d=(rho*(x(2)^2))/(2*x(3)); %drag
   rd(nn)=d; %drag recording
     
   %next system state
   x(1)=x(1)+(x(2)*T);
   x(2)=x(2)+((g+d)*T);
   x(3)=x(3);   
   %system output
   ym=sqrt(L2+(x(1)^2)); %measurement
   ry(nn)=ym; %measurement recording 
   
   %jacobians
   f21=-d/k; f22=(rho*x(2)/x(3)); f23=-(d/x(3)); %state jacob.
   ya=sqrt(L2+x(1)^2);
   h1=xa(1)/ya; %measurement jacob.
    
    rf2(:,nn)=[f21; f22; f23]; rh1(nn)=h1; %jacob. recording
    
  nn=nn+1;       
end; 


%------------------------------------------------------
%display

figure(1)
subplot(1,3,1)
plot(tim,rf2(1,1:Nf),'k');
title('jacobian term f21');
xlabel('seconds');
axis([0 Nf*T -0.14 0.02]);

subplot(1,3,2)
plot(tim,rf2(2,1:Nf),'k');
title('jacobian term f22');
xlabel('seconds');
axis([0 Nf*T -0.7 0.1]);


subplot(1,3,3)
plot(tim,rf2(3,1:Nf),'k');
title('jacobian term f23');
xlabel('seconds');
axis([0 Nf*T -2.5 0.4]);



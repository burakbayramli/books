%Example of nonlinear dynamics and measurement
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

x=[10^5; -5000; 400]; %initial state

%---------------------------------------------------
%simulation
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
    
  nn=nn+1;       
end; 

%------------------------------------------------------
%display

figure(1)
subplot(1,2,1)
plot(tim,rx(1,1:Nf),'kx');
title('altitude'); xlabel('seconds')
axis([0 Nf*T 0 12*10^4]);

subplot(1,2,2)
plot(tim,rx(2,1:Nf),'kx');
title('velocity'); xlabel('seconds');
axis([0 Nf*T -6000 1000]);

figure(2)
subplot(1,2,1)
plot(tim,ry(1:Nf),'k');
title('distance measurement');
xlabel('seconds');
axis([0 Nf*T 0 12*10^4]);

subplot(1,2,2)
plot(tim,rd(1:Nf),'k');
title('drag');
xlabel('seconds');
axis([0 Nf*T 0 1000]);




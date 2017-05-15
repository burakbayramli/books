%Propagation of ellipsoids
%Radar monitoring of falling body

%----------------------------------------------------
%Prepare for the simulation of the falling body

T=0.4; %sampling period
g=-9.81;
rho0=1.225; %air density, sea level
k=6705.6; %density vs. altitude constant
L=100; %horizontal distance radar<->object
L2=L^2;

Nf=42; %maximum number of samples

%space for ellipsoids
Ne=200;
ax1=zeros(1,Ne); 
ax2=zeros(1,Ne);
px1=zeros(1,Ne);
px2=zeros(1,Ne);

tim=0:T:(Nf-1)*T; %time

x=[10^5; -5000; 400]; %initial state

%---------------------------------------------------
%simulation till a desired moment
nn=1;
while nn<Nf+1,
      
   %system
   rx(:,nn)=x; %state recording
   rho=rho0*exp(-x(1)/k); %air density   
   d=(rho*(x(2)^2))/(2*x(3)); %drag
        
   %next system state
   x(1)=x(1)+(x(2)*T);
   x(2)=x(2)+((g+d)*T);
   x(3)=x(3);   
      
  nn=nn+1;       
end; 

%propagation of ellipsoids
% with display

for R=10:40:330,
 for m=1:Ne+1,   
   %a priori ellipsoid
   phi=(2*pi*(m-1))/Ne;
   ax1(m)=x(1)+10*R*cos(phi);
   ax2(m)=x(2)+R*sin(phi);
   %posterior ellipsoid
   rho=rho0*exp(-ax1(m)/k); %air density   
   d=(rho*(ax2(m)^2))/(2*x(3)); %drag
   px1(m)=ax1(m)+(ax2(m)*T); 
   px2(m)=ax2(m)+((g+d)*T);
 end;

%display
m=1:Ne+1;
figure(1)
subplot(1,2,1)
plot(ax1(m),10*ax2(m),'k'); hold on;
title('before');xlabel('x1'); ylabel('10*x2');
axis([1.4e4 2.2e4 -3.2e4 -2.2e4]);

subplot(1,2,2);
plot(px1(m),10*px2(m),'k'); hold on;
title('after');xlabel('x1'); ylabel('10*x2');
axis([1.4e4 2.2e4 -3.2e4 -2.2e4]);

end;


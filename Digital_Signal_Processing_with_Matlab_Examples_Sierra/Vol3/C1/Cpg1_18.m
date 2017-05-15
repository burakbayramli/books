%Particle filter example
%Radar monitoring of falling body

disp('please wait a bit');

%----------------------------------------------------
%Prepare for the simulation of the falling body

T=0.4; %sampling period
g=-9.81;
rho0=1.225; %air density, sea level
k=6705.6; %density vs. altitude constant
L=100; %horizontal distance radar<->object
L2=L^2;

Nf=100; %maximum number of steps

rx=zeros(3,Nf); %space for state record
tim=0:T:(Nf-1)*T; %time

%process noise
Sw=[10^5 0 0; 0 10^3 0; 0 0 10^2]; %cov
w11=sqrt(Sw(1,1)); w22=sqrt(Sw(2,2)); w33=sqrt(Sw(3,3));
w=[w11; w22; w33];
%observation noise
Sv=10^6; %cov.
v11=sqrt(Sv);

%----------------------------------------------------
%Prepare for filtering

%space for recording er(n), xe(n)
rer=zeros(3,Nf); rxe=zeros(3,Nf);

%----------------------------------------------------
%Behaviour of the system and the filter after initial state

x=[10^5; -5000; 400]; %initial state
xe=x; %initial estimation

%prepare particles
Np=1000; %number of particles
 %reserve space
  px=zeros(3,Np); %particles
  apx=zeros(3,Np); %a priori particles
  ny=zeros(1,Np); %particle measurements
  vy=zeros(1,Np); %meas. dif.
  pq=zeros(1,Np); %particle likelihoods
 %particle generation 
wnp=randn(3,Np); %noise (initial particles)
for ip=1:Np,
  px(:,ip)=x+(w.*wnp(:,ip)); %initial particles
end;

%system noises
wx=randn(3,Nf); %process
wy=randn(1,Nf); %output

nn=1;
while nn<Nf+1,
   
   %estimation recording
   rxe(:,nn)=xe; %state
   rer(:,nn)=x-xe; %error
   
 %Simulation of the system 
   %system
   rx(:,nn)=x; %state recording
   rho=rho0*exp(-x(1)/k); %air density   
   d=(rho*(x(2)^2))/(2*x(3)); %drag
   rd(nn)=d; %drag recording     
   %next system state
   x(1)=x(1)+(x(2)*T);
   x(2)=x(2)+((g+d)*T);
   x(3)=x(3); 
   x=x+(w.*wx(:,nn)); %additive noise
   %system output
   y=sqrt(L2+(x(1)^2))+(v11*wy(nn)); %additive noise
   ym=y; %measurement
        
 %Particle propagation
   wp=randn(3,Np); %noise (process)
   vm=randn(1,Np); %noise (measurement)
   for ip=1:Np,
      rho=rho0*exp(-px(1,ip)/k); %air density   
      d=(rho*(px(2,ip)^2))/(2*px(3,ip)); %drag 
     %next state  
      apx(1,ip)=px(1,ip)+(px(2,ip)*T);
      apx(2,ip)=px(2,ip)+((g+d)*T);
      apx(3,ip)=px(3,ip); 
      apx(:,ip)=apx(:,ip)+(w.*wp(:,ip)); %additive noise
      %measurement (for next state)
      ny(ip)=sqrt(L2+(apx(1,ip)^2))+(v11*vm(ip)); %additive noise
      vy(ip)=ym-ny(ip);
   end; 
   
   %Likelihood
   %(vectorized part)
    %scaling
     vs=max(abs(vy))/4; 
     ip=1:Np;
     pq(ip)=exp(-((vy(ip)/vs).^2));      
     spq=sum(pq);
    %normalization
     pq(ip)=pq(ip)/spq;
    
  %Prepare for roughening
    A=(max(apx')-min(apx'))';
    sig=0.2*A*Np^(-1/3);
    rn=randn(3,Np); %random numbers
       
  %===========================================================
    
  %Resampling (systematic)
    acq=cumsum(pq);
    cmb=linspace(0,1-(1/Np),Np)+(rand(1)/Np); %the "comb"
    cmb(Np+1)=1;
    ip=1; mm=1;
    while(ip<=Np),
      if (cmb(ip)<acq(mm)),
         aux=apx(:,mm);
         px(:,ip)=aux+(sig.*rn(:,ip)); %roughening
         ip=ip+1;
      else
         mm=mm+1;
      end;
    end;   
    
  %===========================================================
    
  %Results
  %estimated state (the particle mean)  
    xe=sum(px,2)/Np;
      
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

%Resampling methods
%Particle filter example
%Radar monitoring of falling body

randn('state',0); %initialization of random number generator

%----------------------------------------------------
%Prepare for the simulation of the falling body

T=0.4; %sampling period
g=-9.81;
rho0=1.225; %air density, sea level
k=6705.6; %density vs. altitude constant
L=100; %horizontal distance radar<->object
L2=L^2;

Nf=3; %maximum number of steps

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
 %reserve space for resampling posteriors
  Spx=zeros(3,Np);
  Mpx=zeros(3,Np);
  Rpx=zeros(3,Np);
  Fpx=zeros(3,Np);
  
 %particle generation 
  wnp=randn(3,Np); %noise (initial particles)
  for ip=1:Np,
    px(:,ip)=x+(w.*wnp(:,ip)); %initial particles
  end;
  
  %system noises
wx=randn(3,Nf); %process
wy=randn(1,Nf); %output

%----------------------------------------------------
%Behaviour of the system and the filter after initial state

nn=1;
while nn<Nf+1,
        
 %Simulation of the system 
   %system
   rho=rho0*exp(-x(1)/k); %air density   
   d=(rho*(x(2)^2))/(2*x(3)); %drag   
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
         Spx(:,ip)=aux+(sig.*rn(:,ip)); %roughening
         ip=ip+1;
      else
         mm=mm+1;
      end;
    end;   
    
  %===========================================================
    
  %Resampling (multinomial)
    acq=cumsum(pq);
    mm=1;
    nr=sort(rand(1,Np)); %ordered random numbers (0, 1]
    for ip=1:Np,
       while(acq(mm)<nr(ip)),
          mm=mm+1;
       end;
       aux=apx(:,mm);
       Mpx(:,ip)=aux+(sig.*rn(:,ip)); %roughening
    end;  
  
  %===========================================================
    
  %Resampling (residual)  
    acq=cumsum(pq);
    mm=1;
    
    %preparation
    na=floor(Np*pq); %repetition counts
    NR=sum(na); %total count
    Npr=Np-NR; %number of non-repeated particles
    rpq=((Np*pq)-na)/Npr; %modified weights
    acq=cumsum(rpq); %for the monomial part
    
    %deterministic part
    mm=1;
    for j=1:Np,
       for nn=1:na(j),
          Rpx(:,mm)=apx(:,j);
          mm=mm+1;
       end;
    end;    
        
    %multinomial part:
    nr=sort(rand(1,Npr)); %ordered random numbers (0, 1]
    for j=1:Npr,
       while(acq(mm)<nr(j)),
          mm=mm+1;
       end;
       aux=apx(:,mm);
       Rpx(:,NR+j)=aux+(sig.*rn(:,j)); %roughening
    end;  
  
  %===========================================================
    
  %Resampling (stratified)
    acq=cumsum(pq);
    stf=zeros(1,Np);
    nr=rand(1,Np)/Np;
    j=1:Np;
    stf(j)=nr(j)+((j-1)/Np); %(vectorized code)
    stf(Np+1)=1;
    ip=1; mm=1;
    while(ip<=Np),
      if (stf(ip)<acq(mm)),
         aux=apx(:,mm);
         Fpx(:,ip)=aux+(sig.*rn(:,ip)); %roughening
         ip=ip+1;
      else
         mm=mm+1;
      end;
    end;    

  %===========================================================
    
  px=Spx; %posterior (edit to select a resampling method)
       
  %Results
    %estimated state (the particle mean)  
    xe=sum(px,2)/Np;
          
  nn=nn+1;    
 end; 


%------------------------------------------------------
%display

figure(1)
hist(pq,20);
title('histogram of weights');

figure(2)
plot(acq);
title('cumsum() of weights');

figure(3)
subplot(2,1,1)
bx=9.4e4:1.5e2:9.8e4;
hist(apx(1,:),bx);
title('histogram of prior particles')
subplot(2,1,2)
hist(Mpx(1,:),bx);
title('histogram of (multinomial) resampled particles'); 

figure(4)
Spt=hist(Spx(1,:),bx);
Mpt=hist(Mpx(1,:),bx);
Rpt=hist(Rpx(1,:),bx);
Fpt=hist(Fpx(1,:),bx);
subplot(2,2,1)
stem(bx,Spt,'k');
title('Systematic resampling')
subplot(2,2,2)
stem(bx,Fpt,'k');
title('Stratified resampling')
subplot(2,2,3)
stem(bx,Mpt,'k');
title('Multinomial resampling');
subplot(2,2,4)
stem(bx,Rpt,'k');
title('Residual resampling')
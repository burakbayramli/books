% Example of ARMA parameter estimation
% using PRBS as input

% a test case -----------------
% [a1 a2 a3 b1 b2 c1 c2]
thetao=[0.9 0.7 0.4 0.3 0.8 0.5 0.1]'; 
N=length(thetao);
d=zeros(N,1);

M=64;

% vector with a series of PRBS values
pr=zeros(M,1);
x=[zeros(1,15) 1]; 
for k=1:M,
    pr(k)=2*x(16)-1;
    q=x(16)+x(15)+x(13)+x(4);
    x(2:16)=x(1:15);
    x(1)=mod(q,2);
end; 

% vector with plant output in response to pr
y=zeros(M,1);
e=zeros(M,1); %noises
for nn=1:M,
   up=pr(nn); % input at present
   ep=0.5*randn(1,1); % noise at present 
   e(nn)=ep; %save noise
    % actualization of d
      d(5)=d(4); d(4)=up;
      d(7)=d(6); d(6)=ep;            
    yp=d'*thetao; % plant output at present
    y(nn)=yp; %save plant output
    %actualization of d
      d(3)=d(2); d(2)=d(1); d(1)=-yp;
end

L=7; %number of model parameters
thetaes=zeros(L,1); % estimated parameters
d=zeros(L,1); % vector of inputs/outputs
P=diag(1000*ones(L,1));

% iterative model parameter estimation ----------------
for nn=1:M,   
    up=pr(nn); % input at present
    ep=e(nn); % noise at present    
    % actualization of d
      d(5)=d(4); d(4)=up;
      d(7)=d(6); d(6)=ep;               
    yp=y(nn); % plant output at present 
    % parameter estimation steps:    
    aux=1+(d'*P*d);
    kappa=(P*d)/aux;
    P=P-(kappa*d'*P);  
    err=yp-(d'*thetaes);
    thetaes=thetaes+(kappa*err);    
    %actualization of d
    d(3)=d(2); d(2)=d(1); d(1)=-yp;      
end

thetaes

figure(1)
xx=0;
for nn=1:M-1,
    plot([xx xx+1], [pr(nn) pr(nn)],'k'); hold on;
    plot([xx+1 xx+1], [pr(nn) pr(nn+1)],'k');
    xx=xx+1;
end;
axis([-1 M+1 -1.2 1.1]);
title('PRBS input');



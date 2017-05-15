% Example of ARMA parameter estimation
% (time-series ARMAX modelling)

% a simple test case -----------------
% [a1 a2 b1 b2 c1]
thetao=[0.9 0.7 0.5 0.3 0.2]'; 
N=length(thetao);

% iterative estimation of parameters
M=20;
erec=zeros(1,M); %for error recording
thetaes=zeros(N,1); % estimated parameters
d=zeros(N,1); % vector of inputs/outputs
P=diag(1000*ones(N,1));
 
for nn=1:M,
    up=randn(1,1); % input at present
    ep=0.5*randn(1,1); % noise at present
    
    % actualization of d
      d(4)=d(3); d(3)=up;
      d(5)=ep;
            
    yp=d'*thetao; % plant output at present
    
    % parameter estimation steps:    
    aux=1+(d'*P*d);
    kappa=(P*d)/aux;
    P=P-(kappa*d'*P);  
    err=yp-(d'*thetaes);
    erec(nn)=err;
    thetaes=thetaes+(kappa*err);
    
    %actualization of d
      d(2)=d(1); d(1)=-yp;
end

thetaes

figure(1)
plot(erec,'k');
title('Error evolution along parameter identification iterations');
xlabel('niter');
      

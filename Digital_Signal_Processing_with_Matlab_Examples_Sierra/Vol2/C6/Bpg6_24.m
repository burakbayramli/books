% Example of ARMA parameter estimation
% comparison of original and estimated model
% in terms of squared error between output vectors

% the test case -----------------
% [a1 a2 a3 b1 b2 c1 c2]
thetao=[0.9 0.7 0.4 0.3 0.8 0.5 0.1]'; 
N=length(thetao);

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
d=zeros(N,1);
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

% vector with estimated model outputs in response to pr
ye=zeros(M,1);
thetaes=[0.4020 0.5038 0.3696 0.3904]'; % estimated parameters
L=length(thetaes);
d=zeros(L,1); 
for nn=1:M,
   up=pr(nn); % input at present
   ep=e(nn); % noise at present 
   % actualization of d
      d(3)=up;
      d(4)=ep;            
    yp=d'*thetaes; % plant output at present
    ye(nn)=yp; %save estimated plant output
    %actualization of d
      d(2)=d(1); d(1)=-yp;
end

Aerr=0;
for nn=1:M,
    Aerr=Aerr+((y(nn)-ye(nn))^2);
end

Aerr

figure(1)
plot(y,'rx'); hold on;
plot(ye,'b');
title('real response (X) vs. estimated response (continuous)');



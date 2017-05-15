% Propagation of uncertainty on initial state
%example of state dynamics model
% xn=0.6 xo
%initial state (with some Gaussian uncertainty)
X0=5+(0.5*randn(500,1)); 

Ns=10; %number of samples along time
X=zeros(500,Ns); %reserve space
X(:,1)=X0;
for nn=2:Ns,
   X(:,nn)=0.6*X(:,nn-1);
end;

figure(1)
for np=1:500,   
   plot(X(np,:),'g-'); hold on;
end;   
xlabel('samples'); ylabel('state');
title('Autonomous behaviour, uncertain initial state');


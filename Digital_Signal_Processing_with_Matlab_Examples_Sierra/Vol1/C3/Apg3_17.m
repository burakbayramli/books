% Deterministic state evolution
%example of state dynamics model
% xn=0.6 xo
%initial state
X0=5; 
Ns=10; %number of samples along time
X=zeros(1,Ns); %reserve space

X(1)=X0;
for nn=2:Ns,
   X(nn)=0.6*X(nn-1);
end;

figure(1)
plot(X,'k*-'); hold on;
xlabel('samples'); ylabel('state');
title('Autonomous behaviour');


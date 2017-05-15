% Evolution of state mean and variance
%initial state
X0=5; 
Ns=10; %number of samples along time
mX=zeros(1,Ns); %reserve space
varX=zeros(1,Ns); %"  "  "
mX(1)=X0;
varX(1)=0.5;

for nn=2:Ns,
   mX(nn)=0.6*mX(nn-1);
   varX(nn)=(0.6^2)*varX(nn-1);
end;

figure(1)
subplot(2,1,1)
plot(mX,'k*-');
xlabel('samples'); ylabel('state mean');
title('Autonomous behaviour');
subplot(2,1,2)
plot(varX,'k*-');
xlabel('samples'); ylabel('state variance');

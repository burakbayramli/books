% Influence of Gaussian process noise
%example of state dynamics model
% xn=0.6 xo + 0.5 w;
%autonomous behaviour with  process noise---------------
Ns=10; %number of samples along time
X=zeros(1000,Ns); %reserve space

for np=1:1000,
X(np,1)=5; %initial state
for nn=2:Ns,
   X(np,nn)= 0.6*X(np,nn-1)+ 0.5*randn(1); %with process noise
end;
end;

figure(1) %trajectories
nf=1:Ns;
for ns=1:1000,
   plot(nf,X(ns,nf),'g-'); hold on;
end;
title('Autonomous behaviour, with process noise');
xlabel('samples'); ylabel('state');


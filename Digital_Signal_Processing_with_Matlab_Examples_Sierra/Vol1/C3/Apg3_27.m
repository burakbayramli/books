% Influence of Gaussian process noise 
% and non-Gaussian measurement noise
%example of state dynamics model
% xn=0.6 xo + 0.5 w;
% yn=1.2 xn + 0.8 v;

%autonomous behaviour with  process noise---------------
Ns=10; %number of samples along time
X=zeros(1000,Ns); %reserve space
Y=zeros(1000,Ns); %"  "  "

for np=1:1000,
X(np,1)=5; %initial state
for nn=2:Ns,
   X(np,nn)= 0.6*X(np,nn-1)+ 0.5*randn(1); %with process noise
end;
for nn=1:Ns,
   Y(np,nn)=1.2*X(np,nn) + 1.8*random('beta',7,2,1,1); %with beta measurement noise
end;
end;

figure(1) %trajectories
nf=1:Ns;
for ns=1:1000,
   plot(nf,Y(ns,nf),'g-'); hold on;
end;
title('System output for autonomous behaviour');
xlabel('samples'); ylabel('output');
figure(2) %histogram
hist(Y(:,Ns),40); colormap('cool');
title('Histogram of final measurement');



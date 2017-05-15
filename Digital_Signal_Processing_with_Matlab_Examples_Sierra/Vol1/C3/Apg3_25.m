% Propagation of non-Gaussian process noise
% state evolution
% histograms of noise and X(10)

%example of state dynamics model
% xn=0.6 xo + 0.5 w;

%autonomous behaviour with  process noise---------------
Ns=10; %number of samples along time
X=zeros(500,Ns); %reserve space

for np=1:500,
X(np,1)=5; %initial state
for nn=2:Ns,
   X(np,nn)= 0.6*X(np,nn-1)+ 0.5*random('beta',7,2,1,1); %with beta process noise
end;
end;

R=0.5*random('beta',7,2,500,1);

figure(1) %trajectories
nf=1:Ns;
for ns=1:500,
   plot(nf,X(ns,nf),'g-'); hold on;
end;
title('Autonomous behaviour, with beta process noise');
xlabel('samples'); ylabel('state');

figure(2)
subplot(2,1,1)
hist(R,40); colormap('cool');
title('histogram of process noise');
axis([0 1.5 0 40]);
subplot(2,1,2)
hist(X(:,10),40); colormap('cool');
title('histogram of x(10)')
axis([0 1.5 0 40]);



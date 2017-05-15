% Illustration of Regularized Particle Filter
x=-1:0.01:3;
N=length(x);
Ksum=zeros(1,N);

pmu=[0.2, 0.5, 1, 1.3, 1.7, 1.8];
psig=0.4;

figure(1)
for nn=1:6,
    Kpdf=normpdf(x,pmu(nn),psig); %the kernels
    Ksum=Ksum+Kpdf;
    plot(x,Kpdf,'k'); hold on;    
end;  
plot(x,Ksum,'r'); %the sum of kernels
plot(pmu,zeros(1,6),'rx','MarkerSize',10); %the particles
title('Particles and kernels');

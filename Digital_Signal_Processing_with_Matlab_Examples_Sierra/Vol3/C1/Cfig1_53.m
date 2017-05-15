%Prediction and likelihood
%
xiv=0.1;
x=-10:xiv:15; 

%prediction pdf
pmu=0; psig=2.6; 
ppdf=normpdf(x,pmu,psig);
%measurement pdf
mmu=5; msig=1.7; 
mpdf=normpdf(x,mmu,msig);

%particles
ptx=linspace(0,7.8,15);
pty=zeros(1,15);

figure(1)
plot(x,ppdf,'b'); hold on;
plot(x,mpdf,'r');
plot(ptx,pty,'ko','MarkerSize',6);
title('prediction and likelihood');
xlabel('x');

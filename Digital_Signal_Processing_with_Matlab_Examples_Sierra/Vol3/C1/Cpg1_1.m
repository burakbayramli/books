%Combining prediction and measurement
%
xiv=0.1;
x=-10:xiv:15; 

%prediction pdf
pmu=4; psig=1.8; 
ppdf=normpdf(x,pmu,psig);
%measurement pdf
mmu=0; msig=2.5; 
mpdf=normpdf(x,mmu,msig);

%combine pdfs
cpdf=ppdf.*mpdf;

%area=1
KA=1/(sum(cpdf)*xiv);
cpdf=KA*cpdf;

figure(1)
plot(x,ppdf,'b'); hold on;
plot(x,mpdf,'r');
plot(x,cpdf,'k');
title('combine prediction and measurement: update');
xlabel('x');

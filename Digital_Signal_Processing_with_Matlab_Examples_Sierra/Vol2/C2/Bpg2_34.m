% inverse of the Lifting 
% Sonar signal 
% Daubechies 4 wavelet
% use after lifting analysis (it needs wty)

L=length(wty); %length of the DWT
sg=zeros(1,L);

K=8; %number of scales 
cq=sqrt(3);
a=wty(1); 
for n=1:K,
   wd=2^(n-1);
   bg=1+wd; fl=bg+wd-1;
   d=wty(bg:fl);
   d1=d/((cq+1)/sqrt(2));
   a2=a/((cq-1)/sqrt(2));
   a1=a2+[d1(2:end) d1(1)];
   sg(2:2:(2^n))=d1+((cq/4)*a1)+(((cq-2)/4)*[a1(end) a1(1:end-1)]);
   sg(1:2:(2^n)-1)=a1-(cq*sg(2:2:(2^n)));
   a=sg(1:(2^n));
end;
   

figure(1)
plot(sg,'k');
axis([0 256 1.2*min(sg) 1.2*max(sg)]);
xlabel('samples');
title('the recovered signal');


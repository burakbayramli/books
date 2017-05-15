% inverse of the Lifting 
% Sonar signal 
% CDF 9/7 wavelet
% use after lifting analysis (it needs wty)

L=length(wty); %length of the DWT
sg=zeros(1,L);
%CDF coeffs.
pa=-1.58613; pb=-0.05298; pg=0.88298; pd=0.4435; pp=1.1496;

K=8; %number of scales 
cq=sqrt(3);
a=wty(1); 
for n=1:K,
   wd=2^(n-1);
   bg=1+wd; fl=bg+wd-1;
   d=wty(bg:fl);
   d2=d*pp;
   a2=a/pp;
   a1=a2-(pd*(d2+[d2(end) d2(1:(end-1))]));
   d1=d2-(pg*(a1+[a1(2:end) a1(1)]));
   a0=a1-(pb*(d1+[d1(end) d1(1:(end-1))]));
   d0=d1-(pa*(a0+[a0(2:end) a0(1)]));
   sg(2:2:(2^n))=d0;
   sg(1:2:(2^n)-1)=a0;
   a=sg(1:(2^n));
end;
   

figure(1)
plot(sg,'k');
axis([0 256 1.2*min(sg) 1.2*max(sg)]);
xlabel('samples');
title('the recovered signal');


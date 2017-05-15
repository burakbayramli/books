%Design of cosine modulated filter bank
%Searching for minimum

M=8; %number of channels
As=80; %stopband attenuation
tband=.5; %transition band
N=floor((As-7.95)/14.36*M/tband); %Kaiser formula for filter order
disp(strcat('Prototype order: ',num2str(N)))
beta=0.1102*(As-8.7);

%set exploration range
w1=0.55/M;w2=0.65/M;
dw=(w2-w1)/200;
phimin=1000; nj=1;

%exploration
for wx=w1:dw:w2,
   rwc(nj)=wx;
   p=fir1(N,wx,kaiser(N+1,beta));
   g=conv(p,p);
   aux=g(N+1:(-2*M):1); axl=length(aux);
   auxp= max(abs(aux(2:axl)))/aux(1);
   rphi(nj)=auxp;
   if auxp<phimin,
      phimin=auxp;
      wmin=wx;
   end;
   nj=nj+1;
end;   
 
disp(strcat('phi_min:  ',num2str(phimin)))
disp(strcat('wc_min:  ',num2str(wmin)))

%display
plot(rwc,rphi,'k');
title('evolution of the ojective function')
xlabel('wc');


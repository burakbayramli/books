% Denoising example
% with inverse of the Daubechies DWT 
% Visual Evoked Potential signal 
% Use after analysis routine (to get wty)

L=length(wty); %length of the DWT

%scaling filter
hden=4*sqrt(2); %coeff. denominator
hsq=sqrt(3); %factor
h=[(1+hsq)/hden, (3+hsq)/hden, (3-hsq)/hden, (1-hsq)/hden]; %Daubechies 4
hN=h;
N=length(hN);

K=9; %number of scales 
aux=0;
h0=hN;
h1=fliplr(hN); h1(2:2:N)=-h1(2:2:N);

Ln=1;
a=wty(1);
th=8; %noise threshold

for n=1:K,
   aux= 1+mod(0:N/2-1,Ln);
   d=wty(Ln+1:2*Ln); 
   %denoising at the higher scales
   if n>K-6,
      mm=length(d);
      for m=1:mm,
         if abs(d(m))<th,
            d(m)=0;
         end;
      end;   
   end;    
   ax(1:2:2*Ln+N)= [a a(1,aux)];
   dx(1:2:2*Ln+N)= [d d(1,aux)];
   a=conv(ax,h0)+ conv(dx,h1);   
   a=a(N:(N+2*Ln-1));
   Ln=2*Ln;
end;
   

figure(1)
plot(a,'k');
axis([0 512 1.2*min(a) 1.2*max(a)]);
xlabel('samples');
title('the denoised signal');



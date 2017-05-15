% Example of TV denoising
clear all;

% build a test signal
t=0:0.1:400;
N=length(t);
u= sin(0.3+(0.015*pi*t));
v= 1.2*square(0.018*pi*t,30);
a=u+v; %signal with no noise

x=a+(0.1*randn(1,N)); %signal with noise

% Prepare for computations
niter=20;
z=zeros(1,N-1); 
J=zeros(1,niter);
alpha=3;
lambda=0.5; th=lambda/2;


%start the algorithm ------------------------
for nn=1:niter,
   aux=[-z(1) -diff(z) z(end)];
   y=x-aux;
   aux1=sum(abs(y-x).^2);
   aux2=sum(abs(diff(y)));
   J(nn)=aux1+(lambda*aux2);
   z=z+((1/alpha)*diff(y));
   z=max(min(z,th),-th);
end

% display ----------------------
figure(1)
subplot (1,2,1)
plot(a,'k')
axis([0 4000 -3 3]);
title('original clean signal')
subplot(1,2,2)
plot(x,'k')
axis([0 4000 -3 3]);
title('signal with added noise')

figure(2)
subplot(1,2,1)
plot(J,'k')
xlabel('niter')
title('J evolution')
subplot(1,2,2)
plot(y,'k')
axis([0 4000 -3 3]);
title('denoised signal')

 


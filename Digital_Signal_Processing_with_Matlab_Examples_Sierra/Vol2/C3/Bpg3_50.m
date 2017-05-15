% Inverse Radon transform with filter
% for circle
clear all;

%The Radon transform--------------

%padded circle
A=ones(64,64);B=A;
for nn=1:64;
  for k=1:64,
   m=(nn-32)^2+(k-32)^2;
   if m>32^2, B(nn,k)=0; end;
  end;
end; 
R=zeros(128,128);
R(33:96,33:96)=B;

%projections
theta=0:1:180; %set of angles
N=length(theta);
PR=zeros(128,181);
for nn=1:N,
   aux=imrotate(R,theta(nn),'bilinear','crop');
   PR(:,nn)=sum(aux)';
end

%ramp filter
rampf=[2*[0:63,64:-1:1]'/128];
%Fourier domain filtering
FPR=fft(PR,128);
for nn=1:N,
   FPRf(:,nn)=FPR(:,nn).*rampf;
end;
%inverse Fourier
PR=real(ifft(FPRf));

%Backprojections---------------------- 

figure(1)

subplot(2,2,1)
aux=zeros(128,128); IR=aux;
MA=90;
Rbak;
imshow(nIR);
title('2 positions');

subplot(2,2,2)
aux=zeros(128,128); IR=aux;
MA=60;
Rbak;
imshow(nIR);
title('3 positions');

subplot(2,2,3)
aux=zeros(128,128); IR=aux;
MA=45;
Rbak;
imshow(nIR);
title('4 positions');

subplot(2,2,4)
aux=zeros(128,128); IR=aux;
MA=36;
Rbak;
imshow(nIR);
title('5 positions');
% Inverse Radon transform with filter
% for the rectangle example

%The Radon transform
%padded rectangle
B=ones(32,32);
R=zeros(64,64);
R(17:48,17:48)=B;
theta=0:1:180; %set of angles
N=length(theta);
PR=zeros(64,181);
%projections
for nn=1:N,
   aux=imrotate(R,theta(nn),'bilinear','crop');
   PR(:,nn)=sum(aux)';
end

%ramp filter
rampf=[2*[0:31,32:-1:1]'/64];
%Fourier domain filtering
FPR=fft(PR,64);
for nn=1:N,
   FPRf(:,nn)=FPR(:,nn).*rampf;
end;
%inverse Fourier
PR=real(ifft(FPRf));

%Backprojection   
aux=zeros(64,64);
IR=aux;

for nn=1:N,
   for np=1:64,
      aux(:,np)=PR(:,nn);
   end;
   IR=IR+imrotate(aux,theta(nn),'bilinear','crop');
end;

nIR=IR/max(max(IR)); %normalize

figure(1)
imshow(nIR);
title('Recovered rectangle, with ramp filter');
% Audio compression example
% analysis of music signal
% Haar Wavelet
% Hear original and compressed signals

%The music signal 
[y,fs]=wavread('Bizet1.wav'); %read wav file
y1=y(:,1); %mono channel
y1=y1-mean(y1); %zero mean
Nss=300*2048; %number of signal samples
sg=y1(1:Nss);
tiv=1/fs;

disp('the original music');
soundsc(sg,fs);
pause(16);
disp('now, wavelets in action');

%analysis of the signal with wavelets
%divide the signal into 230 segments
%apply wavelet analysis to each segment
K=11; %number of scales (2048=2^11)
cq=sqrt(3);
wty=zeros(300,2048);

En=zeros(1,K); %for energy measurement

for nst=1:300,
   bg=((nst-1)*2048)+1;
   y=sg(bg:(bg+2047)); %music segment 
   
   %Haar wavelet
   NN=2048;
   for n=1:K,
      aux1= y(1:2:NN-1) + y(2:2:NN);
      aux2= y(1:2:NN-1) - y(2:2:NN);
      y(1:NN)=[aux1,aux2]/sqrt(2);
      En(n)=En(n)+sum(y((NN/2)+1:NN).^2);
      NN=NN/2;
   end;
   
  wty(nst,:)=y';  
end;

%--------------------------------------------
%compress the signal by deleting highest scale data
aux=zeros(300,1024);
cwty=zeros(300,1024); %50% smaller
for nn=1:300,
   aux(nn,:)=wty(nn,1:(2^10)); %delete the upper half of wty
end;
cwty=aux; %compressed audio (to be stored)


%--------------------------------------------
% read compressed audio and recover the signal
aux=zeros(300,1024);
rwty=zeros(300,2048);
%append zeros for the upper half of rwty
for nn=1:300,
   rwty(nn,:)=[cwty(nn,:) aux(nn,:)];
end;

%---------------
%conventional signal recovery
ry=zeros(1,Nss);
J=K+1;
a=zeros(J,(2^K)); %space for a(j,k) coefficients

%signal recovering (wavelet synthesis)
for nst=1:300,
   m=1;
   z=rwty(nst,:);
   a(1,1)=z(1);
   for n=1:K,            
      a(n+1,1:2:(2*m-1))=(a(n,1:m)+z((1+m):(2*m)))/sqrt(2);
      a(n+1,2:2:(2*m))=(a(n,1:m)-z((1+m):(2*m)))/sqrt(2);
      m=m*2;
   end;   
   bg=((nst-1)*2048)+1;
   ry(bg:(bg+2047))=a(J,:); %signal recovery  
 end;  
      
disp('and the decompressed music');
soundsc(ry,fs);

%display of signal energy vs scale------------------------
figure(1)
bar(En); 
title('signal energy vs. scale');
xlabel('scale number');
% ECG analysis by continuous wavelet transform
% Morlet Wavelet
% Plot of signal and scalogram
clear all;
disp('please wait'); %ask for patience

%The ECG signal 
fs=200; %samplig frequency in Hz
tiv=1/fs; %time interval between samples
Ts=tiv; %sampling period

%read signal file
fer=0;
while fer==0,  
fid2=fopen('ECG_short.txt','r');
if fid2==-1, disp('read error') 
else sg=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

Nss=length(sg); %number of signal samples
duy=(Nss-1)*tiv; %duration of signal
tss=0:tiv:duy; %time intervals set

y=[sg(1)*ones(1,128) sg' sg(end)*ones(1,128)]; %padding
y=y-mean(y); %zero-mean signal
ND=length(y); %number of data
NS=128; %number of scales

CC=zeros(NS,ND); %for output (coeffs)

% Pre-compute wavelet tensor------------
PSIa=zeros(NS,ND,ND); %array 

nn=1:ND;
t=Ts*(nn-1);
for ee=1:NS, 
   s=(ee*0.006)+0.05; %scales
   for rr=1:ND, %delays
      a=Ts*(rr-1);
      val=0;     
      %vectorized part (t)         
         x=(t-a)/s; 
         PSIa(ee,rr,:)=(1/sqrt(s))*(exp(-(x.^2)/2).*cos(5*x));
    end;
end;
 
disp('wavelet tensor is now ready')
 
%CWT------------------------
nd=1:ND;
for ne=1:NS,
   aux=squeeze(PSIa(ne,nd,:));
   val=(y)*aux;
   CC(ne,nd)=val;
end;


%display--------------------
figure (1)
subplot(2,1,1)
plot(tss,y(129:(128+Nss)),'k');
axis([0 (Nss-1)*tiv min(y)-0.1 max(y)+0.1]);
xlabel('sec'); ylabel('signal');
title('wavelet analysis');
subplot(2,1,2)
imagesc(CC(:,129:(128+Nss)));
colormap('jet'); 
xlabel('samples'); ylabel('scales');


% Haar wavelet transform of a basic image
% square and rhombus
% Using filters
%
%Haar filter
c=1/sqrt(2);
h0=[c c]; %low-pass filter
h1=[-c c]; %high-pass filter
%
fg=ones(256,256); %white plane
%the rhombus
for n=1:32,
   fgx=64+(-n:n); fgy=96+n;
   fg(fgx,fgy)=0; %one triangle
   fg(fgx,256-fgy)=0; %the other triangle
end
%the square
fg(128:196,96:160)=0;
%
%Step1
%1 scale wavelet transform of rows
lfg1=conv2(fg,h0); %low-pass filtering of rows
lfg1=lfg1(:,2:2:end); %downsampling to get L
hfg1=conv2(fg,h1); %high-pass filtering of rows
hfg1=hfg1(:,2:2:end); %downsampling to get H
%Step 2
%1 scale wavelet transform of columns of previous step
llfg1=conv2(lfg1,h0'); %low-pass filtering of L columns
llfg1=llfg1(2:2:end,:); %downsampling
hlfg1=conv2(lfg1,h1'); %high-pass filtering of L columns
hlfg1=hlfg1(2:2:end,:); %downsampling

lhfg1=conv2(hfg1,h0'); %low-pass filtering of H columns
lhfg1=lhfg1(2:2:end,:); %downsampling
hhfg1=conv2(hfg1,h1'); %high-pass filtering of H columns
hhfg1=hhfg1(2:2:end,:); %downsampling

%normalization to bw
A=zeros(128,128);
Nllfg1=A; Nlhfg1=A; Nhlfg1=A; Nhhfg1=A;
for nn=1:128,
   for mm=1:128,
      if abs(llfg1(nn,mm))>0, Nllfg1(nn,mm)=1; end;
      if abs(lhfg1(nn,mm))>0, Nlhfg1(nn,mm)=1; end;
      if abs(hlfg1(nn,mm))>0, Nhlfg1(nn,mm)=1; end;
      if abs(hhfg1(nn,mm))>0, Nhhfg1(nn,mm)=1; end;
   end;
end;

tfg1=zeros(256,256); %space for compound image

tfg1(1:128,1:128)=Nllfg1; %LL image
tfg1(1:128,129:256)=Nlhfg1; %LH image
tfg1(129:256,1:128)=Nhlfg1; %HL image
tfg1(129:256,129:256)=Nhhfg1; %HH image


%Figure
figure(1)
imshow(tfg1);
title('Haar image transform');

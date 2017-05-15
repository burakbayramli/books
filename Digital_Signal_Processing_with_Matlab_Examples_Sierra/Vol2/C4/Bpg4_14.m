% Haar 2-Level wavelet transform of an image
% Young image
% Using filters
%
%Haar filter
c=1/sqrt(2);
h0=[c c]; %low-pass filter
h1=[-c c]; %high-pass filter
%
% The image
ufg=imread('Chica.tif'); %read the image file into a matrix
fg=double(ufg); %convert to float
fg=fg-mean(mean(fg)); %zero mean
[Nr,Nc]=size(fg); 
tfg=zeros(Nr,Nc); %space for wavelet plane

xfg=fg;
for nL=1:2, %levels
   Nv=floor(Nr/2^nL); Nh=floor(Nc/2^nL);
   lfg=zeros(2*Nv,Nh); hfg=zeros(2*Nv,Nh);
   llfg=zeros(Nv,Nh); hlfg=zeros(Nv,Nh); 
   lhfg=zeros(Nv,Nh); hhfg=zeros(Nv,Nh);
  %
  %Step1
  %wavelet transform of rows
  aux=conv2(xfg,h0); %low-pass filtering of rows
  lfg=0.5*aux(:,2:2:end); %downsampling to get L
  aux=conv2(xfg,h1); %high-pass filtering of rows
  hfg=0.5*aux(:,2:2:end); %downsampling to get H
  %Step 2
  %wavelet transform of columns of previous step
  aux=conv2(lfg,h0'); %low-pass filtering of L columns
  llfg=aux(2:2:end,:); %downsampling
  aux=conv2(lfg,h1'); %high-pass filtering of L columns
  hlfg=aux(2:2:end,:); %downsampling

  aux=conv2(hfg,h0'); %low-pass filtering of H columns
  lhfg=aux(2:2:end,:); %downsampling
  aux=conv2(hfg,h1'); %high-pass filtering of H columns
  hhfg=aux(2:2:end,:); %downsampling

  %save on wavelet plane
  V1=1:Nv; V2=Nv+1:2*Nv; H1=1:Nh; H2=Nh+1:2*Nh; %ranges
  tfg(V1,H1)=llfg; tfg(V1,H2)=lhfg;
  tfg(V2,H1)=hlfg; tfg(V2,H2)=hhfg;
  
  xfg=llfg; %prepare next level
  
end;

%Figure
figure(1)
imshow(tfg,[-20 20]);
title('2-level Haar image transform');


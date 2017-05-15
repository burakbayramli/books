% Image denoising experiment

%Read a 256x256 image
ufg=imread('trees1.jpg'); %read the image file into a matrix
fg=double(ufg); %convert to float
x=fg-mean(mean(fg)); %zero mean
[Nr,Nc]=size(fg); 
tfg=zeros(Nr,Nc); %space for wavelet plane

%(A) ==========================================
v=zeros(256,256);
%or add fine noise
%v=50*randn(256,256);

%or add patched noise
pa=ones(2,2);
for ni=1:128,
   for nj=1:128,
      mi=1+(ni-1)*2; mj=1+(nj-1)*2;
      v(mi:mi+1,mj:mj+1)=50*randn(1,1)*pa;
   end;
end; 

%mix image and noise
Kmix=0.8; %to be edited
x=Kmix*x; v=(1-Kmix)*v;
fg=x+v;
%=============================================

%show the noisy image
figure(1)
imshow(fg,[min(min(fg)),max(max(fg))]);
title('noisy image');

%Wavelet analysis--------------------------------------------
% Haar 2-Level wavelet image transform
c=1/sqrt(2);

xfg=fg;
%Level 1---------------------------
%Step1
N=256; M=128;
%1 scale wavelet transform of rows
lfg1=zeros(N,M); hfg1=zeros(N,M);
for nn=1:N,
   auxL=fg(nn,1:2:N)+fg(nn,2:2:N); 
   auxH=fg(nn,1:2:N)-fg(nn,2:2:N);
   lfg1(nn,1:M)=c*auxL; hfg1(nn,1:M)=c*auxH;
end;

%Step 2
%1 scale wavelet transform of columns of previous step
llfg1=zeros(M,M); hlfg1=zeros(M,M);
hhfg1=zeros(M,M); lhfg1=zeros(M,M);
%columns of L
for nn=1:M,
   auxL=lfg1(1:2:N,nn)+lfg1(2:2:N,nn);
   auxH=lfg1(1:2:N,nn)-lfg1(2:2:N,nn);
   llfg1(1:M,nn)=c*auxL; %image LL
   hlfg1(1:M,nn)=c*auxH; %image HL
end;
%columns of H
for nn=1:M,
   auxL=hfg1(1:2:N,nn)+hfg1(2:2:N,nn);
   auxH=hfg1(1:2:N,nn)-hfg1(2:2:N,nn);
   lhfg1(1:M,nn)=c*auxL; %image LH
   hhfg1(1:M,nn)=c*auxH; %image HH
end;

%save on wavelet plane
tfg(1:M,M+1:N)=lhfg1; 
tfg(M+1:N,1:M)=hlfg1; tfg(M+1:N,M+1:N)=hhfg1;

%Level 2---------------------------
fg=llfg1;
%Step1
N=128; M=64;
%1 scale wavelet transform of rows
lfg2=zeros(N,M); hfg2=zeros(N,M);
for nn=1:N,
   auxL=fg(nn,1:2:N)+fg(nn,2:2:N); 
   auxH=fg(nn,1:2:N)-fg(nn,2:2:N);
   lfg2(nn,1:M)=c*auxL; hfg2(nn,1:M)=c*auxH;
end;

%Step 2
%1 scale wavelet transform of columns of previous step
llfg2=zeros(M,M); hlfg2=zeros(M,M);
hhfg2=zeros(M,M); lhfg2=zeros(M,M);
%columns of L
for nn=1:M,
   auxL=lfg2(1:2:N,nn)+lfg2(2:2:N,nn);
   auxH=lfg2(1:2:N,nn)-lfg2(2:2:N,nn);
   llfg2(1:M,nn)=c*auxL; %image LL
   hlfg2(1:M,nn)=c*auxH; %image HL
end;
%columns of H
for nn=1:M,
   auxL=hfg2(1:2:N,nn)+hfg2(2:2:N,nn);
   auxH=hfg2(1:2:N,nn)-hfg2(2:2:N,nn);
   lhfg2(1:M,nn)=c*auxL; %image LH
   hhfg2(1:M,nn)=c*auxH; %image HH
end;

%save on wavelet plane
tfg(1:M,1:M)=llfg2; tfg(1:M,M+1:N)=lhfg2; 
tfg(M+1:N,1:M)=hlfg2; tfg(M+1:N,M+1:N)=hhfg2;


%(B) ======================================
% display of analysis result--------

%total wavelet plane
figure(3)
imshow(tfg,[-20 20]);
title('2-level Haar image transform');

%figure(4)
%imshow(llfg1,[min(min(llfg1)), max(max(llfg1))]);
%title('LL1, Haar image transform');

%figure(5)
%imshow(llfg2,[min(min(llfg2)), max(max(llfg2))]);
%title('LL2, Haar image transform');

%==========================================

%(C) ======================================
% Hard denoising
th=30; %threshold (edit)
for ni=1:128,
   for nj=1:128,
      if abs(hhfg1(ni,nj))<th, hhfg1(ni,nj)=0; end;
      if abs(hlfg1(ni,nj))<th, hlfg1(ni,nj)=0; end;      
      if abs(lhfg1(ni,nj))<th, lhfg1(ni,nj)=0; end;
   end;
end;   

th=60; %threshold (edit)
for ni=1:64,
   for nj=1:64,
      if abs(hhfg2(ni,nj))<th, hhfg2(ni,nj)=0; end;
      if abs(lhfg2(ni,nj))<th, lhfg2(ni,nj)=0; end;
      if abs(lhfg2(ni,nj))<th, lhfg2(ni,nj)=0; end;
   end;
end;

%display of thresholding result---------
dtfg=zeros(256,256);
%save on denoised wavelet plane
N=256; M=128;
dtfg(1:M,1:M)=llfg1; dtfg(1:M,M+1:N)=lhfg1; 
dtfg(M+1:N,1:M)=hlfg1; dtfg(M+1:N,M+1:N)=hhfg1;
N=128; M=64;
dtfg(1:M,1:M)=llfg2; dtfg(1:M,M+1:N)=lhfg2; 
dtfg(M+1:N,1:M)=hlfg2; dtfg(M+1:N,M+1:N)=hhfg2;

figure(6)
imshow(dtfg,[-20 20]);
title('2-level Haar denoised image transform');

%=========================================


%Wavelet synthesis-------------------------------------------

rfg=zeros(256,256); %space for image

%recovery from level 2--------------------
%
N=128; M=64;
%recovery of L
for nn=1:M,
   auxL(1:2:N)=llfg2(1:M,nn)+hlfg2(1:M,nn);
   auxL(2:2:N)=llfg2(1:M,nn)-hlfg2(1:M,nn);
   lfg2(1:N,nn)=c*auxL; %image L   
end;
%recovery of H
for nn=1:M,
   auxH(1:2:N)=lhfg2(1:M,nn)+hhfg2(1:M,nn);
   auxH(2:2:N)=lhfg2(1:M,nn)-hhfg2(1:M,nn);
   hfg2(1:N,nn)=c*auxH; %image H   
end;

%recovery of original
for nn=1:N,
   auxL(1:2:N)=lfg2(nn,1:M)+hfg2(nn,1:M);
   auxL(2:2:N)=lfg2(nn,1:M)-hfg2(nn,1:M);
   rfg(nn,1:N)=c*auxL'; %image H   
end;

llfg1=rfg;

%recovery from level 1--------------------
%
N=256; M=128;

%recovery of L
for nn=1:M,
   auxL(1:2:N)=llfg1(1:M,nn)+hlfg1(1:M,nn);
   auxL(2:2:N)=llfg1(1:M,nn)-hlfg1(1:M,nn);
   lfg1(1:N,nn)=c*auxL; %image L   
end;
%recovery of H
for nn=1:M,
   auxH(1:2:N)=lhfg1(1:M,nn)+hhfg1(1:M,nn);
   auxH(2:2:N)=lhfg1(1:M,nn)-hhfg1(1:M,nn);
   hfg1(1:N,nn)=c*auxH; %image H   
end;

%recovery of original
for nn=1:N,
   auxL(1:2:N)=lfg1(nn,1:M)+hfg1(nn,1:M);
   auxL(2:2:N)=lfg1(nn,1:M)-hfg1(nn,1:M);
   rfg(nn,1:N)=c*auxL'; %image H   
end;

% display------------------
figure(2)
imshow(rfg,[min(min(rfg)),max(max(rfg))]);
title('denoised image');


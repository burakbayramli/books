%Laplacian pyramid
%use of masks in the Fourier domain

%get an image
ufg=imread('Parth1.tif');
fg=double(ufg); %convert to float
fg=fg-mean(mean(fg)); %zero mean
[Nr,Nc]=size(fg);
knr=sqrt(Nr*Nc); %normalization constant

FIG=fftshift(fft2(ifftshift(fg)))/knr; %2D fft of the picture

nlevels=5; %number of pyramid levels

%preparing indexed matrix sets
PH=cell(nlevels,1); %for high pass
PL=cell(nlevels,1); %for low pass
MK=cell(nlevels,1); %for masks

PL(1)={fg}; PH(1)={ones(Nr,Nc)};

mkb1=[0.2 0.4 0.6 0.8]; mkb2=fliplr(mkb1); %mask borders

aux=FIG; mr=Nr; mc=Nc;
faux=fg;

for nn=2:nlevels,
   
   %prepare a low-pass rectangular mask
   sz1=zeros(1,mc/4); so1=ones(1,(mc/2)-8);
   mk1=[sz1,mkb1,so1,mkb2,sz1]; %mask side 1
   sz2=zeros(1,mr/4); so2=ones(1,(mr/2)-8);
   mk2=[sz2,mkb1,so2,mkb2,sz2]; %mask side 2
   mask=mk2'*mk1; %the low-pass mask
   
   %low-pass filtering in 2D Fourier domain
   FFL=aux.*mask; %image filtering (full size)
   %extract internal rectangle (subsampling)
   dFFL=FFL((1+(mr/4)):((3*mr)/4),(1+(mc/4)):((3*mc)/4));
   aux=dFFL;
   
   %back to image domain   
   FL=fftshift(ifft2(ifftshift(FFL)))*knr; %L*
   dFL=fftshift(ifft2(ifftshift(dFFL)))*knr; %L
   FH=faux-FL; %H
   faux=dFL;
   PL(nn)={real(dFL)};
   PH(nn)={real(FH)};
   MK(nn-1)={mask};
   mc=mc/2; mr=mr/2;
end;

%display of masks
figure(1)
aw=0;
for nn=1:nlevels-1
   ax=2^(nn-1);
   subplot('Position',[0.02+aw,0.01,0.45/ax,0.45/ax])
   aux=MK{nn}; 
   imshow(1-aux);   
   s=num2str(nn); msg=['Mask',s];
   title(msg);
   aw=aw+(0.45/ax);
end;   

%display (with conversion to imshow range)
figure(2)
for nn=1:nlevels
   subplot(2,nlevels,nn)
   aux=PL{nn}; 
   m=min(min(aux));
   M=max(max(aux));
   imshow((aux-m)/(M-m));
   s=num2str(nn-1); msg=['L',s];
   title(msg);
end;   
for nn=2:nlevels
   subplot(2,nlevels,nlevels+nn)
   aux=PH{nn}; 
   m=min(min(aux));
   M=max(max(aux));
   imshow((aux-m)/(M-m));
   s=num2str(nn-1); msg=['H',s];
   title(msg);
end;   

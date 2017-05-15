%Laplacian pyramid

%get an image
ufg=imread('Parth1.tif');
fg=double(ufg); %convert to float
fg=fg-mean(mean(fg)); %zero mean
[Nr,Nc]=size(fg); 

nlevels=4; %number of pyramid levels

%preparing indexed matrix sets
PH=cell(nlevels,1); %for high pass
PL=cell(nlevels,1); %for low pass

PL(1)={fg}; PH(1)={ones(Nr,Nc)};

aux=fg;
for nn=2:nlevels,
   fil=fspecial('gaussian',[16,16],4);
   FL=filter2(fil,aux);
   dFL=FL(1:2:end,1:2:end); %subsampling
   FH=aux-FL;
   PL(nn)={dFL};
   PH(nn)={FH};
   aux=dFL;
end;

%display (with conversion to imshow range)
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

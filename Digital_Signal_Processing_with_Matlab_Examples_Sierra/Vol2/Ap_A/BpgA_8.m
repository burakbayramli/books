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
aw=0;
for nn=1:nlevels
   ax=2^(nn-1); 
   subplot('Position',[0.01+aw,0.50,0.44/ax,0.44/ax])
   aux=PL{nn}; 
   m=min(min(aux));
   M=max(max(aux));
   imshow((aux-m)/(M-m));
   hold on;
   s=num2str(nn-1); msg=['L',s];
   title(msg);
   aw=aw+(0.49/ax);
end;
aw=0.15;
for nn=2:nlevels
   ax=2^(nn-2);
   subplot('Position',[0.01+aw,0.01,0.44/ax,0.44/ax])
   aux=PH{nn}; 
   m=min(min(aux));
   M=max(max(aux));
   imshow((aux-m)/(M-m));
   hold on;
   s=num2str(nn-1); msg=['H',s];
   title(msg);
   aw=aw+(0.45/ax);
end;   

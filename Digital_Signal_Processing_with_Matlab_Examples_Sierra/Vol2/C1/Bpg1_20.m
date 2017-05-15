% Display of 2D DCT bases 
% example of 8x8
%
N=8; 
% computation of bases
i=0:N-1; 
j=0:N-1; 
[ni,nj]=meshgrid(i,j); 
A=sqrt(2/N)*cos(((2.*ni+1).*nj*pi)/(N*2)); 
A(1,:)=A(1,:)./sqrt(2); 
A=A';   
B=zeros(N,N,N,N);
for i=1:N, 
 for j=1:N, 
    B(:,:,i,j)=A(:,i)*A(:,j)'; 
 end; 
end;     

%image composition
L=2+N; %side of a square
Bimg=zeros(N*L,N*L); %space for the image
aux=zeros(8,8);
for i=0:N-1, 
 for j=0:N-1,
    ix=((i*L)+2):(((i+1)*L)-1); 
    jx=((j*L)+2):(((j+1)*L)-1);
    aux=B(i+1,j+1,:,:);
    Bimg(ix,jx)=squeeze(aux);
 end; 
end;     

%display
imagesc(0:7,0:7,Bimg);
colormap('copper');
title('DCT2 bases');
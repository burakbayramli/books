%Example of picture denoising using Perona-Malik diffusion
%
%
lambda=160; %constant for P-M diffusivity
D=0.1; %general diffusion constant

P=imread('face1.jpg'); %read image
A=imnoise(P,'salt&pepper',0.01); %add salt&pepper noise

u=double(A); un=u;
[ly,lx]=size(u);

for nn=1:8,

%zero padding around image
udif=zeros(ly+2,lx+2);
udif(2:ly+1,2:lx+1)=u;

%differences: north, south, east, west
difN=udif(1:ly,2:lx+1)-u;
difS=udif(3:ly+2,2:lx+1)-u;
difE=udif(2:ly+1,3:lx+2)-u;
difW=udif(2:ly+1,1:lx)-u;

%Diffusivities
DN=1./(1+(difN/lambda).^2);
DS=1./(1+(difS/lambda).^2);
DE=1./(1+(difE/lambda).^2);
DW=1./(1+(difW/lambda).^2);

%diffusion
un=u+D*(DN.*difN + DS.*difS + DE.*difE + DW.*difW);
u=un;

end;

%display:
figure(1)
subplot(1,2,1)
imshow(A);
xlabel('original');
title('Salt&pepper denoising');
subplot(1,2,2)
imshow(uint8(un));
xlabel('denoised image');

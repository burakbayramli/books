% Radon transform of a rectangle

%padded rectangle
B=ones(32,32);
R=zeros(64,64);
R(17:48,17:48)=B;

theta=0:1:180; %set of angles
N=length(theta);
PR=zeros(64,181);

%projections
for nn=1:N,
   aux=imrotate(R,theta(nn),'bilinear','crop');
   PR(:,nn)=sum(aux)';
end

nPR=PR/max(max(PR)); %normalize for display
    
figure(1)
imshow(R);
title('rectangular image');

figure(2)
imshow(nPR);
title('Radon transform (sinogram)');
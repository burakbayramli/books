% Radon transform of a "phantom"

%a "phantom"
x=-127:1:128; y=-127:1:128;
[X,Y]=meshgrid(x,y);
z1=(4*(X.*X)+(Y.*Y))<(128/1.5)^2;
aX=X-15; aY=Y;
z2=((aX.*aX)+(aY.*aY))<(128/8)^2;
R=(0.9*z2)+(0.5*z1);

theta=0:1:180; %set of angles
N=length(theta);
PR=zeros(256,181);

%projections
for nn=1:N,
   aux=imrotate(R,theta(nn),'bilinear','crop');
   PR(:,nn)=sum(aux)';
end

nPR=PR/max(max(PR)); %normalize for display
    
figure(1)
imshow(R);
title('a "phantom"');

figure(2)
imshow(nPR);
title('Radon transform (sinogram)');
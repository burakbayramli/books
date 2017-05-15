% Example of non-separability
% 

% 2 data clusters (non linearly separable) on 2D
N=300;
X1=zeros(N,2); X2=zeros(N,2);
mu1=0; sigma1=0.35; mu2=0; sigma2=0.25;

for nn=1:N,
   R1=normrnd(mu1,sigma1);
   phi1=rand(1)*2*pi; 
   X1(nn,1)=R1*cos(phi1); X1(nn,2)=R1*sin(phi1);
   
   R2=2.5+normrnd(mu2,sigma2);
   phi2=-1.5+(1.1*rand(1)* pi); 
   X2(nn,1)=R2*cos(phi2); X2(nn,2)=R2*sin(phi2);
end;

figure(1)
scatter(X1(:,1),X1(:,2),'k'); hold on;
scatter(X2(:,1),X2(:,2),'rd');
axis([-2 4 -4 4]);
title('two classes, non-separable data');




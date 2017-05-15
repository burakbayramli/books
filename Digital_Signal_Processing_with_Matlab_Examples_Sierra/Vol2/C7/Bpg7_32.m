% From 2D to 3D, data separability
% 

% 2 data clusters (non linearly separable) on 2D
N=200;
X1=zeros(N,2); X2=zeros(N,2);
mu1=0; sigma1=0.4; mu2=0; sigma2=0.3;

for nn=1:N,
   R1=normrnd(mu,sigma1);
   phi1=rand(1)*2*pi; 
   X1(nn,1)=R1*cos(phi1); X1(nn,2)=R1*sin(phi1);
   
   R2=3+normrnd(mu,sigma2);
   phi2=rand(1)*2*pi; 
   X2(nn,1)=R2*cos(phi2); X2(nn,2)=R2*sin(phi2);
end;

figure(1)
scatter(X1(:,1),X1(:,2),'k'); hold on;
scatter(X2(:,1),X2(:,2),'rd');
title('two classes, non-separable data (2D)');

figure(2)
kq=sqrt(2);
x=X1(:,1).^2; y=X1(:,2).^2; z= kq*x.*y;
plot3(x,y,z,'ko'); hold on;
view(50,40);
x=X2(:,1).^2; y=X2(:,2).^2; z= kq*x.*y;
plot3(x,y,z,'rd'); hold on;
grid;
title('separable classes (3D)');



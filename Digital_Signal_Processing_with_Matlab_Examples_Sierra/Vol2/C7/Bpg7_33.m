% kernel-SVM classification example
% 2 data clusters (non linearly separable) on 2D

%data
N=200;
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
X=[X1;X2];
Y=ones(2*N,1); Y(1:N,1)=-Y(1:N,1); %labels

%----------------------------------------------------
%prepare quadratic optimization
lambda = 1e-7;  
c = 1000;

ps=X*X'; %scalar product
normx=sum(X.^2,2);
[nps,mps]=size(ps);
aux=-2*ps+repmat(normx,1,mps)+repmat(normx',nps,1);
K=exp(-aux/2); %gaussian kernel

A=Y;
b=0;
H =K.*(Y*Y'); 
e = ones(size(Y));

[alpha , lambda , pos] = qpg(H,e,A,b,c,lambda,0,X,ps,[]); %quadratic prog.
%pos is position in X of the support vectors

xsup=X(pos,:);
ysup=Y(pos);
w=alpha.*ysup;

%------------------------------------------------------
% Prepare visualization
[xg1,xg2]=meshgrid([-3:0.2:4],[-4:0.2:4]);
[nl,nc]=size(xg1); q=nl*nc; 
xt1=reshape(xg1,1,q); xt2=reshape(xg2,1,q);
xt=[xt1;xt2]'; %set of test points
xtest=xt; 

ps=xt*xsup'; %scalar product
normxt=sum(xt.^2,2);
normxsup=sum(xsup.^2,2);
[nps,mps]=size(ps);
yt=zeros(nps,1);
aux=-2*ps+repmat(normxt,1,mps)+repmat(normxsup',nps,1);
Kt=exp(-aux/2); %gaussian kernel
yt(:)=yt(:)+Kt*w(1:mps);
yt=yt+lambda;
yt2d=reshape(yt,nl,nc);

%-----------------------------------------------------
%Display 

figure(1)
%background levels
colormap('hsv')
contourf(xg1,xg2,yt2d,50);shading flat; hold on

%separating line and margins
[L,A]=contour(xg1,xg2,yt2d,[-1 0 1],'r');
clabel(L,A); %plot of lines with labels

%data
scatter(X(:,1),X(:,2),16,'k')

%support vectors
scatter(xsup(:,1),xsup(:,2),40,'kd');

xlabel('x1');ylabel('x2');
title('Kernel-SVM, non-separable data');
axis([-3 4 -4 4]);



% kernel-SVM classification example
% IRIS data
% x=sepal width, y=petal length

%read IRIS data
D=dlmread('iris.data'); %columns

X=[D(:,2), D(:,3)]; %two columns (x,y)
Y=ones(150,1); Y(1:50)=-Y(1:50); %labels:1 or -1

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
[xg1,xg2]=meshgrid([0:0.2:6],[-1:0.2:8]);
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
colormap('cool')
contourf(xg1,xg2,yt2d,50);shading flat; hold on

%separating line and margins
[L,A]=contour(xg1,xg2,yt2d,[-1 0 1],'r');
clabel(L,A); %plot of lines with labels

%data
scatter(X(:,1),X(:,2),16,'k')

%support vectors
scatter(xsup(:,1),xsup(:,2),40,'kd');

xlabel('x1');ylabel('x2');
title('Kernel-SVM, IRIS data');
axis([0 6 -1 8]);



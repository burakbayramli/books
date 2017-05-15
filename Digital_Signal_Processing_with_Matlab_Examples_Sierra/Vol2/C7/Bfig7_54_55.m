% From 1D to 2D, data separability
% 

% 2 data clusters (non linearly separable) on a line
N=20;
X1=zeros(2*N,1); X2=zeros(2*N,1);
mu=0; sigma=0.45;
%left hand data:
X1(1:N)=normrnd(mu,sigma,N,1)-2.5;
X2(1:N)=-normrnd(mu,sigma,N,1);
%right hand data
X1(N+1:2*N)=normrnd(mu,sigma,N,1)+2.5;
X2(N+1:2*N)=normrnd(mu,sigma,N,1);

Y=ones(2*N,1);
figure(1)
plot(X1,Y,'r*','MarkerSize',12); hold on;
plot(X2,Y,'kd');
plot([-4 4],[1 1],'b');
axis([-4 4 -0.5 5]);
title('two classes, non-separable data (1D)');

aux=-4:(1/N):4; Y2=aux.^2;
figure(2)
plot(X1,X1.^2,'r*','MarkerSize',12); hold on;
plot(X2,X2.^2,'kd');
plot(aux,Y2,'b')
plot([-4 4],[1.5 1.5],'g');
axis([-4 4 -0.5 15]);
title('two classes, separable data (2D)');



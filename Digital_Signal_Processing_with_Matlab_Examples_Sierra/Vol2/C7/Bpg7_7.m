% PCA example: correlated variables
%showing the 2 PCA components

%data--------------
N=1000; M=2;
D=zeros(M,N);
D(1,:)=normrnd(0,1,1,N);
D(2,:)=(0.6*D(1,:))+(0.4*normrnd(0,1,1,N));

%PCA---------------
me=mean(D,2); %mean of each row
X=D-repmat(me,1,N); %subtract mean in each row
A=X'/sqrt(N-1);
%singular value decomposition
[U,S,V]=svd(A); %V contains principal components 
SD=diag(S);
vr=SD.*SD; %variances
prd=V'*X; %data projection

%display
plot(X(1,:),X(2,:),'k.'); hold on;
plot([-4*V(1,1) 4*V(1,1)],[-4*V(2,1) 4*V(2,1)],'r');
plot([-4*V(1,2) 4*V(1,2)],[-4*V(2,2) 4*V(2,2)],'r');
axis([-4 4 -4 4]);
title('PCA components'); xlabel('x1'); ylabel('x2');

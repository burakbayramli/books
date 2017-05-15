% SVM classification example
% IRIS data
% x=sepal width, y=petal length

%read IRIS data
D=dlmread('iris.data'); %columns

X=[D(:,2), D(:,3)]; %two columns (x,y)
Y=ones(150,1); Y(1:50)=-Y(1:50); %labels:1 or -1

%prepare quadratic optimization
lambda = 1e-7;  
c = 1000;
ps=X*X'; %scalar product
A=Y;
b=0;

H =ps.*(Y*Y'); 
e = ones(size(Y));

[alpha , lambda , pos] = qpg(H,e,A,b,c,lambda,0,X,ps,[]); %quadratic prog.
%pos is position in X of the support vectors

%slope of separation line
mx=alpha'*(X(pos,1).*Y(pos));
my=alpha'*(X(pos,2).*Y(pos));
m=-mx/my; %

%compute line parameters
b1=X(pos(1),2)-(m*X(pos(1),1));
b2=X(pos(3),2)-(m*X(pos(3),1));
bm=(b1+b2)/2;

%display
figure(1)
%the data
scatter(X(1:50,1),X(1:50,2),32,'k'); hold on;
scatter(X(51:150,1),X(51:150,2),32,'r');
%the support vectors
scatter(X(pos,1),X(pos,2),64,'bd');
%lines for visualizing separation margin 
plot([0 6],[b1 (m*6)+b1],'b--');
plot([0 6],[b2 (m*6)+b2],'b--');
plot([0 6],[bm (m*6)+bm],'r');
axis([0 6 0 8]);
title('Linear SVM on IRIS data'); 
xlabel('sepal width'); ylabel('petal length');

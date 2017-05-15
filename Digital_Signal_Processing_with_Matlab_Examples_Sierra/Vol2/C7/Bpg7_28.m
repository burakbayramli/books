% LDA line
% x=sepal width, y=petal length

%read IRIS data
D=dlmread('iris.data')';

D1=[D(2,1:50);D(3,1:50)]; %class 1, x-y data
D2=[D(2,51:150);D(3,51:150)]; %class 2, x-y data

%within-class mean
meanD1=mean(D1,2);
meanD2=mean(D2,2);
%data mean
meanD=(meanD1+meanD2)/2;

%within-class covariance matrix
mcovD1=cov(D1');
mcovD2=cov(D2');

%within-class scatter matrix
Sw=(mcovD1+mcovD2)/2;

%between-class scatter matrix
Sb1=(meanD1-meanD)*(meanD1-meanD)';
Sb2=(meanD2-meanD)*(meanD2-meanD)';
Sb=Sb1+Sb2;

%compute direction vector
A=inv(Sw)*Sb;
[V,E]=eig(A);

%in this case, the second eigenvector is the largest, 
%so we take the second column of V

%LDA line: y=mx + b, 
m=V(2,2)/V(1,2);
b=meanD(2)-(m*meanD(1)); %this line crosses the data centroid

%display
figure(1)
scatter(D1(1,:),D1(2,:),32,'k'); hold on;
scatter(D2(1,:),D2(2,:),32,'r');
axis([0 7 0 7]);
plot(meanD1(1),meanD1(2),'r*','MarkerSize',12); %centroid of set1
plot(meanD2(1),meanD2(2),'k*','MarkerSize',12); %centroid of set2
plot(meanD(1),meanD(2),'b*','MarkerSize',12); %data centroid
len=7; %line length, arbitrary value
plot([0 len],[b ((m*len)+b)],'k'); %LDA line
title('IRIS data: LDA line'); 
xlabel('sepal width'); ylabel('petal length');


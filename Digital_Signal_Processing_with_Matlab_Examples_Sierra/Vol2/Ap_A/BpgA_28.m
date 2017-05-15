% Projection on the LDA line
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
b=meanD(2)-(m*meanD(1));
alpha=atan(m);

%traslation of the origin
beta=(pi/2)-alpha;
L=abs(b*sin(beta));
xt0=abs(L*cos(beta)); yt0=abs(L*sin(beta));

%data projection on LDA direction
pD1=V(:,2)'*D1;
pD2=V(:,2)'*D2;
[m1,mk1]=min(pD1); [M1,Mk1]=max(pD1);
[m2,mk2]=min(pD2); [M2,Mk2]=max(pD2);
dist=m1-M2; %free distance between projections

%x,y coordinates
xD1=xt0+(pD1*cos(alpha)); yD1=yt0+(pD1*sin(alpha));
xD2=xt0+(pD2*cos(alpha)); yD2=yt0+(pD2*sin(alpha));

%display
figure(1)
scatter(D1(1,:),D1(2,:),32,'k'); hold on;
scatter(D2(1,:),D2(2,:),32,'r');
axis([0 7 0 7]);
plot(meanD(1),meanD(2),'b*','MarkerSize',12); %data centroid

len=7; %line length, arbitrary value
plot([0 len],[b ((m*len)+b)],'k'); %LDA line
plot(xD1,yD1,'go'); %projections of D1
plot(xD2,yD2,'go'); %projections of D2

%visualize some projections
plot([D1(1,mk1),xD1(mk1)],[D1(2,mk1),yD1(mk1)],'b');
plot([D1(1,Mk1),xD1(Mk1)],[D1(2,Mk1),yD1(Mk1)],'b');
plot([D2(1,mk2),xD2(mk2)],[D2(2,mk2),yD2(mk2)],'b');
plot([D2(1,Mk2),xD2(Mk2)],[D2(2,Mk2),yD2(Mk2)],'b');

title('Projections on LDA line');
xlabel('x'); ylabel('y');

dist


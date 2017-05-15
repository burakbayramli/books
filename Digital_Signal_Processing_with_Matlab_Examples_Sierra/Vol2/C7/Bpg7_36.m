% Example of E-M algorithm
% Three clusters of Gaussian data

N=100;
%cluster 1
mux1=3; muy1=3; sigmax1=1; sigmay1=0.4;
x1=normrnd(mux1,sigmax1,1,N);
y1=normrnd(muy1,sigmay1,1,N);
%cluster 2
mux2=10; muy2=4; sigmax2=1; sigmay2=0.8;
x2=normrnd(mux2,sigmax2,1,N);
y2=normrnd(muy2,sigmay2,1,N);
%cluster 3
mux3=6; muy3=7; sigmax3=1; sigmay3=0.6;
x3=normrnd(mux3,sigmax3,1,N);
y3=normrnd(muy3,sigmay3,1,N);

% complete data set
D=zeros(2,3*N);
D(1,:)=[x1,x2,x3];
D(2,:)=[y1,y2,y3];

% centroids: initial guess (3 clusters)
mu=zeros(2,3);
mu(1,1)=D(1,1);mu(2,1)=D(2,1);
mu(1,2)=D(1,120); mu(2,2)=D(2,120);
mu(1,3)=D(1,215); mu(2,3)=D(2,215);
%variance: initial guess
si=2*ones(2,3);

pd=zeros(3,3*N); %for PDF value comparison
mL=zeros(3*N,1); %membership label

%iterations-----------------------------------------------------------------
for it=1:100, 
   mn=zeros(2,3); %new centroid
   nk=zeros(1,3);
%step E 
 %values of PDFs at each datum (vectorized code)
  pd(1,:)=(normpdf(D(1,:),mu(1,1),si(1,1))).*normpdf(D(2,:),mu(2,1),si(2,1));
  pd(2,:)=(normpdf(D(1,:),mu(1,2),si(1,2))).*normpdf(D(2,:),mu(2,2),si(2,2));
  pd(3,:)=(normpdf(D(1,:),mu(1,3),si(1,3))).*normpdf(D(2,:),mu(2,3),si(2,3));
  for nn=1:(3*N),
     k=1; v1=pd(1,nn); v2= pd(2,nn); v3= pd(3,nn); 
     if v1<v2, k=2; end;
     if(k==1 & v1<v3), k=3; end;
     if(k==2 & v2<v3), k=3; end;
     mL(nn)=k; %assign membership label;
     mn(:,k)=mn(:,k)+D(:,nn); nk(k)=nk(k)+1; %accumulate
  end;
%step M
%new centroids
mn(1,:)=mn(1,:)./nk(1,:); %average
mn(2,:)=mn(2,:)./nk(1,:); % "  "
%new variances
for nn=1:(3*N), 
   k=mL(nn); %read label
   si(1,k)=si(1,k)+((D(1,nn)-mn(1,k))^2);
   si(2,k)=si(2,k)+((D(2,nn)-mn(2,k))^2);
end;
for n=1:3,
   si(1,n)=sqrt(si(1,n)/nk(n)); si(2,n)=sqrt(si(2,n)/nk(n));
end;

mu=mn; %change of centroid
end;
%---------------------------------------------------------------------------

%prepare contour display
p=0:0.2:100;
px1=normpdf(p,mu(1,1),si(1,1));py1=normpdf(p,mu(2,1),si(2,1));
pz1=px1'*py1; %matrix
px2=normpdf(p,mu(1,2),si(1,2));py2=normpdf(p,mu(2,2),si(2,2));
pz2=px2'*py2; %matrix
px3=normpdf(p,mu(1,3),si(1,3));py3=normpdf(p,mu(2,3),si(2,3));
pz3=px3'*py3; %matrix


%display
figure(1)
scatter(D(1,:),D(2,:),32,'ro'); hold on; %the data
axis([0 14 0 9]);
plot(mu(1,:),mu(2,:),'b*','MarkerSize',16); hold on; %centroids
contour(p,p,pz1',6); %gaussian PDF
contour(p,p,pz2',6); %"  "  "
contour(p,p,pz3',6); %"  "  "
title('Classification with EM')
xlabel('x'); ylabel('y');

%print mu and sigma for each cluster
mu
si
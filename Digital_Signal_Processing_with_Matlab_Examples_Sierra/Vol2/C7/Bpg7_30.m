% Example of k-means
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
mo=zeros(2,3);
mo(1,1)=D(1,1);mo(2,1)=D(2,1);
mo(1,2)=D(1,120); mo(2,2)=D(2,120);
mo(1,3)=D(1,215); mo(2,3)=D(2,215);

d=zeros(1,3);

%K-means
for it=1:100,
   mn=zeros(2,3); %new centroid
   nk=zeros(1,3);
for nn=1:(3*N),   
   k=1;
   d(1)=(D(1,nn)-mo(1,1))^2+(D(2,nn)-mo(2,1))^2; %distance^2
   d(2)=(D(1,nn)-mo(1,2))^2+(D(2,nn)-mo(2,2))^2; %distance^2
   d(3)=(D(1,nn)-mo(1,3))^2+(D(2,nn)-mo(2,3))^2; %distance^2
   if d(2)<d(1), k=2; end;
   if (k==1 & d(3)<d(1)), k=3; end;
   if (k==2 & d(3)<d(2)), k=3; end;
   mn(:,k)=mn(:,k)+D(:,nn); nk(k)=nk(k)+1; %accumulate
end;
mn(1,:)=mn(1,:)./nk(1,:); %average
mn(2,:)=mn(2,:)./nk(1,:); % "  "

mo=mn; %change of centroid
end;
 
%display
figure(1)
scatter(D(1,:),D(2,:),32,'ro'); hold on;
axis([0 14 0 9]);

title('Estimation of cluster centroids with K-means')
xlabel('x'); ylabel('y');
plot(mn(1,:),mn(2,:),'b*','MarkerSize',16);
plot(mn(1,:),mn(2,:),'ks','MarkerSize',16);

%Marker colors to discern clusters
for nn=1:(3*N),   
   k=1;
   d(1)=(D(1,nn)-mo(1,1))^2+(D(2,nn)-mo(2,1))^2; %distance^2
   d(2)=(D(1,nn)-mo(1,2))^2+(D(2,nn)-mo(2,2))^2; %distance^2
   d(3)=(D(1,nn)-mo(1,3))^2+(D(2,nn)-mo(2,3))^2; %distance^2
   if d(2)<d(1), k=2; end;
   if (k==1 & d(3)<d(1)), k=3; end;
   if (k==2 & d(3)<d(2)), k=3; end;
   %
   if k==1, plot(D(1,nn),D(2,nn),'bx'); end;
   if k==2, plot(D(1,nn),D(2,nn),'gx'); end;
   if k==3, plot(D(1,nn),D(2,nn),'mx'); end;
end;


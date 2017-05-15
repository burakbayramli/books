% Example of K-nearest neighbor
% Three clusters of Gaussian data

K=12; %number of nearest neighbors to consider

N=200;
X=zeros(3*N,2); Y=zeros(3*N);
%cluster 1
mux1=3; muy1=3; sigmax1=1; sigmay1=0.4;
X(1:N,1)=normrnd(mux1,sigmax1,N,1);
X(1:N,2)=normrnd(muy1,sigmay1,N,1);
Y(1:N)=1;
%cluster 2
mux2=8; muy2=3; sigmax2=1; sigmay2=0.4;
X((N+1):(2*N),1)=normrnd(mux2,sigmax2,N,1);
X((N+1):(2*N),2)=normrnd(muy2,sigmay2,N,1);
Y((N+1):(2*N))=2;
%cluster 3
mux3=6; muy3=4; sigmax3=1; sigmay3=0.4;
X((2*N+1):(3*N),1)=normrnd(mux3,sigmax3,N,1);
X((2*N+1):(3*N),2)=normrnd(muy3,sigmay3,N,1);
Y((2*N+1):(3*N))=3;

%new datum to be classified
nx=5.5; ny=3;

%distances from new datum to all (vectorized code)
d=zeros(3*N,1);
nn=1:(3*N);
d(nn)=(X(nn,1)-nx).^2+(X(nn,2)-ny).^2; %distance^2
[od,idx]=sort(d); %sort in ascending order

%see the most voted class 
kix=idx(1:K); %select K neighbors
cl=zeros(3,1); 
aux1=0; aux2=0;
for nn=1:K,
   aux1=kix(nn);
   aux2=Y(aux1); %class value
   cl(aux2)=cl(aux2)+1; %increase votes
end;

[oc,icx]=max(cl); %the most voted class

%display
figure(1)
plot(X(1:N,1),X(1:N,2),'ko'); hold on; %cluster 1, black
plot(X((N+1):(2*N),1),X((N+1):(2*N),2),'rx'); %cluster 2, red
plot(X((2*N+1):(3*N),1),X((2*N+1):(3*N),2),'bd'); %cluster 3, blue
% display classified new datum
switch icx
	case 1,   
		plot(nx,ny,'ks','MarkerSize',12);
   case 2
      plot(nx,ny,'rs','MarkerSize',12);
   case 3
      plot(nx,ny,'bs','MarkerSize',12);
end;
title('K-NN example, with 3 data clusters')
xlabel('x'); ylabel('y');

%print
disp('assigned class')
icx
disp('the votes for each class')
cl
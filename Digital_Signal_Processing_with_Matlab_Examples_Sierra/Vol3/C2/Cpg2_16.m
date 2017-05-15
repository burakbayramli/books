% Example of Matrix Completion 
% (using Douglas-Rachford)
clear all;
disp('working...')

% a random matrix of rank r
n=100; 
r=12; %rank
A=randn(n,r)*randn(r,n); % Original random matrix

disp('nuclear norm of original matrix:');
[u,d,v]=svd(A); 
nuc_N = sum(diag(d))

% a subset of A is measured by 
% sampling at random a number p of entries of A
p=round(n*log(n)*r); % see theory
aux=randperm(n*n); 
ix=aux(1:p)'; %extract p integer random numbers
% b is a column vector:
b=A(ix); %retain a subset of entries of A

% start the algorithm ---------------------------
X=zeros(n,n);
Y=zeros(n,n);
L=n*n;
niter=100;
lambda=1; gamma=2;
rnx=zeros(niter,1);

for nn=1:niter, 
   % X update
    % R(b-M(Y)) term
     O=zeros(L,1);
     % accumulating repeated entries
     for j=1:p;
        ox=ix(j);
        O(ox)=O(ox)+(b(j)-Y(ox)); 
     end;
     Q=reshape(O,[n n]);
   % proxF (indicator function) 
   X=Y+Q;
        
   % Y update
   %proxG (soft thresholding of singular values):
   P=(2*X)-Y;
   [U,D,V]=svd(P);   
   for j=1:n,
       aux=D(j,j);
       if abs(aux)<=gamma,
           D(j,j)=0;
       else
           if aux>gamma, D(j,j)=aux-gamma; end
           if aux<-gamma, D(j,j)=aux+gamma; end;
       end; 
   end;
   S=U*D*V'; % result of thresholding  
   Y=Y+ (lambda*(S-X));   
    
   % recording
   [u,d,v]=svd(X); 
   rnx(nn)=sum(diag(d)); %nuclear norm
   
 end   
  
%display -----------------------------

% evolution of nuclear norm
figure(1)
plot(rnx,'k');
title('evolution of nuclear norm')
xlabel('niter');

% see a matrix row: original and reconstructed
figure(2)
Nrow=10;  %(edit this number)
plot(A(Nrow,:),'r-x'); hold on;
plot(X(Nrow,:),'k');
title('original row: red x; reconstructed: black');

% a measure of error
er=A(Nrow,:)-X(Nrow,:);
E=sum(er.^2)  %to be printed


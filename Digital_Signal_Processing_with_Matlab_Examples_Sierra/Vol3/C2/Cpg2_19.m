% Example of Picture Completion 
% (using Douglas-Rachford)
clear all;
disp('working...')

% original picture
figu=imread('tartan.jpg'); %read picture
F=double(figu);
A=F(1:200,1:200); %crop;
aux=mean(mean(A));
A=A-aux;
n=200;

disp('nuclear norm of original matrix:');
[u,d,v]=svd(A); 
nuc_N = sum(diag(d))


% a subset of A is measured by 
% sampling at random a number p of entries of A
p=10000;
aux=randperm(n*n); 
ix=aux(1:p)'; %extract p integer random numbers
% b is a column vector:
b=A(ix); %retain a subset of entries of A

% start the algorithm---------------------------
X=zeros(n,n);
Y=zeros(n,n);
L=n*n;
niter=30;
lambda=1; gamma=1000; %(notice the value of gamma)
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

figure(2)
imshow(A,[]);
title('original picture')

figure(3)
imshow(X,[]);
title('reconstructed picture')


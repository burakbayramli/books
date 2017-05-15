% decomposition into low-rank (L) and sparse (S) matrices
% Douglas-Rachford

% a random matrix of rank r
n=100; 
r=8; %rank
L0=randn(n,r)*randn(r,n); % a low-rank matrix
% a sparse (diagonal) matrix
nn=1:100;
S0=diag(0.2*nn,0); 

% composite original matrix
M=L0+S0;

% parameter settings
lambda=1;
tk=1; 
Th=3; %Threshold

nnL=30; % number of loops
rnx=zeros(nnL,1); 
L=zeros(n,n); S=zeros(n,n);

% start the algorithm --------------------------------

for nn=1:nnL,
     
    Le=0.5*(M+L-S); Se=0.5*(M-L+S);
    % shrinking---------
    aux1=(2*Le)-L;
    [U D V]=svd(aux1);
    for j=1:n,
        D(j,j)=max(D(j,j)-Th,0);
    end;
    aux=U*D*V';
    L=L+(tk*(aux-Le));
    % soft_threshold---------
    aux1=(2*Se)-S;
    aux=sign(aux1).*max(0, abs(aux1)-lambda);
    S=S+(tk*(aux-Se));
    
   [u,d,v]=svd(L); 
   rnx(nn)=sum(diag(d)); %nuclear norm, record
end;
     
% display ------------------------------
figure(1)
subplot(1,2,1)
imshow(L0,[]);
title('original low-rank matrix')
subplot(1,2,2)
imshow(S0,[]);
title('original sparse matrix')

figure(2)
imshow(M,[]);
title('original composite matrix');

figure(3)
subplot(1,2,1)
imshow(L,[]);
title('recovered low-rank matrix')
subplot(1,2,2)
imshow(S,[]);
title('recovered sparse matrix')



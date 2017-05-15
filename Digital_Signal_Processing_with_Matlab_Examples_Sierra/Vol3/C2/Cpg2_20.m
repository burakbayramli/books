% decomposition into low-rank (L) and sparse (S) matrices
% Alternating Minimization

% load image
figu=imread('wall.jpg'); %read picture
F=double(figu);
n=380;
M=F(1:n,1:n); %crop;
aux=mean(mean(M));
M=M-aux;

% parameter settings
lambda=0.5;
lambF=1.01; 
Th=0.01; %Threshold
rank0=1; %intial rank guess
irk=1; %for rank increments
nnL=50; % number of loops
rank=rank0; %current rank

% start the algorithm --------------------------------
%
[UL SL VL] = lansvd(M, rank, 'L'); %partial SVD
L1=UL*SL*VL'; %initial low-rank approximation
aux=M-L1;
S1= sign(aux).*max(0,abs(aux)-lambda); %shrinkage

for nn=2:nnL,
    if irk==1,
     lambda = lambda * lambF; % lambda is modified in each iteration
     rank = rank + irk;         % rank is increased  "  "  "
    end;
    
     [UL SL VL] = lansvd(M-S1, rank, 'L'); %partial SVD
     L1=UL*SL*VL'; %current low-rank approximation
     aux=M-L1;
     S1= sign(aux).*max(0,abs(aux)-lambda); %shrinkage
     
     % change rank increment when appropriate
     vv=diag(SL);
     rho=vv(end)/sum(vv(1:end-1));
     if rho<Th,
         irk=0;
     else
         irk=1;
     end;
    
end;    
     
% display ------------------------------
figure(1)
imshow(M,[]);
title('original picture')

figure(2)
subplot(1,2,1)
imshow(L1,[]);
title('low-rank component')

subplot(1,2,2)
imshow(S1,[]);
title('sparse component')



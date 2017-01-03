function [Hs,HH]=Hessenberg(A)
%Transform into an almost upper triangular matrix
% having only zeros below lower subdiagonal
N=size(A,1); Hs=A; HH=eye(N); %HH*A*HH'=Hs
for k=1:N-2
   H=Householder(Hs(:,k),   );
   Hs=H*Hs*H; HH=H*HH;
end

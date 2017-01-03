function [Q,R]=qr_my(A)
%QR factorization
N= size(A,1); R=A; Q=eye(N);
for k=1:N-1
H=Householder(R(:,k),k);
R=H*R; %Eq.(P8.4-13)
Q=Q*H; %Eq.(P8.4-15)
end
